unit SortingClasses;

interface

uses
  Windows, SysUtils, Classes, Generics.Defaults, Generics.Collections, Math,
  StringsIO;

type
  // интерфейс для отображениея прогресса сортировки и возможности её прерывания
  ISortProgress = interface
    // добавление текста в лог потока
    procedure ToLog(AThread: TThread; AThreadNum: integer; const AText: string;
      ANewLine: boolean);
    // установка количества активных потоков (для отображения общего прогресса)
    procedure SetThreadCount(ACount: integer);
    // установка прогресса конкретного потока
    procedure SetThreadPorgress(AThreadNum: integer; AProgress: double);
    // проверка на завершение (прерывание)
    function Terminating: boolean;
  end;

  // поток управлющий сортировкой - поэтапно запускает потоки сортировки
  TSortManager = class(TThread)
  private
    Progress: ISortProgress;
    InputFn: string;
    OutputFn: string;
    TmpPath: string;
  protected
    procedure Execute; override;
  public
    // значения для считывания в OnTerminate:
    Res: boolean;        // признак успешного завершения
    StrCount: int64;     // кол-во отсортированных строк
    WorkTime: integer;   // затрачено времени на сортировку, сек

    constructor Create(AInputFn, AOutputFn: string; AProgress: ISortProgress;
      AOnTerminate: TNotifyEvent);
    procedure DoTerminate; override;
    function TmpFn(AThreadNum, AFileNum: integer; AMergeOut: boolean = false): string;
  end;

  // базовый класс, содержащий общие поля и методы для разных типов сортировки
  TBaseSortThread = class(TThread)
  private const
    // прогресс потока будет сообщатся (вызов ISortProgress.SetThreadPorgress)
    // не чаще данного интервала времени, мсек
    ProgressNotifyPeriod = 200;
  protected
    Progress: ISortProgress;
    // номер потока
    ThreadNum: integer;
    // флаг определяющий файл для записи очередной серии (первый или второй)
    CurOutPlace: boolean;
    // кол-во записаных серий (в оба файла)
    RunsWritten: integer;
    // прогресс при работе будет изменяться от ProgBgn до ProgBgn + ProgLn
    ProgBgn, ProgLn: double;
    // время отправки последнего оповещения о прогрессе, мсек
    LastNotify: dword;
    // разделитель для логов - строка вида '------------------------------'
    LogDelimiter: string;

    procedure ToLog(const AText: string; ANewLine: boolean = True);
    procedure ProgressNotify(AReader: TStringReader);
    procedure DoTerminate; override;
    constructor Create(AThreadNum: integer; AProgress: ISortProgress;
      AProgBgn, AProgLn: double);
  end;

  // класс считываниет строки из фрагмента исходного файла порциями,
  // сортирует их (формирует серии) и поочерёдно записывает их в два
  // результирующих файла
  TQuickSort = class(TBaseSortThread)
  private
    Reader: TStringReader;
    Writer1: TStringsWriter;
    Writer2: TStringsWriter;
    StrList: TObjectList<TString>;

    procedure SortAndFlush;
  protected
    procedure Execute; override;
  public
    StrCount: int64;   // кол-во обработанных строк

    constructor Create(AThreadNum: integer; AManger: TSortManager; AInuptFn: string;
        ABgnPos, AEndPos: int64; AProgBgn, AProgLn: double; AAvailMem: integer);
    destructor Destroy; override;
  end;

  // класс осуществляет слияние серий из двух исходных файлов. получаемые
  // в результате серии поочерёдно записывает в два результирующих файла
  TMergePass = class(TBaseSortThread)
  private
    CurOut: TStringsWriter;

    procedure WriteLn(AStr: TString);
  protected
    I1fn, I2fn, O1fn, O2fn: string;
    i1, i2: TRunReader;
    o1, o2: TStringsWriter;
    IOBufSize: integer;

    procedure Pass;
    procedure Execute; override;
  public
    constructor Create(AThreadNum: integer; AManger: TSortManager;
        const Ai1fn, Ai2fn, Ao1fn, Ao2fn: string; AProgBgn, AProgLn: double;
        AAvailMem: integer);
  end;

  // класс многокртано повторяет слияние серий из двух файлов (функционал
  // класса TMergePass) до тех пор, пока не будет получена лишь одна серия (в
  // одном файле)
  TMergeSort = class(TMergePass)
  protected
    // границы прогресса (от GProgBgn до GProgBgn + GProgLn) по всем проходам
    GProgBgn, GProgLn: double;

    procedure Execute; override;
  public
    constructor Create(AThreadNum, ARunsCnt: integer; AManger: TSortManager;
        AProgBgn, AProgLn: double; AAvailMem: integer);
  end;

implementation

{ TBaseSortThread }

constructor TBaseSortThread.Create(AThreadNum: integer;
  AProgress: ISortProgress; AProgBgn, AProgLn: double);
begin
  inherited Create(True);
  LogDelimiter := StringOfChar('-', 44);
  ThreadNum := AThreadNum;
  Progress := AProgress;
  ProgBgn := AProgBgn;
  ProgLn := AProgLn;
end;

procedure TBaseSortThread.ProgressNotify(AReader: TStringReader);
// метод сообщает о прогрессе, но не чаще ProgressNotifyPeriod,
// предпологается достаточно частый вызов данного метода из потомков.
// также проверяет флаг завершения работы потока (IProgress.Terminating)
var
  tick: dword;
begin
  tick := GetTickCount;
  if tick - LastNotify >= ProgressNotifyPeriod then
  begin
    LastNotify := tick;
    Progress.SetThreadPorgress(ThreadNum, ProgBgn + ProgLn * AReader.Progress);
  end;

  if Progress.Terminating then
    Abort;
end;

procedure TBaseSortThread.ToLog(const AText: string; ANewLine: boolean);
begin
  Progress.ToLog(Self, ThreadNum, AText, ANewLine);
end;

procedure TBaseSortThread.DoTerminate;
// проверка при завершении потока на исключения
var
  e: Exception;
begin
  e := Exception(FatalException);
  if Assigned(e) then
    if e is EAbort then
      ToLog('ПРЕРВАНО ПОЛЬЗОВАТЕЛЕМ')
    else
      ToLog('ОШИБКА: ' + e.Message);
end;

{ TQuickSort }

constructor TQuickSort.Create(AThreadNum: integer; AManger: TSortManager;
  AInuptFn: string; ABgnPos, AEndPos: int64; AProgBgn, AProgLn: double;
  AAvailMem: integer);
const
  io_buf_size = 8192;
var
  i, lst_count: integer;
  o1fn, o2fn: string;
begin
  inherited Create(AThreadNum, AManger.Progress, AProgBgn, AProgLn);

  // кол-во строк в листе, при котором будет использоваться не более AAvailMem
  // байт под них. 3 - кол-во потоков ввода-вывода.
  lst_count := (AAvailMem - 3 * io_buf_size) div TString.MaxLn;
  Assert(lst_count > 0, 'Не достаточно памяти');
  o1fn := AManger.TmpFn(AThreadNum, 1);
  o2fn := AManger.TmpFn(AThreadNum, 2);
  Reader := TStringReader.CreatePartial(AInuptFn, ABgnPos, AEndPos, io_buf_size);
  Writer1 := TStringsWriter.Create(o1fn, io_buf_size);
  Writer2 := TStringsWriter.Create(o2fn, io_buf_size);

  StrList := TObjectList<TString>.Create(TComparer<TString>.Construct(TString.Compare));
  for i := 1 to lst_count do
    StrList.Add(TString.Create);

  ToLog(Format('Формирование cерий по %d строк (быстрая сортировка) из %s, фрагмент %d - %d, ' +
      'в файлы: %s, %s ...', [lst_count,
                              ExtractFileName(AInuptFn),
                              ABgnPos, AEndPos,
                              ExtractFileName(o1fn),
                              ExtractFileName(o2fn)]));
  Resume;
end;

destructor TQuickSort.Destroy;
begin
  Reader.Free;
  Writer1.Free;
  Writer2.Free;
  StrList.Free;
  inherited;
end;

procedure TQuickSort.Execute;
var
  i: integer;
  end_file: boolean;
begin
  end_file := false;

  repeat
    // считываем очередную порцию строк
    for i := 0 to StrList.Count - 1 do
      if not Reader.ReadLn(StrList[i]) then
      begin
        end_file := true;
        break;
      end;

    if end_file then
      if i = 0 then
        break
      else
        StrList.Count := i;

    // сортируем быстрой сортировкой и сохраняем в один из файлов
    SortAndFlush;

  until end_file;

  ToLog(IntToStr(RunsWritten) + ' серий записано');
  ToLog(LogDelimiter);
end;

procedure TQuickSort.SortAndFlush;
var
  writer: TStringsWriter;
  i: integer;

begin
  CurOutPlace := not CurOutPlace;  // чередование файлов
  if CurOutPlace then
    writer := Writer1
  else
    writer := Writer2;

  StrList.Sort;
  for i := 0 to StrList.Count - 1 do  // запись серии в файл
    writer.WriteLn(StrList[i]);
  inc(RunsWritten);
  inc(StrCount, StrList.Count);

  ProgressNotify(Reader);
end;

{ TMergePass }

constructor TMergePass.Create(AThreadNum: integer; AManger: TSortManager;
  const Ai1fn, Ai2fn, Ao1fn, Ao2fn: string; AProgBgn, AProgLn: double;
  AAvailMem: integer);
// если исходные файлы содержат лишь по одной серии (т.е. полностью
// отсортированы), возможен вызов конструктора с O2fn = EmptyStr
begin
  inherited Create(AThreadNum, AManger.Progress, AProgBgn, AProgLn);
  I1fn := Ai1fn; I2fn := Ai2fn;
  O1fn := Ao1fn; O2fn := Ao2fn;
  if O2fn <> EmptyStr then
    IOBufSize := AAvailMem div 4
  else
  begin
    // использование для слияния двух полностью отсортированных файлов в третий
    IOBufSize := AAvailMem div 3;
    ToLog('Слияние ' + Format('(%s, %s) -> %s', [ExtractFileName(I1fn),
                                                 ExtractFileName(I2fn),
                                                 ExtractFileName(O1fn)]));
    Resume;
  end;
end;

procedure TMergePass.Execute;
begin
  try
    i1 := TRunReader.Create(I1fn, IOBufSize);
    i2 := TRunReader.Create(I2fn, IOBufSize);
    o1 := TStringsWriter.Create(O1fn, IOBufSize);
    if O2fn <> EmptyStr then
      o2 := TStringsWriter.Create(O2fn, IOBufSize);

    Pass;
  finally
    FreeAndNil(i1); FreeAndNil(i2);
    FreeAndNil(o1); FreeAndNil(o2);
  end;

  if O2fn = EmptyStr then
    ToLog(LogDelimiter);
end;

procedure TMergePass.WriteLn(AStr: TString);
begin
  CurOut.WriteLn(AStr);
  ProgressNotify(i1);
end;

procedure TMergePass.Pass;
// слияние серий из двух файлов-источников
begin
  RunsWritten := 0;
  CurOutPlace := False;

  repeat
    CurOutPlace := not CurOutPlace; // чередование выходных файлов
    if CurOutPlace then
      CurOut := o1
    else
      CurOut := o2;

    // слияние очередных серий
    while (not i1.EndRun) and (not i2.EndRun) do
     if TString.Compare(i1.Peek, i2.Peek) <= 0 then
       WriteLn(i1.Get)
     else
       WriteLn(i2.Get);

    while (not i1.EndRun) do
      Writeln(i1.Get);

    while (not i2.EndRun) do
      Writeln(i2.Get);

    i1.NextRun;
    i2.NextRun;

    inc(RunsWritten);

  until i1.EndRun and i2.EndRun;
  // если после NextRun, EndRun продолжает возвращать истину - файл закончился
end;

{ TMergeSort }

constructor TMergeSort.Create(AThreadNum, ARunsCnt: integer; AManger: TSortManager;
  AProgBgn, AProgLn: double; AAvailMem: integer);
begin
  inherited Create(AThreadNum, AManger,
                               AManger.TmpFn(AThreadNum, 1, False),
                               AManger.TmpFn(AThreadNum, 2, False),
                               AManger.TmpFn(AThreadNum, 1, True),
                               AManger.TmpFn(AThreadNum, 2, True), 0, 0, AAvailMem);

  RunsWritten := ARunsCnt;
  GProgBgn := AProgBgn; GProgLn := AProgLn;

  if ARunsCnt > 1 then
  begin
    ToLog('Двухпутевое слияние:');
    ToLog(Format('(%s, %s) -> (%s, %s)', [ExtractFileName(I1fn),
                                          ExtractFileName(I2fn),
                                          ExtractFileName(O1fn),
                                          ExtractFileName(O2fn)]));
  end;
  Resume;
end;

procedure TMergeSort.Execute;
var
  pass_cnt: integer;
  pass_remains: integer;
begin
  pass_cnt := 0;
  while RunsWritten > 1 do
  begin
    // кол-во оставшихся проходв
    pass_remains := Ceil(Log2(RunsWritten));
    // рассчитываем диапазон прогресса для очередного прохода
    ProgBgn := GProgBgn + GProgLn * pass_cnt / (pass_cnt + pass_remains);
    ProgLn := GProgLn / (pass_cnt + pass_remains);
    inc(pass_cnt);
    ToLog('Проход ' + IntToStr(pass_cnt) + '... ');
    // производим проход
    inherited Execute;
    ToLog(IntToStr(RunsWritten) + ' серий записано', False);
    // удаляем исходные файлы и переименовываем результирующие в исходные
    Assert(DeleteFile(I1fn) and MoveFile(PChar(O1fn), PChar(I1fn)));
    Assert(DeleteFile(I2fn) and MoveFile(PChar(O2fn), PChar(I2fn)));
  end;

  // в последнем проходе была записана лишь одна серия (в файл I1fn),
  // поэтому файл I2fn пустой, удаляем его
  DeleteFile(I2fn);

  if pass_cnt > 0 then
    ToLog(LogDelimiter);
end;

{ TSortManagerThread }

constructor TSortManager.Create(AInputFn, AOutputFn: string;
  AProgress: ISortProgress; AOnTerminate: TNotifyEvent);
begin
  inherited Create(True);
  InputFn := AInputFn;
  OutputFn := AOutputFn;
  TmpPath := ExtractFilePath(OutputFn) + 'Temp\';
  Progress := AProgress;
  OnTerminate := AOnTerminate;
  FreeOnTerminate := True;
  Resume;
end;

function TSortManager.TmpFn(AThreadNum, AFileNum: integer;
  AMergeOut: boolean): string;
// формирование имени временного файла
begin
  Result := TmpPath + IntToStr(AThreadNum);
  if AFileNum > 0 then
    Result := Result + '_' + IntToStr(AFileNum);
  if AMergeOut then
    Result := Result + 'a';
  Result := Result + '.txt';
end;

procedure TSortManager.Execute;
// метод последовательно реализует все этапы сортировки
const
  qstm = 0.12;       // прогресс под этап формирования серий быстрой сортировкой
  mstm = 1 - qstm;   // остальной прогресс - под слияния
  mb = 1024 * 1024;  // доступная память - один магабайт
  min_file_size = TString.MaxLn * 4;
var
  s: ansistring;
  stm: TFileStream;
  i: integer;
  quarter: int64;
  file_pos: int64;
  cr_pos: integer;
  parts: array[0..4] of int64;
  th: array[1..4] of TBaseSortThread;
  runs: array[1..4] of integer;
  start_time: dword;
  pc: integer; // passes count - предпологаемое количество проходов TMergeSort
begin
  Assert(InputFn <> OutputFn, 'Исходный и результирующий файлы должны различаться!');

  if not DirectoryExists(TmpPath) then
    Assert(CreateDir(TmpPath), 'Не удалось создать каталог ' + TmpPath);

  if FileExists(OutputFn) then
    Assert(DeleteFile(OutputFn), 'Не удалось удалить файл ' + OutputFn);

  start_time := GetTickCount;

  // разбиваем исходный файл на 4 части, границы частей выравниваем
  // по границам сторк. результат - массив parts
  stm := TFileStream.Create(InputFn, fmOpenRead or fmShareDenyNone);
  try
    Assert(stm.Size >= min_file_size, 'Рзамер исходного файла должен быть ' +
                              'не меньше ' + IntToStr(min_file_size) + ' байт');
    quarter := stm.Size div 4;
    SetLength(s, TString.MaxLn + 3);
    // в строке длиной MaxLn + 2, могло бы оказаться: lf, MaxLn символов, cr.
    // тогда поиск CrLf не дал бы результата. поэтому берём длину MaxLn + 3

    parts[0] := 0;
    for i := 1 to 3 do
    begin
      file_pos := quarter * i;
      stm.Seek(file_pos, soBeginning);  // считываем MaxLn + 3 символов от
      stm.ReadBuffer(s[1], Length(s));  // границы четверти,
      cr_pos := Pos(CrLf, s);           // ищем CrLf
      Assert(cr_pos > 0, TooLongStr);
      parts[i] := file_pos + cr_pos + 1;  // корректируем file_pos
    end;
    parts[4] := stm.Size;
  finally
    stm.Free;
  end;

  // начало сортировки.
  // каждый поток формирует по 2 файла с сериями,
  // сортируя быстрой сортировкой порции строк исходной четверти
  Progress.SetThreadCount(4);
  for i := 1 to 4 do
    th[i] := TQuickSort.Create(i, Self, InputFn, parts[i - 1], parts[i],
                                  0, qstm,
                                  mb div 4);
  // дожидаемся завершения потоков и считываем результаты
  Res := True;
  for i := 1 to 4 do
  begin
    th[i].WaitFor;
    inc(StrCount, TQuickSort(th[i]).StrCount);
    runs[i] := th[i].RunsWritten;
    Res := Res and (th[i].FatalException = nil);
    th[i].Free;
  end;
  if not Res then
    Exit;

  // расчёт предпологаемого количества проходов TMergeSort.
  // производдится для отображения дальнейшего прогресса - условно разбиваем
  // оставшийся прогресс на pc + 6 частей
  pc := Ceil(Log2((runs[1] + runs[2] + runs[3] + runs[4]) / 4));
  // запускаем итеративное слияние полученных пар файлов;
  // диапазон прогресса - [0; pc / (pc + 6)]
  for i := 1 to 4 do
    th[i] := TMergeSort.Create(i, runs[i], Self, qstm, mstm * pc / (pc + 6),
                                                                  mb div 4);
  // дожидаемся завершения потоков
  for i := 1 to 4 do
  begin
    th[i].WaitFor;
    res := res and (th[i].FatalException = nil);
    th[i].Free;
  end;
  if not res then
    Exit;

  // имеем 4 файла, запускаем 2 потока слияния;
  // поргрессс - [pc / (pc + 6); (pc + 2) / (pc + 6)]
  Progress.SetThreadCount(2);
  for i := 1 to 2 do
    th[i] := TMergePass.Create(i, Self,
                                  TmpFn(2 * i - 1, 1),
                                  TmpFn(2 * i, 1),
                                  TmpFn(i, 0),
                                  EmptyStr,
                                  qstm + mstm * pc / (pc + 6),
                                  mstm * 2 / (pc + 6),
                                  mb div 2);
  for i := 1 to 2 do
  begin
    th[i].WaitFor;
    res := res and (th[i].FatalException = nil);
    th[i].Free;
  end;
  if not res then
    Exit;

  for i := 1 to 4 do
  begin
    DeleteFile(TmpFn(i, 1, False));
    DeleteFile(TmpFn(i, 2, False));
  end;

  // имеем 2 файла, запускаем поток слияния;
  // поргрессс - [(pc + 2) / (pc + 6); 1]
  Progress.SetThreadCount(1);
  th[1] := TMergePass.Create(1, Self,
                                TmpFn(1, 0),
                                TmpFn(2, 0),
                                OutputFn,
                                EmptyStr,
                                qstm + mstm * (pc + 2) / (pc + 6),
                                mstm * 4 / (pc + 6),
                                mb);

  th[1].WaitFor;
  res := th[1].FatalException = nil;
  th[1].Free;
  if not res then
    Exit;

  Progress.SetThreadPorgress(1, 1);
  WorkTime := round((GetTickCount - start_time) / 1000);
end;

procedure TSortManager.DoTerminate;
// завершение потока - удаляем временные файлы
var
  i, j: integer;
begin
  for i := 1 to 4 do
    for j := 1 to 2 do
    begin
      DeleteFile(TmpFn(i, j));
      DeleteFile(TmpFn(i, j, True));
    end;

  DeleteFile(TmpFn(1, 0));
  DeleteFile(TmpFn(2, 0));
  RemoveDir(TmpPath);
  inherited;
end;

end.
