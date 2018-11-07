unit SortingDemoForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Buttons, SortingClasses;

type
  // форма запускает/останавливает сортировку и отображает прогресс
  TSortFm = class(TForm, ISortProgress)
    btnStartStop: TButton;
    Memo1: TMemo;
    pb: TProgressBar;
    Memo2: TMemo;
    Memo3: TMemo;
    Memo4: TMemo;
    edInputFn: TEdit;
    Label1: TLabel;
    edOutputFn: TEdit;
    Label2: TLabel;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    btnSelectInputFile: TBitBtn;
    btnSelectOutputFile: TBitBtn;
    procedure btnStartStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSelectInputFileClick(Sender: TObject);
    procedure btnSelectOutputFileClick(Sender: TObject);
  private const
    _General = 'General';
    _InputFn = 'InputFn';
    _OutputFn = 'OutputFn';
  private
    IniFn: string;
    Memo: array[1..4] of TMemo;
    ThreadCnt: integer;
    ThreadProgress: array[1..4] of integer;
    SortActive: boolean;          // признак работы сортировки
    SortTerminating: boolean;     // флаг завершения сортировки

    procedure SaveFileNamesToIniFile;
    procedure OnThreadNotifyProgress(var Msg: TMessage); message WM_APP;
    procedure OnSortManagerThreadTerminate(Sender: TObject);

    // потоко-безопасная реализация ISortProgress
    procedure ToLog(AThread: TThread; AThreadNum: integer; const AText: string;
      ANewLine: boolean);
    procedure SetThreadCount(ACount: integer);
    procedure SetThreadPorgress(AThreadNum: integer; AProgress: double);
    function Terminating: boolean;

  end;

var
  SortFm: TSortFm;

implementation

{$R *.dfm}

uses IniFiles;

procedure TSortFm.ToLog(AThread: TThread; AThreadNum: integer;
  const AText: string; ANewLine: boolean);
// добавление текста в лог потока
begin
  TThread.Synchronize(AThread,
    procedure
    var
      strs: TStrings;
    begin
      if ANewLine then
        Memo[AThreadNum].Lines.Add(AText)
      else
      begin
        strs := Memo[AThreadNum].Lines;
        strs[strs.Count - 1] := strs[strs.Count - 1] + AText;
      end;
    end);
end;

procedure TSortFm.SetThreadCount(ACount: integer);
// установка количества активных потоков (для отображения общего прогресса)
begin
  PostMessage(Handle, WM_APP, 0, ACount);
end;

procedure TSortFm.SetThreadPorgress(AThreadNum: integer; AProgress: double);
// поток сообщает свой прогресс
begin
  PostMessage(Handle, WM_APP, AThreadNum, round(AProgress * 1000));
end;

function TSortFm.Terminating: boolean;
// поток проверяет флаг завершения
begin
  Result := SortTerminating;
end;

procedure TSortFm.FormCreate(Sender: TObject);
begin
  IniFn := ChangeFileExt(ParamStr(0), '.ini');
  with TIniFile.Create(IniFn) do
  try
    edInputFn.Text := ReadString(_General, _InputFn, 'Input.txt');
    edOutputFn.Text := ReadString(_General, _OutputFn, 'Output.txt');
  finally
    Free;
  end;

  Application.Title := Caption;
  Memo[1] := Memo1; Memo[2] := Memo2;
  Memo[3] := Memo3; Memo[4] := Memo4;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  Constraints.MinHeight := Height;
end;

procedure TSortFm.FormDestroy(Sender: TObject);
begin
  // останавливаем сортировку (если запущена)
  SortTerminating := True;
  // ожидаем завершения сортировки
  while SortActive do
    Application.HandleMessage;
  SaveFileNamesToIniFile;
end;

procedure TSortFm.SaveFileNamesToIniFile;
begin
  try
    with TIniFile.Create(IniFn) do
    try
      WriteString(_General, _InputFn, edInputFn.Text);
      WriteString(_General, _OutputFn, edOutputFn.Text);
    finally
      Free;
    end;
  except
    on E: EIniFileException do ;
    else raise;
  end;
end;

procedure TSortFm.btnSelectInputFileClick(Sender: TObject);
begin
  OpenDialog.InitialDir := ExtractFileDir(edInputFn.Text);
  if OpenDialog.Execute then
  begin
    edInputFn.Text := OpenDialog.FileName;
    SaveFileNamesToIniFile;
  end;
end;

procedure TSortFm.btnSelectOutputFileClick(Sender: TObject);
begin
  SaveDialog.InitialDir := ExtractFileDir(edOutputFn.Text);
  if SaveDialog.Execute then
  begin
    edOutputFn.Text := SaveDialog.FileName;
    SaveFileNamesToIniFile;
  end;
end;

procedure TSortFm.btnStartStopClick(Sender: TObject);
var
  i: integer;
begin
  if not SortActive then
  begin
    // запуск сортировки
    SaveFileNamesToIniFile;
    for i := 1 to 4 do
      Memo[i].Clear;
    btnStartStop.Caption := 'Стоп';
    SortActive := True;
    SortTerminating := False;
    TSortManager.Create(edInputFn.Text, edOutputFn.Text, Self,
                        OnSortManagerThreadTerminate);
  end
  else // или остановка
    SortTerminating := True;
end;

procedure TSortFm.OnSortManagerThreadTerminate(Sender: TObject);
// обработка завершения сортировки (потоко-безопасный вызов)
var
  e: Exception;
  th: TSortManager;
begin
  btnStartStop.Caption := 'Старт';
  th := TSortManager(Sender);
  e := Exception(th.FatalException);

  // сообщаем пользоватею результат работы
  if Assigned(e) then
    Application.ShowException(e)
  else if th.Res then
    Application.MessageBox(PChar(
         Format('%d строк отсортировано за %d сек.', [th.StrCount, th.WorkTime])),
                                  PChar(Application.Title), MB_ICONINFORMATION);

  SortActive := False;
  PostMessage(Handle, WM_APP, 0, 1); // на случай ожидания в FormDestroy
end;

procedure TSortFm.OnThreadNotifyProgress(var Msg: TMessage); // message WM_APP;
// обработка сообщений WM_APP, отправляемых из потоко-безопасных
// вызовов SetThreadCount и SetThreadPorgress
var
  i: integer;
  sum: integer;
begin
  if Msg.WParam = 0 then
    ThreadCnt := Msg.LParam
  else if Msg.WParam in [1..4] then
  begin
    // обновился прогресс по потоку
    ThreadProgress[Msg.WParam] := Msg.LParam;

    // подсчитываем общий прогресс
    sum := 0;
    for i := 1 to ThreadCnt do
      Inc(sum, ThreadProgress[i]);
    pb.Position := round(sum / ThreadCnt);
  end;
end;

end.
