unit StringsIO;

interface

uses SysUtils, Classes;

type
  // стандартные строки Delphi, будучи динамическими, плохо подходят для
  // интенсивной работы в параллельных потоках -
  // менеджер памяти обслужвает запросы последоватльно, что
  // приводит к простою в потоках и существенному снижению производительности


  // строка с фиксированным буфером. для удобства сравнения предполагается
  // налчие ноля после последнго символа (если строка короче CompareLn).
  TString = class
  public const
    MaxLn = 500;
    CompareLn = 50;  // кол-во сравниваемых символов
  public
    Ln: word;
    Data: array[0..MaxLn - 1] of byte;

    class function Compare(const Left, Right: TString): Integer; static;
  end;

  // вспомогательный класс, обеспечивающий работу с частью файла как с файлом
  // меньшего размера (ограничивает четние)
  TPartialFileStream = class(TFileStream)
  private
    Limit: int64;
  public
    constructor Create(const AFileName: string; AMode: Word;
      ABgnPos, AEndPos: int64);
    function Read(var Buffer; Count: Longint): Longint; override;
  end;

  // класс реализует построчное чтение текстового файла
  TStringReader = class
  private
    InputFile: TFileStream;
    // указатель на буфер
    Buffer: PByte;
    BufSize: integer;
    // текущая позиция (курсор) и указатель на конец данных в буфере
    BufPos, BufEnd: PByte;
    FileSize: int64;
    // кол-во байт прочитанных из файла (не считая данных в буфере)
    BaseFileOffset: int64;

    function FillBuffer: boolean;
    procedure InitBuffer(ABufSize: integer);
  public
    constructor Create(const AFileName: string; ABufSize: integer);
    constructor CreatePartial(const AFileName: string; ABgnPos, AEndPos: int64;
        ABufSize: integer);
    destructor Destroy; override;
    function ReadLn(AStr: TString): boolean;
    function Progress: double;
  end;

  // класс реализует построчное чтение текстового файла, попутно отслеживая
  // границы серий (отстортированных последовательностей строк)
  TRunReader = class(TStringReader)
  private
    s1, s2: TString;
    FEndRun: boolean;  // признак конца серии
    NoMore: boolean;   // признак конца файла
    PrevStr: TString;
    CurStr: TString;
  public
    constructor Create(const AFileName: string; ABufSize: integer);
    destructor Destroy; override;

    // метод возвращает очередную строку, и читает следующую; при этом
    // новопрочитанная строка становится "очередной", и сравнивается с
    // возвращённой; по результату сравнения выставляется флаг FEndRun
    function Get: TString;
    // сброс флага FEndRun, но только если файл ещё не до конца прочитан
    procedure NextRun;
    // свойство даёт возможность просмотреть "очередную" строку без продвижения
    // по файлу
    property Peek: TString read CurStr;
    property EndRun: boolean read FEndRun;
  end;

  // класс реализует построчную запись текстового файла с буферизацией.
  // предполагается, что размер буфера должен быть не меньше TString.MaxLn + 2
  TStringsWriter = class(TFileStream)
    Buffer: PByte;
    DataSize: Integer;
    BufSize: integer;
  public
    constructor Create(const AFileName: string; ABufSize: integer);
    destructor Destroy; override;
    procedure WriteLn(AStr: TString);
  end;

const
  CrLf: ansistring = #13#10;
  TooLongStr = 'Обнаружена слишком длинная строка';

implementation

{ TString }

class function TString.Compare(const Left, Right: TString): Integer; assembler;
// сравнение двух TString
asm
    push  esi
    push  edi

    mov   esi, Left
    mov   edi, Right
    xor   eax, eax
    movzx ecx, word [esi.Ln]
    movzx edx, word [edi.Ln]
    cmp   ecx, edx
    ja    @0                   // определяем кол-во байт для сравнения -
    jb    @1                   // минимальная из длин строк + 1,
    or    ecx, ecx             // но в случае если длины строк равны
    jz    @4                   // (и ненулевые), то
    jmp   @2                   // прибавление единицы не нужно
@0: mov   ecx, edx
@1: inc   ecx
@2: cmp   ecx, CompareLn
    jbe   @3
    mov   ecx, CompareLn

@3: add   esi, offset Data
    add   edi, offset Data
    repe  cmpsb
    xor   edx, edx
    mov   al, [esi - 1]
    mov   dl, [edi - 1]
    sub   eax, edx             // возвращаем разность между символами, на
                               // которых сравнение остановилось
@4: pop   edi
    pop   esi
end;

{ TPartialFileStream }

constructor TPartialFileStream.Create(const AFileName: string; AMode: Word;
  ABgnPos, AEndPos: int64);
begin
  inherited Create(AFileName, AMode);
  Seek(ABgnPos, soBeginning);
  Limit := AEndPos;
end;

function TPartialFileStream.Read(var Buffer; Count: Integer): Longint;
// ограничение чтения из файла
begin
  if Position + Count > Limit then
    Count := Limit - Position;
  Result := inherited Read(Buffer, Count);
end;

{ TStringReader }

constructor TStringReader.Create(const AFileName: string; ABufSize: integer);
begin
  InputFile := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  FileSize := InputFile.Size;
  InitBuffer(ABufSize);
end;

constructor TStringReader.CreatePartial(const AFileName: string; ABgnPos,
  AEndPos: int64; ABufSize: integer);
begin
  InputFile := TPartialFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone,
                                                              ABgnPos, AEndPos);
  FileSize := AEndPos - ABgnPos;
  InitBuffer(ABufSize);
end;

procedure TStringReader.InitBuffer(ABufSize: integer);
begin
  BufSize := ABufSize;
  GetMem(Buffer, BufSize);
  BufPos := Buffer;
  FillBuffer;
end;

destructor TStringReader.Destroy;
begin
  InputFile.Free;
  FreeMem(Buffer, BufSize);
  inherited;
end;

function TStringReader.Progress: double;
// прогресс чтения файла
begin
  Result := (BaseFileOffset + BufPos - Buffer) / FileSize;
end;

function TStringReader.FillBuffer: boolean;
// заполнение буфера данными полнстью, или меньше (если в файле осталось
// данных меньше чем размер буфера)
begin
  inc(BaseFileOffset, BufPos - Buffer);
  BufPos := Buffer;
  BufEnd := Buffer + InputFile.Read(Buffer^, BufSize);
  Result := BufPos <> BufEnd;
end;

function TStringReader.ReadLn(AStr: TString): boolean;
// чтение очередной строки
var
  start: PByte;
  ln: integer;
begin
  // буфер должен быть не пуст (это гарантирует предыдущий вызов ReadLn либо
  // InitBuffer). если буфер пуст - файл прочитан полностью.
  Result := BufPos <> BufEnd;
  if not Result then
    Exit;

  AStr.Ln := 0;
  repeat
    start := BufPos;
    asm                  // ищем в буфере символ #13
      mov   edx, edi
      mov   eax, Self
      mov   edi, [eax.BufPos]
      mov   ecx, [eax.BufEnd]
      sub   ecx, edi
      mov   al, 13
      repne scasb
      jnz   @1
      dec   edi
@1:
      mov   eax, Self
      mov   [eax.BufPos], edi  // BufPos теперь указывает на найденный символ,
      mov   edi, edx           // либо на конец буфера
    end;

    // присоединяем часть буфера от start до BufPos к AStr
    ln := BufPos - start;
    Assert(AStr.Ln + ln <= TString.MaxLn, TooLongStr);
    Move(start^, AStr.Data[AStr.Ln], ln);
    Inc(AStr.Ln, ln);
  until (BufPos <> BufEnd) or not FillBuffer;
  // выходим из цикла если нашли сивол #13, либо файл закончился

  if AStr.Ln < TString.CompareLn then
    AStr.Data[AStr.Ln] := 0;            // ноль после последнего символа

  if BufPos = BufEnd then     // если файл закончился -
    Exit;                     // больше делать нечего

  inc(BufPos);                // смещаемся на следующий символ,
  if BufPos = BufEnd then
    if not FillBuffer then
      Exit;
  if BufPos^ = 10 then        // если он оказался равным #10, то пропускаем его
  begin
    inc(BufPos);
    if BufPos = BufEnd then
      FillBuffer;
  end;
end;

{ TStringsReader }

constructor TRunReader.Create(const AFileName: string; ABufSize: integer);
begin
  inherited Create(AFileName, ABufSize);

  s1 := TString.Create;
  s2 := TString.Create;

  if ReadLn(s1) then
    CurStr := s1
  else
  begin
    FEndRun := True;
    NoMore := True;
  end;
end;

destructor TRunReader.Destroy;
begin
  s1.Free;
  s2.Free;
  inherited;
end;

function TRunReader.Get: TString;
begin
  Result := CurStr;

  if CurStr = s1 then
  begin
    PrevStr := s1;
    CurStr := s2;
  end
  else
  begin
    PrevStr := s2;
    CurStr := s1;
  end;

  if not ReadLn(CurStr) then
  begin                       // если нет больше сторк (не с чем сравнивать) -
    FEndRun := True;          // это тоже конец серии
    NoMore := True;
  end
  else
    FEndRun := TString.Compare(CurStr, PrevStr) < 0;
end;

procedure TRunReader.NextRun;
begin
  if not NoMore then
    FEndRun := False;
end;

{ TStringsWriter }

constructor TStringsWriter.Create(const AFileName: string; ABufSize: integer);
begin
  inherited Create(AFileName, fmCreate);
  BufSize := ABufSize;
  Assert(BufSize >= TString.MaxLn + 2, 'Недостаточный размер буфера');
  GetMem(Buffer, BufSize);
end;

destructor TStringsWriter.Destroy;
begin
  Write(Buffer^, DataSize);
  FreeMem(Buffer, BufSize);
  inherited;
end;

procedure TStringsWriter.WriteLn(AStr: TString);
begin
 if DataSize + AStr.Ln + 2 > BufSize then
 begin
   WriteBuffer(Buffer^, DataSize);
   DataSize := 0;
 end;

 Move(Astr.Data, (Buffer + DataSize)^, Astr.Ln);
 Inc(DataSize, Astr.Ln);
 Move(CrLf[1], (Buffer + DataSize)^, 2);
 Inc(DataSize, 2);
end;

end.
