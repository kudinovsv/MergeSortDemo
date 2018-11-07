program MergeSortDemo;

uses
  Forms,
  SortingDemoForm in 'SortingDemoForm.pas' {SortFm},
  StringsIO in 'StringsIO.pas',
  SortingClasses in 'SortingClasses.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSortFm, SortFm);
  Application.Run;
end.
