program VOIPDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  DW.VOIP.iOS in 'DW.VOIP.iOS.pas',
  DW.VOIP in 'DW.VOIP.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
