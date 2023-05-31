program snappy_demo;

uses
  System.StartUpCopy,
  FMX.Forms,
  umain in 'umain.pas' {Form1},
  libsnappy in '..\..\src\libsnappy.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
