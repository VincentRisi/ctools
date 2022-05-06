program PictureServer;

uses
  Forms,
  UnitMainPage in 'UnitMainPage.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
