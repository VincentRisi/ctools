unit UnitMainPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ToolWin, OleCtrls, SHDocVw, IniFiles,
  Sockets, Winsock;

type
  TFormMain = class(TForm)
    StatusBarMain: TStatusBar;
    PageControlMain: TPageControl;
    BrowserTab: TTabSheet;
    LogTab: TTabSheet;
    LogMemo: TMemo;
    LogPanel: TPanel;
    BrowserPanel: TPanel;
    OptionsTab: TTabSheet;
    PicWebBrowser: TWebBrowser;
    PicTcpServer: TTcpServer;
    CSVLabel: TLabel;
    CSVEdit: TEdit;
    LoadCSVButton: TButton;
    UrlEdit: TEdit;
    PortLabel: TLabel;
    PortEdit: TEdit;
    HostLabel: TLabel;
    HostEdit: TEdit;
    OpenButton: TButton;
    CloseButton: TButton;
    PicMemo: TMemo;
    ShowTimer: TTimer;
    StockCombo: TComboBox;
    ItemsLabel: TLabel;
    procedure LoadCSVButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StockComboChange(Sender: TObject);
    procedure PicTcpServerAccept(Sender: TObject;
      ClientSocket: TCustomIpClient);
    procedure PortEditChange(Sender: TObject);
    procedure HostEditChange(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PicTcpServerCreateHandle(Sender: TObject);
    procedure PicTcpServerDestroyHandle(Sender: TObject);
    procedure PicTcpServerListening(Sender: TObject);
    procedure ShowTimerTimer(Sender: TObject);
  private
    StockItems: TStringList;
    procedure PutLog(line : string);
    procedure DisplayItem(key : string);
    procedure MakeDirectory(name : string);
    function GetIPAddress():String;
  public
    toshow : string;
    property Log : String write PutLog;
  end;

var
  FormMain: TFormMain;

type
  TFields = (id, title, brand, ordernumber, imgpath, is_active, nu1, nu2, nu3);
  TStockRec = class (TObject)
    id: string;
    title: string;
    brand: string;
    ordernumber: string;
    imgpath: string;
    is_active: string;
    nu1, nu2, nu3: string;
  end;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
var
  Ini: TIniFile;
  IPAddress : string;
begin
  IPAddress := GetIpAddress;
  Ini := TIniFile.Create('c:\temp\PictureServer.ini');
  try
    Top     := Ini.ReadInteger('Form', 'Top', 100);
    Left    := Ini.ReadInteger('Form', 'Left', 100);
    Width   := Ini.ReadInteger('Form', 'Width', 600);
    Height  := Ini.ReadInteger('Form', 'Height', 800);
    PortEdit.Text := Ini.ReadString('Server', 'Port', '32145');
    HostEdit.Text := Ini.ReadString('Server', 'Host', IPAddress);
    CSVEdit.Text  := Ini.ReadString('Pictures', 'CSVFile', 'C:\ctools\dev\source\pascal\data\products.csv');
  finally
    Ini.Free;
  end;
  StockItems := TStringList.Create;
  PicTcpServer.LocalPort := PortEdit.Text;
  PicTcpServer.LocalHost := HostEdit.Text;
  LoadCSVButtonClick(Sender);
  toshow := '';
  if OpenButton.Enabled = True then
    OpenButtonClick(Sender);
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create('c:\temp\PictureServer.ini');
  try
    Ini.WriteInteger('Form', 'Top', Top);
    Ini.WriteInteger('Form', 'Left', Left);
    Ini.WriteInteger('Form', 'Width', Width);
    Ini.WriteInteger('Form', 'Height', Height);
    Ini.WriteString('Server', 'Port', PortEdit.Text);
    Ini.WriteString('Server', 'Host', HostEdit.Text);
    Ini.WriteString('Pictures', 'CSVFile', CSVEdit.Text);
  finally
    Ini.Free;
  end;
  if CloseButton.Enabled = True then
    CloseButtonClick(Sender);
end;

procedure TFormMain.LoadCSVButtonClick(Sender: TObject);
var
  tfIn: TextFile;
  s, word, stockKey: string;
  n, items: integer;
  field: TFields;
  rec: TStockRec;
begin
  AssignFile(tfIn, CSVEdit.Text);
  try
    reset(tfIn);
    items := 0;
    StockCombo.Items.Clear;
    while not eof(tfIn) do begin
      readln(tfIn, s);
      s := s + ';';
      inc(items);
      if items = 1 then continue;
      field := id;
      rec := TStockRec.Create;
      n := Pos(';', s);
      while n > 0 do begin
        word := Copy(s, 1, n-1);
        s := Copy(s, n+1, length(s)-n);
        case field of
          id: rec.id := word;
          title: rec.title := word;
          brand: rec.brand := word;
          ordernumber: rec.ordernumber := word;
          imgpath: rec.imgpath := word;
          is_active: rec.is_active := word;
          nu1: rec.nu1 := word;
          nu2: rec.nu2 := word;
          nu3: rec.nu3 := word;
          else break;
        end;
        inc(field);
        n := Pos(';', s);
      end;
      if (field >= is_active) and (length(rec.ordernumber) > 0) then begin
        stockKey := Format('%s: %s', [rec.ordernumber, rec.title]);
        StockItems.AddObject(rec.ordernumber, rec);
        StockCombo.Items.Add(stockKey);
      end;
    end;
    CloseFile(tfIn);
  except
    on E: EInOutError do
       Log := format('File handling error occurred. Details: %s', [E.Message]);
  end;
end;

procedure TFormMain.StockComboChange(Sender: TObject);
var
  s,key : string;
  n,m   : integer;
begin
  if StockCombo.ItemIndex < 0 then exit;
  m := StockCombo.ItemIndex;
  s := StockCombo.Items[StockCombo.ItemIndex];
  n := Pos(':', s);
  if n <= 0 then exit;
  key := Copy(s, 1, n-1);
  DisplayItem(key);
  StockCombo.ItemIndex := m;
end;

procedure TFormMain.DisplayItem(key : string);
var
  n : integer;
  rec: TStockRec;
begin
  n := StockItems.IndexOf(key);
  if n >= 0 then begin
    rec := StockItems.Objects[n] as TStockRec;
    UrlEdit.Text := Format('http://www.christensen.co.za/%s', [rec.imgpath]);
    PicMemo.Clear;
    PicMemo.Lines.Add(format('id          : %s', [rec.id]));
    PicMemo.Lines.Add(format('title       : %s', [rec.title]));
    PicMemo.Lines.Add(format('brand       : %s', [rec.brand]));
    PicMemo.Lines.Add(format('ordernumber : %s', [rec.ordernumber]));
    PicMemo.Lines.Add(format('active      : %s', [rec.is_Active]));
    if length(rec.nu1) > 0 then
      PicMemo.Lines.Add(format(' %s', [rec.nu1]));
    if length(rec.nu2) > 0 then
      PicMemo.Lines.Add(format(' %s', [rec.nu2]));
    if length(rec.nu3) > 0 then
      PicMemo.Lines.Add(format(' %s', [rec.nu3]));
    PicWebBrowser.Navigate(UrlEdit.Text);
    //PicMemo.Lines.Add(UrlEdit.Text);
    UrlEdit.SelectAll;
    UrlEdit.CopyToClipboard;
  end;
end;

procedure TFormMain.PutLog(line : string);
begin
  LogMemo.Lines.Add(line);
end;

procedure TFormMain.PicTcpServerAccept(Sender: TObject;
  ClientSocket: TCustomIpClient);
var
  Buf : array [0..4096] of char;
  key : string;
  BufLen : integer;
begin
  Log := 'accept';
  BufLen := ClientSocket.ReceiveBuf(Buf, sizeof(Buf), 0);
  toshow := Buf;
  Log := format('%d `%s` `%s`', [BufLen, Buf, key]);
  Buf[0] := 'O';
  Buf[1] := 'K';
  Buf[2] := #0;
  ClientSocket.SendBuf(Buf, 3, 0);
  ShowTimer.Enabled := true;
end;

procedure TFormMain.PortEditChange(Sender: TObject);
begin
  PicTcpServer.LocalPort := PortEdit.Text;
end;

procedure TFormMain.HostEditChange(Sender: TObject);
begin
  PicTcpServer.LocalHost := HostEdit.Text;
end;

procedure TFormMain.OpenButtonClick(Sender: TObject);
begin
  OpenButton.Enabled := False;
  CloseButton.Enabled := True;
  PicTcpServer.Active := True;
  PicTcpServer.LocalPort := PortEdit.Text;
  PicTcpServer.LocalHost := HostEdit.Text;
end;

procedure TFormMain.CloseButtonClick(Sender: TObject);
begin
  OpenButton.Enabled := True;
  CloseButton.Enabled := False;
  PicTcpServer.Close();
  PicTcpServer.Active := False;
end;

procedure TFormMain.PicTcpServerCreateHandle(Sender: TObject);
begin
  Log := 'create handle';
end;

procedure TFormMain.PicTcpServerDestroyHandle(Sender: TObject);
begin
  Log := 'destroy handle';
end;

procedure TFormMain.PicTcpServerListening(Sender: TObject);
begin
  Log := 'listening';
end;

procedure TFormMain.ShowTimerTimer(Sender: TObject);
begin
  ShowTimer.Enabled := false;
  DisplayItem(toshow);
  PageControlMain.ActivePageIndex := 0;
  StockCombo.ItemIndex := -1;
end;

procedure TFormMain.MakeDirectory(name: string);
begin
  {$I-}
  mkdir(name);
end;

Function TFormMain.GetIPAddress():String;
type
  pu_long = ^u_long;
var
  varTWSAData : TWSAData;
  varPHostEnt : PHostEnt;
  varTInAddr : TInAddr;
  namebuf : Array[0..255] of char;
begin
  If WSAStartup($101,varTWSAData) <> 0 Then
  Result := 'No. IP Address'
  Else Begin
    gethostname(namebuf,sizeof(namebuf));
    varPHostEnt := gethostbyname(namebuf);
    varTInAddr.S_addr := u_long(pu_long(varPHostEnt^.h_addr_list^)^);
    Result := inet_ntoa(varTInAddr);
  End;
  WSACleanup;
end;

end.
