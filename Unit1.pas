unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  DW.VOIP, FMX.Objects;

type
  TForm1 = class(TForm)
    ReceiveCallTestButton: TButton;
    Memo1: TMemo;
    MakeCallTestButton: TButton;
    VOIPImage: TImage;
    procedure ReceiveCallTestButtonClick(Sender: TObject);
  private
    FToken: string;
    FVOIP: TVOIP;
    procedure SetToken(const AToken: string);
    procedure VOIPPushKitMessageReceivedHandler(Sender: TObject; const AJSON: string);
    procedure VOIPPushKitTokenReceivedHandler(Sender: TObject; const AToken: string; const AIsNew: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.JSON;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  VOIPImage.Visible := False;
  FVOIP := TVOIP.Create;
  FVOIP.Icon := VOIPImage.Bitmap;
  FVOIP.OnPushKitTokenReceived := VOIPPushKitTokenReceivedHandler;
  FVOIP.OnPushKitMessageReceived := VOIPPushKitMessageReceivedHandler;
  SetToken(FVOIP.StoredToken);
  FVOIP.Start;
end;

destructor TForm1.Destroy;
begin
  FVOIP.Free;
  inherited;
end;

procedure TForm1.SetToken(const AToken: string);
begin
  FToken := AToken;
  // Possibly take some action here, if AToken is not empty
end;

procedure TForm1.VOIPPushKitMessageReceivedHandler(Sender: TObject; const AJSON: string);
var
  LJSON: TJSONValue;
  LId, LCaller: string;
begin
  // AJSON contains the json sent in the push notification from your server, so the structure is whatever you determine
  // This example handler assumes the json is like this (for example):
  // { "id": "30DE53F0-8856-4059-881F-5BBDF5C92CCF", "caller": "Marco Cantu" }
  LJSON := TJSONObject.ParseJSONValue(AJSON);
  if LJSON <> nil then
  try
    if LJSON.TryGetValue('id', LId) then
    begin
      LCaller := 'Unknown';
      LJSON.TryGetValue('caller', LCaller);
      FVOIP.ReportIncomingCall(LId, LCaller);
    end;
  finally
    LJSON.Free;
  end;
end;

procedure TForm1.VOIPPushKitTokenReceivedHandler(Sender: TObject; const AToken: string; const AIsNew: Boolean);
begin
  SetToken(AToken);
  // This is where you would update your back end, if AIsNew is set to True
  Memo1.Lines.Add('Token: ' + AToken);
end;

procedure TForm1.ReceiveCallTestButtonClick(Sender: TObject);
begin
  // This is to simulate receiving an incoming call. Normally you might do this in VOIPPushKitMessageReceivedHandler
  // The first parameter value is just a generated GUID
  FVOIP.ReportIncomingCall('30DE53F0-8856-4059-881F-5BBDF5C92CCF', 'Pete Za');
end;

end.
