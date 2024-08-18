unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  FMX.Objects,
  DW.VOIP;

type
  TForm1 = class(TForm)
    ReceiveCallTestButton: TButton;
    Memo1: TMemo;
    MakeCallTestButton: TButton;
    VOIPImage: TImage;
    procedure ReceiveCallTestButtonClick(Sender: TObject);
    procedure MakeCallTestButtonClick(Sender: TObject);
  private
    FToken: string;
    FVOIP: TVOIP;
    procedure SetToken(const AToken: string);
    procedure VOIPCallStateChangeHandler(Sender: TObject; const ACallState: TVOIPCallState);
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
  FVOIP.OnVOIPCallStateChange := VOIPCallStateChangeHandler;
  FVOIP.OnPushKitTokenReceived := VOIPPushKitTokenReceivedHandler;
  SetToken(FVOIP.StoredToken);
  FVOIP.Start;
end;

destructor TForm1.Destroy;
begin
  FVOIP.Free;
  inherited;
end;

procedure TForm1.MakeCallTestButtonClick(Sender: TObject);
var
  LInfo: TVOIPCallInfo;
begin
  // This is to simulate initiating an outgoing call.
  LInfo.DisplayName := 'Pete Za';
  LInfo.Email := 'pete@za.com';
  LInfo.UUID := '30DE53F0-8856-4059-881F-5BBDF5C92CCF';
  FVOIP.StartOutgoingCall(LInfo);
end;

procedure TForm1.SetToken(const AToken: string);
begin
  FToken := AToken;
  // Possibly take some action here, if AToken is not empty
end;

procedure TForm1.VOIPCallStateChangeHandler(Sender: TObject; const ACallState: TVOIPCallState);
begin
  //
end;

procedure TForm1.VOIPPushKitTokenReceivedHandler(Sender: TObject; const AToken: string; const AIsNew: Boolean);
begin
  SetToken(AToken);
  // This is where you would update your back end, if AIsNew is set to True
  Memo1.Lines.Add('Token: ' + AToken);
end;

procedure TForm1.ReceiveCallTestButtonClick(Sender: TObject);
var
  LInfo: TVOIPCallInfo;
begin
  // This is to simulate receiving an incoming call. Normally you might do this in VOIPPushKitMessageReceivedHandler
  LInfo.DisplayName := 'Pete Za';
  LInfo.Email := 'pete@za.com';
  LInfo.UUID := '30DE53F0-8856-4059-881F-5BBDF5C92CCF';
  FVOIP.ReportIncomingCall(LInfo);
end;

end.
