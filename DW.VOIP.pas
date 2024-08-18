unit DW.VOIP;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // FMX
  FMX.Graphics,
  // DW
  DW.PushKit;

type
  TVOIPCallState = (Unknown, IncomingCall, OutgoingCall, CallAnswered, CallDeclined, CallDisconnected);

  TVOIPCallInfo = record
    DisplayName: string;
    Email: string;
    PhoneNumber: string;
    UUID: string;
  end;

  TVOIP = class;

  TCustomPlatformVOIP = class(TObject)
  private
    FIcon: TBitmap;
    FVOIP: TVOIP;
    procedure PushKitMessageReceivedHandler(Sender: TObject; const AJSON: string);
    procedure PushKitTokenReceivedHandler(Sender: TObject; const AToken: string; const AIsNew: Boolean);
    procedure SetIcon(const Value: TBitmap);
  protected
    procedure DoVOIPCallStateChange(const ACallState: TVOIPCallState);
    procedure IconUpdated; virtual;
    procedure PushKitMessageReceived(const AJSON: string); virtual;
    procedure ReportIncomingCall(const AInfo: TVOIPCallInfo); virtual;
    procedure ReportOutgoingCall; virtual;
    function StartOutgoingCall(const AInfo: TVOIPCallInfo): Boolean; virtual;
    property Icon: TBitmap read FIcon write SetIcon;
    property VOIP: TVOIP read FVOIP;
  public
    constructor Create(const AVOIP: TVOIP); virtual;
    destructor Destroy; override;
  end;

  TVOIPCallStateChangeEvent = procedure(Sender: TObject; const CallState: TVOIPCallState) of object;

  TVOIP = class(TObject)
  private
    FIdentProperty: string;
    FPlatformVOIP: TCustomPlatformVOIP;
    FOnPushKitMessageReceived: TPushKitMessageReceivedEvent;
    FOnPushKitTokenReceived: TPushKitTokenReceivedEvent;
    FOnVOIPCallStateChange: TVOIPCallStateChangeEvent;
    function GetStoredToken: string;
    function GetIcon: TBitmap;
    procedure SetIcon(const Value: TBitmap);
  protected
    procedure DoVOIPCallStateChange(const ACallState: TVOIPCallState);
    function DoPushKitMessageReceived(const AJSON: string): Boolean;
    procedure DoPushKitTokenReceived(const AToken: string; const AIsNew: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure ReportIncomingCall(const AInfo: TVOIPCallInfo);
    function StartOutgoingCall(const AInfo: TVOIPCallInfo): Boolean;
    property Icon: TBitmap read GetIcon write SetIcon;
    property IdentProperty: string read FIdentProperty write FIdentProperty;
    property StoredToken: string read GetStoredToken;
    property OnVOIPCallStateChange: TVOIPCallStateChangeEvent read FOnVOIPCallStateChange write FOnVOIPCallStateChange;
    property OnPushKitMessageReceived: TPushKitMessageReceivedEvent read FOnPushKitMessageReceived write FOnPushKitMessageReceived;
    property OnPushKitTokenReceived: TPushKitTokenReceivedEvent read FOnPushKitTokenReceived write FOnPushKitTokenReceived;
  end;

implementation

uses
  DW.OSLog,
  System.JSON,
  {$IF Defined(IOS)}
  DW.VOIP.iOS;
  {$ELSEIF Defined(ANDROID)}
  DW.VOIP.Android;
  {$ELSE}
  DW.VOIP.Default;
  {$ENDIF}

{ TCustomPlatformVOIP }

constructor TCustomPlatformVOIP.Create(const AVOIP: TVOIP);
begin
  inherited Create;
  FIcon := TBitmap.Create;
  // PushKit handles the push notifications that signal an incoming call
  PushKit.OnTokenReceived := PushKitTokenReceivedHandler;
  PushKit.OnMessageReceived := PushKitMessageReceivedHandler;
  FVOIP := AVOIP;
end;

destructor TCustomPlatformVOIP.Destroy;
begin
  FIcon.Free;
  inherited;
end;

procedure TCustomPlatformVOIP.DoVOIPCallStateChange(const ACallState: TVOIPCallState);
begin
  FVOIP.DoVOIPCallStateChange(ACallState);
end;

procedure TCustomPlatformVOIP.IconUpdated;
begin
  //
end;


// {"aps":{"alert":"VoIP Comming"},"callUUID":"db1dff98-bfef-4ec0-8ac6-8187a3b0c645","handle":"sherry@gmail.com"}

procedure TCustomPlatformVOIP.PushKitMessageReceived(const AJSON: string);
var
  LJSON: TJSONValue;
  LInfo: TVOIPCallInfo;
begin
  TOSLog.d('TCustomPlatformVOIP.PushKitMessageReceived');
  // By default, expect a property that identifies the incoming call
  LJSON := TJSONObject.ParseJSONValue(AJSON);
  if LJSON <> nil then
  try
    LJSON.TryGetValue('uuid', LInfo.UUID);
    LJSON.TryGetValue('phone', LInfo.PhoneNumber);
    LJSON.TryGetValue('email', LInfo.Email);
    LInfo.DisplayName := 'Unknown';
    LJSON.TryGetValue('displayName', LInfo.DisplayName);
    FVOIP.ReportIncomingCall(LInfo);
    //!!!! else perhaps have an "unhandled push" event
  finally
    LJSON.Free;
  end;
end;

procedure TCustomPlatformVOIP.PushKitMessageReceivedHandler(Sender: TObject; const AJSON: string);
begin
  if not FVOIP.DoPushKitMessageReceived(AJSON) then
    PushKitMessageReceived(AJSON);
end;

procedure TCustomPlatformVOIP.PushKitTokenReceivedHandler(Sender: TObject; const AToken: string; const AIsNew: Boolean);
begin
  FVOIP.DoPushKitTokenReceived(AToken, AIsNew);
end;

procedure TCustomPlatformVOIP.ReportIncomingCall(const AInfo: TVOIPCallInfo);
begin
  //
end;

procedure TCustomPlatformVOIP.ReportOutgoingCall;
begin
  //
end;

procedure TCustomPlatformVOIP.SetIcon(const Value: TBitmap);
begin
  FIcon.Assign(Value);
  IconUpdated;
end;

function TCustomPlatformVOIP.StartOutgoingCall(const AInfo: TVOIPCallInfo): Boolean;
begin
  Result := False;
end;

{ TVOIP }

constructor TVOIP.Create;
begin
  inherited;
  FIdentProperty := 'ident'; // Do not localize
  FPlatformVOIP := TPlatformVOIP.Create(Self);
end;

destructor TVOIP.Destroy;
begin
  FPlatformVOIP.Free;
  inherited;
end;

procedure TVOIP.DoVOIPCallStateChange(const ACallState: TVOIPCallState);
begin
  if Assigned(FOnVOIPCallStateChange) then
    FOnVOIPCallStateChange(Self, ACallState);
end;

function TVOIP.GetIcon: TBitmap;
begin
  Result := FPlatformVOIP.Icon;
end;

function TVOIP.GetStoredToken: string;
begin
  Result := PushKit.StoredToken;
end;

function TVOIP.DoPushKitMessageReceived(const AJSON: string): Boolean;
begin
  // If not handled, return False. This will cause the default handling to be invoked
  Result := False;
  if Assigned(FOnPushKitMessageReceived) then
  begin
    FOnPushKitMessageReceived(Self, AJSON);
    Result := True;
  end;
end;

procedure TVOIP.DoPushKitTokenReceived(const AToken: string; const AIsNew: Boolean);
begin
  TOSLog.d('Token: %s', [AToken]);
  if Assigned(FOnPushKitTokenReceived) then
    FOnPushKitTokenReceived(Self, AToken, AIsNew);
end;

procedure TVOIP.ReportIncomingCall(const AInfo: TVOIPCallInfo);
begin
  FPlatformVOIP.ReportIncomingCall(AInfo);
end;

procedure TVOIP.SetIcon(const Value: TBitmap);
begin
  FPlatformVOIP.Icon := Value;
end;

procedure TVOIP.Start;
begin
  TOSLog.d('TVOIP.Start');
  PushKit.Start;
end;

function TVOIP.StartOutgoingCall(const AInfo: TVOIPCallInfo): Boolean;
begin
  Result := FPlatformVOIP.StartOutgoingCall(AInfo);
end;

end.
