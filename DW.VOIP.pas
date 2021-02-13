unit DW.VOIP;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

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
    Ident: string;
  end;

  TVOIP = class;

  TCustomPlatformVOIP = class(TObject)
  private
    FIcon: TBitmap;
    FPushKit: TPushKit;
    FVOIP: TVOIP;
    procedure PushKitMessageReceivedHandler(Sender: TObject; const AJSON: string);
    procedure PushKitTokenReceivedHandler(Sender: TObject; const AToken: string; const AIsNew: Boolean);
    procedure SetIcon(const Value: TBitmap);
  protected
    procedure DoVOIPCallState(const ACallState: TVOIPCallState; const AIdent: string);
    procedure IconUpdated; virtual;
    procedure PushKitMessageReceived(const AJSON: string); virtual;
    procedure ReportIncomingCall(const AIdent, ADisplayName: string); virtual;
    procedure ReportOutgoingCall; virtual;
    function StartCall(const AIdent, ADisplayName: string): Boolean; virtual;
    property Icon: TBitmap read FIcon write SetIcon;
    property PushKit: TPushKit read FPushKit;
    property VOIP: TVOIP read FVOIP;
  public
    constructor Create(const AVOIP: TVOIP); virtual;
    destructor Destroy; override;
  end;

  TVOIPCallStateEvent = procedure(Sender: TObject; const CallState: TVOIPCallState; const Ident: string) of object;

  TVOIP = class(TObject)
  private
    FIdentProperty: string;
    FPlatformVOIP: TCustomPlatformVOIP;
    FOnPushKitMessageReceived: TPushKitMessageReceivedEvent;
    FOnPushKitTokenReceived: TPushKitTokenReceivedEvent;
    FOnVOIPCallState: TVOIPCallStateEvent;
    function GetStoredToken: string;
    function GetIcon: TBitmap;
    procedure SetIcon(const Value: TBitmap);
  protected
    procedure DoVOIPCallState(const ACallState: TVOIPCallState; const AIdent: string);
    function DoPushKitMessageReceived(const AJSON: string): Boolean;
    procedure DoPushKitTokenReceived(const AToken: string; const AIsNew: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure ReportIncomingCall(const AIdent, ADisplayName: string);
    function StartCall(const AIdent, ADisplayName: string): Boolean;
    property Icon: TBitmap read GetIcon write SetIcon;
    property IdentProperty: string read FIdentProperty write FIdentProperty;
    property StoredToken: string read GetStoredToken;
    property OnVOIPCallState: TVOIPCallStateEvent read FOnVOIPCallState write FOnVOIPCallState;
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
  FPushKit := TPushKit.Create;
  FPushKit.OnTokenReceived := PushKitTokenReceivedHandler;
  FPushKit.OnMessageReceived := PushKitMessageReceivedHandler;
  FVOIP := AVOIP;
end;

destructor TCustomPlatformVOIP.Destroy;
begin
  FIcon.Free;
  inherited;
end;

procedure TCustomPlatformVOIP.DoVOIPCallState(const ACallState: TVOIPCallState; const AIdent: string);
begin
  FVOIP.DoVOIPCallState(ACallState, AIdent);
end;

procedure TCustomPlatformVOIP.IconUpdated;
begin
  //
end;

procedure TCustomPlatformVOIP.PushKitMessageReceived(const AJSON: string);
var
  LJSON: TJSONValue;
  LIdent, LDisplayName: string;
begin
  // By default, expect a property that identifies the incoming call
  LJSON := TJSONObject.ParseJSONValue(AJSON);
  if LJSON <> nil then
  try
    if LJSON.TryGetValue(VOIP.IdentProperty, LIdent) then
    begin
      LDisplayName := 'Unknown'; // Needs localization
      LJSON.TryGetValue('DisplayName', LDisplayName); // Might need a property too
      ReportIncomingCall(LIdent, LDisplayName);
    end;
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

procedure TCustomPlatformVOIP.ReportIncomingCall(const AIdent, ADisplayName: string);
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

function TCustomPlatformVOIP.StartCall(const AIdent, ADisplayName: string): Boolean;
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

procedure TVOIP.DoVOIPCallState(const ACallState: TVOIPCallState; const AIdent: string);
begin
  if Assigned(FOnVOIPCallState) then
    FOnVOIPCallState(Self, ACallState, AIdent);
end;

function TVOIP.GetIcon: TBitmap;
begin
  Result := FPlatformVOIP.Icon;
end;

function TVOIP.GetStoredToken: string;
begin
  Result := FPlatformVOIP.PushKit.StoredToken;
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

procedure TVOIP.ReportIncomingCall(const AIdent, ADisplayName: string);
begin
  FPlatformVOIP.ReportIncomingCall(AIdent, ADisplayName);
end;

procedure TVOIP.SetIcon(const Value: TBitmap);
begin
  FPlatformVOIP.Icon := Value;
end;

procedure TVOIP.Start;
begin
  TOSLog.d('TVOIP.Start');
  FPlatformVOIP.PushKit.Start;
end;

function TVOIP.StartCall(const AIdent, ADisplayName: string): Boolean;
begin
  Result := FPlatformVOIP.StartCall(AIdent, ADisplayName);
end;

end.
