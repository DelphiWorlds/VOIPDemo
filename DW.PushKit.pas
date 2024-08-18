unit DW.PushKit;

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

// Full instructions on VoIP setup: https://stackoverflow.com/a/28562124/3164070
// Also: https://www.raywenderlich.com/8164-push-notifications-tutorial-getting-started
// Setting up a server: https://docs.developer.pv.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server?language=objc

interface

type
  TPushKitTokenReceivedEvent = procedure(Sender: TObject; const Token: string; const IsNew: Boolean) of object;
  TPushKitMessageReceivedEvent = procedure(Sender: TObject; const JSON: string) of object;

  IPushKit = interface(IInterface)
    ['{EF909BEE-FBD1-4360-8424-643394E548C8}']
    function GetStoredToken: string;
    function GetOnMessageReceived: TPushKitMessageReceivedEvent;
    function GetOnTokenReceived: TPushKitTokenReceivedEvent;
    procedure SetOnMessageReceived(const Value: TPushKitMessageReceivedEvent);
    procedure SetOnTokenReceived(const Value: TPushKitTokenReceivedEvent);
    procedure Start;
    property StoredToken: string read GetStoredToken;
    property OnMessageReceived: TPushKitMessageReceivedEvent read GetOnMessageReceived write SetOnMessageReceived;
    property OnTokenReceived: TPushKitTokenReceivedEvent read GetOnTokenReceived write SetOnTokenReceived;
  end;

  TCustomPlatformPushKit = class(TInterfacedObject, IPushKit)
  private
    FOnMessageReceived: TPushKitMessageReceivedEvent;
    FOnTokenReceived: TPushKitTokenReceivedEvent;
  protected
    procedure DoMessageReceived(const AJSON: string);
    procedure DoTokenReceived(const AToken: string; const AIsNew: Boolean);
  public
    { IPushKit }
    function GetStoredToken: string; virtual;
    function GetOnMessageReceived: TPushKitMessageReceivedEvent;
    function GetOnTokenReceived: TPushKitTokenReceivedEvent;
    procedure SetOnMessageReceived(const Value: TPushKitMessageReceivedEvent);
    procedure SetOnTokenReceived(const Value: TPushKitTokenReceivedEvent);
    procedure Start; virtual;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  PushKit: IPushKit;

implementation

{$IF Defined(IOS)}
uses
  DW.OSLog,
  DW.PushKit.iOS;
{$ELSE}
type
  TPlatformPushKit = class(TCustomPlatformPushKit);
{$ENDIF}

{ TCustomPlatformPushKit }

constructor TCustomPlatformPushKit.Create;
begin
  inherited;
  //
end;

destructor TCustomPlatformPushKit.Destroy;
begin
  //
  inherited;
end;

procedure TCustomPlatformPushKit.DoMessageReceived(const AJSON: string);
begin
  TOSLog.d('TPushKit.DoMessageReceived: %s', [AJSON]);
  if Assigned(FOnMessageReceived) then
    FOnMessageReceived(Self, AJSON);
end;

procedure TCustomPlatformPushKit.DoTokenReceived(const AToken: string; const AIsNew: Boolean);
begin
  if Assigned(FOnTokenReceived) then
    FOnTokenReceived(Self, AToken, AIsNew);
end;

function TCustomPlatformPushKit.GetOnMessageReceived: TPushKitMessageReceivedEvent;
begin
  Result := FOnMessageReceived;
end;

function TCustomPlatformPushKit.GetOnTokenReceived: TPushKitTokenReceivedEvent;
begin
  Result := FOnTokenReceived;
end;

function TCustomPlatformPushKit.GetStoredToken: string;
begin
  Result := '';
end;

procedure TCustomPlatformPushKit.SetOnMessageReceived(const Value: TPushKitMessageReceivedEvent);
begin
  FOnMessageReceived := Value;
end;

procedure TCustomPlatformPushKit.SetOnTokenReceived(const Value: TPushKitTokenReceivedEvent);
begin
  FOnTokenReceived := Value;
end;

procedure TCustomPlatformPushKit.Start;
begin
  //
end;

initialization
  PushKit := TPlatformPushKit.Create;

end.
