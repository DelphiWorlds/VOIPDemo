unit DW.PushKit;

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

// Full instructions on VoIP setup: https://stackoverflow.com/a/28562124/3164070
// Also: https://www.raywenderlich.com/8164-push-notifications-tutorial-getting-started
// Setting up a server: https://docs.developer.pv.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server?language=objc

interface

type
  TPushKit = class;

  TCustomPlatformPushKit = class(TObject)
  private
    FPushKit: TPushKit;
  protected
    procedure DoMessageReceived(const AJSON: string);
    procedure DoTokenReceived(const AToken: string; const AIsNew: Boolean);
    function GetStoredToken: string; virtual;
    procedure Start; virtual;
    property PushKit: TPushKit read FPushKit;
  public
    constructor Create(const APushKit: TPushKit); virtual;
  end;

  TPushKitTokenReceivedEvent = procedure(Sender: TObject; const Token: string; const IsNew: Boolean) of object;
  TPushKitMessageReceivedEvent = procedure(Sender: TObject; const JSON: string) of object;

  TPushKit = class(TObject)
  private
    FPlatformPushKit: TCustomPlatformPushKit;
    FOnMessageReceived: TPushKitMessageReceivedEvent;
    FOnTokenReceived: TPushKitTokenReceivedEvent;
    function GetStoredToken: string;
  protected
    procedure DoMessageReceived(const AJSON: string);
    procedure DoTokenReceived(const AToken: string; const AIsNew: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    property StoredToken: string read GetStoredToken;
    property OnMessageReceived: TPushKitMessageReceivedEvent read FOnMessageReceived write FOnMessageReceived;
    property OnTokenReceived: TPushKitTokenReceivedEvent read FOnTokenReceived write FOnTokenReceived;
  end;

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

constructor TCustomPlatformPushKit.Create(const APushKit: TPushKit);
begin
  inherited Create;
  FPushKit := APushKit;
end;

procedure TCustomPlatformPushKit.DoMessageReceived(const AJSON: string);
begin
  TOSLog.d('TCustomPlatformPushKit.DoMessageReceived: %s', [AJSON]);
  FPushKit.DoMessageReceived(AJSON);
end;

procedure TCustomPlatformPushKit.DoTokenReceived(const AToken: string; const AIsNew: Boolean);
begin
  FPushKit.DoTokenReceived(AToken, AIsNew);
end;

function TCustomPlatformPushKit.GetStoredToken: string;
begin
  Result := '';
end;

procedure TCustomPlatformPushKit.Start;
begin
  //
end;

{ TPushKit }

constructor TPushKit.Create;
begin
  inherited;
  FPlatformPushKit := TPlatformPushKit.Create(Self);
end;

destructor TPushKit.Destroy;
begin
  FPlatformPushKit.Free;
  inherited;
end;

procedure TPushKit.DoMessageReceived(const AJSON: string);
begin
  if Assigned(FOnMessageReceived) then
    FOnMessageReceived(Self, AJSON);
end;

procedure TPushKit.DoTokenReceived(const AToken: string; const AIsNew: Boolean);
begin
  if Assigned(FOnTokenReceived) then
    FOnTokenReceived(Self, AToken, AIsNew);
end;

function TPushKit.GetStoredToken: string;
begin
  Result := FPlatformPushKit.GetStoredToken;
end;

procedure TPushKit.Start;
begin
  FPlatformPushKit.Start;
end;

end.
