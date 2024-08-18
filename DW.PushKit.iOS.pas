unit DW.PushKit.iOS;

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
  // macOS
  Macapi.ObjectiveC,
  // DW
  DW.iOSapi.PushKit, DW.PushKit;

type
  TPlatformPushKit = class;

  TPushRegistryDelegate = class(TOCLocal, PKPushRegistryDelegate)
  private
    FPushKit: TPlatformPushKit;
    FRegistry: PKPushRegistry;
  protected
    procedure RegisterTypes(const APushTypes: array of PKPushType);
  public
    { PKPushRegistryDelegate }
    procedure pushRegistry(registry: PKPushRegistry; didInvalidatePushTokenForType: PKPushType); overload; cdecl;
    procedure pushRegistry(registry: PKPushRegistry; didReceiveIncomingPushWithPayload: PKPushPayload; forType: PKPushType;
      withCompletionHandler: Pointer); overload; cdecl;
    procedure pushRegistry(registry: PKPushRegistry; didUpdatePushCredentials: PKPushCredentials; forType: PKPushType); overload; cdecl;
  public
    constructor Create(const APushKit: TPlatformPushKit);
    destructor Destroy; override;
  end;

  TPlatformPushKit = class(TCustomPlatformPushKit)
  private
    FDelegate: TPushRegistryDelegate;
  protected
    procedure MessageReceived(const APayload: PKPushPayload);
    procedure TokenReceived(const AToken: string; const AIsNew: Boolean);
  public
    function GetStoredToken: string; override;
    procedure Start; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.Classes,
  // macOS
  Macapi.Helpers, Macapi.ObjCRuntime,
  // iOS
  iOSapi.Foundation, iOSapi.CocoaTypes,
  // DW
  DW.OSLog,
  DW.iOSapi.CallKit, DW.UserDefaults.iOS, DW.iOSapi.Helpers;

function NSDataToHexString(const AData: NSData): string;
var
  LData, LHex: TBytes;
begin
  SetLength(LData, AData.length);
  Move(AData.bytes^, LData[0], AData.length);
  SetLength(LHex, AData.length * 2);
  BinToHex(LData, 0, LHex, 0, AData.length);
  Result := StringOf(LHex).ToLower;
end;

{ TPushRegistryDelegate }

constructor TPushRegistryDelegate.Create(const APushKit: TPlatformPushKit);
begin
  inherited Create;
  FPushKit := APushKit;
  FRegistry := TPKPushRegistry.Wrap(TPKPushRegistry.Wrap(TPKPushRegistry.OCClass.alloc).initWithQueue(0));
  FRegistry.setDelegate(GetObjectID);
end;

destructor TPushRegistryDelegate.Destroy;
begin
  //
  inherited;
end;

procedure TPushRegistryDelegate.RegisterTypes(const APushTypes: array of PKPushType);
var
  LArray: NSMutableArray;
  LPushType: PKPushType;
  LSet: NSSet;
begin
  LArray := TNSMutableArray.Create;
  for LPushType in APushTypes do
    LArray.addObject(NSObjectToID(LPushType));
  // LSet := TNSSet.Wrap(TNSSet.OCClass.setWithArray(LArray));
  LSet := TNSSet.Wrap(TNSSet.OCClass.setWithObject(NSObjectToID(PKPushTypeVoIP)));
  FRegistry.setDesiredPushTypes(LSet);
end;

procedure TPushRegistryDelegate.pushRegistry(registry: PKPushRegistry; didInvalidatePushTokenForType: PKPushType);
begin
  // TOSLog.d('TPushRegistryDelegate.pushRegistry didInvalidatePushTokenForType');
end;

procedure TPushRegistryDelegate.pushRegistry(registry: PKPushRegistry; didUpdatePushCredentials: PKPushCredentials; forType: PKPushType);
var
  LHex: string;
  LToken: NSString;
  LStoredToken: NSString;
  LIsNew: Boolean;
begin
  TOSLog.d('TPushRegistryDelegate.pushRegistry didUpdatePushCredentials');
  LHex := NSDataToHexString(didUpdatePushCredentials.token);
  LToken := StrToNSStr(LHex);
  LStoredToken := TUserDefaults.GetValue(didUpdatePushCredentials.&type);
  LIsNew := (LStoredToken = nil) or not LStoredToken.isEqualToString(LToken);
  TUserDefaults.SetValue(didUpdatePushCredentials.&type, LToken);
  FPushKit.TokenReceived(LHex, LIsNew);
end;

procedure TPushRegistryDelegate.pushRegistry(registry: PKPushRegistry; didReceiveIncomingPushWithPayload: PKPushPayload; forType: PKPushType;
  withCompletionHandler: Pointer);
var
  LCompletionHandlerImp: procedure; cdecl;
begin
  TOSLog.d('TPushRegistryDelegate.pushRegistry didReceiveIncomingPushWithPayload');
  FPushKit.MessageReceived(didReceiveIncomingPushWithPayload);
  @LCompletionHandlerImp := imp_implementationWithBlock(withCompletionHandler);
  LCompletionHandlerImp;
  imp_removeBlock(@LCompletionHandlerImp);
end;

{ TPlatformPushKit }

constructor TPlatformPushKit.Create;
begin
  inherited;
  FDelegate := TPushRegistryDelegate.Create(Self);
end;

destructor TPlatformPushKit.Destroy;
begin
  FDelegate.Free;
  inherited;
end;

function TPlatformPushKit.GetStoredToken: string;
var
  LStoredToken: NSString;
begin
  LStoredToken := TUserDefaults.GetValue(PKPushTypeVoIP);
  if LStoredToken <> nil then
    Result := NSStrToStr(LStoredToken)
  else
    Result := '';
end;

procedure TPlatformPushKit.MessageReceived(const APayload: PKPushPayload);
begin
  DoMessageReceived(TiOSHelperEx.NSDictionaryToJSON(APayload.dictionaryPayload));
end;

procedure TPlatformPushKit.Start;
begin
  TOSLog.d('TPlatformPushKit.Start');
  FDelegate.RegisterTypes([PKPushTypeVoIP]);
end;

procedure TPlatformPushKit.TokenReceived(const AToken: string; const AIsNew: Boolean);
begin
  DoTokenReceived(AToken, AIsNew);
end;

end.
