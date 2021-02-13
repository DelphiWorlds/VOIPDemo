unit DW.VOIP.iOS;

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
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.Foundation, iOSapi.AVFoundation,
  // DW
  DW.iOSapi.CallKit, DW.VOIP;

type
  TPlatformVOIP = class;

  TCXProviderDelegate = class(TOCLocal, CXProviderDelegate)
  private
    FVOIP: TPlatformVOIP;
  public
    { CXProviderDelegate }
    [MethodName('provider:didActivateAudioSession:')]
    procedure providerDidActivateAudioSession(provider: CXProvider; audioSession: AVAudioSession); cdecl;
    procedure providerDidBegin(provider: CXProvider); cdecl;
    [MethodName('provider:didDeactivateAudioSession:')]
    procedure providerDidDeactivateAudioSession(provider: CXProvider; audioSession: AVAudioSession); cdecl;
    procedure providerDidReset(provider: CXProvider); cdecl;
    [MethodName('provider:executeTransaction:')]
    function providerExecuteTransaction(provider: CXProvider; transaction: CXTransaction): Boolean; cdecl;
    [MethodName('provider:performAnswerCallAction:')]
    procedure providerPerformAnswerCallAction(provider: CXProvider; action: CXAnswerCallAction); cdecl;
    [MethodName('provider:performEndCallAction:')]
    procedure providerPerformEndCallAction(provider: CXProvider; action: CXEndCallAction); cdecl;
    [MethodName('provider:performPlayDTMFCallAction:')]
    procedure providerPerformPlayDTMFCallAction(provider: CXProvider; action: CXPlayDTMFCallAction); cdecl;
    [MethodName('provider:performSetGroupCallAction:')]
    procedure providerPerformSetGroupCallAction(provider: CXProvider; action: CXSetGroupCallAction); cdecl;
    [MethodName('provider:performSetHeldCallAction:')]
    procedure providerPerformSetHeldCallAction(provider: CXProvider; action: CXSetHeldCallAction); cdecl;
    [MethodName('provider:performSetMutedCallAction:')]
    procedure providerPerformSetMutedCallAction(provider: CXProvider; action: CXSetMutedCallAction); cdecl;
    [MethodName('provider:performStartCallAction:')]
    procedure providerPerformStartCallAction(provider: CXProvider; action: CXStartCallAction); cdecl;
    [MethodName('provider:timedOutPerformingAction:')]
    procedure providerTimedOutPerformingAction(provider: CXProvider; action: CXAction); cdecl;
  public
    constructor Create(const AVOIP: TPlatformVOIP);
  end;

  TPlatformVOIP = class(TCustomPlatformVOIP)
  private
    FController: CXCallController;
    FIncomingUUID: NSUUID;
    FIsCalling: Boolean;
    FProvider: CXProvider;
    FProviderDelegate: TCXProviderDelegate;
    procedure IncomingCallCompletionHandler(error: NSError);
    procedure RequestStartCallTransactionCompletionHandler(error: NSError);
    procedure UpdateProvider;
  protected
    procedure IconUpdated; override;
    procedure PerformAnswerCallAction(const action: CXAnswerCallAction);
    procedure PerformEndCallAction(const action: CXEndCallAction);
    procedure ReportIncomingCall(const AIdent, ADisplayName: string); override;
    procedure ReportOutgoingCall; override;
    procedure StartCall(const ADisplayName: string);
    procedure VOIPCallState(const ACallState: TVOIPCallState; const AIdent: string);
  public
    constructor Create(const AVOIP: TVOIP);
    destructor Destroy; override;
  end;

implementation

uses
  DW.OSLog,
  System.Classes,
  Macapi.Helpers,
  iOSapi.UIKit,
  FMX.Helpers.iOS;

{ TCXProviderDelegate }

constructor TCXProviderDelegate.Create(const AVOIP: TPlatformVOIP);
begin
  inherited Create;
  FVOIP := AVOIP;
end;

procedure TCXProviderDelegate.providerDidActivateAudioSession(provider: CXProvider; audioSession: AVAudioSession);
begin
  TOSLog.d('TCXProviderDelegate.providerDidActivateAudioSession');
end;

procedure TCXProviderDelegate.providerDidBegin(provider: CXProvider);
begin
  TOSLog.d('TCXProviderDelegate.providerDidBegin');
end;

procedure TCXProviderDelegate.providerDidDeactivateAudioSession(provider: CXProvider; audioSession: AVAudioSession);
begin
  TOSLog.d('TCXProviderDelegate.providerDidDeactivateAudioSession');
end;

procedure TCXProviderDelegate.providerDidReset(provider: CXProvider);
begin
  TOSLog.d('TCXProviderDelegate.providerDidReset');
end;

function TCXProviderDelegate.providerExecuteTransaction(provider: CXProvider; transaction: CXTransaction): Boolean;
begin
  TOSLog.d('TCXProviderDelegate.providerExecuteTransaction');
  Result := False;
end;

procedure TCXProviderDelegate.providerPerformAnswerCallAction(provider: CXProvider; action: CXAnswerCallAction);
begin
  TOSLog.d('TCXProviderDelegate.providerPerformAnswerCallAction');
  // Called if the call is answered
  FVOIP.PerformAnswerCallAction(action);
end;

procedure TCXProviderDelegate.providerPerformEndCallAction(provider: CXProvider; action: CXEndCallAction);
begin
  TOSLog.d('TCXProviderDelegate.providerPerformEndCallAction');
  // Called if the call is ended or declined
  FVOIP.PerformEndCallAction(action);
end;

procedure TCXProviderDelegate.providerPerformPlayDTMFCallAction(provider: CXProvider; action: CXPlayDTMFCallAction);
begin
  TOSLog.d('TCXProviderDelegate.providerPerformPlayDTMFCallAction');
end;

procedure TCXProviderDelegate.providerPerformSetGroupCallAction(provider: CXProvider; action: CXSetGroupCallAction);
begin
  TOSLog.d('TCXProviderDelegate.providerPerformSetGroupCallAction');
end;

procedure TCXProviderDelegate.providerPerformSetHeldCallAction(provider: CXProvider; action: CXSetHeldCallAction);
begin
  TOSLog.d('TCXProviderDelegate.providerPerformSetHeldCallAction');
end;

procedure TCXProviderDelegate.providerPerformSetMutedCallAction(provider: CXProvider; action: CXSetMutedCallAction);
begin
  TOSLog.d('TCXProviderDelegate.providerPerformSetMutedCallAction');
end;

procedure TCXProviderDelegate.providerPerformStartCallAction(provider: CXProvider; action: CXStartCallAction);
begin
  TOSLog.d('TCXProviderDelegate.providerPerformStartCallAction');
end;

procedure TCXProviderDelegate.providerTimedOutPerformingAction(provider: CXProvider; action: CXAction);
begin
  TOSLog.d('TCXProviderDelegate.providerTimedOutPerformingAction');
end;

{ TPlatformVOIP }

constructor TPlatformVOIP.Create(const AVOIP: TVOIP);
begin
  inherited;
  FProviderDelegate := TCXProviderDelegate.Create(Self);
  FController := TCXCallController.Create;
  UpdateProvider;
end;

destructor TPlatformVOIP.Destroy;
begin
  FProviderDelegate.Free;
  inherited;
end;

procedure TPlatformVOIP.UpdateProvider;
var
  LConfiguration: CXProviderConfiguration;
  LData: NSData;
begin
  LConfiguration := TCXProviderConfiguration.Create;
  LConfiguration := TCXProviderConfiguration.Wrap(LConfiguration.initWithLocalizedName(StrToNSStr('VOIPDemo'))); // TMacHelperEx.GetBundleValueNS('CFBundleDisplayName')
  if not Icon.IsEmpty then
  begin
    LData := TNSData.Wrap(UIImagePNGRepresentation(NSObjectToID(BitmapToUIImage(Icon))));
    LConfiguration.setIconTemplateImageData(LData); //   120 x 120 transparent PNG
  end;
  // LConfiguration.setRingtoneSound
  // LConfiguration.setSupportsVideo(True); // Default False ?
  // LConfiguration.setIncludeCallsInRecents(False); // Default True ? iOS 11 and above
  if FProvider = nil then
  begin
    FProvider := TCXProvider.Create;
    FProvider := TCXProvider.Wrap(FProvider.initWithConfiguration(LConfiguration));
    FProvider.setDelegate(FProviderDelegate.GetObjectID, 0);
  end
  else
    FProvider.setConfiguration(LConfiguration);
end;

procedure TPlatformVOIP.IconUpdated;
begin
  UpdateProvider;
end;

procedure TPlatformVOIP.IncomingCallCompletionHandler(error: NSError);
begin
  TOSLog.d('TPlatformVOIP.IncomingCallCompletionHandler');
  if error <> nil then
  begin
    TOSLog.d('> error is nil');
    if FIncomingUUID <> nil then
      VOIPCallState(TVOIPCallState.IncomingCall, NSStrToStr(FIncomingUUID.UUIDString));
    //!!!!! else something is wrong
  end
  else
    TOSLog.d('> error: %s', [NSStrToStr(error.localizedDescription)]);
end;

procedure TPlatformVOIP.PerformAnswerCallAction(const action: CXAnswerCallAction);
begin
  FIsCalling := False;
  action.fulfill;
  VOIPCallState(TVOIPCallState.CallAnswered, NSStrToStr(action.callUUID.UUIDString));
end;

procedure TPlatformVOIP.PerformEndCallAction(const action: CXEndCallAction);
var
  LState: TVOIPCallState;
begin
  action.fulfill;
  if FIsCalling then
    LState := TVOIPCallState.CallDeclined
  else
    LState := TVOIPCallState.CallDisconnected;
  VOIPCallState(LState, NSStrToStr(action.callUUID.UUIDString));
end;

procedure TPlatformVOIP.ReportIncomingCall(const AIdent, ADisplayName: string);
var
  LUpdate: CXCallUpdate;
  LHandle: CXHandle;
begin
  TOSLog.d('+TPlatformVOIP.ReportIncomingCall');
  if TThread.CurrentThread.ThreadID = MainThreadID then
    TOSLog.d('> On main thread')
  else
    TOSLog.d('> Not on main thread');
  FIncomingUUID := nil;
  FIncomingUUID := TNSUUID.Wrap(TNSUUID.OCClass.UUID);
  LHandle := TCXHandle.Create;
  LHandle := TCXHandle.Wrap(LHandle.initWithType(CXHandleTypeEmailAddress, StrToNSStr(AIdent)));
  LUpdate := TCXCallUpdate.Create;
  // LUpdate.setHasVideo(True); // for video
  LUpdate.setRemoteHandle(LHandle);
  LUpdate.setLocalizedCallerName(StrToNSStr(ADisplayName));
  TOSLog.d('> reportNewIncomingCallWithUUID: %s', [NSStrToStr(FIncomingUUID.UUIDString)]);
  FProvider.reportNewIncomingCallWithUUID(FIncomingUUID, LUpdate, IncomingCallCompletionHandler);
  TOSLog.d('-TPlatformVOIP.ReportIncomingCall');
end;

procedure TPlatformVOIP.RequestStartCallTransactionCompletionHandler(error: NSError);
begin
  FIsCalling := error = nil;
end;

procedure TPlatformVOIP.StartCall(const ADisplayName: string);
var
  LHandle: CXHandle;
  LUUID: NSUUID;
  LTransaction: CXTransaction;
  LAction: CXStartCallAction;
begin
  LUUID := TNSUUID.Wrap(TNSUUID.OCClass.UUID);
  LHandle := TCXHandle.Create;
  LHandle := TCXHandle.Wrap(LHandle.initWithType(CXHandleTypeGeneric, StrToNSStr(ADisplayName)));
  LAction := TCXStartCallAction.Create;
  LAction := TCXStartCallAction.Wrap(LAction.initWithCallUUID(LUUID, LHandle));
  LTransaction := TCXTransaction.Create;
  LTransaction := TCXTransaction.Wrap(LTransaction.initWithAction(LAction));
  FController.requestTransaction(LTransaction, RequestStartCallTransactionCompletionHandler);
end;

procedure TPlatformVOIP.VOIPCallState(const ACallState: TVOIPCallState; const AIdent: string);
begin
  DoVOIPCallState(ACallState, AIdent);
end;

procedure TPlatformVOIP.ReportOutgoingCall;
var
  LUUID: NSUUID;
begin
  // This tells iOS that the call is considered as being connected, and the in-call timer starts
  LUUID := TCXCall.Wrap(FController.callObserver.calls.objectAtIndex(0)).UUID;
  FProvider.reportOutgoingCallWithUUIDConnectedAtDate(LUUID, nil); // nil NSDate means now
end;

end.
