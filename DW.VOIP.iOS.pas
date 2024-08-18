unit DW.VOIP.iOS;

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
    procedure provider(provider: CXProvider; didActivateAudioSession: AVAudioSession); overload; cdecl;
    function provider(provider: CXProvider; executeTransaction: CXTransaction): Boolean; overload; cdecl;
    procedure provider(provider: CXProvider; performAnswerCallAction: CXAnswerCallAction); overload; cdecl;
    procedure provider(provider: CXProvider; performEndCallAction: CXEndCallAction); overload; cdecl;
    procedure provider(provider: CXProvider; performPlayDTMFCallAction: CXPlayDTMFCallAction); overload; cdecl;
    procedure provider(provider: CXProvider; performSetGroupCallAction: CXSetGroupCallAction); overload; cdecl;
    procedure provider(provider: CXProvider; performSetHeldCallAction: CXSetHeldCallAction); overload; cdecl;
    procedure provider(provider: CXProvider; performSetMutedCallAction: CXSetMutedCallAction); overload; cdecl;
    procedure provider(provider: CXProvider; performStartCallAction: CXStartCallAction); overload; cdecl;
    procedure provider(provider: CXProvider; timedOutPerformingAction: CXAction); overload; cdecl;
    procedure providerDidBegin(provider: CXProvider); cdecl;
    [MethodName('provider:didDeactivateAudioSession:')]
    procedure providerDidDeactivateAudioSession(provider: CXProvider; didDeactivateAudioSession: AVAudioSession); cdecl;
    procedure providerDidReset(provider: CXProvider); cdecl;
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
    function GetVOIPHandle(const AInfo: TVOIPCallInfo): CXHandle;
    procedure IncomingCallCompletionHandler(error: NSError);
    procedure RequestStartCallTransactionCompletionHandler(error: NSError);
    procedure UpdateProvider;
  protected
    procedure IconUpdated; override;
    procedure PerformAnswerCallAction(const AAction: CXAnswerCallAction);
    procedure PerformEndCallAction(const AAction: CXEndCallAction);
    procedure PerformStartCallAction(const AAction: CXStartCallAction);
    procedure ReportIncomingCall(const AInfo: TVOIPCallInfo); override;
    procedure ReportOutgoingCall; override;
    function StartOutgoingCall(const AInfo: TVOIPCallInfo): Boolean; override;
    procedure VOIPCallStateChange(const ACallState: TVOIPCallState);
  public
    constructor Create(const AVOIP: TVOIP); override;
    destructor Destroy; override;
  end;

implementation

uses
  DW.OSLog,
  System.Classes, System.SysUtils,
  Macapi.Helpers,
  iOSapi.UIKit,
  FMX.Helpers.iOS;

const
  libAVFAudio = '/System/Library/Frameworks/AVFAudio.framework/AVFAudio';

type
  AVAudioSessionClass = interface(NSObjectClass)
    ['{F610D17D-E53E-4D85-976B-21D68784880A}']
    {class} function sharedInstance: AVAudioSession; cdecl;
  end;
  TAVAudioSession = class(TOCGenericImport<AVAudioSessionClass, AVAudioSession>) end;

  AVAudioSessionCategory = NSString;

function AVAudioSessionCategoryPlayAndRecord: AVAudioSessionCategory;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionCategoryPlayAndRecord');
end;

{ TCXProviderDelegate }

constructor TCXProviderDelegate.Create(const AVOIP: TPlatformVOIP);
begin
  inherited Create;
  FVOIP := AVOIP;
end;

procedure TCXProviderDelegate.provider(provider: CXProvider; didActivateAudioSession: AVAudioSession);
begin
  TOSLog.d('TCXProviderDelegate.providerDidActivateAudioSession');
end;

procedure TCXProviderDelegate.providerDidBegin(provider: CXProvider);
begin
  TOSLog.d('TCXProviderDelegate.providerDidBegin');
end;

procedure TCXProviderDelegate.providerDidDeactivateAudioSession(provider: CXProvider; didDeactivateAudioSession: AVAudioSession);
begin
  TOSLog.d('TCXProviderDelegate.providerDidDeactivateAudioSession');
end;

procedure TCXProviderDelegate.providerDidReset(provider: CXProvider);
begin
  TOSLog.d('TCXProviderDelegate.providerDidReset');
end;

function TCXProviderDelegate.provider(provider: CXProvider; executeTransaction: CXTransaction): Boolean;
begin
  // TOSLog.d('TCXProviderDelegate.providerExecuteTransaction');
  Result := False; // Return True when customizing how transactions are handled
end;

procedure TCXProviderDelegate.provider(provider: CXProvider; performAnswerCallAction: CXAnswerCallAction);
begin
  TOSLog.d('TCXProviderDelegate.providerPerformAnswerCallAction');
  // Called if the call is answered
  FVOIP.PerformAnswerCallAction(performAnswerCallAction);
end;

procedure TCXProviderDelegate.provider(provider: CXProvider; performEndCallAction: CXEndCallAction);
begin
  TOSLog.d('TCXProviderDelegate.providerPerformEndCallAction');
  // Called if the call is ended or declined
  FVOIP.PerformEndCallAction(performEndCallAction);
end;


procedure TCXProviderDelegate.provider(provider: CXProvider; performSetHeldCallAction: CXSetHeldCallAction);
begin
  TOSLog.d('TCXProviderDelegate.performSetHeldCallAction');
end;

procedure TCXProviderDelegate.provider(provider: CXProvider; performSetGroupCallAction: CXSetGroupCallAction);
begin
  TOSLog.d('TCXProviderDelegate.performSetGroupCallAction');
end;

procedure TCXProviderDelegate.provider(provider: CXProvider; performPlayDTMFCallAction: CXPlayDTMFCallAction);
begin
  TOSLog.d('TCXProviderDelegate.performPlayDTMFCallAction');
end;

procedure TCXProviderDelegate.provider(provider: CXProvider; timedOutPerformingAction: CXAction);
begin
  TOSLog.d('TCXProviderDelegate.timedOutPerformingAction');
end;

procedure TCXProviderDelegate.provider(provider: CXProvider; performStartCallAction: CXStartCallAction);
begin
  TOSLog.d('TCXProviderDelegate.performStartCallAction');
  FVOIP.PerformStartCallAction(performStartCallAction);
end;

procedure TCXProviderDelegate.provider(provider: CXProvider; performSetMutedCallAction: CXSetMutedCallAction);
begin
  TOSLog.d('TCXProviderDelegate.performSetMutedCallAction');
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
    FProvider := TCXProvider.Wrap(TCXProvider.Alloc.initWithConfiguration(LConfiguration));
    FProvider.setDelegate(FProviderDelegate.GetObjectID, 0);
  end
  else
    FProvider.setConfiguration(LConfiguration);
end;

procedure TPlatformVOIP.IconUpdated;
begin
  UpdateProvider;
end;

procedure TPlatformVOIP.PerformAnswerCallAction(const AAction: CXAnswerCallAction);
begin
  FIsCalling := False;
  AAction.fulfill;
  VOIPCallStateChange(TVOIPCallState.CallAnswered);
end;

procedure TPlatformVOIP.PerformEndCallAction(const AAction: CXEndCallAction);
var
  LState: TVOIPCallState;
begin
  AAction.fulfill;
  if FIsCalling then
    LState := TVOIPCallState.CallDeclined
  else
    LState := TVOIPCallState.CallDisconnected;
  VOIPCallStateChange(LState);
end;

procedure TPlatformVOIP.PerformStartCallAction(const AAction: CXStartCallAction);
var
  LAudioSession: AVAudioSession;
  LPointer: Pointer;
begin
  LAudioSession := TAVAudioSession.OCClass.sharedInstance;
  if LAudioSession.setCategory(AVAudioSessionCategoryPlayAndRecord, @LPointer) then
  begin
    TOSLog.d('TPlatformVOIP.PerformStartCallAction > AAction.fulfill');
    AAction.fulfill;
    VOIPCallStateChange(TVOIPCallState.OutgoingCall);
  end
  else
    TOSLog.d('TPlatformVOIP.PerformStartCallAction > Could not start audio session');
end;

function TPlatformVOIP.GetVOIPHandle(const AInfo: TVOIPCallInfo): CXHandle;
var
  LPointer: Pointer;
begin
  if not AInfo.PhoneNumber.IsEmpty then
    LPointer := TCXHandle.Alloc.initWithType(CXHandleTypePhoneNumber, StrToNSStr(AInfo.Email))
  else if not AInfo.Email.IsEmpty then
    LPointer := TCXHandle.Alloc.initWithType(CXHandleTypeEmailAddress, StrToNSStr(AInfo.Email))
  else
    LPointer := TCXHandle.Alloc.initWithType(CXHandleTypeGeneric, StrToNSStr(AInfo.DisplayName));
  Result := TCXHandle.Wrap(LPointer);
end;

procedure TPlatformVOIP.ReportIncomingCall(const AInfo: TVOIPCallInfo);
var
  LUpdate: CXCallUpdate;
  LHandle: CXHandle;
begin
  TOSLog.d('+TPlatformVOIP.ReportIncomingCall');
  FIncomingUUID := nil;
  // if not AInfo.UUID.IsEmpty then
  //   FIncomingUUID := TNSUUID.Wrap(TNSUUID.Alloc.initWithUUIDString(StrToNSStr(AInfo.UUID))); // <---- This no worky?
  // else
  FIncomingUUID := TNSUUID.Wrap(TNSUUID.OCClass.UUID);
  LHandle := GetVOIPHandle(AInfo);
  LUpdate := TCXCallUpdate.Wrap(TCXCallUpdate.Alloc.init);
  LUpdate.setLocalizedCallerName(StrToNSStr(AInfo.DisplayName));
  LUpdate.setHasVideo(False);
  LUpdate.setSupportsDTMF(True);
  // LUpdate.setSupportsHolding(True); // <--- Set this if the user is on another call
  LUpdate.setSupportsGrouping(False);
  LUpdate.setSupportsUngrouping(False);
  LUpdate.setRemoteHandle(LHandle);
  FProvider.reportNewIncomingCallWithUUID(FIncomingUUID, LUpdate, IncomingCallCompletionHandler);
  TOSLog.d('-TPlatformVOIP.ReportIncomingCall');
end;

procedure TPlatformVOIP.IncomingCallCompletionHandler(error: NSError);
begin
  // Even if a call is reported as incoming successfully, error here is non-nil, but causes a crash when accessing it
  TOSLog.d('TPlatformVOIP.IncomingCallCompletionHandler');
end;

function TPlatformVOIP.StartOutgoingCall(const AInfo: TVOIPCallInfo): Boolean;
var
  LHandle: CXHandle;
  LUUID: NSUUID;
  LTransaction: CXTransaction;
  LAction: CXStartCallAction;
begin
  LUUID := TNSUUID.Wrap(TNSUUID.OCClass.UUID);
  LHandle := GetVOIPHandle(AInfo);
  LAction := TCXStartCallAction.Wrap(TCXStartCallAction.Alloc.initWithCallUUID(LUUID, LHandle));
  LTransaction := TCXTransaction.Wrap(TCXTransaction.Alloc.initWithAction(LAction));
  FController.requestTransaction(LTransaction, RequestStartCallTransactionCompletionHandler);
  Result := True;
end;

procedure TPlatformVOIP.RequestStartCallTransactionCompletionHandler(error: NSError);
begin
  FIsCalling := error = nil;
end;

procedure TPlatformVOIP.VOIPCallStateChange(const ACallState: TVOIPCallState);
begin
  TOSLog.d('+TPlatformVOIP.VOIPCallState');
  DoVOIPCallStateChange(ACallState);
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
