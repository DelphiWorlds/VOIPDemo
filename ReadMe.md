## VOIP Test

This is a work in progress for VOIP support (initially iOS) for Delphi

This repo has been created so as to seek help with issues, so is only temporary and may be removed at any time

All required files have been included, so please indicate if there any missing

### Setup

In order to test this project, you will need to:

Log in to the Apple Developer site and:
* Create an App ID that has Push Notifications enabled. Alternatively, use an existing App ID that has them enabled
* Create a VoIP Services certificate, download it and import it into KeyChain Access on your Mac

In the KeyChain Access app on your Mac, right-click the VoIP services certificate, click Export and save the exported certificate in .p12 format (the default) somewhere on the Mac

In the Delphi SDK Manager, add the CallKit and PushKit frameworks to the iOS SDK. [This link](https://delphiworlds.com/2013/10/adding-other-ios-frameworks-to-the-sdk-manager/) has a guide on how to add frameworks.

In the Version Info of the Project Options for the project, select All Configurations - iOS Device 64-bit in the combobox, and modify the CFBundleIdentifier value to match the identifier of the App ID

### Running/testing

On the Mac:
* Run the Console app (in /Applications/Utilities)
* Select your iOS device on the left hand side
* In the filter edit box in the top right of Console, type "VOIPDemo" (without the quotes) and hit enter
* Click where it says "Any" just before VOIPDemo, and select Process from the dropdown
* In the filter edit box, type "DEBUG:" (without the quotes) and hit enter
* Click the Start button

On the Windows machine, compile/deploy/run the app

On the Mac:
* In the Console app, you should see a line that begins with DEBUG: Token. Select this line, and in the memo at the bottom, select and copy the token value
* Open a browser and go to the [APNsPush site](https://apnspush.com/)
* In Authentication Type, ensure that `Authenticate using certificates` is selected
* Click `Choose file` and select the .p12 file that you saved earlier (see Setup section)
* Enter the password that was used when exporting the .p12 file
* Paste the token value copied earlier into the `Device Token` edit box
* In the `apns-topic` edit box, enter your app's bundle identifier followed by `.voip`
* In `apns-push-type`, select `voip`
* In the payload edit box, paste in this text:
  ```json
  {
     "aps" : {
        "alert":"VoIP Call"
     },
     "email" : "davidn@radsoft.com.au",
     "uuid" : "db1dff98-bfef-4ec0-8ac6-8187a3b0c645", 
     "displayName" : "Dave"
  }
  ```
* Click the Send button

This should result in a push notification being sent to the device, which in turn ultimately calls the `TPlatformVOIP.ReportIncomingCall` method in the `DW.VOIP.iOS` unit.
You should see messages in the Console app that reflect this process

You can also "simulate" an incoming call by tapping the `Receive Call Test` button in the demo

Tapping the `Make Call Test` button appears to do nothing, however please see the issues list below (regarding `StartOutgoingCall`) as to what is happening.

### The issues

* When `FProvider.reportNewIncomingCallWithUUID` is called inside the `TPlatformVOIP.ReportIncomingCall` method, it successfully invokes the CallKit UI, however if the device is in the lock screen, there is only a "Slide to answer" option. Not sure whether this is as designed, however I suspect there should be a method of being able to decline without it. There also does not appear to be a way of having the CallKit UI "full screen", like in [this tutorial](https://websitebeaver.com/callkit-swift-tutorial-super-easy):
  
  ![example](https://user-images.githubusercontent.com/26162804/32139717-cc2309a6-bc1f-11e7-91f4-6bbb158cbeb4.png)

* Calling `FController.requestTransaction` inside the `TPlatformVOIP.StartOutgoingCall` method appears to succeed, however no CallKit UI is shown. Swiping the app up then shows the UI - obviously the user should not need to do this. 

Please use [this issue](https://github.com/DelphiWorlds/VOIPDemo/issues/1) for comments/discussion, and discussion can also occur in the #general channel in the Delphi Worlds Slack workspace

This code is destined for the [Kastri](https://github.com/DelphiWorlds/Kastri) library, so anyone who helps out will be able to benefit from it

Thanks in advance for any help!



