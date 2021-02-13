## VOIP Test

This is a work in progress for VOIP support (initially iOS) for Delphi

This repo has been created so as to seek help with at least one outstanding issue, so is only temporary and may be removed at any time

All required files have been included, so please indicate if there any missing

### Setup

In order to test this project, you will need to:

Log in to the Apple Developer site and:
* Create an App ID that has Push Notifications enabled. Alternatively, use an existing App ID that has them enabled
* Create a VoIP Services certificate, download it and import it into KeyChain Access on your Mac

In the KeyChain Access app on your Mac, right-click the VoIP services certificate, click Export and save the exported certificate in .p12 format (the default) somewhere on the Mac

Download the Pusher app (a macOS app used for testing push notifications) from [here](https://github.com/noodlewerk/NWPusher/releases/tag/0.7.5)

In the Delphi SDK Manager, add the CallKit and PushKit frameworks to the iOS SDK. [This link](https://delphiworlds.com/2013/10/adding-other-ios-frameworks-to-the-sdk-manager/) has a guide on how to add frameworks.

In the Version Info of the Project Options for the project, select All Configurations - iOS Device 64-bit in the combobox, and modify the CFBundleIdentifier value to match the identifier of the App ID

### Running/testing

On the Mac:
* Run the Console app (in /Applications/Utilities)
* In the filter edit box in the top right of Console, type "VOIPDemo" (without the quotes) and hit enter
* Click where it says "Any" just before VOIPDemo, and select Process from the dropdown
* In the filter edit box, type "DEBUG:" (without the quotes) and hit enter
* Select the target iOS device from the list on the left

On the Windows machine, compile/deploy/run the app

On the Mac:
* In the Console app, you should see a line that begins with DEBUG: Token. Select this line, and in the memo at the bottom, select and copy the token value
* Run the Pusher app, click the combobox at the top, click the "Import PKCS # 12 file.." option and select the .p12 file that you saved earlier (see Setup section)
* Paste the token value copied earlier into the token edit box
* In the payload memo, paste in this text:  {"id": "someone@somedomain.com", "caller": "Me"}
* Click the Push button

This should result in a push notification being sent to the device, which in turn ultimately calls the `TPlatformVOIP.ReportIncomingCall` method in the `DW.VOIP.iOS` unit.
You should see messages in the Console app that reflect this process

### The issue

The issue is that the call to `FProvider.reportNewIncomingCallWithUUID` inside the `TPlatformVOIP.ReportIncomingCall` method **should** result in the CallKit UI being displayed, as well as the `TPlatformVOIP.IncomingCallCompletionHandler` method being called, neither of which occurs
As a guide, the result should look something like this:

![example](https://user-images.githubusercontent.com/26162804/32139717-cc2309a6-bc1f-11e7-91f4-6bbb158cbeb4.png)

Which is from [this article](https://websitebeaver.com/callkit-swift-tutorial-super-easy)




