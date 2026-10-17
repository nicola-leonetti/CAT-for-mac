#define MyAppName "CAT"
#define MyAppVersion "October 9 2026"
#define MyAppExeName "CAT.lnk"
#define MyAppPublisher ""
#define MyAppURL "http://www.gruppochemiometria.it/"

[Setup]
AppName = {#MyAppName}
AppId = {{AA3048DF-BE73-4968-A243-2BEF994C222B}
DefaultDirName = {sd}\{#MyAppName}
DefaultGroupName = {#MyAppName}
OutputDir = SetUp
OutputBaseFilename = setup_{#MyAppName}
SetupIconFile = cat.ico
AppVersion = {#MyAppVersion}
AppPublisher = {#MyAppPublisher}
AppPublisherURL = {#MyAppURL}
AppSupportURL = {#MyAppURL}
AppUpdatesURL = {#MyAppURL}
PrivilegesRequired = none
;InfoBeforeFile = infobefore.txt
;InfoAfterFile = infoafter.txt
Compression=lzma
SolidCompression = yes
LicenseFile = gpl_3.0.txt

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\cat.ico"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{commonprograms}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\cat.ico"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon; IconFilename: "{app}\cat.ico"

[Files]

Source: "C:\CAT\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs

; NOTE: Don't use "Flags: ignoreversion" on any shared system files





