;; -*- ini-generic -*-
;; $Id$

[Setup]
AppName=Expert Guide for Windows
AppVerName=Expert Guide for Windows v2.14
AppVersion=2.14
AppCopyright=Copyright © 1998-2004 Dave Pearson
AppPublisher=Dave Pearson
AppPublisherURL=http://www.davep.org/
AppSupportURL=http://www.davep.org/
AppUpdatesURL=http://www.davep.org/norton-guides/
AlwaysShowDirOnReadyPage=Yes
AlwaysShowGroupOnReadyPage=Yes
DefaultDirName={pf}\Expert Guide for Windows
DefaultGroupName=Expert Guide for Windows
LicenseFile=COPYING
OutputBaseFileName=WEGSetup
OutputDir=.
ChangesAssociations=Yes
VersionInfoCompany=davep.org <http://www.davep.org/>
VersionInfoVersion=2.14
VersionInfoTextVersion=$Revision$
WizardSmallImageFile=Images\WEGInstallImage.bmp
Compression=bzip
SolidCompression=Yes

[Tasks]
Name: "desktopicon";     Description: "Create a &desktop icon"; GroupDescription: "Additional icons:"
Name: "quicklaunchicon"; Description: "Create a &Quick Launch icon"; GroupDescription: "Additional icons:"; Flags: unchecked
Name: "assocngfiles";    Description: "Associate Expert Guide with Norton Guides"; GroupDescription: "Associations"

[Components]
Name: "WEGSources";    Description: "WEG Sources"; Types: full
Name: "WEGLibSources"; Description: "WEGLib Sources"; Types: full

[Files]
; Main application files.
Source: "WEG.exe";     DestDir: "{app}"; Flags: ignoreversion
Source: "COPYING";     DestDir: "{app}"; Flags: ignoreversion
Source: "README.txt";  DestDir: "{app}"; Flags: ignoreversion isreadme
Source: "CHANGES.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "TODO.txt";    DestDir: "{app}"; Flags: ignoreversion
Source: "BUGS.txt";    DestDir: "{app}"; Flags: ignoreversion
Source: "website.url"; DestDir: "{app}"; Flags: ignoreversion
Source: "email.url";   DestDir: "{app}"; Flags: ignoreversion
; Optional WEG sources.
Source: "*.dpr";        DestDir: "{app}\Source\WEG";        Flags: ignoreversion; Components: WEGSources
Source: "*.res";        DestDir: "{app}\Source\WEG";        Flags: ignoreversion; Components: WEGSources
Source: "*.dof";        DestDir: "{app}\Source\WEG";        Flags: ignoreversion; Components: WEGSources
Source: "*.pas";        DestDir: "{app}\Source\WEG";        Flags: ignoreversion; Components: WEGSources
Source: "*.dfm";        DestDir: "{app}\Source\WEG";        Flags: ignoreversion; Components: WEGSources
Source: "Icons\*.ico";  DestDir: "{app}\Source\WEG\Icons";  Flags: ignoreversion; Components: WEGSources
Source: "Images\*.bmp"; DestDir: "{app}\Source\WEG\Images"; Flags: ignoreversion; Components: WEGSources
; Optional WEGLib sources.
Source: "WEGLib\*.dpk"; DestDir: "{app}\Source\WEGLib"; Flags: ignoreversion; Components: WEGLibSources
Source: "WEGLib\*.res"; DestDir: "{app}\Source\WEGLib"; Flags: ignoreversion; Components: WEGLibSources
Source: "WEGLib\*.dof"; DestDir: "{app}\Source\WEGLib"; Flags: ignoreversion; Components: WEGLibSources
Source: "WEGLib\*.pas"; DestDir: "{app}\Source\WEGLib"; Flags: ignoreversion; Components: WEGLibSources
Source: "WEGLib\*.dcr"; DestDir: "{app}\Source\WEGLib"; Flags: ignoreversion; Components: WEGLibSources
Source: "WEGLib\*.dfm"; DestDir: "{app}\Source\WEGLib"; Flags: ignoreversion; Components: WEGLibSources
Source: "WEGLib\*.txt"; DestDir: "{app}\Source\WEGLib"; Flags: ignoreversion; Components: WEGLibSources

[Icons]
Name: "{group}\Expert Guide for Windows"; Filename: "{app}\WEG.exe"; Comment: "Run Expert Guide for Windows"
Name: "{group}\View README.txt"; Filename: "{app}\README.txt"; Comment: "View the README file for WEG"
Name: "{group}\View Licence"; Filename: "Notepad"; Parameters: "{app}\COPYING"; Comment: "View the licence for WEG"
Name: "{group}\View CHANGES.txt"; Filename: "{app}\CHANGES.txt"; Comment: "View the change documentation for WEG"
Name: "{group}\View TODO.txt"; Filename: "{app}\TODO.txt"; Comment: "View the list of things still to be done to WEG"
Name: "{group}\View BUGS.txt"; Filename: "{app}\BUGS.txt"; Comment: "View the list known WEG bugs"
Name: "{group}\Visit www.davep.org"; Filename: "{app}\website.url"; Comment: "Visit http://www.davep.org/"
Name: "{group}\Email the author"; Filename: "{app}\email.url"; Comment: "Send an email to the author"
Name: "{group}\WEG Sources"; Filename: "{app}\Source\WEG"; Components: WEGSources; Comment: "View the sources for WEG"
Name: "{group}\WEGLib Sources"; Filename: "{app}\Source\WEGLib"; Components: WEGLibSources; Comment: "View the sources for WEGLib"
Name: "{group}\Uninstall Expert Guide for Windows"; Filename: "{uninstallexe}"; Comment: "Uninstall Expert Guide for Windows"
Name: "{userdesktop}\Expert Guide for Windows"; Filename: "{app}\WEG.exe"; Tasks: desktopicon; Comment: "Run Expert Guide for Windows"
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\Expert Guide for Windows"; Filename: "{app}\WEG.exe"; Tasks: quicklaunchicon; Comment: "Run Expert Guide for Windows"

[Registry]
; Application settings.
Root: HKCU; SubKey: "Software\davep.org"; Flags: uninsdeletekeyifempty
Root: HKCU; SubKey: "Software\davep.org\WEG"; Flags: uninsdeletekey
; An entry to give other software a hint as to where WEG is located.
Root: HKCU; SubKey: "Software\davep.org\WEG"; ValueType: string; ValueName: ""; ValueData: "{app}\WEG.exe";
; Windows file association information.
Root: HKCR; SubKey: ".ng"; ValueType: string; ValueName: ""; ValueData: "org.davep.WEG"; Flags: uninsdeletekey; Tasks: assocngfiles
Root: HKCR; SubKey: "org.davep.WEG"; ValueType: string; ValueName: ""; ValueData: "Norton Guide Database"; Flags: uninsdeletekey;  Tasks: assocngfiles
Root: HKCR; SubKey: "org.davep.WEG\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\WEG.exe,0";  Tasks: assocngfiles
Root: HKCR; SubKey: "org.davep.WEG\shell"; ValueType: string; ValueName: ""; ValueData: "open";  Tasks: assocngfiles
Root: HKCR; SubKey: "org.davep.WEG\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\WEG.exe""";  Tasks: assocngfiles
Root: HKCR; SubKey: "org.davep.WEG\shell\open\ddeexec"; ValueType: string; ValueName: ""; ValueData: "open,""%1""";  Tasks: assocngfiles
Root: HKCR; SubKey: "org.davep.WEG\shell\open\ddeexec\Topic"; ValueType: string; ValueName: ""; ValueData: "Execute";  Tasks: assocngfiles

[Run]
Filename: "notepad"; Parameters: "{app}\CHANGES.txt"; Description: "View CHANGES.txt"; Flags: nowait postinstall skipifsilent
Filename: "{app}\WEG.exe"; Description: "Launch Expert Guide for Windows"; Flags: nowait postinstall skipifsilent unchecked
