#define WebsiteUrl "https://andy128k.github.io/password-storage/"

[Setup]
AppName=PasswordStorage
AppVersion={#Version}
AppPublisher=Andrey Kutejko
AppPublisherURL={#WebsiteUrl}
AppSupportURL={#WebsiteUrl}
AppUpdatesURL={#WebsiteUrl}
DefaultDirName={commonpf}\PasswordStorage
DisableDirPage=yes
DisableProgramGroupPage=yes
DisableReadyPage=yes
DefaultGroupName=PasswordStorage
OutputDir=..\
OutputBaseFilename=PasswordStorage-{#Version}
Compression=lzma
SolidCompression=yes
WizardImageFile=.\setup-bg.bmp
PrivilegesRequired=lowest

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "russian"; MessagesFile: "compiler:Languages\Russian.isl"

[Tasks]
Name: "desktopicon";     Description: "{cm:CreateDesktopIcon}";     GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "..\win\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]
Name: "{userprograms}\PasswordStorage";  Filename: "{app}\password-storage.exe";                      WorkingDir: "{app}"
Name: "{commondesktop}\PasswordStorage"; Filename: "{app}\password-storage.exe"; Tasks: desktopicon;  WorkingDir: "{app}"

[Run]
Filename: "{app}\password-storage.exe"; Description: "{cm:LaunchProgram,PasswordStorage}"; Flags: nowait postinstall skipifsilent; WorkingDir: "{app}"
