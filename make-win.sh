#!/bin/bash

while read line
do
    if [[ $line =~ \*ps-version\*[[:blank:]]+\"(.*)\" ]]
    then
	VERSION=${BASH_REMATCH[1]}
    fi
done < pass-storage.version.lisp
if [[ -z "$VERSION" ]]
then
    echo "Can't parse version"
    exit
fi

URL='http://andy128k.github.com/PassStorage'

PREFIX=C:\\projects\\PassStorage

cd windows
mingw32-make
cd ..

rm -rf win
mkdir -p win/PassStorage
cp PassStorage.exe win/PassStorage
cp windows/runner.exe win/PassStorage
cp -aR share win/PassStorage
cp -aR ../gtk-redist/* win/PassStorage

echo "[Setup]" > win/setup.iss
echo "AppName=PassStorage" >> win/setup.iss
echo "AppVerName=PassStorage $VERSION" >> win/setup.iss
echo "AppPublisher=Andrey Kutejko" >> win/setup.iss
echo "AppPublisherURL=$URL" >> win/setup.iss
echo "AppSupportURL=$URL" >> win/setup.iss
echo "AppUpdatesURL=$URL" >> win/setup.iss
echo "DefaultDirName={pf}\PassStorage" >> win/setup.iss
echo "DisableDirPage=yes" >> win/setup.iss
echo "DisableProgramGroupPage=yes" >> win/setup.iss
echo "DisableReadyPage=yes" >> win/setup.iss
echo "DefaultGroupName=PassStorage" >> win/setup.iss
echo "OutputDir=$PREFIX" >> win/setup.iss
echo "OutputBaseFilename=PassStorage-$VERSION" >> win/setup.iss
echo "Compression=lzma" >> win/setup.iss
echo "SolidCompression=yes" >> win/setup.iss
echo "WizardImageFile=$PREFIX\windows\setup-bg.bmp" >> win/setup.iss
echo "[Languages]" >> win/setup.iss
echo "Name: \"english\"; MessagesFile: \"compiler:Default.isl\"" >> win/setup.iss
echo "Name: \"russian\"; MessagesFile: \"compiler:Languages\Russian.isl\"" >> win/setup.iss
echo "[Tasks]" >> win/setup.iss
echo "Name: \"desktopicon\"; Description: \"{cm:CreateDesktopIcon}\"; GroupDescription: \"{cm:AdditionalIcons}\"; Flags: unchecked" >> win/setup.iss
echo "Name: \"quicklaunchicon\"; Description: \"{cm:CreateQuickLaunchIcon}\"; GroupDescription: \"{cm:AdditionalIcons}\"; Flags: unchecked" >> win/setup.iss
echo "[Files]" >> win/setup.iss
echo "Source: \"$PREFIX\win\PassStorage\*\"; DestDir: \"{app}\"; Flags: ignoreversion recursesubdirs createallsubdirs" >> win/setup.iss
echo "[Icons]" >> win/setup.iss
echo "Name: \"{userprograms}\PassStorage\"; Filename: \"{app}\runner.exe\"; WorkingDir: \"{app}\"" >> win/setup.iss
echo "Name: \"{commondesktop}\PassStorage\"; Filename: \"{app}\runner.exe\"; Tasks: desktopicon; WorkingDir: \"{app}\"" >> win/setup.iss
echo "Name: \"{userappdata}\Microsoft\Internet Explorer\Quick Launch\PassStorage\"; Filename: \"{app}\runner.exe\"; Tasks: quicklaunchicon; WorkingDir: \"{app}\"" >> win/setup.iss
echo "[Run]" >> win/setup.iss
echo "Filename: \"{app}\runner.exe\"; Description: \"{cm:LaunchProgram,PassStorage}\"; Flags: nowait postinstall skipifsilent; WorkingDir: \"{app}\"" >> win/setup.iss

"/C/Program Files/Inno Setup 5/ISCC.exe" win/setup.iss
rm -rf win
