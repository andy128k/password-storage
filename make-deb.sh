#!/bin/bash

case `uname -m` in
    x86_64)
        ARCH=amd64
        ;;
    i686)
        ARCH=i386
        ;;
    *)
        echo 'Unknown architecture'
        exit 1
esac

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

DATE=`date -R`

PREFIX=deb

rm -rf $PREFIX
mkdir -p $PREFIX/usr/bin
cp PassStorage $PREFIX/usr/bin/
cp -aR share $PREFIX/usr/

mkdir -p $PREFIX/usr/share/applications/
echo "[Desktop Entry]"                       > $PREFIX/usr/share/applications/PassStorage.desktop
echo "Version=1.0"                          >> $PREFIX/usr/share/applications/PassStorage.desktop
echo "Name=PassStorage"                     >> $PREFIX/usr/share/applications/PassStorage.desktop
echo "GenericName=PassStorage"              >> $PREFIX/usr/share/applications/PassStorage.desktop
echo "Comment=Secure storage for passwords" >> $PREFIX/usr/share/applications/PassStorage.desktop
echo "Exec=PassStorage"                     >> $PREFIX/usr/share/applications/PassStorage.desktop
echo "Icon=pass-storage"                    >> $PREFIX/usr/share/applications/PassStorage.desktop
echo "Terminal=false"                       >> $PREFIX/usr/share/applications/PassStorage.desktop
echo "Type=Application"                     >> $PREFIX/usr/share/applications/PassStorage.desktop
echo "Categories=Utility;"                  >> $PREFIX/usr/share/applications/PassStorage.desktop
echo "StartupNotify=true"                   >> $PREFIX/usr/share/applications/PassStorage.desktop
echo "MimeType=application/x-revelation;"   >> $PREFIX/usr/share/applications/PassStorage.desktop

D=DEBIAN
mkdir -p $PREFIX/$D

echo "Package: pass-storage"                                             > $PREFIX/$D/control
echo "Version: $VERSION"                                                >> $PREFIX/$D/control
echo "Maintainer: Andrey Kutejko <andy128k@gmail.com>"                  >> $PREFIX/$D/control
echo "Architecture: $ARCH"                                              >> $PREFIX/$D/control
echo "Section: misc"                                                    >> $PREFIX/$D/control
echo "Priority: optional"                                               >> $PREFIX/$D/control
echo "Description: Password manager"                                    >> $PREFIX/$D/control
echo " PassStorage manages passwords/secrets and stores them securely." >> $PREFIX/$D/control
echo "Depends: libc6, libgtk2.0-0 (>= 2.18)"                            >> $PREFIX/$D/control

echo "pass-storage ($VERSION) stable; urgency=medium"  > $PREFIX/usr/share/doc/pass-storage/changelog
echo ""                                               >> $PREFIX/usr/share/doc/pass-storage/changelog
echo "  * dummy."                                     >> $PREFIX/usr/share/doc/pass-storage/changelog
echo ""                                               >> $PREFIX/usr/share/doc/pass-storage/changelog
echo " -- Andrey Kutejko <andy128k@gmail.com>  $DATE" >> $PREFIX/usr/share/doc/pass-storage/changelog
echo ""                                               >> $PREFIX/usr/share/doc/pass-storage/changelog
gzip -9 $PREFIX/usr/share/doc/pass-storage/changelog

fakeroot dpkg-deb -Zbzip2 -z9 --build $PREFIX .
#  -z9 -Zxz
rm -rf $PREFIX

