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

DATE=`date -R`
VERSION=`date +0.%-y.%-m.%-d`

PREFIX=deb/PassStorage

rm -rf deb
mkdir -p $PREFIX/usr/bin
cp PassStorage $PREFIX/usr/bin/
cp -aR share $PREFIX/usr/

mkdir -p $PREFIX/usr/share/applications/
echo "[Desktop Entry]"                       > $PREFIX/usr/share/applications/PassStorage.desktop
echo "Version=$VERSION"                     >> $PREFIX/usr/share/applications/PassStorage.desktop
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

mkdir -p $PREFIX/DEBIAN

echo "Package: PassStorage"                             > $PREFIX/DEBIAN/control
echo "Version: $VERSION"                               >> $PREFIX/DEBIAN/control
echo "Maintainer: Andrey Kutejko <andy128k@gmail.com>" >> $PREFIX/DEBIAN/control
echo "Architecture: $ARCH"                             >> $PREFIX/DEBIAN/control
echo "Description: Secure storage for passwords"       >> $PREFIX/DEBIAN/control
echo "Depends: libgtk2.0-0 (>= 2.16)"                  >> $PREFIX/DEBIAN/control

echo "PassStorage ($VERSION) stable; urgency=medium" > $PREFIX/DEBIAN/changelog
echo ""                                             >> $PREFIX/DEBIAN/changelog
echo "* dummy."                                     >> $PREFIX/DEBIAN/changelog
echo ""                                             >> $PREFIX/DEBIAN/changelog
echo "-- Andrey Kutejko <andy128k@gmail.com> $DATE" >> $PREFIX/DEBIAN/changelog

chown -R root:root $PREFIX/

cd deb
dpkg-deb -z9 -Zlzma --build PassStorage
cd ..
cp deb/PassStorage.deb PassStorage_${VERSION}_${ARCH}.deb
rm -rf deb

