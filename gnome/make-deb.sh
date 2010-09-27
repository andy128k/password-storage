#!/bin/sh

VERSION=0.1
DATE=`date -R`

PREFIX=deb

rm -rf $PREFIX
mkdir -p $PREFIX/usr/share/mime/packages/
cp revelation.xml $PREFIX/usr/share/mime/packages/
mkdir -p $PREFIX/usr/share/icons/hicolor/48x48/mimetypes/
cp gnome-mime-application-x-revelation.png $PREFIX/usr/share/icons/hicolor/48x48/mimetypes/
mkdir -p $PREFIX/usr/share/doc/revelation-mime/

D=DEBIAN
mkdir -p $PREFIX/$D

echo "Package: revelation-mime"                                             > $PREFIX/$D/control
echo "Version: $VERSION"                                                   >> $PREFIX/$D/control
echo "Maintainer: Andrey Kutejko <andy128k@gmail.com>"                     >> $PREFIX/$D/control
echo "Architecture: all"                                                   >> $PREFIX/$D/control
echo "Section: misc"                                                       >> $PREFIX/$D/control
echo "Priority: optional"                                                  >> $PREFIX/$D/control
echo "Description: MIME type for Revelation password databases"            >> $PREFIX/$D/control
echo " This package registers MIME type for Revelation password databases" >> $PREFIX/$D/control
echo "Depends: shared-mime-info"                                           >> $PREFIX/$D/control

echo "revelation-mime ($VERSION) stable; urgency=medium"  > $PREFIX/usr/share/doc/revelation-mime/changelog
echo ""                                                  >> $PREFIX/usr/share/doc/revelation-mime/changelog
echo "  * dummy."                                        >> $PREFIX/usr/share/doc/revelation-mime/changelog
echo ""                                                  >> $PREFIX/usr/share/doc/revelation-mime/changelog
echo " -- Andrey Kutejko <andy128k@gmail.com>  $DATE"    >> $PREFIX/usr/share/doc/revelation-mime/changelog
echo ""                                                  >> $PREFIX/usr/share/doc/revelation-mime/changelog
gzip -9 $PREFIX/usr/share/doc/revelation-mime/changelog

fakeroot dpkg-deb -Zbzip2 -z9 --build $PREFIX .
#  -z9 -Zxz
rm -rf $PREFIX

