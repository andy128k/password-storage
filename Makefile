test:
	cargo test -- --nocapture --test-threads 1

coverage:
	cargo tarpaulin --forward --timeout 5 --out Xml --verbose -- --nocapture --test-threads=1

flatpak:
	flatpak-builder build-dir --user --install-deps-from=flathub --force-clean build-flatpak/dev.andy128k.password-storage.yml
	flatpak build-export export build-dir
	flatpak build-bundle export dev.andy128k.password-storage.flatpak dev.andy128k.password-storage

release:
	cargo build --release

osx_icon:
	rm -rf ./PasswordStorage.iconset
	mkdir ./PasswordStorage.iconset
	rsvg-convert -w   16 -h   16 icons/app-icon/password-storage.svg -o ./PasswordStorage.iconset/icon_16x16.png
	rsvg-convert -w   32 -h   32 icons/app-icon/password-storage.svg -o ./PasswordStorage.iconset/icon_16x16@2x.png
	rsvg-convert -w   32 -h   32 icons/app-icon/password-storage.svg -o ./PasswordStorage.iconset/icon_32x32.png
	rsvg-convert -w   64 -h   64 icons/app-icon/password-storage.svg -o ./PasswordStorage.iconset/icon_32x32@2x.png
	rsvg-convert -w  128 -h  128 icons/app-icon/password-storage.svg -o ./PasswordStorage.iconset/icon_128x128.png
	rsvg-convert -w  256 -h  256 icons/app-icon/password-storage.svg -o ./PasswordStorage.iconset/icon_128x128@2x.png
	rsvg-convert -w  256 -h  256 icons/app-icon/password-storage.svg -o ./PasswordStorage.iconset/icon_256x256.png
	rsvg-convert -w  512 -h  512 icons/app-icon/password-storage.svg -o ./PasswordStorage.iconset/icon_256x256@2x.png
	rsvg-convert -w  512 -h  512 icons/app-icon/password-storage.svg -o ./PasswordStorage.iconset/icon_512x512.png
	rsvg-convert -w 1024 -h 1024 icons/app-icon/password-storage.svg -o ./PasswordStorage.iconset/icon_512x512@2x.png
	iconutil -c icns ./PasswordStorage.iconset
	rm -r ./PasswordStorage.iconset

osx_app: release osx_icon
	mkdir -p target/dmg/root/PasswordStorage.app/Contents/MacOS
	mkdir -p target/dmg/root/PasswordStorage.app/Contents/Resources
	cp macos/PkgInfo                                  target/dmg/root/PasswordStorage.app/Contents/
	sed 's/{VERSION}/$(VERSION)/' macos/Info.plist >  target/dmg/root/PasswordStorage.app/Contents/Info.plist
	cp macos/PasswordStorage                          target/dmg/root/PasswordStorage.app/Contents/MacOS/
	cp PasswordStorage.icns                           target/dmg/root/PasswordStorage.app/Contents/Resources/
	cp target/release/password-storage                target/dmg/root/PasswordStorage.app/Contents/Resources/
	rm -f target/dmg/PasswordStorage.dmg
	npx appdmg dmg.json target/dmg/PasswordStorage-$(VERSION).dmg
