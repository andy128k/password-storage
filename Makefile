test:
	cargo test -- --nocapture --test-threads 1

coverage:
	cargo tarpaulin --out Xml -v -- --nocapture --test-threads 1

deb:
	cargo deb
