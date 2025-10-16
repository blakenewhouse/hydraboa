.PRECIOUS: test/%.s

compile:
	cargo run -- -c test.snek test.s

test/%.s: test/%.snek src/main.rs
	cargo run --target x86_64-apple-darwin -- -c $< test/$*.s

test/%.run: test/%.s runtime/start.rs
	nasm -f macho64 test/$*.s -o runtime/our_code.o
	ar rcs runtime/libour_code.a runtime/our_code.o
	rustc --target x86_64-apple-darwin -L runtime/ runtime/start.rs -o test/$*.run