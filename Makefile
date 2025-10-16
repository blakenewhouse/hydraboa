.PRECIOUS: tests/%.s

tests/%.s: tests/%.snek src/main.rs
	cargo run -- -c $< tests/$*.s

tests/%.run: tests/%.s runtime/start.rs
	nasm -f elf64 tests/$*.s -o runtime/our_code.o
	ar rcs runtime/libour_code.a runtime/our_code.o
	rustc -L runtime/ runtime/start.rs -o tests/$*.run