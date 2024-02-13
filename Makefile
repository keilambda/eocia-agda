build/Main:
	agda --compile src/*.agda --compile-dir build

clean:
	rm -rf build

.PHONY: clean
