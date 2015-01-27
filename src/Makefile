all:
	hastec Main.hs --onexec --debug --opt-whole-program --output-jsflow
clean:
	find . -iname '*.hi' -exec rm \{\} \;
	find . -iname '*.o' -exec rm \{\} \;
	find . -iname '*.js' -exec rm \{\} \;
	rm -rf main/
