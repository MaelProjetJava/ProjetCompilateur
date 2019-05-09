srcs != ls *.java

build: 
	javac -deprecation -encoding utf-8 -g:none -Werror --release 8 $(srcs)

run: build
	java Main

clean:
	rm -f *.class

.PHONY: run clean build
.DELETE_ON_ERROR:
