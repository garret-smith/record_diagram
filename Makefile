
default: ebin/plantuml.jar
	./rebar compile
	./rebar escriptize

ebin/plantuml.jar:
	mkdir -p ebin
	curl -L http://sourceforge.net/projects/plantuml/files/plantuml.jar/download -o ebin/plantuml.jar

clean:
	./rebar clean

