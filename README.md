record_diagram
==============

Generate a UML-ish diagram from Erlang record specs.

##Build

To build 'record_diagram' just run 'make'.

Note that this will download and include 'plantuml.jar' from the PlantUML project.

##Run

Copy 'record_diagram' to a place on your path.  Run:

    record_diagram --png diagram.png <erlang source files>

##PlantUML

'record_diagram' uses [PlantUML](http://plantuml.sourceforge.net/) internally to generate the diagrams.

