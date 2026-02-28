JAVA ?= /opt/java/openjdk/bin/java
COBOL=cobc
MVN=mvn

SRC=src
BUILD=build
TOOLS=tools/cobol-preprocessor
TWIN=custproc-java

JAR=$(TOOLS)/target/cobol-preprocessor-0.1.0.jar
CORE=$(BUILD)/CUSTPROC_CORE.cbl
EXE=$(BUILD)/custproc
TWIN_JAR=$(TWIN)/target/custproc-java-0.1.0.jar

all: prep compile

$(JAR):
	cd $(TOOLS) && $(MVN) -q -DskipTests package

prep: $(JAR)
	mkdir -p $(BUILD)
	$(JAVA) -jar $(JAR) $(SRC)/CUSTPROC.cbl $(CORE)

compile:
	$(COBOL) -x -free $(SRC)/TEST-WRAPPER.cbl $(CORE) $(SRC)/DLI-STUB.cbl -o $(EXE)

run:
	cp tests/data/db2_in.dat .
	./$(EXE)

java:
	cd $(TWIN) && $(MVN) -q package

twin: all java
	cp tests/data/db2_in.dat .
	python3 tests/compare.py

clean:
	rm -rf $(BUILD) $(TOOLS)/target $(TWIN)/target db2_in.dat db2_out.dat
