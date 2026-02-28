COBOL=cobc
MVN=mvn

SRC=src
BUILD=build
TOOLS=tools/cobol-preprocessor

JAR=$(TOOLS)/target/cobol-preprocessor-0.1.0.jar
CORE=$(BUILD)/CUSTPROC_CORE.cbl
EXE=$(BUILD)/custproc

all: prep compile

$(JAR):
	cd $(TOOLS) && $(MVN) -q -DskipTests package

prep: $(JAR)
	mkdir -p $(BUILD)
	java -jar $(JAR) $(SRC)/CUSTPROC.cbl $(CORE)

compile:
	$(COBOL) -x -free $(SRC)/TEST-WRAPPER.cbl $(CORE) $(SRC)/DLI-STUB.cbl -o $(EXE)

run:
	./$(EXE)

clean:
	rm -rf $(BUILD) $(TOOLS)/target db2_in.dat db2_out.dat
