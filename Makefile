SML := sml
SMLFLAGS := -Cprint.depth=10 -m
MLTON := mlton

BIN := concordia

CM_FILE := concordia.cm
MLB_FILE := concordia.mlb

MLB_TEST_FILE := concordia-test.mlb
TEST_BIN := concordia-test

all: compile

compile:
	$(SML) $(SMLFLAGS) $(CM_FILE)

$(BIN):
	$(MLTON) $(MLB_FILE)

clean:
	rm $(BIN)
