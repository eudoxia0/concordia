SML := sml
SMLFLAGS := -Cprint.depth=10 -m
MLTON := mlton

BIN := concordia

CM_FILE := concordia.cm
MLB_FILE := concordia.mlb

MLB_TEST_FILE := concordia-test.mlb
TEST_BIN := concordia-test

VENDOR_DIR := vendor
PARSIMONY := $(VENDOR_DIR)/parsimony
PARSIMONY_URL := https://github.com/eudoxia0/parsimony.git

SRC := src/*.sig src/*.sml

all: compile

$(VENDOR_DIR):
	mkdir -p $(VENDOR_DIR)

$(PARSIMONY): $(VENDOR_DIR)
	git clone $(PARSIMONY_URL) $(PARSIMONY)

compile: $(SRC) $(PARSIMONY)
	$(SML) $(SMLFLAGS) $(CM_FILE)

$(BIN): $(SRC) $(PARSIMONY)
	$(MLTON) $(MLB_FILE)

clean:
	rm $(BIN)
	rm -rf $(VENDOR_DIR)
