SML := sml
SMLFLAGS := -Cprint.depth=30

MLTON := mlton
MLB := concordia.mlb

DEPS := ../prover/prover.sml ../parcom/parcom.sml
SRC := util.sml document.sml cst.sml parser.sml transform.sml html_gen.sml html_backend.sml
BIN := concordia

compile: $(SRC)
	$(SML) $(SMLFLAGS) $(DEPS) $(SRC)

test: $(SRC) test.sml concordia.sml
	$(SML) $(SMLFLAGS) $(DEPS) $(SRC) test.sml

$(BIN): $(SRC) concordia.sml $(MLB)
	$(MLTON) $(MLB)

clean:
	rm $(BIN)
