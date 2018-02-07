SML := sml
SMLFLAGS := -Cprint.depth=30

DEPS := ../prover/prover.sml ../parcom/parcom.sml
SRC := util.sml document.sml cst.sml parser.sml transform.sml html_gen.sml html_backend.sml

compile: $(SRC)
	$(SML) $(SMLFLAGS) $(DEPS) $(SRC)

test: $(SRC) test.sml
	$(SML) $(SMLFLAGS) $(DEPS) $(SRC) test.sml
