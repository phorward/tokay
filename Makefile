.PHONY: .FORCE

ETARENEG=awk -f etareneg.awk
TOKAY_BIN=cargo run --
TOKAY_TOK=src/compiler/tokay.tok

help:
	@echo "No target specified."
	@echo ""
	@echo "See README.md build-section for details."
	@echo ""
	@echo "  make builtins     update src/_builtins.rs"
	@echo "  make parser-cbor  update src/compiler/_tokay.cbor from $(TOKAY_TOK)"
	@echo "  make parser-ast   update src/compiler/_tokay.rs from $(TOKAY_TOK)"
	@echo "  make prelude      update src/compiler/prelude.rs from src/prelude.tok"
	@echo ""
	@echo "This is the Tokay source generation toolchain."
	@echo "To build Tokay, simply run 'cargo build' or 'cargo run'."

all:
	make prelude
	make parser-cbor
	make parser-ast
	make builtins

# builtins --------------------------------------------------------------------
BUILTINS=src/_builtins.rs

builtins: $(BUILTINS)

$(BUILTINS): .FORCE
	$(ETARENEG) $@ >$@.1 && mv $@.1 $@

show-builtins:
	$(ETARENEG) $(BUILTINS) 2>/dev/null

reset-builtins:
	git checkout $(BUILTINS)

# parser-cbor ------------------------------------------------------------------
PARSER_CBOR=src/compiler/_tokay.cbor

parser-cbor: $(PARSER_CBOR)

$(PARSER_CBOR): .FORCE
	$(TOKAY_BIN) -c $(PARSER_CBOR) $(TOKAY_TOK)

reset-parser-cbor:
	git checkout $(PARSER_CBOR)

# parser-ast -------------------------------------------------------------------
PARSER_AST=src/compiler/_tokay.rs

parser-ast: $(PARSER_AST)

$(PARSER_AST): .FORCE
	$(ETARENEG) $@ >$@.1 && mv $@.1 $@

show-parser-ast:
	$(ETARENEG) $(PARSER_AST) 2>/dev/null

reset-parser-ast:
	git checkout $(PARSER_AST)

# prelude ----------------------------------------------------------------------
PRELUDE=src/compiler/prelude.rs

prelude: $(PRELUDE)

$(PRELUDE): .FORCE
	$(ETARENEG) $@ >$@.1 && mv $@.1 $@

show-prelude:
	$(ETARENEG) $(PRELUDE) 2>/dev/null

reset-prelude:
	git checkout $(PRELUDE)
