
DEPEND += tokens.hs grammar.hs eval.hs


all: $(DEPEND) MyInterpreter

MyInterpreter: $(DEPEND) myinterpreter.hs
	ghc myinterpreter.hs


grammar.hs : grammar.y
	@rm -f grammar.hs
	happy grammar.y
	@chmod -w grammar.hs

tokens.hs : tokens.x
	@rm -f tokens.hs
	alex tokens.x
	@chmod -w tokens.hs

clean::
	rm -rf tokens.hs grammar.hs *.hi *.o *.info


