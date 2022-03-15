%.lex.sml: %.lex
	mllex $<

# Similarly for the grammar files.
%.grm.sml: %.grm
	mlyacc $<

all: ec


.PHONY: all clean test

clean:
	rm -f *.lex.sml rp
	rm -f *.grm.sml *.grm.desc *.grm.sig ec

# rp: rp.lex.sml rp.mlb rp.sml ${COMMON}
# 	mlton rp.mlb

ec: ec.sml ec.mlb tiger.grm.sml tiger.lex.sml ${COMMON} translate.sml
	mlton ec.mlb


test: all
	${CURDIR}/ec test.expr
