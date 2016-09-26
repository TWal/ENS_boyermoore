OBJS=moore.cmx
PROG=moore.prog
CC=ocamlopt

all: $(PROG)

$(PROG): $(OBJS)
	ocamlopt    -o $@ $<

%.cmx: %.ml
	ocamlopt -c -o $@ $^

clean:
	@rm -f $(PROG) $(OBJS) -

rec: clean all

.PHONY: all clean rec


