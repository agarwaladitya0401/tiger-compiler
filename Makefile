# this is makefile
.PHONY: all clean

all:
    mlton tc.sml
    @echo "done : execuatble"
    
clean: 
    rm tc
    @echo "cleaned"
