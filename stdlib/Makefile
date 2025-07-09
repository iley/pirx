OBJ=builtin.o
LIB=libpirx.a

$(LIB): $(OBJ)
	$(AR) rcs $@ $^

$(OBJ): %.o: %.c %.h
	$(CC) -g -c -o $@ $<

clean: 
	rm -f $(OBJ) $(LIB)

.PHONY: clean
