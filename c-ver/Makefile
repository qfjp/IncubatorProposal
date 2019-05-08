NAME=challenge

SRCDIR = src
INCDIR = inc
OBJDIR = build

CC = gcc
DATACC = clang -MJ
LINKER = gcc -o

INCLUDES =  -I./$(INCDIR) -I/usr/include -I/usr/include/glib-2.0 -I/usr/lib/glib-2.0/include
LIBS = -lglib-2.0 -ligraph

CFLAGS = $(INCLUDES) $(LIBS) -g

INC = $(wildcard $(INCDIR)/*.h)
SRC = $(wildcard $(SRCDIR)/*.c)
DAT = $(SRC:$(SRCDIR)/%.c=$(OBJDIR/%.o.json))
OBJ = $(SRC:$(SRCDIR)/%.c=$(OBJDIR)/%.o)

all = $(NAME)

$(NAME) : $(OBJ)
	$(LINKER) $@ $(CFLAGS) $(OBJ) $(LFLAGS) $(NAME).c

$(OBJ): $(OBJDIR)/%.o : $(SRCDIR)/%.c
	$(CC) $(CFLAGS) -c -o $@	$<

gendata: json_names

json_names: CC = clang -MJ $@.json
json_names: $(OBJ)
	sed -e '1s/^/[\n/' -e '$$s/,$$/\n]/' $(OBJDIR)/*.o.json > $(OBJDIR)/compile_commands.json
	rm $(OBJDIR)/*.o.json

clean:
	rm $(OBJ) $(NAME)

.PHONY: all clean
