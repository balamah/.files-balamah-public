# sxw - simple X widgets
# See LICENSE file for copyright and license details.

include config.mk

SRCDIR = src/
OBJDIR = obj/
BINDIR = bin/

PROGS = weather mpdplay mpdinfo brightness iconbutton infowidget volume quote planets playerctlplay

SRC = $(addprefix $(SRCDIR), $(addsuffix .c, $(PROGS))) $(SRCDIR)xresources.c
OBJ = $(addprefix $(OBJDIR), $(addsuffix .o, $(PROGS))) $(OBJDIR)xresources.o obj/drw.o obj/util.o
OUT = $(addprefix $(BINDIR), $(PROGS))

options:
	@echo date build options:
	@echo "CFLAGS   = $(CFLAGS)"
	@echo "LDFLAGS  = $(LDFLAGS)"
	@echo "CC       = $(CC)"

$(OBJ): $(SRCDIR)config.h config.mk $(SRCDIR)drw.h $(SRCDIR)util.h

$(OBJDIR)%.o : $(SRCDIR)%.c
	@mkdir -p $(@D)
	$(CC) -c $(CFLAGS) $< -o $@

$(BINDIR)% : $(OBJDIR)%.o $(OBJDIR)drw.o $(OBJDIR)util.o $(OBJDIR)xresources.o
	@mkdir -p $(@D)
	$(CC) -o $@ $< $(OBJDIR)drw.o $(OBJDIR)util.o $(OBJDIR)xresources.o $(LDFLAGS)

clean:
	rm -f $(OUT) $(OBJ)  

install: $(OUT)
