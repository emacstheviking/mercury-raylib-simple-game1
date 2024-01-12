BIN=g1

DEPS=g1.m easing.m raylib.m starfield.m assets.m gamestate.m animtext.m \
datatypes.m splashpage.m level_ufo.m rnd.m layout.m osd.m player.m audio.m \
ufo.m animsprite.m collision.m falling_object.m

FILES=$(patsubst %.m,%,$(DEPS))
GENEXT=d,o,mh,err,c,c_date,mh,mih
GRADE=hlc.gc.tr
# this one links to the extras folder in case you need it.
# but note that you may need to change the /usr/local/MERCURY-ROOT/..
# FLAGS=--ml posix --mld /usr/local/mercury-rotd-2021-04-15/extras/lib/mercury -s $(GRADE) -O4 -E
FLAGS=-s $(GRADE) -O4 -E --optimize-constant-propagation

# Graphics backend: raylib
GFX_CFLAGS=`pkg-config --cflags raylib`
GFX_LFLAGS=`pkg-config --libs raylib`
#
# ...local buil of master branch with extra funtions
#GFX_CFLAGS=-I/Users/seancharles/Documents/code/c/raylib/src
#GFX_LFLAGS=-L/Users/seancharles/Documents/code/c/raylib/src -lraylib


all:: $(BIN)

install:: $(BIN)
	mv -f -v $(BIN) $(HOME)/bin/

%: %.m $(DEPS)
	mmc $(FLAGS) --cflags "$(GFX_FLAGS)" --make $@

$(BIN): $(DEPS)
	echo $(GFX_CFLAGS)
	echo $(GFX_LFLAGS)
	mmc $(FLAGS) \
	    --cflags "$(GFX_CFLAGS)" \
	    --ld-flags "$(GFX_LFLAGS)" \
        --make $@
	mv -fv $(BIN) $(BIN)

clean::
	rm -rf Mercury
	rm -fv $$(for x in $(FILES); do echo $$x.{$(GENEXT)}; done)
	rm -fv $(BIN)
