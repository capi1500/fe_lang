GHC        = ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

.PHONY : all lang fe clean clean_all

all : lang fe

lang : 
	-bnfc --haskell -d --functor fe.cf
	-${ALEX} ${ALEX_OPTS} Fe/Lex.x
	-${HAPPY} ${HAPPY_OPTS} Fe/Par.y
	-${GHC} ${GHC_OPTS} Fe/Par.hs
	-${GHC} ${GHC_OPTS} Fe/Print.hs
	-${GHC} ${GHC_OPTS} Fe/Abs.hs
	-${GHC} ${GHC_OPTS} Fe/Lex.hs

fe :
	-${GHC} ${GHC_OPTS} -o interpreter fe.hs

clean :
	-find . -name "*.hi" -type f -delete
	-find . -name "*.o" -type f -delete
	-find . -name "*.bak" -type f -delete
	-rm -rf Fe
clean_all : clean
	-rm -f interpreter

