suffix ?= 

all: NS SOS AM AM1 AM2

NS$(suffix): State.hs Aexp.hs Bexp.hs Stm.hs Parser.hs StmRepl.hs NS.hs
	echo "import qualified NS" > Main.hs
	echo "main = NS.main" >> Main.hs
	ghc$(suffix) -o NS$(suffix) -O Main.hs $^
	rm Main.hs Main.o

SOS$(suffix): State.hs Aexp.hs Bexp.hs Stm.hs Parser.hs StmRepl.hs SOS.hs
	echo "import qualified SOS" > Main.hs
	echo "main = SOS.main" >> Main.hs
	ghc$(suffix) -o SOS$(suffix) -O Main.hs $^
	rm Main.hs Main.o

AM$(suffix): State.hs Stack.hs Memory.hs Aexp.hs Bexp.hs AMCode.hs Stm.hs Parser.hs AM.hs AMRepl.hs
	echo "import qualified AMRepl" > Main.hs
	echo "main = AMRepl.replAM" >> Main.hs
	ghc$(suffix) -o AM$(suffix) -O Main.hs $^
	rm Main.hs Main.o

AM1$(suffix): State.hs Stack.hs Memory.hs Aexp.hs Bexp.hs AM1Code.hs Stm.hs Parser.hs AM1.hs AM1Repl.hs
	echo "import qualified AM1Repl" > Main.hs
	echo "main = AM1Repl.replAM" >> Main.hs
	ghc$(suffix) -o AM1$(suffix) -O Main.hs $^
	rm Main.hs Main.o

AM2$(suffix): State.hs Stack.hs Memory.hs Aexp.hs Bexp.hs AM2Code.hs Stm.hs Parser.hs AM2.hs AM2Repl.hs
	echo "import qualified AM2Repl" > Main.hs
	echo "main = AM2Repl.replAM" >> Main.hs
	ghc$(suffix) -o AM2$(suffix) -O Main.hs $^
	rm Main.hs Main.o

NS: NS$(suffix)
SOS: SOS$(suffix)
AM: AM$(suffix)
AM1: AM1$(suffix)
AM2: AM2$(suffix)

Parser.hs: Parser.y
	happy$(suffix) $<

clean:
	rm -rf *.hi
	rm -rf *.o
	rm -f Main.hs
	rm -f Parser.hs
	rm -f NS SOS AM AM1 AM2 NS.exe
	rm -f SOS.exe AM.exe AM1.exe AM2.exe

.PHONY: clean all

