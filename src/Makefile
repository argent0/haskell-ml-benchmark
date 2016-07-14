BUILD_DIR = "../build/"
all:
	make MainGADT
	make MainST
	make MainSTU

MainGADT: MainGADT.hs
	ghc -O2 $< -o ${BUILD_DIR}$@

MainST: MainST.hs
	ghc -O2 $< -o ${BUILD_DIR}$@

MainSTU: MainSTU.hs
	ghc -O2 $< -o ${BUILD_DIR}$@
