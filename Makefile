result/main.md:  R/make.R measurements/Python.dat measurements/GADT.dat measurements/STU.dat measurements/ST.dat R/main.Rmd
	R --slave -f $<
	mv main.md result/
	mv figure result/

measurements/Python.dat: src/Main.py sh/poorManBenchmarTool.sh
	bash sh/poorManBenchmarTool.sh "python $<" > $@

measurements/GADT.dat: build/MainGADT sh/poorManBenchmarTool.sh
	bash sh/poorManBenchmarTool.sh $< > $@

measurements/ST.dat: build/MainST sh/poorManBenchmarTool.sh
	bash sh/poorManBenchmarTool.sh $< > $@

measurements/STU.dat: build/MainSTU sh/poorManBenchmarTool.sh
	bash sh/poorManBenchmarTool.sh $< > $@

build/MainGADT: src/MainGADT.hs
	ghc -O2 $< -o $@

build/MainST: src/MainST.hs
	ghc -O2 $< -o $@

build/MainSTU: src/MainSTU.hs
	ghc -O2 $< -o $@
