.PHONY: build test run watch serve format check-format

esbuild-cmd := "esbuild \
			./output/Main/index.js \
			--bundle \
			--outfile=bundle/purs.js \
			--platform=browser \
			--format=esm \
			--external:@emurgo/cardano-serialization-lib-browser"

build:
	spago build 

clean:
	rm -r output/Example.*
	rm -r output/Main/ 		

test:
	echo "TODO: implement purs tests"

bundle:
	spago build --then ${esbuild-cmd}

watch:
	spago build --then ${esbuild-cmd} --watch

serve:
	(make -B bundle && npm run build && npm run serve)
	
format:
	purs-tidy format-in-place "src/**/*.purs"

check-format:
	purs-tidy check "src/**/*.purs"

