.PHONY: clean build test bundle watch serve format check-format

ESBUILD = esbuild \
	./output/Main/index.js \
	--bundle \
	--outfile=bundle/purs.js \
	--platform=browser \
	--format=esm \
	--external:@emurgo/cardano-serialization-lib-browser

clean:
	rm -r output/Frontend.*
	rm -r output/Main/
	rm -r bundle/
	rm -r dist/ 		

build:
	spago build 

test:
	echo "TODO: implement tests"

bundle: build
	${ESBUILD}

watch: build
	${ESBUILD} --watch

serve: bundle
	npm run build
	npm run serve
	
format:
	purs-tidy format-in-place "src/**/*.purs"

check-format:
	purs-tidy check "src/**/*.purs"


