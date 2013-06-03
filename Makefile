CABAL=cabal
EXPORTS=PATH=$$PATH:cabal-dev/bin
CONFIG_OPTS=
.PHONY: test, tag_watcher

all: build

build: configure src/**/*.hs
	$(EXPORTS) $(CABAL) build

install: build
	cabal install

uninstall:
	ghc-pkg unregister librato

sdist: configure
	$(CABAL) sdist
	
configure: librato.cabal install_dependencies
	$(CABAL) configure $(CONFIG_OPTS)

install_dependencies:
	$(CABAL) install --only-dependencies

test: configure_tests
	PATH=$$PATH:cabal-dev/bin $(CABAL) build
	$(CABAL) test --show-details=always

configure_tests:
	$(CABAL) configure --enable-tests $(CONFIG_OPTS)

docs:
	$(CABAL) haddock

clean:
	$(CABAL) clean
	rm -f **/*.{o,hi} **/**/*.{o,hi}

tags: src/**/*.hs
	hasktags -c -o tags src

tags_inotify:
	while true; do inotifywait src/**/*.hs -e modify -q; make tags; done
