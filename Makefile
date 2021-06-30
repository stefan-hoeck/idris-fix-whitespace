export IDRIS2 ?= idris2

IDRIS2_PREFIX ?= $(HOME)/.idris2

lib_pkg = fix-whitespace.ipkg

.PHONY: all lib install clean clean-install

all: lib

clean-install: clean install

lib:
	@${IDRIS2} --build ${lib_pkg}

install: lib
	@cp -rv build/exec/* $(IDRIS2_PREFIX)/bin

uninstall:
	@${RM} -rv $(IDRIS2_PREFIX)/bin/fix_whitespace
	@${RM} -rv $(IDRIS2_PREFIX)/bin/fix_whitespace_app

clean:
	@${IDRIS2} --clean ${lib_pkg}
	@${RM} -r build
