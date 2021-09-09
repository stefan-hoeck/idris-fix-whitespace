export IDRIS2 ?= idris2

IDRIS2_PREFIX ?= $(HOME)/.idris2

lib_pkg = fix-whitespace.ipkg

.PHONY: all
all: lib

.PHONY: clean-install
clean-install: clean install

.PHONY: lib
lib:
	@${IDRIS2} --build ${lib_pkg}

.PHONY: install
install: lib
	@cp -rv build/exec/* $(IDRIS2_PREFIX)/bin

.PHONY: uninstall
uninstall:
	@${RM} -rv $(IDRIS2_PREFIX)/bin/fix_whitespace
	@${RM} -rv $(IDRIS2_PREFIX)/bin/fix_whitespace_app

.PHONY: clean
clean:
	@${IDRIS2} --clean ${lib_pkg}
	@${RM} -r build

.PHONY: develop
develop:
	find -name "*.idr" | entr -d idris2 --typecheck ${lib_pkg}
