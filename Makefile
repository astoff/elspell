EMACS ?= emacs
BATCH = $(EMACS) --batch -L .
BATCH_NO_NATIVE = $(BATCH) --eval '(setq native-comp-eln-load-path nil)'

EL = elspell.el elspell-tests.el elspell-bench.el elspell-compare.el
ELC = $(EL:.el=.elc)

# Versions upon which the Emacs Lisp port is based.
HUNSPELL_VERSION = v1.7.5
NUSPELL_VERSION = v5.1.9

all: $(ELC)

%.elc: %.el
	$(BATCH) -f batch-byte-compile $<

clean:
	rm -rf $(ELC) .tmp

# Hunspell is built from the hunspell/ checkout as a static library,
# compiling its sources directly; this needs no autotools run, since
# the only generated headers (hunvisapi.h, hunversion.h) are checked in.
CC ?= cc
CXX ?= c++
HUNSPELL_BUILD = .tmp/hunspell-build
HUNSPELL_LIB = $(HUNSPELL_BUILD)/libhunspell.a
HUNSPELL_MODULES = affentry affixmgr csutil filemgr hashmgr hunspell \
	hunspelltrace hunzip phonet replist suggestmgr
HUNSPELL_OBJS = $(HUNSPELL_MODULES:%=$(HUNSPELL_BUILD)/%.o)

$(HUNSPELL_BUILD)/%.o: hunspell/src/hunspell/%.cxx
	mkdir -p $(@D)
	$(CXX) -O2 -DHUNSPELL_STATIC -c -o $@ $<

$(HUNSPELL_LIB): $(HUNSPELL_OBJS)
	$(AR) rcs $@ $^

.tmp/hunspell-check: hunspell-check.c $(HUNSPELL_LIB)
	$(CC) -O2 -DHUNSPELL_STATIC -Ihunspell/src -o $@ $< $(HUNSPELL_LIB) -lstdc++

# The hunspell command-line tool, handy for its --trace option.  It wants
# a config.h, which normally comes from configure; this minimal one is
# enough on a glibc system.
HUNSPELL_PARSERS = textparser htmlparser latexparser manparser \
	firstparser xmlparser odfparser

$(HUNSPELL_BUILD)/config.h:
	mkdir -p $(@D)
	printf '%s\n' '#define HAVE_ICONV 1' '#define ICONV_CONST' \
		'#define HAVE_LOCALE_H 1' '#define HAVE_LANGINFO_H 1' \
		'#define VERSION "$(HUNSPELL_VERSION:v%=%)"' '#define DATADIR ""' > $@

$(HUNSPELL_BUILD)/hunspell: hunspell/src/tools/hunspell.cxx \
		$(HUNSPELL_PARSERS:%=hunspell/src/parsers/%.cxx) \
		$(HUNSPELL_BUILD)/config.h $(HUNSPELL_LIB)
	$(CXX) -O2 -DHUNSPELL_STATIC -I$(HUNSPELL_BUILD) -Ihunspell/src/hunspell \
		-o $@ $(filter %.cxx,$^) $(HUNSPELL_LIB)

hunspell:
	git clone --depth 1 --branch $(HUNSPELL_VERSION) https://github.com/hunspell/hunspell.git $@

nuspell:
	git clone --depth 1 --branch $(NUSPELL_VERSION) https://github.com/nuspell/nuspell.git $@

# Benchmark texts and dictionaries.
DATA = data/alice.txt data/lusiadas.txt data/mujeong.txt data/egri.txt \
	data/verwandlung.txt data/hunspell-dictionary

.PHONY: data
data: $(DATA)

# Everything in data/ is downloaded, so mark it as a cache directory
# (see https://bford.info/cachedir/) for backup tools to skip.
data/CACHEDIR.TAG:
	mkdir -p data
	printf '%s\n' 'Signature: 8a477f597d28d172789f06886806bc55' \
		'# This file is a cache directory tag created by elspell.' \
		'# For information about cache directory tags, see:' \
		'#	https://bford.info/cachedir/' > $@

data/alice.txt: | data/CACHEDIR.TAG
	mkdir -p .tmp
	curl -fL -o .tmp/$(@F) https://www.gutenberg.org/ebooks/11.txt.utf-8
	sed -e '1,/^\*\*\* START OF/d' -e '/^\*\*\* END OF/,$$d' .tmp/$(@F) > $@

data/lusiadas.txt: | data/CACHEDIR.TAG
	mkdir -p .tmp
	curl -fL -o .tmp/$(@F) https://www.gutenberg.org/ebooks/3333.txt.utf-8
	sed -e '1,/^\*\*\* START OF/d' -e '/^\*\*\* END OF/,$$d' .tmp/$(@F) > $@

# First part (chapters 1-20) of Yi Kwang-su's Mujeong, from Korean
# Wikisource; the sed script drops the wiki templates (the page header
# and the paragraph-break markers), leaving plain text.
data/mujeong.txt: | data/CACHEDIR.TAG
	curl -L -A elspell-bench \
		'https://ko.wikisource.org/w/index.php?title=%EB%AC%B4%EC%A0%95/1%EC%9E%A5~20%EC%9E%A5&action=raw' \
		| sed -e '/^{{.*}}$$/d' -e '/^{{/,/^}}$$/d' > $@

# First volume of Gárdonyi Géza's Az egri csillagok.
data/egri.txt: | data/CACHEDIR.TAG
	mkdir -p .tmp
	curl -fL -o .tmp/$(@F) https://www.gutenberg.org/ebooks/76235.txt.utf-8
	sed -e '1,/^\*\*\* START OF/d' -e '/^\*\*\* END OF/,$$d' .tmp/$(@F) > $@

data/verwandlung.txt: | data/CACHEDIR.TAG
	mkdir -p .tmp
	curl -fL -o .tmp/$(@F) https://www.gutenberg.org/ebooks/22367.txt.utf-8
	sed -e '1,/^\*\*\* START OF/d' -e '/^\*\*\* END OF/,$$d' .tmp/$(@F) > $@

# Every Debian sid package providing hunspell-dictionary, as listed on
# packages.debian.org, except the myspell-* ones and, of the French
# variants (which all ship fr.aff), all but hunspell-fr-classical, the
# first choice of the hunspell-fr metapackage.  The .aff/.dic files
# (minus symlinks) go straight to data/.  The package file names are
# looked up in the sid index; all these packages are Arch: all.
DEBIAN_MIRROR = http://deb.debian.org/debian
HUNSPELL_DICT_SKIP = ^myspell-|^hunspell-fr-(revised|comprehensive)$$

data/hunspell-dictionary: | data/CACHEDIR.TAG
	mkdir -p .tmp/debian
	curl -fsSL https://packages.debian.org/sid/hunspell-dictionary \
		| grep -o 'href="/sid/[^"/]*"' | cut -d/ -f3 | tr -d '"' \
		| grep -Ev '$(HUNSPELL_DICT_SKIP)' \
		> .tmp/debian/hunspell-dictionary.txt
	curl -fsSL -o .tmp/debian/Packages.xz \
		$(DEBIAN_MIRROR)/dists/sid/main/binary-all/Packages.xz
	xz -dc .tmp/debian/Packages.xz \
		| awk 'NR == FNR { want[$$1]; next } \
		       /^Package:/ { p = $$2 } \
		       /^Filename:/ && p in want { print p, $$2 }' \
		      .tmp/debian/hunspell-dictionary.txt - \
		| while read pkg file; do \
			curl -fsSL -o .tmp/debian/$$pkg.deb $(DEBIAN_MIRROR)/$$file \
			&& rm -rf .tmp/debian/$$pkg && mkdir .tmp/debian/$$pkg \
			&& ar p .tmp/debian/$$pkg.deb data.tar.xz \
				| tar -xJ -C .tmp/debian/$$pkg --strip-components=4 --wildcards \
					'./usr/share/hunspell/*.aff' './usr/share/hunspell/*.dic' \
			&& find .tmp/debian/$$pkg -type f -exec cp -t data {} + \
			|| exit 1; \
		done
	touch $@

.PHONY: tests
tests: hunspell nuspell
	$(BATCH) -l ert -l elspell-tests.el -f ert-run-tests-batch-and-exit

# Check that elspell and Hunspell find the same misspelled words in
# Emacs's etc/HELLO (or COMPARE_FILE), for every dictionary in data/.
COMPARE_FILE ?=

.PHONY: compare
compare: data/hunspell-dictionary all .tmp/hunspell-check
	$(BATCH) -l elspell-compare.el -f elspell-compare $(COMPARE_FILE)

BENCH_FLAGS ?=

.PHONY: bench
bench: $(DATA) all .tmp/hunspell-check
	@echo ================================================================================
	@.tmp/hunspell-check $(BENCH_FLAGS) en_US data/alice.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH) -l elspell-bench.el -f elspell-bench $(BENCH_FLAGS) en_US data/alice.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH_NO_NATIVE) -l elspell-bench.el -f elspell-bench $(BENCH_FLAGS) en_US data/alice.txt
# Suggestions for Korean take too long to be worth benchmarking.
ifeq ($(filter --suggest,$(BENCH_FLAGS)),)
	@echo ================================================================================
	@.tmp/hunspell-check $(BENCH_FLAGS) ko data/mujeong.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH) -l elspell-bench.el -f elspell-bench $(BENCH_FLAGS) ko data/mujeong.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH_NO_NATIVE) -l elspell-bench.el -f elspell-bench $(BENCH_FLAGS) ko data/mujeong.txt
endif
	@echo ================================================================================
	@.tmp/hunspell-check $(BENCH_FLAGS) pt_BR data/lusiadas.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH) -l elspell-bench.el -f elspell-bench $(BENCH_FLAGS) pt_BR data/lusiadas.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH_NO_NATIVE) -l elspell-bench.el -f elspell-bench $(BENCH_FLAGS) pt_BR data/lusiadas.txt
	@echo ================================================================================
	@.tmp/hunspell-check $(BENCH_FLAGS) de_DE data/verwandlung.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH) -l elspell-bench.el -f elspell-bench $(BENCH_FLAGS) de_DE data/verwandlung.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH_NO_NATIVE) -l elspell-bench.el -f elspell-bench $(BENCH_FLAGS) de_DE data/verwandlung.txt
	@echo ================================================================================
	@.tmp/hunspell-check $(BENCH_FLAGS) hu_HU data/egri.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH) -l elspell-bench.el -f elspell-bench $(BENCH_FLAGS) hu_HU data/egri.txt
	@echo --------------------------------------------------------------------------------
	@$(BATCH_NO_NATIVE) -l elspell-bench.el -f elspell-bench $(BENCH_FLAGS) hu_HU data/egri.txt
