/* hunspell-check.c --- apples-to-apples C benchmark for elspell-bench.el
 *
 * Measures how long Hunspell_create() takes to load a dictionary, and
 * how long Hunspell_spell() then takes to check every word of a file,
 * the same way elspell-bench.el measures elspell-create and
 * elspell-check: the file is read into memory once, up front,
 * before any checking is timed; words are then found and checked on
 * the fly, one at a time, rather than first collected into a list; a
 * word is a maximal run of Unicode alphabetic characters, apostrophes
 * and hyphens, decoded from the file's assumed UTF-8 encoding, minus
 * any leading and trailing apostrophes and hyphens (a run of nothing
 * but those is skipped) -- the same definition
 * `(rx alpha (? (* (any alpha ?' ?-)) alpha))' uses in
 * elspell-bench.el.  Each word is converted from UTF-8 to the
 * dictionary's own declared encoding (its .aff file's SET directive,
 * read back via Hunspell_get_dic_encoding) before being checked,
 * since Hunspell_spell expects words already in that encoding and
 * does no conversion of its own; some dictionaries (e.g. pt_BR) are
 * not UTF-8.
 *
 * With --suggest, it also measures how long Hunspell_suggest() takes,
 * called once for each misspelled word occurrence the check found, in
 * a separately timed pass after it, as elspell-bench.el does.
 * With --lines N, only the first N lines of FILE are used.
 *
 * With --print, "good: WORD" or "bad: WORD" is also printed for each
 * word checked; elspell-compare.el uses this.
 *
 * Usage: hunspell-check [--suggest] [--lines N] [--print] DICT-NAME FILE
 * DICT-NAME is looked up as data/DICT-NAME.{aff,dic}, relative to the
 * current directory (the project directory, when run by make), the
 * same files elspell-bench.el loads.
 *
 * Build: make .tmp/hunspell-check, which links against a static Hunspell built
 * from the hunspell/ source checkout.
 */

#include <errno.h>
#include <iconv.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <wchar.h>
#include <wctype.h>

#include <hunspell/hunspell.h>

static double
now(void)
{
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return ts.tv_sec + ts.tv_nsec / 1e9;
}

static int
is_word_char(wchar_t wc)
{
	return iswalpha((wint_t)wc) || wc == L'\'' || wc == L'-';
}

/* Counts kept while checking, and, with --suggest, the misspelled
 * word occurrences (in the dictionary's encoding) to suggest for. */
struct tally {
	long word_count, correct_count;
	int suggest, print;
	char **misspelled;
	long misspelled_count, misspelled_cap;
};

/* Check WORD (WLEN bytes, UTF-8), minus leading and trailing
 * apostrophes and hyphens, converting it to the dictionary's own
 * encoding via CD first unless CD is (iconv_t)-1 (dictionary is
 * already UTF-8), and record the result in T. */
static void
count_word(Hunhandle *h, iconv_t cd, char *word, size_t wlen, struct tally *t)
{
	while (wlen > 0 && (*word == '\'' || *word == '-')) {
		word++;
		wlen--;
	}
	while (wlen > 0 && (word[wlen - 1] == '\'' || word[wlen - 1] == '-'))
		wlen--;
	if (wlen == 0)
		return;
	word[wlen] = '\0';
	char converted[1024];
	const char *w = word;
	if (cd != (iconv_t)-1) {
		char *in = word, *out = converted;
		size_t inleft = wlen, outleft = sizeof converted - 1;
		iconv(cd, &in, &inleft, &out, &outleft);
		*out = '\0';
		w = converted;
	}
	t->word_count++;
	int good = Hunspell_spell(h, w);
	if (t->print)
		printf("%s: %s\n", good ? "good" : "bad", word);
	if (good) {
		t->correct_count++;
	} else if (t->suggest) {
		if (t->misspelled_count == t->misspelled_cap) {
			t->misspelled_cap = t->misspelled_cap ? 2 * t->misspelled_cap : 256;
			t->misspelled = realloc(t->misspelled,
			                        t->misspelled_cap * sizeof *t->misspelled);
		}
		t->misspelled[t->misspelled_count++] = strdup(w);
	}
}

/* Read FILE fully into a NUL-terminated buffer; set *SIZE to its
 * length (not counting the added NUL).  Exits the program on error. */
static char *
read_file(const char *path, long *size)
{
	FILE *f = fopen(path, "rb");
	if (!f) {
		fprintf(stderr, "hunspell-check: %s: %s\n", path, strerror(errno));
		exit(1);
	}
	if (fseek(f, 0, SEEK_END) != 0) {
		fprintf(stderr, "hunspell-check: %s: cannot seek\n", path);
		exit(1);
	}
	long len = ftell(f);
	fseek(f, 0, SEEK_SET);
	char *buf = malloc((size_t)len + 1);
	if (!buf) {
		fprintf(stderr, "hunspell-check: out of memory\n");
		exit(1);
	}
	size_t got = fread(buf, 1, (size_t)len, f);
	fclose(f);
	buf[got] = '\0';
	*size = (long)got;
	return buf;
}

int
main(int argc, char **argv)
{
	int suggest = 0, print = 0;
	long lines = -1;
	int argi = 1;
	for (; argi < argc && strncmp(argv[argi], "--", 2) == 0; argi++) {
		if (strcmp(argv[argi], "--suggest") == 0)
			suggest = 1;
		else if (strcmp(argv[argi], "--print") == 0)
			print = 1;
		else if (strcmp(argv[argi], "--lines") == 0 && argi + 1 < argc)
			lines = atol(argv[++argi]);
		else
			break;
	}
	if (argc - argi != 2) {
		fprintf(stderr, "usage: %s [--suggest] [--lines N] [--print] DICT-NAME FILE\n",
		        argv[0]);
		return 1;
	}
	const char *dict_name = argv[argi];
	const char *file_path = argv[argi + 1];

	/* Unicode-aware iswalpha() needs a UTF-8 locale. */
	if (!setlocale(LC_CTYPE, "C.UTF-8") && !setlocale(LC_CTYPE, "en_US.UTF-8")
	    && !setlocale(LC_CTYPE, ""))
		fprintf(stderr, "hunspell-check: warning: no UTF-8 locale available\n");

	char aff_path[4096], dic_path[4096];
	snprintf(aff_path, sizeof aff_path, "data/%s.aff", dict_name);
	snprintf(dic_path, sizeof dic_path, "data/%s.dic", dict_name);

	double t0 = now();
	Hunhandle *h = Hunspell_create(aff_path, dic_path);
	double load_seconds = now() - t0;
	if (!h) {
		fprintf(stderr, "hunspell-check: could not load dictionary %s\n", dict_name);
		return 1;
	}

	/* Words are matched out of the UTF-8 input, then converted to
	 * whatever encoding the dictionary itself declares (often not
	 * UTF-8), since Hunspell_spell() does no conversion itself. */
	const char *dic_encoding = Hunspell_get_dic_encoding(h);
	iconv_t cd = (iconv_t)-1;
	if (strcmp(dic_encoding, "UTF-8") != 0) {
		cd = iconv_open(dic_encoding, "UTF-8");
		if (cd == (iconv_t)-1) {
			fprintf(stderr, "hunspell-check: iconv_open UTF-8 -> %s: %s\n",
			        dic_encoding, strerror(errno));
			return 1;
		}
	}

	long file_size;
	char *buf = read_file(file_path, &file_size);
	if (lines >= 0) {
		/* Keep only the first LINES lines. */
		char *q = buf;
		for (long n = 0; n < lines && (q = memchr(q, '\n', buf + file_size - q)); n++)
			q++;
		if (q && q < buf + file_size) {
			file_size = q - buf;
			buf[file_size] = '\0';
		}
	}

	/* Timing starts here, once the file is already loaded: words are
	 * found and checked on the fly, one at a time, rather than first
	 * collected into a list. */
	t0 = now();
	struct tally t = {.suggest = suggest, .print = print};
	mbstate_t mbs;
	memset(&mbs, 0, sizeof mbs);
	char *p = buf, *end = buf + file_size;
	char word[1024];
	size_t wlen = 0;
	while (p < end) {
		wchar_t wc;
		size_t n = mbrtowc(&wc, p, (size_t)(end - p), &mbs);
		if (n == (size_t)-1 || n == (size_t)-2) {
			/* invalid or incomplete UTF-8 sequence: skip a byte */
			memset(&mbs, 0, sizeof mbs);
			p++;
			continue;
		}
		if (n == 0)
			n = 1; /* embedded NUL byte */
		if (is_word_char(wc)) {
			if (wlen + n < sizeof word) {
				memcpy(word + wlen, p, n);
				wlen += n;
			}
		} else if (wlen > 0) {
			count_word(h, cd, word, wlen, &t);
			wlen = 0;
		}
		p += n;
	}
	if (wlen > 0)
		count_word(h, cd, word, wlen, &t);
	double check_seconds = now() - t0;

	/* Suggestions are timed separately, after the check pass. */
	long suggestion_count = 0;
	double suggest_seconds = 0;
	if (suggest) {
		t0 = now();
		for (long i = 0; i < t.misspelled_count; i++) {
			char **slst = NULL;
			int n = Hunspell_suggest(h, &slst, t.misspelled[i]);
			suggestion_count += n;
			if (n > 0)
				Hunspell_free_list(h, &slst, n);
		}
		suggest_seconds = now() - t0;
	}

	printf("benchmark:  hunspell %s %s\n", dict_name, file_path);
	printf("load dict:  %.3fs\n", load_seconds);
	printf("check:      %ld words, %ld correct\n", t.word_count, t.correct_count);
	printf("            %.3fs total\n", check_seconds);
	printf("            %.2f µs/word\n",
	       1e6 * check_seconds / (t.word_count > 0 ? t.word_count : 1));
	if (suggest) {
		printf("suggest:    %ld words, %ld suggestions\n", t.misspelled_count,
		       suggestion_count);
		printf("            %.3fs total\n", suggest_seconds);
		printf("            %.2f ms/word\n",
		       1e3 * suggest_seconds / (t.misspelled_count > 0 ? t.misspelled_count : 1));
	}

	if (cd != (iconv_t)-1)
		iconv_close(cd);
	for (long i = 0; i < t.misspelled_count; i++)
		free(t.misspelled[i]);
	free(t.misspelled);
	Hunspell_destroy(h);
	free(buf);
	return 0;
}
