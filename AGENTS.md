# elspell.el
- Private symbols start with `-`.  This is expanded via the shorthand mechanism to the usual `elspell--` prefix.
- Public symbols are always written in full and have the `elspell-` prefix.
- Use `named-let` as your go-to looping construct (instead e.g. of `cl-block` or obscure `cl-loop` constructs).
- Instead of `(cl-loop for v across arr ...)`, prefer `(dotimes (i (length arr)) (let (v (aref arr i)) ...))`.
- Instead of `(zerop (length s))`, use `string-empty-p`.
- Prefer `pcase` over `cl-case`, `dolist` and variations over simple `cl-loop`.
- Make sure to byte-compile code being benchmarked.
- Nuspell and Hunspell sources are available in this directory for your reference.
- Don't edit the Commentary section, use ""LLM notes to self" section.

## Target Emacs 31: prefer core functions over cl-lib habits

We target Emacs 31 (see `Package-Requires` in elspell.el), where several operations availong reached for via `cl-lib` out of habit now have a plain, non-`cl-` core equivalent.  Prefer the right-hand side below:

- `cl-some` / `cl-every` → `any` / `all` (Emacs 31; same `(PRED LIST)` arg order).
- `cl-incf` / `cl-decf` → `incf` / `decf` (Emacs 31).
- `cl-oddp` / `cl-evenp` / `cl-plusp` / `cl-minusp` → `oddp` / `evenp` / `plusp` / `minusp` (Emacs 31).
- `seq-take` / `seq-drop` → `take` / `drop` (Emacs 29 / 30), when the sequence is already a plain list -- skips seq.el's generic-sequence dispatch.
- `(not (eq (gethash k table 'missing) 'missing))`-style sentinel checks → `hash-table-contains-p` (Emacs 31), when only presence matters, not the value.
- `cl-member-if` → `member-if` (Emacs 31; `any` is literally an alias for it).
- A `while` loop whose condition is "keep going while this binding is non-nil" → `while-let` (Emacs 29).
- Stripping a fixed leading/trailing run of a predicate from a list → `take-while` / `drop-while` (Emacs 31).
