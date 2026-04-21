# Pirx-in-OCaml: Progress Log

Running notes on what's been done, what's in-flight, and anything surprising
along the way. Oldest entries at the top.

## M0 — Scaffold ✅

Goal: prove end-to-end plumbing (dune build → install → testrunner) with a
trivial compiler.

Done:
- `ocaml/dune-project` (lang dune 3.16).
- `ocaml/bin/pirxc/` — single-file executable (`main.ml`) that parses the Go
  `pirxc` flag surface (`-o`, `-O0`, `-t`, positional inputs) and emits a
  hardcoded aarch64-darwin `.s` file. Input files are not read.
- `ocaml/Makefile` with `build`, `test`, `clean`, and `install-pirxc`. Default
  target runs `build` then `install-pirxc`, which copies
  `_build/default/bin/pirxc/main.exe` to `../pirxc` (replacing the Go
  binary — reversible via a top-level `make pirxc`).
- Smoke test: `./testrunner test 000` passes against the OCaml-built `pirxc`.

Environment notes:
- OCaml toolchain is via opam, default switch = OCaml 5.3.0, dune 3.19.1. Need
  `eval $(opam env)` before running `make` (not persisted in the user shell).
- `./pirxc` is gitignored, so overwriting is safe and recoverable.

Not yet addressed (intentionally — deferred to later milestones):
- No library skeletons under `ocaml/lib/` yet. They'll be created as real
  code lands (M1 onward), to avoid empty placeholder dirs.
- No OCaml unit-test framework chosen yet. `dune runtest` is wired up but
  has nothing to run. Will pick (likely `alcotest`) when the lexer arrives.
- Top-level `Makefile` does not yet know about OCaml. Integration is M10.
- Linter/formatter (`ocamlformat`) not configured. Add when the codebase is
  large enough to care about style drift.

## M1 — Hello world slice

Not started.
