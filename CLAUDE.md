# CLAUDE.md — cosmic-comp

Orientation for agents working in this repo.

## Comments

**Keep comments small and concise — 1–2 lines.** Explain *why*, not *what*: the
code already says what it does. Cut restatement, background the reader does not
need, and anything a name or a type already conveys.

```rust
// ✅ one line, says why
// Immutable storage cannot be resized, so a change means reallocating.
size: Size<i32, Physical>,

// ❌ a paragraph restating the code
/// Physical size of the target texture. This is stored so that we can compare
/// it against the presentation size each frame and decide whether the existing
/// texture can be reused or whether a new one has to be allocated, because
/// immutable storage cannot be resized after creation.
size: Size<i32, Physical>,
```

Same for doc comments on functions and tests: one or two lines on the reason or
the failure it guards, not a description of the body. A subtle invariant, a
deadlock, or a non-obvious ordering constraint earns more space — nothing else
does.

## Logging

Release builds compile out `debug!` and `trace!` (`release_max_level_info`). Use
`info!` for anything that must be visible in a release binary. Runtime log:
`$XDG_RUNTIME_DIR/cosmic-comp.log`.

## Build & verify

```bash
cargo build
cargo fmt
cargo clippy --workspace --all-targets   # fix at the source, not with #[allow]
cargo test
```

## Conventions

- Commit to `master`; semantic-release runs off it. No feature branches.
- commitlint requires a lowercase-start subject.
- Never commit local cargo `[patch]` entries — those are dev-local.
- Don't reference external projects by name in committed code or commit
  messages; this is a public repo.
