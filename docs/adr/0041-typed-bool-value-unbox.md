# ADR 0041: typed-bool-value — unbox host boolean to i32 0/1 word

- Status: Accepted
- Date: 2026-07-30
- Related: compiler ADR 0191 A (language profile 5)

## Decision

Add host import `kotoba:typed/bool-value` with type
`(param externref) (result i32)` — the inverse of `kotoba:typed/bool`
`(param i32) (result externref)`.

Intrinsic name: `typed-bool-value`.

## Why

Profile 5 carries `:bool` as a plain 0/1 word inside modules, while aggregate
slots (record/vector fields) continue to store sealed JS booleans. Reading a
`:bool` field needs unboxing; writing already boxes via `typed-bool`.

## Non-claims

The import is **gated** (`has-bool-value? false`) until emit-get for `:bool`
fields wires it with language profile 5. Always-on registration would force
every typed module to import `bool-value` and break hosts that have not landed
the companion yet. Host-side `bool-value` (compiler browser-host) can land first.
