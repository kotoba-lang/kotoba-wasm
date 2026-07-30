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

This PR only **registers** the import. Emitter wiring (emit-get for `:bool`
fields) lands with the profile-5 frontend so main stays consumer-green.
