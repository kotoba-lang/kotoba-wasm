# kotoba-wasm

Amu's Wasm **綾** — checked KIR to core Wasm and Canonical ABI.

This is not a being. [`amu`](https://github.com/kotoba-lang/amu) weaves;
this repository is one pattern in that weave. It does not link at runtime
and does not execute. That is [`kototama`](https://github.com/kotoba-lang/kototama).
See root ADR-2608139980.

**Tier**: `T2`  **Role**: `backend` of amu

Split out of the overloaded core repos by ADR-2607266000 so that each
responsibility has exactly one owner and the dependency direction is
checkable from outside.

## Owns

- `kotoba.wasm.core (KIR -> core wasm)`
- `kotoba.wasm.typed (typed lowering)`
- `kotoba.wasm.canonical-abi (Canonical ABI lowering)`

The Canonical ABI owner also normalizes Kotoba's public `:vector-i64` and
`:vector-f64` descriptors to the standard Component Model `list<s64>` and
`list<f64>` pointer/length layouts. Hosts and component producers consume
that single checked layout instead of reimplementing vector ABI rules.
Selected indirect list leaves use the shared `component-list-count` lowering,
which checks alignment, the descriptor item bound, unsigned byte-size/range
overflow, and the module's actual linear-memory size before exposing a count.
The matching `component-list-at-i64`/`component-list-at-f64` lowerings reuse
that exact validation, reject an unsigned out-of-range index, and only then
load one aligned scalar item from the borrowed Canonical buffer.
The `component-list-get-*` forms preserve those list checks but choose a
caller-supplied fallback for a negative or out-of-range index without
addressing memory.

Declared core-Wasm fuel is a positive exact i64 subset through `2^62 - 1` on
both JVM Clojure and ClojureScript/Node. The Node branch keeps policy values as
BigInt through validation and signed-LEB128 emission; it never narrows them
through JavaScript Number.

## Does not own

- parse .kotoba source
- own language semantics
- link or execute

## Depends on

- `kotoba-lang/kotoba-kir`

## Test

```bash
clojure -M:test
```
