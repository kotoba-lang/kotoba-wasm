# kotoba-wasm

Kotoba WebAssembly backend — checked KIR to core Wasm and Canonical ABI.

**Tier**: `T2`  **Role**: `backend`

Split out of the overloaded core repos by ADR-2607266000 so that each
responsibility has exactly one owner and the dependency direction is
checkable from outside.

## Owns

- `kotoba.wasm.core (KIR -> core wasm)`
- `kotoba.wasm.typed (typed lowering)`
- `kotoba.wasm.canonical-abi (Canonical ABI lowering)`

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
