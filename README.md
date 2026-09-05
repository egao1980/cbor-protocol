# cbor-protocol

CLOS **CBOR** encode/decode for [cl-stack](https://github.com/egao1980/cl-stack) (RFC 8949). Same Lisp mapping as [`json-protocol`](https://github.com/egao1980/json-protocol), plus byte strings and tags. Implements [`serdes-protocol`](https://github.com/egao1980/serdes-protocol) `:cbor`.

OCI **0.1.0** — `ghcr.io/egao1980/cl-systems/cbor-protocol:0.1.0`

```lisp
(asdf:load-system "cbor-protocol")   ; nick stack-cbor; registers :cbor

(stack-cbor:encode 42)                          ; octets
(serdes-protocol:encode ht :format :cbor)
(serdes-protocol:format-media-type :cbor)       ; "application/cbor"
```

| JSON | CBOR extra |
|------|------------|
| object → equal hash-table | byte string → `(vector (unsigned-byte 8))` |
| array → vector | tag → `cbor-tag` (2/3 become bignums) |
| null → `:null` · false → `nil` · true → `t` | indefinite lengths decoded, definite encoded |

## License

MIT — see [LICENSE](LICENSE).
