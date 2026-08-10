# CIRCE

CRC cyclic-redundancy check

Supports CRC16 & CRC32

Usage

```
crc --help
crc 16 -s "hello world"
crc 16 -f 100gb.bin
```

Development

```
cabal build
cabal haddock
cabal run test
cabal run crc -- --help
cabal install
```
