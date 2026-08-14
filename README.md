# CIRCE :leopard:

CRC cyclic redundancy check

Support CRC8, CRC16, CRC32, and CRC64

Usage

```
crc (8|16|32|64) --help        # crc options
echo -n "hello world" | crc 16 # stream stdin
crc 32 100gb.bin               # stream file
```

Development

```
cabal build
cabal haddock
cabal run test
cabal run crc
cabal install
```
