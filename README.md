# CIRCE :leopard:

CRC cyclic-redundancy check

Support CRC16 & CRC32

Usage

```
crc 16 --help               # cli options
echo "hello world" | crc 16 # stream stdin
crc 16 100gb.bin            # stream file
```

Development

```
cabal build
cabal haddock
cabal run test
cabal run crc
cabal install
```
