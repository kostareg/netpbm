# Netpbm

A practice parser for the [netpbm](https://netpbm.sourceforge.net/doc/ppm.html) image filetype format.

<div style="float: left">
  <img src="https://github.com/kostareg/netpbm/blob/main/examples/dog.png?raw=true" alt="examples/dog.png" height="300"/>
  <img src="https://github.com/kostareg/netpbm/blob/main/examples/feep.png?raw=true" alt="examples/feep.png" height="300"/>
  <img src="https://github.com/kostareg/netpbm/blob/main/examples/smiley-20x.png?raw=true" alt="examples/smiley-20x.png" height="300"/>
</div>

See `examples` for more details.

## Development

Enter the development shell with `nix develop` or install dependencies, then:

```bash
cabal update
cabal install
cabal run
```

Both the plain (`P3`) and binary (`P6`) PPM formats are supported.

### Converting images to PPM for testing purposes

`-compress none` produces a plain `P3` file; omitting it produces a binary `P6`
file.

```
$ magick <original> -compress none -scale 20x <target>   # plain P3
$ magick <original> -scale 20x <target>                  # binary P6
```

## Next steps

I'd like to implement PPM-adjacent formats like PGM, PBM, PAM, etc. I'd also like to implement a test suite for both parsers.
