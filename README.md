# Netpbm

A practice parser for the [netpbm](https://netpbm.sourceforge.net/doc/ppm.html) image filetype format.

<div style="float: left">
  <img src="https://github.com/kostareg/netpbm/blob/main/examples/P3/feep.png?raw=true" alt="examples/P3/feep.png" height="300"/>
  <img src="https://github.com/kostareg/netpbm/blob/main/examples/P3/smiley-20x.png?raw=true" alt="examples/P3/smiley-20x.png" height="300"/>
</div>

See `examples` for more details.

## Converting images to PPM for testing purposes

```
$ magick <original> -compress none -scale 20x <target>
```
