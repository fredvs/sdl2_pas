# sdl2_pas
Some pascal demos using SDL2.
Source of SDL2 headers, BGRABitmap and LCL-utils included.

Compile the programs source with:

 cd /directory/of/sdl2_pas

 fpc -B -FUunits -Fu./sdl2-pas nameofprogram.pas

_________________

For source using BGRABitmap, compile it with:

 cd /directory/of/sdl2_pas

 fpc -B -FUunits -Fu./sdl2-pas -Fu./bgrabitmap -Fu./lcl-dep nameofprogram.pas
