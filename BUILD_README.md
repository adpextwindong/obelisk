# Build Notes

For SDL2:
which ghc
/cygdrive/c/tools/ghc-8.8.4/bin

Stick SDL2.dll in the same bin folder as your ghc.

For SDL2_gfx, SDL2_ttf:

Add the directories containing the .pc files to PKG_CONFIG_PATH
```
configure && make
```

## SDL2_gfx
SDL2_gfx lib stuff license:
(C) A. Schiffler, aschiffler [at] ferzkopp.net 2012-2018, licensed under the zlib license


===================================

Make sure your PKG_CONFIG_PATH is pointing at your SDL2_gfx install.

Your PATH should have the SDL2/bin in it as well.

Figuring out linker issues when building sd2_gfx in cabal can be helped with
```
ld -LSDL2 -verbose
```
This is crucial because ghc will use its' ld so.


I also had to edit SDL_main.h to fix a  your SDL2_gfx install.


Your PATH should have the SDL2/bin in it as well.

```
error: In file included from C:/tools/SDL2-2.0.14/x86_64-w64-mingw32/include/SDL2/SDL.h:32:0,
                 from C:\tools\ghc-8.8.4\lib/include/SDL2_framerate.h:40,
                 from Framerate.hsc:32:
C:/tools/SDL2-2.0.14/x86_64-w64-mingw32/include/SDL2/SDL_main.h:109:17: error: conflicting types for 'SDL_main'
 #define main    SDL_main
                 ^
Framerate.hsc:34:5: note: in expansion of macro 'main'
In file included from C:/tools/SDL2-2.0.14/x86_64-w64-mingw32/include/SDL2/SDL.h:32:0,
                 from C:\tools\ghc-8.8.4\lib/include/SDL2_framerate.h:40,
                 from Framerate.hsc:32:
C:/tools/SDL2-2.0.14/x86_64-wde/SDL2/SDL_main64-mingw32/inclu.h:121:29: note: previous declaration of 'SDL_main' was
 here
 extern SDLMAIN_DECLSPEC int SDL_main(int argc, char **argv);
                             ^~~~~~~iled to build sd~

cabal.exe: Fal2-gfx-0.2 (which is required by
exe:sdl2-gfx-example from sdl2-gfx-0.2). See the build log above for details.
```

No clue why this occurs. I just commented the extern line out.

=====

Building SDL2_gfx attempt 2

You need automake and autoconf installed in Cygwin.

./autogen.sh and ./configure will create a SDL2_gfx.pc that you need to point your pkg_config at.

Run make, make install then cabal install sdl2-gfx -v3 to see which ld its using.

Then using that ld check where its looking for your libs.

Ghc's ld wants a folder like /cygdrive/c/building/msys64/mingw64/usr/lib

For the sake of include file simplicity I shoved all the SDL2_gfx headers into

/cygdrive/c/tools/msys64/mingw64/include

because it was already in the include path given to gcc.

Then we need to edit SDL_main.h line that it complains about.

Oh yeah make sure the SDL2_gfx.a la lai files are in your /cygdrive/c/tools/msys64/mingw64/lib so Cabal repl can link to it.


Have cygwin install zlib and libpng devel stuff
