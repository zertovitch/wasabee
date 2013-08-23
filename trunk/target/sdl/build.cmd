gnatmake -c wasabee_sdl.adb

gnatbind -x wasabee_sdl.ali

gnatlink wasabee_sdl.ali -lSDL -lSDL_ttf
