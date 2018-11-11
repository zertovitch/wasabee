echo Option "%1"
rem e.g. "-march=i686" for MinGW 4.7.2

REM Example for the variable GNAT_SOURCE_PATH
REM C:\Ada\GID;C:\Ada\zip-ada\zip_lib;C:\Ada\gnavi\gwindows\framework;C:\Ada\gnavi\gwindows\contrib;C:\Ada\gnavi\gnatcom\framework;C:\Ada\GNAT\2013\include\xmlada

rem goto debug_mode

rem *** Build in Fast mode

gprbuild -p -P wasa_gwindows %1 -XBuild_Mode=Fast
copy wasabee_gwindows.exe Wasabee.exe
copy Wasabee.exe "Wasabee (ver) win32.exe"
rem upx --ultra-brute "Wasabee (ver) win32.exe"
copy "Wasabee (ver) win32.exe" Wasabee.exe

del wasabee_gwindows.exe

:debug_mode

rem *** Build in Debug mode

gprbuild -p -P wasa_gwindows %1 -XBuild_Mode=Debug -XPRJ_TARGET=Windows_NT
copy wasabee_gwindows.exe Wasabee_Debug.exe

