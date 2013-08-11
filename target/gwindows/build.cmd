echo Option "%1"
rem e.g. "-march=i686" for MinGW 4.7.2

REM Example for the variable ADA_INCLUDE_PATH
REM C:\Ada\zip-ada\zip_lib;C:\Ada\gnavi\gwindows\framework;C:\Ada\gnavi\gwindows\contrib;C:\Ada\gnavi\gnatcom\framework;C:\GNAT\2013\include\xmlada

set ADA_INCLUDE_PATH=C:\Projects\gnavi-code\gwindows\framework;%ADA_INCLUDE_PATH%
set ADA_INCLUDE_PATH=C:\Projects\gnavi-code\gwindows\contrib;%ADA_INCLUDE_PATH%
set ADA_INCLUDE_PATH=C:\Projects\gnavi-code\gnatcom\framework;%ADA_INCLUDE_PATH%
set ADA_INCLUDE_PATH=C:\GNAT\2013\include\xmlada;%ADA_INCLUDE_PATH%

rem *** Build in Fast mode

gnatmake -P wasa_gwindows %1 -XBuild_Mode=Fast
copy wasabee_gwindows.exe Wasabee.exe
copy Wasabee.exe "Wasabee (ver) win32.exe"
rem upx --ultra-brute "Wasabee (ver) win32.exe"
copy "Wasabee (ver) win32.exe" Wasabee.exe

del wasabee_gwindows.exe

rem *** Build in Debug mode

gnatmake -P wasa_gwindows %1 -XBuild_Mode=Debug
copy wasabee_gwindows.exe Wasabee_Debug.exe

