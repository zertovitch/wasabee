SET PATH=c:\GNAT\2013\bin;%PATH%

SET ADA_INCLUDE_PATH=c:\GNAT\2013\include\xmlada
SET ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;c:\GNAT\2013\include\gnat_sdl
SET ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;..\..\..\gen-img-dec-code
SET ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;..\..\common

SET

gnatmake -P wasabee_sdl

