SET PATH=c:\GNAT\2014\bin;%PATH%

SET ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;c:\GNAT\2014\include\xmlada
SET ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;c:\GNAT\2014\include\gnat_sdl
SET ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;..\..\..\gen-img-dec-code
SET ADA_INCLUDE_PATH=%ADA_INCLUDE_PATH%;..\..\common

SET

gprbuild -p -P wasabee_sdl.gpr -XBuild_Mode=Debug

