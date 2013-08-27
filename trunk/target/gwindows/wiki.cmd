@echo off

if "%1"=="" goto blabla

if "%2"=="" w http://en.wikipedia.org/wiki/%1 
if "%2"=="" goto fin

w http://%2.wikipedia.org/wiki/%1 

goto fin

:blabla
echo Please provide a valid Wikipedia topic (case-sensitive!)
echo and optionally a language.
echo.
echo Examples:
echo   wiki Wikipedia
echo   wiki Wikipedia fr
pause

:fin