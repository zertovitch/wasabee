rem Open example

if "%1"=="" echo Syntax: ex n  (n=1,2,3,4,5,6,_css)
if "%1"=="" goto fin

wasabee_text file://../../tests/example%1.html

:fin
