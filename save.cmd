rem --------------------------
rem Nice date YYYY-MM-DD_HH.MM
rem --------------------------

set year=%date:~-4,4%

set month=%date:~-7,2%
if "%month:~0,1%" equ " " set month=0%month:~1,1%

set day=%date:~-10,2%
if "%day:~0,1%" equ " " set day=0%day:~1,1%

set hour=%time:~0,2%
if "%hour:~0,1%" equ " " set hour=0%hour:~1,1%

set min=%time:~3,2%

set nice_date=%year%-%month%-%day%_%hour%.%min%
set nice_date=%year%-%month%-%day%

rem --------------------------

del obj\debug\b__*.ad*
del obj\debug\b~*.ad*
del obj\fast\b__*.ad*
del obj\fast\b~*.ad*

set ver=%1
if "%1"=="" echo *** No revision number given, putting XXX
if "%1"=="" set ver=XXX

rem zip -9 -R Wasa_branch_Zrt_Dev_ver_%ver%_date_%nice_date%_.zip *.ads *.adb *.gpr *.pra *.gwen *.rc *.h *.ico *.bmp *.rbj *.cmd *.html *.jpg *.png *.css *.xls

zipada -r2 -ep2 Wasa_branch_Zrt_Dev_ver_%ver%_date_%nice_date%.zip '*.ads' '*.adb' '*.gpr' '*.pra' '*.gwen' '*.rc' '*.h' '*.ico' '*.bmp' '*.rbj' '*.cmd' '*.html' '*.jpg' '*.png' '*.css' '*.xls'
