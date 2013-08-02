all : wasabee_main wasabee_xhttp

wasabee_main : wasabee-net.adb wasabee-xhtml.adb wasabee_main.adb
	gnatmake -g -I/usr/share/ada/adainclude/xmlada wasabee_main

wasabee_xhttp : wasabee-net.adb wasabee-xhtml.adb wasabee_xhttp.adb
	gnatmake -g -I/usr/share/ada/adainclude/xmlada wasabee_xhttp

clean:
	gnatclean wasabee_main wasabee_xhttp

