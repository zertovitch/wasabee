all : wasabee_main wasabee_xhttp wasabee-request

wasabee-request : wasabee-net.adb wasabee-xhtml.adb wasabee-request.adb
	gnatmake -g -I/usr/share/ada/adainclude/xmlada wasabee-request

wasabee_main : wasabee-net.adb wasabee-xhtml.adb wasabee_main.adb
	gnatmake -g -I/usr/share/ada/adainclude/xmlada wasabee_main

wasabee_xhttp : wasabee-net.adb wasabee-xhtml.adb wasabee_xhttp.adb
	gnatmake -g -I/usr/share/ada/adainclude/xmlada wasabee_xhttp

clean:
	gnatclean wasabee_main wasabee_xhttp wasabee-request


