all : wasabee_main wasabee_xhttp wasabee-request



wasabee-request : wasabee-net.adb wasabee-xhtml.adb wasabee-request.adb
	/cygdrive/c/GNAT/2013/bin/gnatmake -g -I/cygdrive/c/GNAT/2013/include/xmlada wasabee_request

wasabee_main : wasabee-net.adb wasabee-xhtml.adb wasabee_main.adb
	/cygdrive/c/GNAT/2013/bin/gnatmake -g -I/cygdrive/c/GNAT/2013/include/xmlada wasabee_main

wasabee_xhttp : wasabee-net.adb wasabee-xhtml.adb wasabee_xhttp.adb
	/cygdrive/c/GNAT/2013/bin/gnatmake -g -I/cygdrive/c/GNAT/2013/include/xmlada wasabee_xhttp

clean:
	/cygdrive/c/GNAT/2013/bin/gnatclean wasabee_main wasabee_xhttp wasabee-request


