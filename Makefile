# $Id: Makefile,v 1.3 2004/04/08 13:12:44 davep Exp $

cleanish:
	rm -f .cvsignore~ *~ *.~* WEGLib/*~ WEGLib/*.~*

clean: cleanish
	rm -f WEGSetup.exe

installer:
	"c:/Program Files/Inno Setup 4/iscc" weg.iss
