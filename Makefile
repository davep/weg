cleanish:
	rm -f .cvsignore~ *~ *.~* WEGLib/*~ WEGLib/*.~*

clean: cleanish
	rm -f WEGSetup.exe

installer:
	"c:/Program Files/Inno Setup 4/iscc" weg.iss
