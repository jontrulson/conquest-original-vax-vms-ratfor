$ !
$ ! Assemble the configuration file
$ !
$ macro /nolist conqfig
$ library /replace conquest.olb conqfig.obj
$ delete conqfig.obj;*
$ !
$ ! conquest
$ !
$ file = "conquest.exe"
$ if "''f$search(file)'" .nes. "" then delete 'file';*
$ !
$ link /nomap /notraceback /executable='file' sys$input/options
conquest/include=conquest/libr
conqcm.exe/share
$ !
$ set prot=(sy:rwed,ow:rwed,gr:e,wo:e) 'file'
$ !
$ ! conqdriv
$ !
$ file = "conqdriv.exe"
$ if "''f$search(file)'" .nes. "" then delete 'file';*
$ !
$ link /nomap /notraceback /executable='file' sys$input/options
conquest/include=conqdriv/libr
conqcm.exe/share
$ !
$ set prot=(sy:rwed,ow:rwed,gr:e,wo:e) 'file'
$ !
$ ! conqoper
$ !
$ file = "conqoper.exe"
$ if "''f$search(file)'" .nes. "" then delete 'file';*
$ !
$ link /nomap /notraceback /executable='file' sys$input/options
conquest/include=conqoper/libr
conqcm.exe/share
$ !
$ set prot=(sy:rwed,ow:rwed,gr:e,wo:e) 'file'
