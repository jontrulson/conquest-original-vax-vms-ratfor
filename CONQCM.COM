$ !
$ ! conqcm
$ !
$ file = "conqcm.exe"
$ if "''f$search(file)'" .nes. "" then delete 'file';*
$ !
$ link /nomap /notraceback /shareble='file' sys$input/options
conquest/include=conqcm$data/libr
$ !
$ set prot=(sy:rwed,ow:rwed,gr,wo) 'file'
