# sclb - terminal-independent screen control package

# Ver  Date   Who Remarks
# --- ------- --- ------------------------------------------------------
# 04s 31Jan85 cal .Fixed bell sequence in built in dumb capabilities code.
# 04r 12Oct84 cal .Added sclinit() to allow use of sclb when terminal
#                  information isn't available. Trimmed and reformatted
#                  revision history.
# 04q 04Oct84 cal .Fixed minor format bug in scerror().
# 04p 26Sep84 cal .Modified to use environments to get the terminal type
#                  and to find the termcap file.
# 04o 18Jul84 cal .Modified to use new iolb routine names.
# 04n 08Jun84 cal .Changed scend() to not fool with the cursor postion.
#                  Added sctermcap().
#
# This library assumes that the characters and integers can be assigned to
# each other, and that characters have ascii values when viewed as integers.
#

include "sclb"
include "sclb.i"
include "tslb"


### scinit - initialization for the screen package
#
#  SYNOPSIS
#    call scinit
#
#  DESCRIPTION
#      One of scinit() or sclinit() must be called before any others in
#      this package except scsetup(), which can be used to read a termcap
#      file without actually doing work on a terminal.
#
subroutine scinit
NOIMPLICIT

    call sclinit ( .false. )		# don't assume "dumb" as a last resort

    return

end


### sclinit - low level initialization for the screen package
#
#  SYNOPSIS
#    logical dodumb
#    call sclinit ( dodumb )
#
#	dodumb - .true. means to assume "dumb" as a last resort
#                .false. means to abort if there are problems
#
#  DESCRIPTION
#      One of scinit() or sclinit() must be called before any others in
#      this package except scsetup(), which can be used to read a termcap
#      file without actually doing work on a terminal.
#
subroutine sclinit ( dodumb )
NOIMPLICIT
logical dodumb

    integer i, j, enopen, engetnext, enfldn, envget
    character buf(MAXLINE), tcfilename(FILENAMESIZE), ttype(32)
    include "sclb.c"
    include "sclb.ic"


    # Get terminal type.
    if ( envget ( "TERM", ttype ) != OK )
	if ( ! dodumb )
	    call error ( "scinit: Environment TERM undefined." )
	else
	    {
	    call scsetup ( .true. )		# do dumb entry
	    call screinit
	    call ioflush
	    return
	    }

    # Find the correct entry from the termcap file.
    call sctermcap ( tcfilename )
    if ( enopen ( tcfilename ) != OK )
	if ( ! dodumb )
	    {
	    call fprintf ( ERROUT, "scinit: Can't open termcap file %s",
		tcfilename )
	    call error ( "" )
	    }
	else
	    {
	    call scsetup ( .true. )		# do dumb entry
	    call screinit
	    call ioflush
	    return
	    }
    repeat
	{
	if ( engetnext(0) == EOF )
	    if ( ! dodumb )
		{
		call fprintf ( ERROUT, "scinit: Unknown terminal type @"%s@"",
		    ttype )
		call error ( "" )
		}
	    else
		{
		call scsetup ( .true. )		# do dumb entry
		call screinit
		call ioflush
		return
		}
	if ( enfldn ( 2, buf, MAXLINE ) != OK )
	    call error ( "scinit: enfldn() failed" )

	# Scan the various names.
	i = 1
	while ( buf(i) != EOS )
	    {
	    j = 1
	    while ( ttype(j) == buf(i) )
		{
		j = j + 1
		i = i + 1
		if ( ( buf(i) == '|' | buf(i) == EOS ) & ttype(j) == EOS )
		    break 3				# found it
		}
	    for ( ; buf(i) != '|'; i = i + 1 )
		if ( buf(i) == EOS )
		    next 3				# try next entry
	    i = i + 1
	    }
	}
    call enclose

    # Set up from termcap entry.
    call scsetup ( .false. )				# read termcap file

    # Reinitialize the terminal.
    call screinit
    call ioflush

    return

end


### scsetup - set up termcap values from current entry
#
#  SYNOPSIS
#    call scsetup ( dodumb )
#
#  DESCRIPTION
#    This routine sets up the tables describing the terminal from the
#    current termcap entry.
#
subroutine scsetup ( dodumb )
NOIMPLICIT
logical dodumb

    integer i
    integer scnxtent
    integer n
    character name ( MAXLINE )
    character rest ( MAXLINE )
    integer length
    integer min0
    include "sclb.c"
    include "sclb.ic"

    string horiz "()-"
    string vert "()|"
    string cross "()+"

    # Set up so that nothing is specified.
    scnatt = 0
    scautonl = .false.
    sccbeep = CANT
    sccbol = CANT
    scbs = .false.
    scccel = CANT
    sccchscroll = CANT
    sccclear = CANT
    scpcols = 0
    # sccdepill = CANT
    sccdlc = CANT
    sccdll = CANT
    sccdown = CANT
    # sccenpill = CANT
    for ( i = 1 ; i <= NUMBER_GRAPHICS_CHARS ; i = i + 1 )
	sccgraphics ( i ) = CANT
    schardcopy = .false.
    scchome = CANT
    scciim = CANT
    sccikey = CANT
    sccinc = CANT
    sccinit = CANT
    sccinl = CANT
    sccleft = CANT
    scplins = 0
    sclshigh = .false.
    sccmove = CANT
    scnoautonl = .false.
    # scblclear = .true.
    scnotilde = .false.
    sccoim = CANT
    sccokey = CANT
    scoverstrike = .false.
    scpbeep = 0.0
    scpbol = 0.0
    scpcel = 0.0
    scpclear = 0.0
    scpdlc = 0.0
    scpdll = 0.0
    scpdown = 0.0
    scpinc = 0.0
    scpinl = 0.0
    scpsdown = 0.0
    scpsup = 0.0
    scptab = 0.0
    scpadc = '@^?'
    sccright = CANT
    sccsdown = CANT
    sccsup = CANT
    scctab = CANT
    # for ( i = 1 ; i <= MAX_ATTRIBUTES ; i = i + 1 )
    #	sctabs ( i ) = .false.
    sccup = CANT
    scvisdel = .false.
    scwrtop = .false.
    scxonoff = .false.


    if ( ! dodumb )
	{
	# Read termcap entries.
	n = 2
	while ( scnxtent ( n, name, rest ) == OK )
	    call scsu ( name, rest )
	}
    else
	{
	# Fake it.
	call scsu ( "lins#", "24" )
	call scsu ( "cols#", "80" )
	##
	## The following should be "^G", but esc() is (stupidly)
	## called in enfldn() instead of scsu().
	##
	call scsu ( "beep=", "@^G" )
	call scsu ( "autonl", "" )
	call scsu ( "bs", "" )
	}

    # use defaults, if necessary
    if ( sccbol == CANT )
	{
	seqbol(1) = '@^m'
	seqbol(2) = EOS
	sccbol = length ( seqbol )
	}
    # if ( sccdepill == CANT )
    #	{
    #	seqdepill(1) = EOS
    #	sccdepill = length ( seqdepill )
    #	}
    # if ( sccenpill == CANT )
    #	{
    #	seqenpill(1) = EOS
    #	sccenpill = length ( seqenpill )
    #	}
    if ( sccleft == CANT )
	{
	if ( scbs )
	    {
	    seqleft(1) = '@^h'
	    seqleft(2) = EOS
	    sccleft = length ( seqleft )
	    }
	}

    if ( sccgraphics ( GRAPHICS_PILLOW ) == CANT )
	call sccapg ( "()X", GRAPHICS_PILLOW )

    if ( sccgraphics ( LSEG_D ) == CANT )
	call sccapg ( vert, LSEG_D )
    if ( sccgraphics ( LSEG_L ) == CANT )
	call sccapg ( horiz, LSEG_L )
    if ( sccgraphics ( LSEG_LD ) == CANT )
	call sccapg ( cross, LSEG_LD )
    if ( sccgraphics ( LSEG_U ) == CANT )
	call sccapg ( vert, LSEG_U )
    if ( sccgraphics ( LSEG_UD ) == CANT )
	call sccapg ( vert, LSEG_UD )
    if ( sccgraphics ( LSEG_UL ) == CANT )
	call sccapg ( cross, LSEG_UL )
    if ( sccgraphics ( LSEG_ULD ) == CANT )
	call sccapg ( cross, LSEG_ULD )
    if ( sccgraphics ( LSEG_R ) == CANT )
	call sccapg ( horiz, LSEG_R )
    if ( sccgraphics ( LSEG_RD ) == CANT )
	call sccapg ( cross, LSEG_RD )
    if ( sccgraphics ( LSEG_RL ) == CANT )
	call sccapg ( horiz, LSEG_RL )
    if ( sccgraphics ( LSEG_RLD ) == CANT )
	call sccapg ( cross, LSEG_RLD )
    if ( sccgraphics ( LSEG_RU ) == CANT )
	call sccapg ( cross, LSEG_RU )
    if ( sccgraphics ( LSEG_RUD ) == CANT )
	call sccapg ( cross, LSEG_RUD )
    if ( sccgraphics ( LSEG_RUL ) == CANT )
	call sccapg ( cross, LSEG_RUL )
    if ( sccgraphics ( LSEG_RULD ) == CANT )
	call sccapg ( cross, LSEG_RULD )

    # figure out usable lines and columns
    sclins = min0 ( scplins, MAXSCLINS )
    sccols = min0 ( scpcols, MAXSCCOLS )

    # if neither autonl or noautonl was specified (or if both were),
    # delete a column from the screen
    if ( scautonl == scnoautonl )
	# delete a column from the screen
	sccols = sccols - 1

    # Adjust sccmove to account for the "l" and the "c".
    if ( sccmove != CANT )
	sccmove = sccmove - 2		# ** not correct for VT100s... **

    # Initialize the scprch array, which tells which characters are
    # printable on the terminal.
    for ( i=0; i <= '@^_'; i=i+1 )
        scprch(i+1) = .false.
    for ( i=' '; i <= '~'; i=i+1 )
        scprch(i+1) = .true.
    scprch('@^?'+1) = .false.
    if ( scnotilde )
	scprch('~'+1) = .false.
    if ( scvisdel )
        scprch('@^?'+1) = .true.

    # figure out the number of pad character required
    call tsinit
    call tsgspeed ( scispeed, scospeed )
    call tsend
    scnbeep = scpbeep * scospeed
    scnbol = scpbol * scospeed
    scncel = scpcel * scospeed
    scnclear = scpclear * scospeed
    scndlc = scpdlc * scospeed
    scndll = scpdll * scospeed
    scndown = scpdown * scospeed
    scninc = scpinc * scospeed
    scninl = scpinl * scospeed
    scnsdown = scpsdown * scospeed
    scnsup = scpsup * scospeed
    scntab = scptab * scospeed

    # decode attribute groups
    call scasu

    # set current and desired attributes off
    for ( i = 1 ; i <= MAX_ATTRIBUTES ; i = i + 1 )
	{
	curatt ( i ) = .false.
	desatt ( i ) = .false.
	}

    # # Pillow mode starts out off.
    # pillmode = .false.

    # assume we're not debugging things
    debug = .false.

    # Initialize the raw I/O package.
    call ioinit

    # No Characters in input buffer
    inbufp = 0

    return

end


### scsu - internal routine to process termcap entry
#
#  SYNOPSIS
#   call scsu ( name, rest )
#
#  DESCRIPTION
#    Process termcap entry with name 'name' and argument 'rest'.
#
#  PASSED
#    name - name of termcap entry
#    rest - rest of termcap entry
#
#  COMMENT
#    Yes, this is one mother of a subroutine.
#
subroutine scsu ( name, rest )
NOIMPLICIT
character name ( ARB )
character rest ( ARB )

    integer equal, sccapn
    logical done, sccapb
    real sccapr
    character sccapc
    include "sclb.c"
    include "sclb.ic"

    done = .true.				# assume done

    if ( equal ( name, "att=" ) )
	call sccapa ( rest )
    else if ( equal ( name, "autonl" ) == YES )
	scautonl = sccapb ( rest )
    else if ( equal ( name, "beep=" ) == YES )
	call sccaps ( rest, seqbeep, sccbeep )
    else if ( equal ( name, "bol=" ) == YES )
	call sccaps ( rest, seqbol, sccbol )
    else if ( equal ( name, "bs" ) == YES )
	scbs = sccapb ( rest )
    else if ( equal ( name, "cel=" ) == YES )
	call sccaps ( rest, seqcel, scccel )
    else if ( equal ( name, "chscroll=" ) == YES )
	call sccaps ( rest, seqchscroll, sccchscroll )
    else if ( equal ( name, "clear=" ) == YES )
	call sccaps ( rest, seqclear, sccclear )
    else if ( equal ( name, "cols#" ) == YES )
	scpcols = sccapn ( rest )
    # else if ( equal ( name, "depill=" ) == YES )
    #	call sccaps ( rest, seqdepill, sccdepill )
    else if ( equal ( name, "dlc=" ) == YES )
	call sccaps ( rest, seqdlc, sccdlc )
    else if ( equal ( name, "dll=" ) == YES )
	call sccaps ( rest, seqdll, sccdll )
    else if ( equal ( name, "down=" ) == YES )
	call sccaps ( rest, seqdown, sccdown )
    # else if ( equal ( name, "enpill=" ) == YES )
    #	call sccaps ( rest, seqenpill, sccenpill )
    else if ( equal ( name, "f0=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_F0 ), scckey ( KEYPAD_F0 ) )
    else if ( equal ( name, "f1=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_F1 ), scckey ( KEYPAD_F1 ) )
    else if ( equal ( name, "f2=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_F2 ), scckey ( KEYPAD_F2 ) )
    else if ( equal ( name, "f3=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_F3 ), scckey ( KEYPAD_F3 ) )
    else if ( equal ( name, "f4=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_F4 ), scckey ( KEYPAD_F4 ) )
    else if ( equal ( name, "f5=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_F5 ), scckey ( KEYPAD_F5 ) )
    else if ( equal ( name, "f6=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_F6 ), scckey ( KEYPAD_F6 ) )
    else if ( equal ( name, "f7=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_F7 ), scckey ( KEYPAD_F7 ) )
    else if ( equal ( name, "f8=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_F8 ), scckey ( KEYPAD_F8 ) )
    else if ( equal ( name, "f9=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_F9 ), scckey ( KEYPAD_F9 ) )
    else if ( equal ( name, "hardcopy" ) == YES )
	schardcopy = sccapb ( rest )
    else
	done = .false.

    if ( done )
	return

    if ( equal ( name, "home=" ) == YES )
	call sccaps ( rest, seqhome, scchome )
    else if ( equal ( name, "iim=" ) == YES )
	call sccaps ( rest, seqiim, scciim )
    else if ( equal ( name, "ikey=" ) == YES )
	call sccaps ( rest, seqikey, sccikey )
    else if ( equal ( name, "inc=" ) == YES )
	call sccaps ( rest, seqinc, sccinc )
    else if ( equal ( name, "init=" ) == YES )
	call sccaps ( rest, seqinit, sccinit )
    else if ( equal ( name, "inl=" ) == YES )
	call sccaps ( rest, seqinl, sccinl )
    else if ( equal ( name, "k0=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_0 ), scckey ( KEYPAD_0 ) )
    else if ( equal ( name, "k1=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_1 ), scckey ( KEYPAD_1 ) )
    else if ( equal ( name, "k2=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_2 ), scckey ( KEYPAD_2 ) )
    else if ( equal ( name, "k3=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_3 ), scckey ( KEYPAD_3 ) )
    else if ( equal ( name, "k4=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_4 ), scckey ( KEYPAD_4 ) )
    else if ( equal ( name, "k5=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_5 ), scckey ( KEYPAD_5 ) )
    else if ( equal ( name, "k6=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_6 ), scckey ( KEYPAD_6 ) )
    else if ( equal ( name, "k7=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_7 ), scckey ( KEYPAD_7 ) )
    else if ( equal ( name, "k8=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_8), scckey ( KEYPAD_8 ) )
    else if ( equal ( name, "k9=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_9 ), scckey ( KEYPAD_9 ) )
    else if ( equal ( name, "kcomma=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_COMMA ), scckey (KEYPAD_COMMA) )
    else if ( equal ( name, "kdown=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_DOWN ), scckey (KEYPAD_DOWN) )
    else if ( equal ( name, "kenter=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_ENTER ), scckey (KEYPAD_ENTER) )
    else if ( equal ( name, "kleft=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_LEFT ), scckey (KEYPAD_LEFT) )
    else if ( equal ( name, "kminus=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_MINUS ), scckey (KEYPAD_MINUS) )
    else if ( equal ( name, "kperiod=" ) == YES )
	call sccaps ( rest, seqkey (1, KEYPAD_PERIOD), scckey (KEYPAD_PERIOD) )
    else if ( equal ( name, "kright=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_RIGHT ), scckey (KEYPAD_RIGHT) )
    else if ( equal ( name, "kup=" ) == YES )
	call sccaps ( rest, seqkey ( 1, KEYPAD_UP ), scckey (KEYPAD_UP) )
    else if ( equal ( name, "left=" ) == YES )
	call sccaps ( rest, seqleft, sccleft )
    else
	done = .false.

    if ( done )
	return

    if ( equal ( name, "lins#" ) == YES )
	scplins = sccapn ( rest )
    else if ( equal ( name, "lsegd=" ) == YES )
	call sccapg ( rest, LSEG_D )
    else if ( equal ( name, "lsegl=" ) == YES )
	call sccapg ( rest, LSEG_L )
    else if ( equal ( name, "lsegld=" ) == YES )
	call sccapg ( rest, LSEG_LD )
    else if ( equal ( name, "lsegu=" ) == YES )
	call sccapg ( rest, LSEG_U )
    else if ( equal ( name, "lsegud=" ) == YES )
	call sccapg ( rest, LSEG_UD )
    else if ( equal ( name, "lsegul=" ) == YES )
	call sccapg ( rest, LSEG_UL )
    else if ( equal ( name, "lseguld=" ) == YES )
	call sccapg ( rest, LSEG_ULD )
    else if ( equal ( name, "lsegr=" ) == YES )
	call sccapg ( rest, LSEG_R )
    else if ( equal ( name, "lsegrd=" ) == YES )
	call sccapg ( rest, LSEG_RD )
    else if ( equal ( name, "lsegrl=" ) == YES )
	call sccapg ( rest, LSEG_RL )
    else if ( equal ( name, "lsegrld=" ) == YES )
	call sccapg ( rest, LSEG_RLD )
    else if ( equal ( name, "lsegru=" ) == YES )
	call sccapg ( rest, LSEG_RU )
    else if ( equal ( name, "lsegrud=" ) == YES )
	call sccapg ( rest, LSEG_RUD )
    else if ( equal ( name, "lsegrul=" ) == YES )
	call sccapg ( rest, LSEG_RUL )
    else if ( equal ( name, "lsegruld=" ) == YES )
	call sccapg ( rest, LSEG_RULD )
    else if ( equal ( name, "lshigh" ) == YES )
	sclshigh = sccapb ( rest )
    else if ( equal ( name, "move=" ) == YES )
	call sccaps ( rest, seqmove, sccmove )
    else if ( equal ( name, "noautonl" ) == YES )
	scnoautonl = sccapb ( rest )
    # else if ( equal ( name, "noblclear" ) == YES )
    #	scblclear = ! sccapb ( rest )
    else if ( equal ( name, "notilde" ) == YES )
	scnotilde = sccapb ( rest )
    else if ( equal ( name, "oim=" ) == YES )
	call sccaps ( rest, seqoim, sccoim )
    else if ( equal ( name, "okey=" ) == YES )
	call sccaps ( rest, seqokey, sccokey )
    else if ( equal ( name, "overstrike" ) == YES )
	scoverstrike = sccapb ( rest )
    else if ( equal ( name, "padc=" ) == YES )
	scpadc = sccapc ( rest )
    else
	done = .false.

    if ( done )
	return

    if ( equal ( name, "pbeep#" ) == YES )
	scpbeep = sccapr ( rest )
    else if ( equal ( name, "pbol#" ) == YES )
	scpbol = sccapr ( rest )
    else if ( equal ( name, "pcel#" ) == YES )
	scpcel = sccapr ( rest )
    else if ( equal ( name, "pclear#" ) == YES )
	scpclear = sccapr ( rest )
    else if ( equal ( name, "pdlc#" ) == YES )
	scpdlc = sccapr ( rest )
    else if ( equal ( name, "pdll#" ) == YES )
	scpdll = sccapr ( rest )
    else if ( equal ( name, "pdown#" ) == YES )
	scpdown = sccapr ( rest )
    else if ( equal ( name, "pillow=" ) == YES )
	call sccapg ( rest, GRAPHICS_PILLOW )
    else if ( equal ( name, "pinc#" ) == YES )
	scpinc = sccapr ( rest )
    else if ( equal ( name, "pinl#" ) == YES )
	scpinl = sccapr ( rest )
    else if ( equal ( name, "psdown#" ) == YES )
	scpsdown = sccapr ( rest )
    else if ( equal ( name, "psup#" ) == YES )
	scpsup = sccapr ( rest )
    else if ( equal ( name, "ptab#" ) == YES )
	scptab = sccapr ( rest )
    else if ( equal ( name, "right=" ) == YES )
	call sccaps ( rest, seqright, sccright )
    else if ( equal ( name, "sdown=" ) == YES )
	call sccaps ( rest, seqsdown, sccsdown )
    else if ( equal ( name, "sup=" ) == YES )
	call sccaps ( rest, seqsup, sccsup )
    else if ( equal ( name, "tab=" ) == YES )
	call sccaps ( rest, seqtab, scctab )
    # else if ( equal ( name, "tabs=" ) == YES )
    #	call sccapt ( rest )
    else if ( equal ( name, "up=" ) == YES )
	call sccaps ( rest, sequp, sccup )
    else if ( equal ( name, "visdel" ) == YES )
	scvisdel = sccapb ( rest )
    else if ( equal ( name, "wrtop" ) == YES )
	scwrtop = sccapb ( rest )
    else if ( equal ( name, "xonoff" ) == YES )
	scxonoff = sccapb ( rest )
    # If we didn't reconize it, ignore it.

    return

end


### sctermcap - internal routine to return the pathname of the termcap file
#
#  SYNOPSIS
#   call sctermcap ( file )
#
#  RETURNED
#    file - name of termcap
#
subroutine sctermcap ( file )
NOIMPLICIT
character file(ARB)

    call scopy ( "+LIB/termcap", 1, file, 1 )

    return

end


### scnxtent - internal routine to get next argument of termcap entry
#
# SYNOPSIS
#   status = scnxtent ( i, name, rest )
#
# DESCRIPTION
#   scnxtent gets the next termcap entry and puts the name of the entry
#   in 'name' and the rest (usually an escape sequence) in 'rest'
#
# PASSED
#   i - the last entry that was gotten (initially should be '2')
#
# RETURNED
#   name - the name of the next termcap entry
#   rest - the rest of the termcap entry
#   status - OK if there was another termcap entry, ERR otherwise

integer function scnxtent ( i, name, rest )
NOIMPLICIT
integer i
character name ( MAXLINE )
character rest ( MAXLINE )

    integer enfldn
    character temp ( MAXLINE )
    character type
    integer n


    # advance pointer
    i = i + 1

    # try to get field 'i'
    scnxtent = enfldn ( i, temp, MAXLINE )

    if ( scnxtent == OK )
	{
	# split into 'name' and 'rest'

	# name is of form [a-zA-Z0-9]*[[#:]]
	n = 1
	while ( type ( temp ( n ) ) == LETTER  |
	        type ( temp ( n ) ) == DIGIT )
	    call chcopy ( temp ( n ), name, n )
	if ( temp ( n ) == '#'  |  temp ( n ) == '=' )
	    call chcopy ( temp ( n ), name, n )
	name ( n ) = EOS  # in case it is null string

	# put rest of string in 'rest'
	call scopy ( temp, n, rest, 1 )
	}

    return

end


### sccapa - internal routine to add attribute description
#
# SYNOPSIS
#   call sccapa ( rest )
#
# DESCRIPTION
#   Adds attribute specification to the list of attribute sequences.
#   The format of 'rest', the argument to the "att=" command should be
#       ([+|-]{h|u|r|b|g|a})*=seq
#   (where h, u, r, b, g, and a are the currently recognized attributes)
#
# PASSED
#   rest - the argument to the "att=" command

subroutine sccapa ( rest )
NOIMPLICIT
character rest ( ARB )

    integer i
    integer action
    integer index
    integer n
    include "sclb.c"
    include "sclb.ic"
    string atts ATTRIBUTE_STRING


    scnatt = scnatt + 1

    for ( i = 1 ; i <= MAX_ATTRIBUTES ; i = i + 1 )
	scattact ( i, scnatt ) = ATTRIBUTE_UNCHANGED

    for ( i = 1 ; rest ( i ) != '='  &  rest ( i ) != EOS ; i = i + 1 )
	{
	if ( rest ( i ) == '-' )
	    {
	    action = ATTRIBUTE_OFF
	    i = i + 1
	    }
	else if ( rest ( i ) == '+' )
	    {
	    action = ATTRIBUTE_ON
	    i = i + 1
	    }
	else
	    {
	    # assume they want to turn the attribute on
	    action = ATTRIBUTE_ON
	    }

	# add action (if legal)
	n = index ( atts, rest ( i ) )
	if ( 0 < n  &  n <= MAX_ATTRIBUTES )
	    scattact ( n, scnatt ) = action
	}

    if ( rest ( i ) != EOS )
	i = i + 1

    # copy control sequence to seqatt
    call scopy ( rest, i, seqatt ( 1, scnatt ), 1 )

    return

end


### sccapb - internal routine to get a boolean field from the termcap entry
#
#  SYNOPSIS
#    logical fieldpresent, sccapb
#    character rest()
#    fieldpresent = sccapb ( rest )
#
#  DESCRIPTION
#    Interprets the argument to a termcap command as a boolean quantity.
#    Returns .true. if the boolean quantity is set, .false. otherwise.
#
#    The entry is set if and only if rest is the null string.
#
#  PASSED
#    rest - the argument to the termcap command
#
#  RETURNED
#    fieldpresent - whether the entry is set or not

logical function sccapb ( rest )
NOIMPLICIT
character rest ( ARB )

    integer length


    sccapb = ( length ( rest ) == 0 )

    return

end


### sccapc - internal routine to get a character from the termcap entry
#
#  SYNOPSIS
#    character char, sccapc
#    char = sccapc ( rest )
#
#  DESCRIPTION
#    Interprets the argument to a termcap command as a character.
#
#  PASSED
#    rest - the argument to the termcap command
#
#  RETURNED
#    char - the character corresponding to rest

character function sccapc ( rest )
NOIMPLICIT
character rest ( ARB )


    sccapc = rest ( 1 )

    return

end


### sccapg - internal routine to decode attribute list string combination
#
# call sccapg ( rest, n )
#
# description
#   decodes the string 'rest' as an attribute list followed by a command
#   sequence, and sets graphics character 'n' to this combination
#
# passed
#   rest - of the form ({h|u|r|b|g|a}*)seq
#   n - the number of the graphics character

subroutine sccapg ( rest, n )
NOIMPLICIT
character rest ( ARB )
integer n

    character resta ( MAXLINE )
    character rests ( MAXLINE )
    integer i
    integer j

    include "sclb.c"
    include "sclb.ic"


    if ( rest ( 1 ) == '(' )
	{
	j = 1
	for ( i = 2 ; rest ( i ) != ')'  &  rest ( i ) != EOS ; i = i + 1 )
	    call chcopy ( rest ( i ), resta, j )
	resta ( j ) = EOS

	if ( rest ( i ) == ')' )
	    i = i + 1

	call scopy ( rest, i, rests, 1 )
	}

    else
	{
	resta ( 1 ) = EOS
	call scopy ( rest, 1, rests, 1 )
	}

    call sccapl ( resta, satgraphics ( 1, n ) )
    call sccaps ( rests, seqgraphics ( 1, n ), sccgraphics ( n ) )

    return

end


### sccapl - internal routine to decode attribute list
#
# SYNOPSIS
#   call sccapl ( rest, att )
#
# DESCRIPTION
#   Looks at the attributes listed in 'rest' and sets the corresponding
#   entries in the logical array 'att' to .true. (and the rest to .false.)
#
# PASSED
#   rest - a list of attributes (i.e. {h|u|r|b|g|a}*)
#
# RETURNED
#   att - each element .true. if and only if it is list 'rest'

subroutine sccapl ( rest, att )
NOIMPLICIT
character rest ( ARB )
logical att ( MAX_ATTRIBUTES )

    integer i
    integer index
    integer n
    string atts ATTRIBUTE_STRING

    for ( n = 1 ; n <= MAX_ATTRIBUTES ; n = n + 1 )
	att ( n ) = .false.

    for ( i = 1 ; rest ( i ) != EOS ; i = i + 1 )
	{
	n = index ( atts, rest ( i ) )

	# if legal, add it
	if ( 0 < n  &  n <= MAX_ATTRIBUTES )
	    att ( n ) = .true.
	}

    return

end


### sccapn - internal routine to get a numeric field from the termcap entry
#
#  SYNOPSIS
#    integer num, sccapn
#    character rest()
#    num = sccapn ( rest )
#
#  DESCRIPTION
#    Returns the value of the numeric string 'rest', which is normally
#    the argument to a termcap command (i.e., line width or page length)
#
#    The null string is assigned a value of '0'
#
#  PASSED
#    rest - the argument to a termcap command
#
#  RETURNED
#    num - the number specified by 'rest'

integer function sccapn ( rest )
NOIMPLICIT
character rest(ARB)

    integer ctoi
    integer i

    i = 1
    sccapn = ctoi ( rest, i )

    return

end


### sccapr - internal routine to get a real numeric field from termcap entry
#
#  SYNOPSIS
#    real num, sccapr
#    character rest()
#    num = sccapr ( rest )
#
#  DESCRIPTION
#    Returns the value of the numeric string 'rest', which is normally
#    the argument to a termcap command (i.e., line width or page length)
#
#    The null string is assigned a value of '0.0'
#
#  PASSED
#    rest - the argument to a termcap command
#
#  RETURNED
#    num - the number specified by 'rest'

real function sccapr ( rest )
NOIMPLICIT
character rest(ARB)

    real ctor
    integer i

    i = 1
    sccapr = ctor ( rest, i )

    return

end


### sccaps - internal routine to get a string field from the termcap entry
#
#  SYNOPSIS
#    character rest()
#    character seqxxx()
#    integer sccxxx
#    call sccaps ( rest, seqxxx, sccxxx )
#
#  DESCRIPTION
#    Copies the termcap argument from 'rest' to 'seqxxx', sets
#    'sccxxx' to the length of 'seqxxx'
#
#  PASSED
#    rest - the argument to the termcap command
#
#  RETURNED
#    seqxxx - the argument to the termcap command
#    sccxxx - the length of seqxxx

subroutine sccaps ( rest, seqxxx, sccxxx )
NOIMPLICIT
character rest ( ARB )
character seqxxx ( ARB )
integer sccxxx

    integer length


    call scopy ( rest, 1, seqxxx, 1 )
    sccxxx = length ( seqxxx )

    return

end


ifdef(CRAZY_CODE)	# skip this junk
### sccapt - internal routine to read 'tabs' entry
#
# SYNOPSIS
#   call sccapt ( rest )
#
# DESCRIPTION
#   Sets tab stops to the ones specified in 'rest'.
#   'rest' should be of the form
#       [n[,n]*]
#
# PASSED
#   rest - the tabs stops to set

subroutine sccapt ( rest )
NOIMPLICIT
character rest ( ARB )

    integer ctoi
    integer n
    integer i
    include "sclb.c"


    for ( n = 1 ; n <= MAXSCCOLS ; n = n + 1 )
	sctabs ( n ) = .false.

    i = 1

    while ( rest ( i ) != EOS )
	{
	# add next tab stop
	n = ctoi ( rest, i )

	if ( 0 < n  &  n <= MAXSCCOLS )
	    sctabs ( n ) = .true.

	# skip delimiter
	if ( rest ( i ) != EOS )
	    i = i + 1
	}

    return

end
enddef


### scasu - internal routine to set up attribute structures
#
# SYNOPSIS
#   call scasu
#
# DESCRIPTION
#   Set up tables for attribute routines

subroutine scasu
NOIMPLICIT

    logical scatfind
    integer want ( MAX_ATTRIBUTES )
    integer n

    integer i
    integer j

    include "sclb.c"
    include "sclb.ic"


    # divide attributes into groups
    call scasug

    # figure out how to set each individual attribute
    for ( i = 1 ; i <= MAX_ATTRIBUTES ; i = i + 1 )
	{
	# see if we can set just this attribute
	for ( j = 1 ; j <= MAX_ATTRIBUTES ; j = j + 1 )
	    want ( j ) = ATTRIBUTE_UNCHANGED
	want ( i ) = ATTRIBUTE_ON

	if ( scatfind ( want, n ) )
	    attset ( i ) = n
	else
	    attset ( i ) = NO_ATTRIBUTE_FUNCTION
	}

    # figure out how to clear all attributes in group
    for ( i = 1 ; i <= MAX_ATTRIBUTES ; i = i + 1 )
	{
	# see if we can clear all attributes in group
	for ( j = 1 ; j <= MAX_ATTRIBUTES ; j = j + 1 )
	    if ( attg ( i ) == attg ( j ) )
		want ( j ) = ATTRIBUTE_OFF
	    else
		want ( j ) = ATTRIBUTE_UNCHANGED

	if ( scatfind ( want, n ) )
	    attgclear ( i ) = n
	else
	    attgclear ( i ) = NO_ATTRIBUTE_FUNCTION
	}

    # figure out strategy to change each group
    for ( i = 1 ; i <= MAX_ATTRIBUTES ; i = i + 1 )
	if ( attset ( i ) == NO_ATTRIBUTE_FUNCTION )
	    attstrat ( i ) = STRATEGY_ALL
	else
	    attstrat ( i ) = STRATEGY_INDIVIDUAL

    # figure out what available attributes are
    for ( i = 1 ; i <= MAX_ATTRIBUTES ; i = i + 1 )
	schasatt ( i ) = .false.
    for ( i = 1 ; i <= scnatt ; i = i + 1 )
	for ( j = 1 ; j <= MAX_ATTRIBUTES ; j = j + 1 )
	    if ( scattact ( j, i ) != ATTRIBUTE_UNCHANGED )
		schasatt ( j ) = .true.

    return

end


### scasug - internal routine to split attributes into groups
#
# SYNOPSIS
#   call scasug
#
# DESCRIPTION
#   Groups attributes.  The structure attg is set up.  Elements of disjoint
#   attribute groups have different values in attg.

subroutine scasug
NOIMPLICIT

    integer i
    integer j
    integer k
    integer l
    integer m
    integer n

    include "sclb.c"
    include "sclb.ic"


    # assume in separate groups
    for ( n = 1 ; n <= MAX_ATTRIBUTES ; n = n + 1 )
	attg ( n ) = n

    # combine groups where necessary
    for ( i = 1 ; i <= scnatt ; i = i + 1 )
	{
	# combine groups used by attribute sequence 'i'
	for ( j = 1 ; j <= MAX_ATTRIBUTES ; j = j + 1 )
	    {
	    # if this is in the same group as something used by this sequence,
	    # combine
	    for ( k = 1 ; k <= MAX_ATTRIBUTES ; k = k + 1 )
		{
		# see if in same group
		if ( attg ( k ) == attg ( j )  &
		     scattact ( k, i ) != ATTRIBUTE_UNCHANGED )
		    break
		}
	    if ( k <= MAX_ATTRIBUTES )
		{
		# add this attribute group to group 'n'
		m = attg ( j )
		for ( l = 1 ; l <= MAX_ATTRIBUTES ; l = l + 1 )
		    if ( attg ( l ) == m )
			attg ( l ) = n
		}
	    }
	n = n + 1
	}

    return

end


### scatfind - internal routine to find attribute group
#
# SYNOPSIS
#   log = scatfind ( wanted, n )
#
# DESCRIPTION
#   Tries to find an attribute sequence the same as 'wanted'.
#   If it is found, log is .true. and n is the number of the
#   attribute sequence.
#   If it is not found, log is .false.
#
# PASSED
#   wanted - attribute sequence to find
#
# RETURNED
#   n - the number of the matching attribute sequence
#   log - whether we found it or not

logical function scatfind ( wanted, n )
NOIMPLICIT
integer wanted ( MAX_ATTRIBUTES )
integer n

    include "sclb.c"

    integer i


    scatfind = .false.

    for ( n = 1 ; n <= scnatt ; n = n + 1 )
	{
	# see if matches
	for ( i = 1 ; i <= MAX_ATTRIBUTES ; i = i + 1 )
	    if ( wanted ( i ) != scattact ( i, n ) )
		break
	if ( i > MAX_ATTRIBUTES )
	    {
	    scatfind = .true.
	    break
	    }
	}

    return

end


### scattexec - internal routine to execute attribute sequence
#
# SYNOPSIS
#   call scattexec ( n )
#
# DESCRIPTION
#   Executes attribute sequence n, and changes the current attributes
#   accordingly.
#   'n' may be NO_ATTRIBUTE_FUNCTION, in which case we execute scerror.
#
# PASSED
#   n - attribute sequence to execute

subroutine scattexec ( n )
NOIMPLICIT
integer n

    integer i

    include "sclb.c"
    include "sclb.ic"


    if ( n == NO_ATTRIBUTE_FUNCTION  |  ! ( 0 < n  &  n <= scnatt ) )
	call scerror ( "scattexec()" )
    else
	{
	# execute command sequence
	call iopstr ( seqatt ( 1, n ) )

	# change current attributes accordingly
	for ( i = 1 ; i <= MAX_ATTRIBUTES ; i = i + 1 )
	    if ( scattact ( i, n ) == ATTRIBUTE_ON )
		curatt ( i ) = .true.
	    else if ( scattact ( i, n ) == ATTRIBUTE_OFF )
		curatt ( i ) = .false.
	}

    return

end


### scattset - internal routine to actually set attributes
#
# SYNOPSIS
#   call scattset ( wanted )
#
# DESCRIPTION
#   Actually sets the attributes to 'wanted'.
#   An element of 'wanted' is .true. iff that attribute should be set.
#
# PASSED
#   wanted - which attributes to set

subroutine scattset ( wanted )
NOIMPLICIT
logical wanted ( MAX_ATTRIBUTES )

    logical scatfind
    integer temp ( MAX_ATTRIBUTES )
    integer n

    integer i
    integer j
    integer k

    include "sclb.c"
    include "sclb.ic"


    for ( i = 1 ; i <= MAX_ATTRIBUTES ; i = i + 1 )
	{
	if ( curatt ( i ) != wanted ( i ) )
	    {
	    # do this attribute group
	    if ( attstrat ( i ) == STRATEGY_INDIVIDUAL )
		{
		# see if any need to be cleared in this group
		for ( j = 1 ; j <= MAX_ATTRIBUTES ; j = j + 1 )
		    if ( attg ( i ) == attg ( j )  &
		         curatt ( j )  &  ! wanted ( j ) )
			break
		if ( j <= MAX_ATTRIBUTES )
		    # clear attributes in this group
		    call scattexec ( attgclear ( i ) )
		# set remaining attributes
		for ( k = 1 ; k <= MAX_ATTRIBUTES ; k = k + 1 )
		    {
		    if ( attg ( k ) == attg ( i )  &
		         ! curatt ( k )  &  wanted ( k ) )
			call scattexec ( attset ( i ) )
		    }
		}
	    else
		{
		# strategy is to do all at once
		for ( j = 1 ; j <= MAX_ATTRIBUTES ; j = j + 1 )
		    if ( attg ( j ) == attg ( i ) )
			if ( wanted ( j ) )
			    temp ( j ) = ATTRIBUTE_ON
			else
			    temp ( j ) = ATTRIBUTE_OFF
		    else
			temp ( j ) = ATTRIBUTE_UNCHANGED
		# find command sequence
		if ( scatfind ( temp, n ) )
		    call scattexec ( n )
		else
		    call scerror ( "scattset()" )
		}
	    }
	}

    return

end


### scbeep - ring bell
#
# SYNOPSIS
#   call scbeep
#
# DESCRIPTION
#   Rings the bell.

subroutine scbeep
NOIMPLICIT

    include "sclb.c"
    include "sclb.ic"


    if ( sccbeep != CANT )
	{
        call iopstr ( seqbeep )
	call scpad ( scnbeep )
	}

    return

end


### scbol - move the cursor to the beginning of the current line
#
#  SYNOPSIS
#    call scbol
#
#  DESCRIPTION
#    Moves the cursor to the beginning of the current line.

subroutine scbol
NOIMPLICIT

    include "sclb.c"
    include "sclb.ic"

    if ( sccbol == CANT )
        call scerror ( "scbol()" )
    else
	{
        call iopstr ( seqbol )
	call scpad ( scnbol )
	}

    return

end


### sccel - clear from cursor to end of line (inclusive)
#
# SYNOPSIS
#   call sccel
#
# DESCRIPTION
#   Clears all characters on the current line on or after the cursor.
#   Doesn't move cursor.

subroutine sccel
NOIMPLICIT

    include "sclb.c"
    include "sclb.ic"


    if ( scccel == CANT )
        call scerror ( "sccel()" )
    else
	{
        call iopstr ( seqcel )
	call scpad ( scncel )
	}

    return

end


### scchscroll - change scrolling region
#
# SYNOPSIS
#   call scchscroll ( f, l )
#
# DESCRIPTION
#   Changes the scrolling region to the area from line 'f' to line 'l'.
#   Moves the cursor home.
#
#   Initially, the scrolling region is the whole screen.
#
#   WARNING: You can't use relative cursor movements to cross
#   the boundaries of a scrolling region.  Use absolute cursor movements
#   instead.
#
# PASSED
#   f - first line of scrolling region
#   l - last line of scrolling region

subroutine scchscroll ( f, l )
NOIMPLICIT
integer f
integer l

    integer i
    include "sclb.c"
    include "sclb.ic"


    if ( sccchscroll == CANT )
	call scerror ( "scchscroll()" )

    else
	{
	i = 1

	while ( seqchscroll ( i ) != EOS )
	    {
	    if ( seqchscroll ( i ) == '%' )
		{
		i = i + 1
		if ( seqchscroll ( i ) == 'f' )
		    {
		    i = i + 1
		    call scputf ( seqchscroll, i, f )
		    }
		else if ( seqchscroll ( i ) == 'l' )
		    {
		    i = i + 1
		    call scputf ( seqchscroll, i, l )
		    }
		else if ( seqchscroll ( i ) != EOS )
		    {
		    call iopchar ( seqchscroll ( i ) )
		    i = i + 1
		    }
		}
	    else
		{
		call iopchar ( seqchscroll ( i ) )
		i = i + 1
		}
	    }
	}

    return

end


### scclear - clear the entire screen
#
# SYNOPSIS
#   call scclear
#
# DESCRIPTION
#   Clears the entire screen.  Moves the cursor home.

subroutine scclear
NOIMPLICIT

    include "sclb.c"
    include "sclb.ic"


    if ( sccclear == CANT )
        call scerror( "scclear()" )
    else
	{
        call iopstr ( seqclear )
	call scpad ( scnclear )
	}

    return

end


### scdisp - determine if a character can be displayed
#
# SYNOPSIS
#   logical truth, scdisp
#   character ch
#   truth = scdisp ( ch )
#
# DESCRIPTION
#   This routine returns .true. if the passed character can be displayed
#   on the terminal; otherwise, it returns .false.

logical function scdisp ( ch )
NOIMPLICIT
character ch

    integer i
    character ich
    equivalence (i, ich)
    include "sclb.c"

    i = 0			# unsigned byte to integer move
    ich = ch
    if ( i < 0 | i > '@^?' )
	return ( .false. )

    return ( scprch ( 1 + i ) )

end


### scdlc - delete character
#
# SYNOPSIS
#   call scdlc
#
# DESCRIPTION
#   Deletes the character the cursor is on, and moves the
#   characters to the right of the cursor one character to the
#   left, putting a blank in the rightmost column.
#   Doesn't move the cursor.

subroutine scdlc
NOIMPLICIT

    include "sclb.c"
    include "sclb.ic"


    if ( sccdlc == CANT )
        call scerror ( "scdlc()" )
    else
	{
        call iopstr ( seqdlc )
	call scpad ( scndlc )
	}

    return

end


### scdll - delete line
#
# SYNOPSIS
#   call scdll
#
# DESCRIPTION
#   Deletes the line the cursor is on, moving the lines below it up.
#   An empty line is put at the bottom of the screen.
#   The cursor should be in the first column of the line.
#   The position of the cursor is not changed.

subroutine scdll
NOIMPLICIT

    include "sclb.c"
    include "sclb.ic"


    if ( sccdll == CANT )
        call scerror ( "scdll()" )
    else
	{
        call iopstr ( seqdll )
	call scpad ( scndll )
	}

    return

end


### scdown - move the cursor down n lines
#
#  SYNOPSIS
#    integer n
#    call scdown ( n )
#
#  DESCRIPTION
#    Moves the cursor down n lines.
#
#  PASSED
#    n - number of lines to move the cursor down
#
subroutine scdown ( n )
NOIMPLICIT
integer n

    integer m
    include "sclb.c"
    include "sclb.ic"

    if ( sccdown == CANT )
        call scerror ( "scdown()" )
    else
        for ( m=1; m <= n; m=m+1 )
	    {
            call iopstr ( seqdown )
	    call scpad ( scndown )
	    }

    return

end


### scgraphics - output special graphics character
#
# call scgraphics ( n )
#
# description
#   output special graphics character with number n
#   sample graphics characters are LSEG_RULD (a cross), and
#   GRAPHICS_PILLOW
#
# passed
#   n - the number of the graphics character

subroutine scgraphics ( n )
NOIMPLICIT
integer n

    include "sclb.c"
    include "sclb.ic"


    if ( 1 <= n  &  n <= NUMBER_GRAPHICS_CHARS )
	{
	if ( sccgraphics ( n ) == ERR )
	    call scerror ( "scgraphics(1)" )

	else
	    {
	    # set attributes
	    call scattset ( satgraphics ( 1, n ) )

	    # send sequence
	    call iopstr ( seqgraphics ( 1, n ) )
	    }
	}

    else
	call scerror ( "scgraphics(2)" )

    return

end


### schome - move the cursor to the upper left corner of the screen
#
#  SYNOPSIS
#    call schome
#
#  DESCRIPTION
#    Moves the cursor to the home position (line 1, column 1).
#
#    (Note: the home position is not necessarily the beginning
#    of the scrolling region.)
#
subroutine schome
NOIMPLICIT

    include "sclb.c"
    include "sclb.ic"

    if ( scchome == CANT )
	call scerror ( "schome()" )
    else
        call iopstr ( seqhome )

    return

end


### sciim - go in insert mode
#
# SYNOPSIS
#   call sciim
#
# DESCRIPTION
#   Enters insert mode.  While insert mode is on, a 'put' of a character
#   is equivalent to an 'insert character' and a 'put'.

subroutine sciim
NOIMPLICIT

    include "sclb.c"
    include "sclb.ic"


    if ( scciim == CANT )
	call scerror ( "sciim()" )
    else
        call iopstr ( seqiim )

    return

end


### scikey - go in keypad mode
#
# SYNOPSIS
#   call scikey
#
# DESCRIPTION
#   Enters keypad mode.  Characters on the keypad should now send
#   some sort of escape sequence.

subroutine scikey
NOIMPLICIT

    include "sclb.c"
    include "sclb.ic"


    if ( sccikey == CANT )
	call scerror ( "scikey()" )
    else
        call iopstr ( seqikey )

    return

end


### scinc - insert character
#
# SYNOPSIS
#   call scinc
#
# DESCRIPTION
#   Moves all characters on the same line as the cursor that are right
#   of the cursor one to the right, and puts a blank where the cursor it.
#   The last character on the line is lost.
#   The cursor is moved one character to the right.
#   This is not defined if the cursor is on the last column of the
#   line.

subroutine scinc
NOIMPLICIT

    include "sclb.c"
    include "sclb.ic"


    if ( sccinc == CANT )
	call scerror ( "scinc()" )
    else
	{
        call iopstr ( seqinc )
	call scpad ( scninc )
	}

    return

end


### scinl - insert line
#
# SYNOPSIS
#   call scinl
#
# DESCRIPTION
#   Scrolls all lines on or after the line the cursor is on down one,
#   leaving a blank line where the cursor is.
#   The cursor is not moved.
#   This command should only be executed in column one.

subroutine scinl
NOIMPLICIT

    include "sclb.c"
    include "sclb.ic"


    if ( sccinl == CANT )
	call scerror ( "scinl()" )
    else
	{
        call iopstr ( seqinl )
	call scpad ( scninl )
	}

    return

end


### scleft - move the cursor to the left n columns
#
#  SYNOPSIS
#    integer n
#    call scleft ( n )
#
#  DESCRIPTION
#    Moves the cursor left n columns.
#
#  PASSED
#    n - the number of columns to move the cursor left
#
subroutine scleft ( n )
NOIMPLICIT
integer n

    integer m
    include "sclb.c"
    include "sclb.ic"

    if ( sccleft == CANT )
	call scerror ( "scleft()" )
    else
        for ( m=1; m <= n; m=m+1 )
            call iopstr ( seqleft )

    return

end


### scmove - directly address the cursor
#
#  SYNOPSIS
#    integer lin, col
#    call scmove ( lin, col )
#
#  DESCRIPTION
#    Moves the cursor to line 'lin' column 'col'.
#
subroutine scmove ( lin, col )
NOIMPLICIT
integer lin, col

    integer i
    include "sclb.c"
    include "sclb.ic"


    if ( sccmove == CANT )
	call scerror ( "scmove()" )

    else
	{
	for ( i=1; seqmove(i) != EOS; i=i+1 )
	    {
	    if ( (seqmove(i) == 'l') &
		 (seqmove(i+1) >= '0') & (seqmove(i+1) <= '4') )
		{
		i=i+1
		switch ( seqmove(i) )
		    {
		    case '0':
			call iopchar ( lin )
		    case '1':
			call iopchar ( lin+31 )
		    case '2':
			call scnump ( lin )
		    case '3':
			call iopchar ( lin+63 + (lin-1)/20*12 )
		    case '4':
			call scnump ( lin-1 )
		    }
		}
	    else if ( (seqmove(i) == 'c') &
		 (seqmove(i+1) >= '0') & (seqmove(i+1) <= '4') )
		{
		i=i+1
		switch ( seqmove(i) )
		    {
		    case '0':
			call iopchar ( col )
		    case '1':
			call iopchar ( col+31 )
		    case '2':
			call scnump ( col )
		    case '3':
			call iopchar ( col-1  + (col-1)/10*6  )
		    case '4':
			call scnump ( col-1 )
		    }
		}
	    else
		call iopchar ( seqmove(i) )
	    }
	}

    return

end


### scoim - exit insert mode
#
# SYNOPSIS
#   call scoim
#
# DESCRIPTION
#   Exists insert mode, going back to normal mode, where a character
#   typed does not push all characters on or to the right of the cursor
#   one character further to the right.

subroutine scoim
NOIMPLICIT

    include "sclb.c"
    include "sclb.ic"


    if ( sccoim == CANT )
	call scerror ( "scoim()" )
    else
        call iopstr ( seqoim )

    return

end


### scoffatt - turn attribute off
#
# SYNOPSIS
#   call scoffatt ( n )
#
# DESCRIPTION
#   Until sconatt is called, all characters will be written without
#   attribute 'n'.
#   'n' may be ATTRIBUTE_HIGHLIGHT, ATTRIBUTE_UNDERLINE, etc.
#   All attributes are initially off.
#   Scoffatt turns attribute n off, sconatt turns attribute n on.
#
# PASSED
#   n - the attribute to turn on

subroutine scoffatt ( n )
NOIMPLICIT
integer n

    include "sclb.ic"


    if ( 0 < n  &  n <= MAX_ATTRIBUTES )
	desatt ( n ) = .false.
    else
	call scerror ( "scoffatt()" )

    return

end


### scokey - leave keypad mode
#
# SYNOPSIS
#   call scokey
#
# DESCRIPTION
#   Exits keypad mode.  The keys on the keypad will not send escape sequences
#   any more (unless they always send escape sequences).

subroutine scokey
NOIMPLICIT

    include "sclb.c"
    include "sclb.ic"


    if ( sccokey == CANT )
	call scerror ( "scokey()" )
    else
        call iopstr ( seqokey )

    return

end


### sconatt - turn attribute on
#
# SYNOPSIS
#   call sconatt ( n )
#
# DESCRIPTION
#   Until scoffatt is called, all characters will be written with
#   attribute 'n'.
#   'n' may be ATTRIBUTE_HIGHLIGHT, ATTRIBUTE_UNDERLINE, etc.
#   All attributes are initially off.
#   Scoffatt turns attribute n off, sconatt turns attribute n on.
#
# PASSED
#   n - the attribute to turn on

subroutine sconatt ( n )
NOIMPLICIT
integer n

    include "sclb.ic"


    if ( 0 < n  &  n <= MAX_ATTRIBUTES )
	desatt ( n ) = .true.
    else
	call scerror ( "sconatt()" )

    return

end


### screinit - reinitialize the terminal
#
# call screinit
#
# description
#   sends the initialization sequence to the terminal

subroutine screinit
NOIMPLICIT

    include "sclb.c"
    include "sclb.ic"


    # send initialization string to terminal
    if ( sccinit != CANT )
	call iopstr ( seqinit )

    return

end


### scright - move the cursor to the right n columns
#
#  SYNOPSIS
#    integer n
#    call scright ( n )
#
#  DESCRIPTION
#    Moves the cursor n columns to the right.
#
#  PASSED
#    n - number of columns to move the cursor to the right
#
subroutine scright ( n )
NOIMPLICIT
integer n

    integer m
    include "sclb.c"
    include "sclb.ic"

    if ( sccright == CANT )
	call scerror ( "scright()" )
    else
        for ( m=1; m <= n; m=m+1 )
            call iopstr ( seqright )

    return

end


### scsdown - scroll down
#
# SYNOPSIS
#   call scsdown
#
# DESCRIPTION
#   Scrolls all lines in the scrolling region down one.
#   The cursor must be at the top of the scrolling region.
#   A blank line is put at the top of the scrolling region,
#   the last line of the scrolling region is lost.
#   The cursor position is not changed.

subroutine scsdown
NOIMPLICIT

    include "sclb.c"
    include "sclb.ic"


    if ( sccsdown == CANT )
	call scerror ( "scsdown()" )
    else
	{
        call iopstr ( seqsdown )
	call scpad ( scnsdown )
	}

    return

end


### scsup - scroll up
#
# SYNOPSIS
#   call scsup
#
# DESCRIPTION
#   Scrolls all lines in the scrolling region up one.
#   The cursor must be at the bottom of the scrolling region.
#   A blank line is put at the bottom of the scrolling region,
#   the first line of the scrolling region is lost.
#   The cursor position is not changed.

subroutine scsup
NOIMPLICIT

    include "sclb.c"
    include "sclb.ic"


    if ( sccsup == CANT )
	call scerror ( "scsup()" )
    else
	{
        call iopstr ( seqsup )
	call scpad ( scnsup )
	}

    return

end


### sctab - move cursor one tab stop
#
# SYNOPSIS
#   call sctab
#
# DESCRIPTION
#   Moves the cursor one tab stop.  The tab stops are defined in sctabs.
#   If the cursor is exactly on a tab stop, is moves to the following one.

subroutine sctab
NOIMPLICIT

    include "sclb.c"
    include "sclb.ic"


    if ( scctab == CANT )
	call scerror ( "sctab()" )
    else
	{
        call iopstr ( seqtab )
	call scpad ( scntab )
	}

    return

end


### scup - move the cursor up n lines
#
#  SYNOPSIS
#    integer n
#    call scup ( n )
#
#  DESCRIPTION
#    Moves the cursor up n lines.
#
#  PASSED
#    n - the number of lines to move the cursor up
#
subroutine scup ( n )
NOIMPLICIT
integer n

    integer m
    include "sclb.c"
    include "sclb.ic"

    if ( sccup == CANT )
	call scerror ( "scup()" )
    else
        for ( m=1; m <= n; m=m+1 )
            call iopstr ( sequp )

    return

end


### scpad - write out pad characters
#
#  SYNOPSIS
#    call scpad ( n )
#
#  DESCRIPTION
#    write out 'n' pad characters
#
#  PASSED
#    n - the number of pad characters to write out

subroutine scpad ( n )
NOIMPLICIT
integer n

    integer i

    include "sclb.c"


    for ( i = 1 ; i <= n ; i = i + 1 )
	call iopchar ( scpadc )

    return

end


### scputf - write out formated information
#
#  SYNOPSIS
#    call scputf ( line, i, n )
#
#  DESCRIPTION
#    Writes out 'n' in format specified at line ( i ), and advances i.
#    The format is of the form
#        [{+|-}num]{c|n}
#
#    'c' prints 'n' as a character (0 -> NULL, etc)
#    'l' prints 'n' as a number (in decimal)
#
#    [+|-]num adds or subtracts num from the ascii value of the character
#    before printing out

subroutine scputf ( line, i, n )
NOIMPLICIT
character line ( ARB )
integer i
integer n

    integer ctoi
    integer m
    integer s
    character c


    m = n

    while ( line ( i ) == '+'  |  line ( i ) == '-' )
	{
	if ( line ( i ) == '+' )
	    s = +1
	else
	    s = -1

	# modify 'm' as specified
	m = m + s * ctoi ( line, i )
	}

    if ( line ( i ) == 'c' )
	{
	i = i + 1
	c = m
	call iopchar ( c )
	}
    else if ( line ( i ) == 'n' )
	{
	i = i + 1
	call scnump ( m )
	}
    else
	{
	# we don't understand this
	call scerror ( "scputf()" )
	}

    return

end


### scnump - internal routine to write out a number
#
#  SYNOPSIS
#    integer number
#    call scnump ( number )
#
#  DESCRIPTION
#    Send the number 'number' (in decimal) to the terminal
#
#  PASSED
#    number - the number to send to the terminal
#
subroutine scnump ( number )
NOIMPLICIT
integer number

    character buf(10)
    integer len, itoc

    len = itoc ( number, buf, 10 )
    call iopstr ( buf )

    return

end


### scpillow - write out a filled-in block or close to it
#
#  SYNOPSIS
#    call scpillow
#
#  DESCRIPTION
#    Write a filled-in block or close to it.
#
subroutine scpillow
NOIMPLICIT

    include "sclb.c"
    include "sclb.ic"


    # output pillow character
    call scgraphics ( GRAPHICS_PILLOW )

    return

end


### scput - call iopchar(), handling pillow mode
#
#  SYNOPSIS
#    character ch
#    call scput ( ch )
#
#  DESCRIPTION
#    Writes a character to the terminal where the cursor is.
#    The cursor is moved one position to the right.
#
#    However, if the cursor is at the last column of the physical
#    line, it can do one of two things
#
#    1.  Go to the first column of the next line.
#        (This happens when scautonl is set.)
#    2.  Stay at the last column.
#        (This happens when scautonl is not set.)
#
#    However, if sclins < scplins, you don't know which of these it will do.
#
#    (Note: the vt100 has a sort of autonl mode where the cursor is not
#    advanced until the next character is typed.  This mode is assumed
#    to be off.  Otherwise, you have the situation where a 'put' of a
#    character sometimes puts the character where the cursor was, and
#    sometimes puts it on the next line.  This is too hard to support.)
#
#    If scautnl is set and the cursor is at the lower right character of the
#    physical screen, this routine should not be called, unless scwrtop is set,
#    in which case the cursor is moved to the upper left corner of the
#    screen after the character is written.

subroutine scput ( ch )
NOIMPLICIT
character ch

    include "sclb.ic"


    # set attributes
    call scattset ( desatt )

    call iopchar ( ch )

    return

end


### scunsetup - termination for screen package (for tset & similar tools)
#
# call scunsetup
#
# description
#   terminates screen packages for programs that use scsetup (like tset)

subroutine scunsetup
NOIMPLICIT

    integer i
    logical clearatt ( MAX_ATTRIBUTES )

    include "sclb.c"
    include "sclb.ic"


    # clear attributes
    for ( i = 1 ; i <= MAX_ATTRIBUTES ; i = i + 1 )
	clearatt ( i ) = .false.
    call scattset ( clearatt )

    call ioend

    return

end


### scend - termination for the screen package
#
#  SYNOPSIS
#    call scend
#
#  DESCRIPTION
#    This routine should be called when the user is finished
#    with the terminal, and wants it to go back into normal
#    modes.
#
subroutine scend
NOIMPLICIT

    call scunsetup

    return

end


### scchav - see if character is available
#
# SYNOPSIS
#   logical scchav
#   logical log
#   log = scchav ( 0 )
#
# DESCRIPTION
#   Returns .true. if there is a character is available, .false. otherwise
#
# RETURNED
#   log - whether there is a character available or not

logical function scchav ( dummy )
NOIMPLICIT
integer dummy

    logical iochav

    include "sclb.ic"


    scchav = .true.

    if ( inbufp == 0 )
	if ( ! iochav ( dummy ) )
	    scchav = .false.

    return

end


### scget - get character from terminal
#
# SYNOPSIS
#   character scget, c, d
#   d = scget ( c )
#
# DESCRIPTION
#   Gets a character from the terminal and returns it in 'c' and 'd'
#
# RETURNED
#   c - the character from the terminal
#   d - the character from the terminal

character function scget ( c )
NOIMPLICIT
character c

    character iogchar

    include "sclb.ic"


    if ( inbufp > 0 )
	{
	c = inbuf ( inbufp )
	inbufp = inbufp - 1
	scget = c
	}

    else
	scget = iogchar ( c )

    return

end


### scgetk - get character, possibly from the keypad
#
# SYNOPSIS
#   logical scgetk, log
#   integer k
#   character c
#
#   log = scgetk ( k, c )
#
# DESCRIPTION
#   Gets the next character or keypad sequence from the terminal.
#   If it is from the keypad, log is .true., and k is the number
#   of the key that was typed (e.g., KEYPAD_0).
#   Otherwise, log is .false., and c is the character that was typed.
#
# RETURNED
#   log - whether the sequence came from the keypad or not
#   k - if log is .true., the number of the sequence typed
#   c - if log is .false., the character typed

logical function scgetk ( key, c )
NOIMPLICIT
integer key
character c

    character temp ( MAXSEQLEN )
    logical possible ( MAX_KEYPAD_KEYS )
    integer npossible

    integer i
    integer j
    integer k

    character scget

    logical match
    integer matchk

    include "sclb.c"
    include "sclb.ic"


    # so far, since no characters have been read, any key
    # that the terminal has is a potential match
    # (ignore keys that have 0 length)
    npossible = 0
    for ( i = 1 ; i <= MAX_KEYPAD_KEYS ; i = i + 1 )
	{
	possible ( i ) = ( scckey ( i ) != CANT  &  scckey ( i ) > 0 )
	if ( possible ( i ) )
	    npossible = npossible + 1
	}

    # go as far as we can until we can't go any further
    # if there are several ways of making a match, the longest
    # match is taken
    match = .false.

    # in this 'for' statement, npossible is the number of things which
    # just might be extendible to j characters
    for ( j = 1 ; npossible > 0 ; j = j + 1 )
	{
	temp ( j ) = scget ( temp ( j ) )

	# figure out what things match exactly, and, otherwise,  what things
	# match to j characters, and thus just might be extendible to j + 1
	# characters

	# assume that nothing will extend
	npossible = 0

	# in case of a duplicate keys having the same sequence,
	# we will take the 'first' key (wrt key number) which has that sequence
	# (this is probably not very important, however)
	for ( i = MAX_KEYPAD_KEYS ; i >= 1 ; i = i - 1 )
	    {
	    # possible ( i ) tells if the key might be extended to j characters

	    # when this loop is finished, it will tell if the key might be
	    # extended to j + 1 characters
	    if ( possible ( i ) )
		{
		# assume that this sequence can't be extendible to j + 1
		# characters
		possible ( i ) = .false.

		if ( seqkey ( j, i ) == temp ( j ) )
		    {
		    if ( j == scckey ( i ) )
			{
			# we have a match (but perhaps not the longest one)
			# for j characters
			match = .true.
			matchk = i
			}
		    else
			{
			# this is still a potential match for j + 1 characters
			possible ( i ) = .true.
			npossible = npossible + 1
			}
		    }
		}
	    }
	}

    # figure out what we got
    # we only return a character if we failed to get a keypad sequence
    if ( match )
	{
	# we got a keypad sequence
	key = matchk
	scgetk = .true.
	}

    else
	{
	# the command sequence didn't work
	# we got a single character

	# push back command sequence

	# (note that it is possible to have read no characters yet,
	# so all characters are pushed back, not just all except
	# temp(1))
	for ( k = j - 1 ; k >= 1 ; k = k - 1 )
	    {
	    # push back character
	    call scunget ( temp ( k ) )
	    }

	# the last character read is the one we want
	c = scget ( c )
	scgetk = .false.
	}

    return

end


### scpeek - see what next character is (without actually 'reading' it)
#
# SYNOPSIS
#   character scpeek, c, d
#   d = scpeek ( c )
#
# DESCRIPTION
#   Looks at the next character and returns it in c and d.
#   The next call to scget will also return c.
#
# RETURNED
#   c - the next character
#   d - the next character

character function scpeek ( c )
NOIMPLICIT
character c

    character scget


    scpeek = scget ( c )
    call scunget ( scpeek )

    return

end


### scunget - internal routine to push character back on input
#
# SYNOPSIS
#   call scunget ( c )
#
# DESCRIPTION
#   Pushes character 'c' back on input
#
# PASSED
#   c - character to push back on input

subroutine scunget ( c )
NOIMPLICIT
character c

    include "sclb.ic"


    if ( inbufp < MAXSEQLEN )
	{
	inbufp = inbufp + 1
	inbuf ( inbufp ) = c
	}

    else
	call scerror ( "scunget()" )

    return

end


### scerror - internal routine to output an error message and halt the program
#
#  SYNOPSIS
#    character msg()
#    call scerror
#
subroutine scerror ( msg )
NOIMPLICIT
character msg(ARB)

    include "sclb.ic"

    if ( debug )
	call scbeep
    call iopstr ( "sclb$" )
    call iopstr ( msg )
    if ( ! debug )
	{
	call iopstr ( ": Fatal internal error." )
	call ioflush
	call ioend
	call error ( "" )
	}

    return

end
