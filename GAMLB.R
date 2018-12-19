# gamlb - game restriction library

# ver  date   who remarks
# --- ------- --- ------------------------------------------------------
# 03c 10sep85 jp  .Added feature to disallow specified groups of
#                  terminals.  Default is to disallow nty*.
#                 .Removed gamcronfile() and added the cronfile argument
#                  to gamlinit().
# 03b 27May85 cal .Reformatted. Added simple truename feature. Modified
#                  to use vmserror(). Fixed bugs in gamcmp().
# 03a 11Jun84 cal .Added gamcronfile() and removed the cronfile argument
#                  to gamlinit(). Cleaned up some system calls.
# 02e 19Apr84 cal .Modified to use getnow() and wkday(). Added code to
#                  set up an exit handler to insure restoring the
#                  process priority.
# 02d 15Jul83 cal .Ratfix, added NOIMPLICITs.
# 02c 24Apr83 cal .Added a lower level of initialization.
# 02b 31jan82 cal .Added code to protect system logical names from
#                  group or process names.
# 02a 01jan82 cal .Modifed to use formatted file to determine legal
#                  game hours. Modified gamnumtim to bias day of week
#                  with Monday == 1 (just like unix.)
# 01b 12nov82 NJD .Changed game-restriction hours.
# 01a 02nov82 JP  .First written.
#

include "syssym"			# to get LOGICAL_NAME_PREFIX

define(JPI$_IMAGNAM,16%207)
define(DVI$_DEVDEPEND,16%A)
define(TT$M_REMOTE,16%2000)
define(TT$M_MODEM,16%200000)

define(IMAGESIZE,64)			# size of largest VMS file spec

define(LOOKING,1)
define(ALLOW,2)
define(DISALLOW,3)

define(GAMCOM,integer oldpri, estatus; common /gamcom/ oldpri, estatus)


###  gamcheck - check game cron entry (internal)
#
#  SYNOPSIS
#    logical truth, gamcheck
#    truth = gamcheck( 0 )
#
# Returns YES if the current game cron entry fits the current time and
# date, else NO. There are 6 fields in a valid entry in the game cron file.
# Fields are seperated with COLONs; the fields are minute (0-59), hour
# (0-23), day of month (1-31), month of year (1-12), day of week (1-7
# where 1 == Monday), and finally a random message. Each entry specifies
# one case when it is allowable to play a game.
#
# A range may be specified by using a MINUS. A list is seperated with
# COMMAs. A STAR matches any time. Also, the HASH character can be used
# for comment lines.
#
logical function gamcheck( dummy )
NOIMPLICIT
integer dummy

    integer status, now(7), day, wkday, enfldn
    logical gamcmp
    character ranmsg(MAXLINE), buf(MAXLINE)

    # Get the current time and date.
    call getnow( now )

    # Check day of week.
    if ( enfldn( 5, buf, MAXLINE ) == ERR )
	return ( .false. )			# not enough fields

    # wkday() returns 1 for Sunday, 7 for Saturday,
    #  but we need 1 for Monday, 7 for Sunday.
    day = wkday( now(2), now(3), now(1) ) - 1
    if ( day == 0 )
	day = 7
    if ( ! gamcmp( day, buf ) )
	return ( .false. )

    # Check month of year
    status = enfldn( 4, buf, MAXLINE )
    # Assume ok since last test was.
    if ( ! gamcmp( now(2), buf ) )
	return ( .false. )

    # Check day of month.
    status = enfldn( 3, buf, MAXLINE )
    if ( ! gamcmp( now(3), buf ) )
	return ( .false. )

    # Check hour.
    status = enfldn( 2, buf, MAXLINE )
    if ( ! gamcmp( now(4), buf ) )
	return ( .false. )

    # Check minute.
    if ( enfldn( 1, buf, MAXLINE ) == ERR )
	return ( .false. )
    if ( ! gamcmp( now(5), buf ) )
	return ( .false. )

    # Print out the random message (if there was one).
    ranmsg(1) = EOS
    status = enfldn( 6, ranmsg, MAXLINE )
    if ( ranmsg(1) != EOS )
	call printf( "%s@n", ranmsg )

    return ( .true. )

end


###  gamcmp - check game time field (internal)
#
#  SYNOPSIS
#    logical truth, gamcmp
#    integer num
#    character buf()
#    truth = gamcmp( num, buf )
#
logical function gamcmp( num, buf )
NOIMPLICIT
integer num
character buf(ARB)

    integer i, ctoi, index, type

    # First look for the match all.
    if ( buf(1) == '*' & buf(2) == EOS )
	return ( .true. )

    # Next look for a range.
    if ( index( buf, '-' ) != 0 )
	{
	i = 1
	if ( num < ctoi( buf, i ) )
	    return ( .false. )
	if ( buf(i) != '-' )
	    return ( .false. )		# garbage in buffer
	i = i + 1			# step over MINUS
	if ( num > ctoi( buf, i ) )
	    return ( .false. )
	if ( buf(i) != EOS )
	    return ( .false. )		# garbage in buffer
	return ( .true. )
	}

    # Must be a list or simple a number.
    i = 1
    while ( buf(i) != EOS )
	{
	if ( type( buf(i) ) != DIGIT )
	    return ( .false. )		# garbage in buffer
	if ( num == ctoi( buf, i ) )
	    return ( .true. )
	if ( buf(i) == ',' )
	    i = i + 1			# step over COMMA
	}

    # If we got here, we failed all the tests.
    return ( .false. )

end


###  gamcronfile - return the pathname of the cron file (internal)
#
#  SYNOPSIS
#    character file()
#    call gamcronfile( file )
#
subroutine gamcronfile( file )
NOIMPLICIT
character file(ARB)

    call strcpy( "/misc/lib/gamcron", file )

    return

end


###  gamdialup - check whether the user is on a dialup
#
#  SYNOPSIS
#    logical truth, gamdialup
#    truth = gamdialup( 0 )
#
#  DESCRIPTION
#    If a terminal has the attributes remote and modem, it is
#    considered to be a dialup.
#
logical function gamdialup( dummy )
NOIMPLICIT
integer dummy

    integer ttdesc(2), bufa, status, sys$getdvi, iand
    integer*4 buf
    integer*2 dvilst(8)
    external ss$_normal, ss$_concealed
    equivalence (bufa, dvilst(3))
    data dvilst / 4, DVI$_DEVDEPEND, 6*0 /

    call dscbld( ttdesc, "TT" )
    bufa = %loc(buf)
    status = sys$getdvi( , , ttdesc, dvilst, , , , )
    if ( status != %loc(ss$_normal) & status != %loc(ss$_concealed) )
	call vmserror( "gamlb$gamdialup: sys$getdvi(), %s.", status )

    return ( iand( buf, TT$M_REMOTE ) != 0 & iand( buf, TT$M_MODEM ) != 0 )

end


###  gamend - terminate a game
#
#  SYNOPSIS
#    call gamend
#
subroutine gamend
NOIMPLICIT

    integer status, sys$setpri
    external ss$_normal
    GAMCOM

    if ( oldpri != 0 )
	{
	status = sys$setpri( , , %val(oldpri), )
	oldpri = 0
	}

    return

end


###  gaminit - initialize for a game
#
#  SYNOPSIS
#    character truename()
#    call gaminit( truename )
#
# This is the default routine used to initialize games. Dialup and network
# use are not allowed, priority altering is desired, checking of the game
# cron file is desired, the priority is lowered to the default, and "guest"
# is not allowed to play.
#
subroutine gaminit( truename )
NOIMPLICIT
character truename(ARB)

    character cronfile(FILENAMESIZE)

    call gamcronfile( cronfile )
    call gamlinit( .false., .true., .true., 0, "guest", "nty", truename,
        cronfile )

    return

end


###  gamlinit - initialize for a game, low level
#
#  SYNOPSIS
#    logical fdial, fprio, fcron
#    integer despri
#    character badlist(), badttylist(), truename()
#    call gamlinit( fdial, fprio, fcron, despri, badlist, badttylist,
#        truename, cronfile )
#
subroutine gamlinit( fdial, fprio, fcron, despri, badlist, badttylist,
    truename, cronfile )
NOIMPLICIT
logical fdial, fprio, fcron
integer despri
character badlist(ARB), badttylist(ARB), truename(ARB), cronfile(ARB)

    integer i, status, sys$setpri, strcmp, remove, lastsn, sys$dclexh
    integer newpri, state, engetnext, enopen
    integer*4 desblk(4)
    logical gamtname, gamdialup, gamcheck
    character buf(MAXLINE), gamcron(FILENAMESIZE), image(FILENAMESIZE)
    character ntruename(FILENAMESIZE)
    external ss$_normal, gamend
    GAMCOM

    # No copies of games. If truename contains a simple filename, simply
    # make sure that the simple name of the current image is the same.
    # But if it's a full pathname, we do some logical name magic to
    # prevent cretins from tricking us with process logical names.
    if ( truename(1) != EOS )
	{
	i = lastsn( truename )
	if ( i == 1 )
	    {
	    # Simple filename.
	    call gamimage( buf )
	    call fold( buf )
	    call scopy( buf, lastsn( buf ), image, 1 )
	    call strcpy( truename, ntruename )
	    }
	else
	    {
	    # Full pathname
	    call gamlogic( truename )
	    call mkpath( truename, ntruename )
	    call gamimage( buf )
	    call gamlogic( buf )
	    call mkpath( buf, image )
	}
	if ( strcmp( ntruename, image ) != 0 )
	    {
	    status = remove( buf )
	    call error( "You are using a bootleg binary.  Shame on you." )
	    }
	}

    # No games for cretins.
    if ( badlist(1) != EOS )
	{
	call glname( buf )
	if ( gamtname( buf, badlist, .false. ) )
	    call error( "You are NOT allowed to play this game." )
	}

    # No games on specified terminals.
    if ( badttylist(1) != EOS )
	{
	call termin( buf )
	call fold( buf )
	if ( gamtname( buf, badttylist, .true. ) )
	    call error( "Games are not allowed on this terminal." )
	}

    # Check dialup usage.
    if ( ! fdial )
	if ( gamdialup( 0 ) )
	    call error( "Games are not allowed over dial-up lines." )

    # Check the game cron file to see if it's a good time to play.
    # Again we toy with logical names.
    if ( fcron )
	{
	if ( cronfile(1) != EOS )
	    call strcpy( cronfile, gamcron )
	else
	    call gamcronfile( gamcron )
	call gamlogic( gamcron )
	if ( enopen( gamcron ) == ERR )
	    call eprintf(
		"gamlb$gaminit: Failed to open the game cron file, %s.@n",
		gamcron )
	else
	    {
	    state = LOOKING
	    while ( state == LOOKING )
		{
		if ( engetnext( 0 ) == EOF )
		    state = DISALLOW
		else if ( gamcheck( 0 ) )
		    state = ALLOW
		}
	    call enclose

	    if ( state != ALLOW )
		{
		call eprintf(
		    "Now is not a good time to play games. For valid times,@n" )
		call eprintf( "please consult %s", gamcron )
		call error( "" )
		}
	    }
	}

    # Ok to play, handle priority stuff.
    oldpri = 0
    if ( fprio )
	{
	status = sys$setpri( , , %val(4), oldpri )
	if ( status != %loc(ss$_normal) )
	    call vmserror( "gamlb$gaminit: sys$setpri(1), %s", status )

	i = despri
	if ( i == 0 )
	    i = -2				# default to 2 less than current

	if ( i < 0 )
	    {
	    newpri = oldpri + i			# relative priority
	    if ( newpri <= 0 )
		newpri = 1
	    }
	else
	    newpri = i				# absolute priority
	status = sys$setpri( , , %val(newpri), )
	if ( status != %loc(ss$_normal) )
	    call vmserror( "gamlb$gaminit: sys$setpri(2), %s", status )

	# Set up an exit handler to restore the priority.
	desblk(1) = 0				# forward link
	desblk(2) = %loc(gamend)		# address of exit handler
	desblk(3) = 0				# number of arguments
	desblk(4) = %loc(estatus)		# address of exit status
	status = sys$dclexh( desblk )
	if ( status != %loc(ss$_normal) )
	    {
	    call gamend
	    call vmserror( "gamlb$gaminit: sys$dclexh(), %s", status )
	    }
	}

    return

end


###  gamimage - get the pathname this image was executed from
#
#  SYNOPSIS
#    character image()
#    call gamimage( image )
#
subroutine gamimage( image )
NOIMPLICIT
character image(ARB)

    integer*2 jpilst(8)
    integer bufa, lena, len, status, sys$getjpi, i, index, fntopn
    external ss$_normal
    equivalence (bufa, jpilst(3)), (lena, jpilst(5))
    data jpilst / IMAGESIZE, JPI$_IMAGNAM, 6*0 /

    bufa = %loc(image)
    lena = %loc(len)
    status = sys$getjpi( , , , jpilst, , , )
    if ( status != %loc(ss$_normal) )
	call vmserror( "gamlb$gamimage: sys$getjpi(), %s", status )
    image(len+1) = EOS
    i = index( image, ';' )
    if ( i > 0 )
	{
	image(i) = EOS
	if ( i > 1 )
	    {
	    i = i - 1
	    if ( image(i) == '.' )
		image(i) = EOS
	    }
	}

    return

end


###  gamtname - test for a name (internal)
#
#  SYNOPSIS
#    logical gamtname, flag, leadingmatch
#    character name(), list()
#    flag = gamtname( name, list, leadingmatch )
#
# The names in list are seperated with COLONs.  The leadingmatch boolean
# says that the names in list only have to match a leading substring of
# name - e.g. a list of "nty:ttb" would match all terminals beginning with
# nty or ttb.
#
logical function gamtname( name, list, leadingmatch )
NOIMPLICIT
character name(ARB), list(ARB)
logical leadingmatch

    integer i, j
    character clower

    i = 1
    while ( list(i) != EOS )
	{
	j = 1
	while ( name(j) == clower( list(i) ) )
	    {
	    j = j + 1
	    i = i + 1
	    if ( ( list(i) == ':' | list(i) == EOS ) &
		 ( name(j) == EOS | leadingmatch ) )
		return ( .true. )
	    }
	for ( ; list(i) != ':'; i = i + 1 )
	    if ( list(i) == EOS )
		return ( .false. )
	i = i + 1
	}

    return ( .false. )

end


###  gamlogic - hack logical names to prevent abuse (internal)
#
#  SYNOPSIS
#    logical truth, gamlogic
#    character file()
#    truth = gamlogic( file )
#
# The routine is used to protect the logical name that might appear
# as part of a tools or vms file spec.
#
subroutine gamlogic( file )
NOIMPLICIT
character file(ARB)

    integer i, index
    logical truth, gamlogical
    character name(FILENAMESIZE)

    if ( file(1) == '/' )
	{
	# Tools pathname, construct the logical name with the prefix.
	i = 1
	call stcopy( LOGICAL_NAME_PREFIX, 1, name, i )
	call scopy( file, 2, name, i )
	i = index( name, '/' )
	if ( i > 0 )
	    name(i) = EOS
	}
    else
	{
	# Must be a VMS file specification.
	call strcpy( file, name )
	i = index( name, ':' )
	if ( i > 0 )
	    name(i) = EOS
	i = index( name, '[' )
	if ( i > 0 )
	    name(i) = EOS
	}
    call upper( name )
    truth = gamlogical( name )
    return

end


###  gamlogical - hack a logical name to prevent abuse (internal)
#
#  SYNOPSIS
#    logical truth, gamlogical
#    character name()
#    truth = gamlogical( name )
#
# If a passed name is a system logical name, and there is a group or
# process logical name, make a user mode process that is the same as
# the system name.
#
logical function gamlogical( name )
NOIMPLICIT
character name(ARB)

define(PSL$C_USER,3)			# create user mode logical name
define(TABLE_MASK,2%0110)		# only search system table
define(TAB_SYSTEM,0)			# system logical name table
define(TAB_PROCESS,2)			# process logical name table

    integer status, sys$crelog, sys$trnlog, index, length
    integer i, table, ldsc(2), rdsc(2)
    character buf(FILENAMESIZE)
    external SS$_NORMAL

    # build the string descriptors for the logical and translated names
    ldsc(1) = length(name)
    ldsc(2) = %loc(name)
    rdsc(1) = MAXLINE - 1
    rdsc(2) = %loc(buf)

    status = sys$trnlog( ldsc, i, rdsc, table, , )
    if ( status != %loc(SS$_NORMAL) )
	return ( .false. )		# couldn't translate the name
    if ( table == TAB_SYSTEM )
	return ( .false. )		# came from system table

    #
    # If we got here then there is a group or process logical name
    # and it may be in the way of a system name. If we can translate
    # the name from the system name table, then we create a user mode
    # process logical name (that will go away after image exit) so
    # that open() and friends will get the right name (and also so
    # cretins can become vastly confused.)
    #
    status = sys$trnlog( ldsc, i, rdsc, table, , %val(TABLE_MASK) )
    if ( status != %loc(SS$_NORMAL) )
	return ( .false. )		# no system logical name

    rdsc(1) = i + 1			# update actual length
    status = sys$crelog( %val(TAB_PROCESS), ldsc, rdsc, PSL$C_USER )

    return ( .true. )			# indicate we changed something

end
