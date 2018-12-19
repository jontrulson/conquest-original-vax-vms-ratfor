###############################################################################
#
#                               C O N Q O P E R
#
#            Copyright (C)1983-1986 by Jef Poskanzer and Craig Leres
#
#    Permission to use, copy, modify, and distribute this software and
#    its documentation for any purpose and without fee is hereby granted,
#    provided that this copyright notice appear in all copies and in all
#    supporting documentation. Jef Poskanzer and Craig Leres make no
#    representations about the suitability of this software for any
#    purpose. It is provided "as is" without express or implied warranty.
#
###############################################################################
#
#	Detailed revision history lives in "incl/conqdef"
#
###############################################################################

include "conqdef"


###  conqoper - main program
#
DRIVER(conqoper)
NOIMPLICIT

    integer cdcols, cdlins, strcmp, spawn, i
    logical l, isagod, ioautobroad
    character name(MAXUSERNAME)
    string cpr COPYRIGHT
    string vern VERSION_NUMBER
    string verd VERSION_DATE
    include "conqcom2"

    call glname( name )
    if ( ! isagod( name ) )
	{
	call putlin( "Poor cretins such as yourself lack the", ERROUT )
	call error( " skills necessary to use this program." )
	}

    call rndini( 0, 0 )				# initialize random numbers
    call cdinit					# initialize display environment

    l = ioautobroad( .false. )			# turn OFF automatic broadcasts
    cunum = MSG_GOD				# stow user number
    cmaxlin = cdlins( 0 )			# number of lines
    cmaxcol = cdcols( 0 )			# number of columns

    call operate

    call cdend

    DRETURN

end


###  bigbang - fire a torp from every ship
#
#  SYNOPSIS
#    call bigbang
#
subroutine bigbang
NOIMPLICIT

    integer i, snum, cnt
    real dir, mod360
    logical launch
    include "conqcom"

    dir = 0.0
    cnt = 0
    for ( snum = 1; snum <= MAXSHIPS; snum = snum + 1 )
	if ( sstatus(snum) == SS_LIVE )
	    for ( i = 1; i <= MAXTORPS; i = i + 1 )
		if ( ! launch( snum, dir ) )
		    break
		else
		    {
		    dir = mod360( dir + 40.0 )
		    cnt = cnt + 1
		    }
    call cerror( MSG_GOD,
	"bigbang: Fired %d torpedos, hoo hah won't they be surprised!", cnt )

    return

end


###  debugdisplay - verbose and ugly version of display
#
#  SYNOPSIS
#    integer snum
#    call debugdisplay( snum )
#
subroutine debugdisplay( snum )
NOIMPLICIT
integer snum

# The following aren't displayed by this routine...
#
# logical ssrpwar(snum,NUMPLANETS)	# self-ruled planets s/he is at war
# integer slastmsg(snum)		# last message seen
# integer salastmsg(snum)		# last message allowed to be seen
# logical smap(snum)			# strategic map or not
# integer spfuse(snum)			# tenths until phasers can be fired
# integer sscanned(snum,NUMTEAMS)	# fuse for which ships have been
# logical stalert(snum)			# torp alert!
# integer sctime(snum)			# cpu hundreths at last check
# integer setime(snum)			# elapsed hundreths at last check
# integer scacc(snum)			# accumulated cpu time
# integer seacc(snum)			# accumulated elapsed time

    integer i, j, unum, lin, col, tcol, dcol
    real x
    character cupper, buf(MSGMAXLINE)
    include "conqcom"
    include "conqcom2"

    call cdclrl( 1, MSG_LIN1 - 1 )		# don't clear the message lines
    unum = suser(snum)

    lin = 1
    tcol = 1
    dcol = tcol + 10
    call cdputs( "    ship:", lin, tcol )
    buf(1) = EOS
    call appship( snum, buf )
    if ( srobot(snum) )
	call appstr( " (ROBOT)", buf )
    call cdputs( buf, lin, dcol )
    lin = lin + 1
    call cdputs( "      sx:", lin, tcol )
    call cdputr( oneplace(sx(snum)), 0, lin, dcol )
    lin = lin + 1
    call cdputs( "      sy:", lin, tcol )
    call cdputr( oneplace(sy(snum)), 0, lin, dcol )
    lin = lin + 1
    call cdputs( "     sdx:", lin, tcol )
    call cdputr( oneplace(sdx(snum)), 0, lin, dcol )
    lin = lin + 1
    call cdputs( "     sdy:", lin, tcol )
    call cdputr( oneplace(sdy(snum)), 0, lin, dcol )
    lin = lin + 1
    call cdputs( "  skills:", lin, tcol )
    call cdputr( oneplace(skills(snum)), 0, lin, dcol )
    lin = lin + 1
    call cdputs( "   swarp:", lin, tcol )
    x = oneplace(swarp(snum))
    if ( x == ORBIT_CW )
	call cdputs( "ORBIT_CW", lin, dcol )
    else if ( x == ORBIT_CCW )
	call cdputs( "ORBIT_CCW", lin, dcol )
    else
	call cdputr( x, 0, lin, dcol )
    lin = lin + 1
    call cdputs( "  sdwarp:", lin, tcol )
    call cdputr( oneplace(sdwarp(snum)), 0, lin, dcol )
    lin = lin + 1
    call cdputs( "   shead:", lin, tcol )
    call cdputn( round(shead(snum)), 0, lin, dcol )
    lin = lin + 1
    call cdputs( "  sdhead:", lin, tcol )
    call cdputn( round(sdhead(snum)), 0, lin, dcol )
    lin = lin + 1
    call cdputs( " sarmies:", lin, tcol )
    call cdputn( sarmies(snum), 0, lin, dcol )

    lin = 1
    tcol = 23
    dcol = tcol + 12
    call cdputs( "      name:", lin, tcol )
    if ( spname(1,snum) != EOS )
	call cdputs( spname(1,snum), lin, dcol )
    lin = lin + 1
    call cdputs( "  username:", lin, tcol )
    buf(1) = EOS
    if ( unum >= 1 & unum <= MAXUSERS )
	{
	call strcpy( uname(1,unum), buf )
	if ( buf(1) != EOS )
	    call appchr( ' ', buf )
	}
    call appchr( '(', buf )
    call appint( unum, buf )
    call appchr( ')', buf )
    call cdputs( buf, lin, dcol )
    lin = lin + 1
    call cdputs( "     slock:", lin, tcol )
    call cdputs( "       dtt:", lin + 1, tcol )
    i = slock(snum)
    if ( -i >= 1 & -i <= NUMPLANETS )
	{
        call cdputs( pname(1,-i), lin, dcol )
	call cdputn( round( dist( sx(snum), sy(snum), px(-i), py(-i) ) ),
	    0, lin + 1, dcol )
	}
    else if ( i != 0 )
        call cdputn( i, 0, lin, dcol )
    lin = lin + 2
    call cdputs( "     sfuel:", lin, tcol )
    call cdputn( round(sfuel(snum)), 0, lin, dcol )
    lin = lin + 1
    call cdputs( "       w/e:", lin, tcol )
    call prints( buf, "%d/%d", sweapons(snum), sengines(snum) )
    if ( swfuse(snum) > 0 | sefuse(snum) > 0 )
	{
	call appstr( " (", buf )
	call appint( swfuse(snum), buf )
	call appchr( '/', buf )
	call appint( sefuse(snum), buf )
	call appchr( ')', buf )
	}
    call cdputs( buf, lin, dcol )
    lin = lin + 1
    call cdputs( "      temp:", lin, tcol )
    call prints( buf, "%d/%d", round(swtemp(snum)), round(setemp(snum)) )
    call cdputs( buf, lin, dcol )
    lin = lin + 1
    call cdputs( "   ssdfuse:", lin, tcol )
    i = ssdfuse(snum)
    buf(1) = EOS
    if ( i != 0 )
	{
	call prints( buf, "%d ", i )
	}
    if ( scloaked(snum) )
	call appstr( "(CLOAKED)", buf )
    call cdputs( buf, lin, dcol )
    lin = lin + 1
    call cdputs( "      spid:", lin, tcol )
    i = spid(snum)
    if ( i != 0 )
	{
	call prints( buf, "%08x", i )
	call cdputs( buf, lin, dcol )
	}
    lin = lin + 1
    call cdputs( "slastblast:", lin, tcol )
    call cdputr( oneplace(slastblast(snum)), 0, lin, dcol )
    lin = lin + 1
    call cdputs( "slastphase:", lin, tcol )
    call cdputr( oneplace(slastphase(snum)), 0, lin, dcol )

    lin = 1
    tcol = 57
    dcol = tcol + 12
    call cdputs( "   sstatus:", lin, tcol )
    buf(1) = EOS
    call appsstatus( sstatus(snum), buf )
    call cdputs( buf, lin, dcol )
    lin = lin + 1
    call cdputs( " skilledby:", lin, tcol )
    i = skilledby(snum)
    if ( i != 0 )
	{
	buf(1) = EOS
	call appkb( skilledby(snum), buf )
	call cdputs( buf, lin, dcol )
	}
    lin = lin + 1
    call cdputs( "   shields:", lin, tcol )
    call cdputn( round(sshields(snum)), 0, lin, dcol )
    if ( ! sshup(snum) )
	call cdput( 'D', lin, dcol+5 )
    lin = lin + 1
    call cdputs( "   sdamage:", lin, tcol )
    call cdputn( round(sdamage(snum)), 0, lin, dcol )
    if ( srmode(snum) )
	call cdput( 'R', lin, dcol+5 )
    lin = lin + 1
    call cdputs( "  stowedby:", lin, tcol )
    i = stowedby(snum)
    if ( i != 0 )
	{
	buf(1) = EOS
	call appship( i, buf )
	call cdputs( buf, lin, dcol )
	}
    lin = lin + 1
    call cdputs( "   stowing:", lin, tcol )
    i = stowing(snum)
    if ( i != 0 )
	{
	buf(1) = EOS
	call appship( i, buf )
	call cdputs( buf, lin, dcol )
	}
    lin = lin + 1
    call cdputs( "      swar:", lin, tcol )
    buf(1) = '('
    for ( i = 1; i <= NUMTEAMS; i = i + 1 )
	if ( swar(snum,i) )
	    buf(i+1) = chrteams(i)
	else
	    buf(i+1) = '-'
    buf(NUMTEAMS+2) = ')'
    buf(NUMTEAMS+3) = EOS
    call fold( buf )
    call cdputs( buf, lin, dcol )

    lin = lin + 1
    call cdputs( "     srwar:", lin, tcol )
    buf(1) = '('
    for ( i = 1; i <= NUMTEAMS; i = i + 1 )
	if ( srwar(snum,i) )
	    buf(i+1) = chrteams(i)
	else
	    buf(i+1) = '-'
    buf(NUMTEAMS+2) = ')'
    buf(NUMTEAMS+3) = EOS
    call cdputs( buf, lin, dcol )

    lin = lin + 1
    call cdputs( "   soption:", lin, tcol )
    call strcpy( "(gpainte)", buf )
    for ( i = 1; i <= MAXOPTIONS; i = i + 1 )
	if ( soption(snum,i) )
	    buf(i+1) = cupper(buf(i+1))
    call cdputs( buf, lin, dcol )

    lin = lin + 1
    call cdputs( "   uoption:", lin, tcol )
    if ( unum >= 1 & unum <= MAXUSERS )
	{
	call strcpy( "(gpainte)", buf )
	for ( i = 1; i <= MAXOPTIONS; i = i + 1 )
	    if ( uoption(unum,i) )
		buf(i+1) = cupper(buf(i+1))
	call cdputs( buf, lin, dcol )
	}

    lin = lin + 1
    call cdputs( "   saction:", lin, tcol )
    i = saction(snum)
    if ( i != 0 )
	{
	call robstr( i, buf )
	call cdputs( buf, lin, dcol )
	}

    lin = cmaxlin - MAXTORPS - 2
    call cdputs(
    "tstatus    tfuse    tmult       tx       ty      tdx      tdy     twar",
	lin, 3 )
    for ( i = 1; i <= MAXTORPS; i = i + 1 )
	{
        lin = lin + 1
	call cdputn( tstatus(snum,i), 9, lin, 1 )
	if ( tstatus(snum,i) != TS_OFF )
	    {
	    call cdputn( tfuse(snum,i), 9, lin, 10 )
	    call cdputr( oneplace(tmult(snum,i)), 9, lin, 19 )
	    call cdputr( oneplace(tx(snum,i)), 9, lin, 28 )
	    call cdputr( oneplace(ty(snum,i)), 9, lin, 37 )
	    call cdputr( oneplace(tdx(snum,i)), 9, lin, 46 )
	    call cdputr( oneplace(tdy(snum,i)), 9, lin, 55 )
	    buf(1) = '('
	    for ( j = 1; j <= NUMTEAMS; j = j + 1 )
		if ( twar(snum,i,j) )
		    buf(j+1) = chrteams(j)
		else
		    buf(j+1) = '-'
	    buf(NUMTEAMS+2) = ')'
	    buf(NUMTEAMS+3) = EOS
	    call cdputs( buf, lin, 67 )
	    }
	}

    call cdmove( 1, 1 )
    call cdplay( .true. )

    return

end


###  debugplan - debugging planet list
#
#  SYNOPSIS
#    call debugplan
#
subroutine debugplan
NOIMPLICIT

    integer i, j, lin, col, olin, sv(NUMPLANETS)
    logical l, more, init
    character buf(MSGMAXLINE), junk(10), uninhab(20)
    include "conqcom"
    string hd "planet        C T arm uih scan        planet        C T arm uih scan"
    data init / .false. /

    if ( ! init )
	{
	for ( i = 1; i <= NUMPLANETS; i = i + 1 )
	    sv(i) = i
	init = .true.
	}
    call sortplanets( sv )

    call cdclear
    call strcpy( hd, buf )
    lin = 1
    call cdputc( buf, lin )
    for ( i = 1; buf(i) != EOS; i = i + 1 )
	if ( buf(i) != ' ' )
	    buf(i) = '-'
    lin = lin + 1
    call cdputc( buf, lin )
    lin = lin + 1
    olin = lin
    col = 6
    for ( i = 1; i <= NUMPLANETS; i = i + 1 )
	{
	for ( j = 1; j <= NUMTEAMS; j = j + 1 )
	    if ( pscanned(sv(i),j) & j >= 1 & j <= NUMTEAMS )
		junk(j) = chrteams(j)
	    else
		junk(j) = '-'
	junk(j+1) = EOS
	j = puninhabtime(sv(i))
	if ( j != 0 )
	    call prints( uninhab, "%d", j )
	else
	    uninhab(1) = EOS
	call prints( buf, "%-13s %c %c %3d %3s %4s",
	    pname(1,sv(i)), chrplanets(ptype(sv(i))), chrteams(pteam(sv(i))),
	    parmies(sv(i)), uninhab, junk )

	call cdputs( buf, lin, col )
	if ( ! preal(sv(i)) )
	    call cdput( '-', lin, col - 1 )

	lin = lin + 1
	if ( lin == MSG_LIN1 )
	    {
	    lin = olin
	    col = 44
	    }
	}

    l = more( "" )

    return

end


###  doomdisplay - watch the doomsday machine
#
#  SYNOPSIS
#    call doomdisplay
#
subroutine doomdisplay
NOIMPLICIT

    integer i, lin, col, dcol
    character buf(MSGMAXLINE)
    include "conqcom"

    call cdclear

    lin = 1
    col = 1
    call strcpy( dname, buf )
    if ( dtype != 0 )
	{
	call appchr( '(', buf )
	call appint( dtype, buf )
	call appchr( ')', buf )
	}
    call cdputc( buf, 1 )

    lin = lin + 2
    dcol = col + 11
    call cdputs( "  dstatus:", lin, col )
    call cdputn( dstatus, 0, lin, dcol )
    lin = lin + 1
    call cdputs( "       dx:", lin, col )
    call cdputr( oneplace(dx), 0, lin, dcol )
    lin = lin + 1
    call cdputs( "       dy:", lin, col )
    call cdputr( oneplace(dy), 0, lin, dcol )
    lin = lin + 1
    call cdputs( "      ddx:", lin, col )
    call cdputr( oneplace(ddx), 0, lin, dcol )
    lin = lin + 1
    call cdputs( "      ddy:", lin, col )
    call cdputr( oneplace(ddy), 0, lin, dcol )
    lin = lin + 1
    call cdputs( "    dhead:", lin, col )
    call cdputn( round(dhead), 0, lin, dcol )
    lin = lin + 1
    call cdputs( "    dlock:", lin, col )
    call cdputs( "      ddt:", lin+1, col )
    i = dlock
    if ( -i >= 1 & -i <= NUMPLANETS )
	{
        call cdputs( pname(1,-i), lin, dcol )
	call cdputn( round( dist( dx, dy, px(-i), py(-i) ) ),
	    0, lin + 1, dcol )
	}
    else if ( i >= 1 & i <= MAXSHIPS )
	{
	buf(1) = EOS
	call appship( i, buf )
	call cdputs( buf, lin, dcol )
	call cdputn( round( dist( dx, dy, sx(i), sy(i) ) ),
	    0, lin + 1, dcol )
	}
    else
        call cdputn( i, 0, lin + 1, dcol )

    lin = lin + 2

    call cdmove( 1, 1 )

    return

end


###  gplanmatch - GOD's check if a string matches a planet name
#
#  SYNOPSIS
#    integer gplanmatch, pnum
#    character str()
#    logical status
#    status = gplanmatch( str, pnum )
#
logical function gplanmatch( str, pnum )
NOIMPLICIT
character str(ARB)
integer pnum

    integer i, alldig
    logical planmatch, safectoi

    if ( alldig( str ) == YES )
	{
	i = 1
	if ( ! safectoi( pnum, str, i ) )
	    return ( .false. )
	if ( pnum < 1 | pnum > NUMPLANETS )
	    return ( .false. )
	}
    else
	return ( planmatch( str, pnum, .true. ) )

    return ( .true. )

end


###  kiss - give the kiss of death
#
#  SYNOPSIS
#    call kiss
#
subroutine kiss
NOIMPLICIT

    integer i, snum, unum, alldig
    character ch, cdgetx, buf(MSGMAXLINE)
    logical l, didany, confirm, gunum, safectoi, stmatch
    include "conqcom"

    # Find out what to kill.
    call cdclrl( MSG_LIN1, 2 )
    ch = cdgetx( "Kill what (<cr> for driver)? ", MSG_LIN1, 1,
	TERMS, buf, MSGMAXLINE )
    if ( ch == TERM_ABORT )
	{
	call cdclrl( MSG_LIN1, 1 )
	call cdmove( 1, 1 )
	return
	}
    call delblanks( buf )

    # Kill the driver?
    if ( buf(1) == EOS )
	{
	if ( confirm( 0 ) )
	    if ( drivstat == DRS_RUNNING )
		drivstat = DRS_KAMIKAZE
	call cdclrl( MSG_LIN1, 2 )
	call cdmove( 1, 1 )
	return
	}

    # Kill a ship?
    if ( alldig( buf ) == YES )
	{
	i = 1
	l = safectoi( snum, buf, i )		# ignore status
	if ( snum <= 0 | snum > MAXSHIPS )
	    call cdputs( "No such ship.", MSG_LIN2, 1 )
	else if ( sstatus(snum) != SS_LIVE )
	    call cdputs( "You can't kill that ship.", MSG_LIN2, 1 )
	else if ( confirm( 0 ) )
	    {
	    call kill( snum, KB_GOD )
	    call cdclrl( MSG_LIN1, 2 )
	    }
	call cdmove( 1, 1 )
	return
	}

    # Kill EVERYBODY?
    if ( stmatch( buf, "all", .false. ) )
	{
	didany = .false.
	for ( snum = 1; snum <= MAXSHIPS; snum = snum + 1 )
	    if ( sstatus(snum) == SS_LIVE )
		{
		didany = .true.
		call cdclrl( MSG_LIN1, 1 )
		call strcpy( "Kill ship ", buf )
		call appship( snum, buf )
		call cdputs( buf, MSG_LIN1, 1 )
		if ( confirm( 0 ) )
		    call kill( snum, KB_GOD )
		}
	if ( didany )
	    call cdclrl( MSG_LIN1, 2 )
	else
	    call cdputs( "Nobody here but us GODs.", MSG_LIN2, 1 )
	call cdmove( 1, 1 )
	return
	}

    # Kill a user?
    if ( ! gunum( unum, buf ) )
	{
	call cdputs( "No such user.", MSG_LIN2, 1 )
	call cdmove( 1, 1 )
	return
	}

    # Yes.
    didany = .false.
    for ( snum = 1; snum <= MAXSHIPS; snum = snum + 1 )
	if ( sstatus(snum) == SS_LIVE )
	    if ( suser(snum) == unum )
		{
		didany = .true.
		call cdclrl( MSG_LIN1, 1 )
		call strcpy( "Kill ship ", buf )
		call appship( snum, buf )
		call cdputs( buf, MSG_LIN1, 1 )
		if ( confirm( 0 ) )
		    call kill( snum, KB_GOD )
		}
    if ( ! didany )
	call cdputs( "That user isn't flying right now.", MSG_LIN2, 1 )
    else
	call cdclrl( MSG_LIN1, 2 )

    return

end


###  opback - put up the background for the operator program
#
#  SYNOPSIS
#    integer lastrev, savelin
#    call opback( lastrev, savelin )
#
subroutine opback( lastrev, savelin )
NOIMPLICIT
integer lastrev, savelin

    integer i, lin, col
    include "conqcom2"

    call cdclear

    lin = 2
    if ( lastrev == COMMONSTAMP )
	call cdputc( "CONQUEST OPERATOR PROGRAM", lin )
    else
	{
	call prints( cbuf, "CONQUEST COMMON BLOCK MISMATCH %d != %d",
	    lastrev, COMMONSTAMP )
	call cdputc( cbuf, lin )
	}

    lin = lin + 2
    savelin = lin
    lin = lin + 3

    call cdputc( "Options:", lin )
    lin = lin + 2
    i = lin

    col = 5
    call cdputs( "(f) - flip the open/closed flag", lin, col )
    lin = lin + 1
    call cdputs( "(d) - flip the doomsday machine!", lin, col )
    lin = lin + 1
    call cdputs( "(h) - hold the driver", lin, col )
    lin = lin + 1
    call cdputs( "(I) - initialize", lin, col )
    lin = lin + 1
    call cdputs( "(b) - big bang", lin, col )
    lin = lin + 1
    call cdputs( "(H) - user history", lin, col )
    lin = lin + 1
    call cdputs( "(/) - player list", lin, col )
    lin = lin + 1
    call cdputs( "(\) - full player list", lin, col )
    lin = lin + 1
    call cdputs( "(?) - planet list", lin, col )
    lin = lin + 1
    call cdputs( "($) - debugging planet list", lin, col )
    lin = lin + 1
    call cdputs( "(p) - edit a planet", lin, col )
    lin = lin + 1
    call cdputs( "(w) - watch a ship", lin, col )
    lin = lin + 1
    call cdputs( "(i) - info", lin, col )

    lin = i
    col = 45
    call cdputs( "(r) - create a robot ship", lin, col )
    lin = lin + 1
    call cdputs( "(L) - review messages", lin, col )
    lin = lin + 1
    call cdputs( "(m) - message from GOD", lin, col )
    lin = lin + 1
    call cdputs( "(T) - team stats", lin, col )
    lin = lin + 1
    call cdputs( "(U) - user stats", lin, col )
    lin = lin + 1
    call cdputs( "(S) - more user stats", lin, col )
    lin = lin + 1
    call cdputs( "(s) - special stats page", lin, col )
    lin = lin + 1
    call cdputs( "(a) - add a user", lin, col )
    lin = lin + 1
    call cdputs( "(e) - edit a user", lin, col )
    lin = lin + 1
    call cdputs( "(R) - resign a user", lin, col )
    lin = lin + 1
    call cdputs( "(k) - kiss of death", lin, col )
    lin = lin + 1
    call cdputs( "(q) - exit", lin, col )

    return

end


###  operate - main part of the conquest operator program
#
#  SYNOPSIS
#    call operate
#
subroutine operate
NOIMPLICIT

    integer i, lin, modp1, savelin, cntlockword, cntlockmesg
    integer lastrev, msgrand, now, dgrand
    character ch, buf(MSGMAXLINE), junk(MSGMAXLINE)
    logical l, redraw, iochav, iogtimed, gunum, confirm
    logical getamsg, readone, review
    include "conqcom"

    glastmsg = lastmsg
    call glname( buf )
    if ( gunum( i, buf ) )
	uooption(i,MAXOOPTIONS) = .true.
    cntlockword = 0
    cntlockmesg = 0
    lastrev = commonrev
    call grand( msgrand )

    redraw = .true.
    repeat
	{
	if ( redraw | lastrev != commonrev )
	    {
	    lastrev = commonrev
	    call opback( lastrev, savelin )
	    redraw = .false.
	    }
	# Line 1.
	call strcpy( "game ", buf )
	if ( closed )
	    call appstr( "CLOSED", buf )
	else
	    call appstr( "open", buf )
	call appstr( ", driver ", buf )
	switch ( drivstat )
	    {
	    case DRS_OFF:
		call appstr( "OFF", buf )
	    case DRS_RESTART:
		call appstr( "RESTART", buf )
	    case DRS_STARTING:
		call appstr( "STARTING", buf )
	    case DRS_HOLDING:
		call appstr( "HOLDING", buf )
	    case DRS_RUNNING:
		call appstr( "on", buf )
	    case DRS_KAMIKAZE:
		call appstr( "KAMIKAZE", buf )
	    default:
		call appstr( "???", buf )
	    }
	call appstr( ", eater ", buf )
	i = dstatus
	if ( i == DS_OFF )
	    call appstr( "off", buf )
	else if ( i == DS_LIVE )
	    {
	    call appstr( "ON (", buf )
	    i = dlock
	    if ( -i >= 1 & -i <= NUMPLANETS )
		call appstr( pname(1,-i), buf )
	    else
		call appship( i, buf )		# this will handle funny numbers
	    call appchr( ')', buf )
	    }
	else
	    call appstr( "???", buf )
	lin = savelin
	call cdclrl( lin, 1 )
	call cdputc( buf, lin )

	# Line 2.
	call strcpy( "lockword", buf )
	if ( lockword == 0 )
	    cntlockword = 0
	else if ( cntlockword < 5 )
	    cntlockword = cntlockword + 1
	else
	    call upper( buf )
	call strcpy( buf, junk )
	call appstr( " = %d, ", junk )

	call strcpy( "lockmesg", buf )
	if ( lockmesg == 0 )
	    cntlockmesg = 0
	else if ( cntlockmesg < 5 )
	    cntlockmesg = cntlockmesg + 1
	else
	    call upper( buf )
	call appstr( buf, junk )
	call appstr( " = %d", junk )

	call prints( buf, junk, lockword, lockmesg )
	lin = lin + 1
	call cdclrl( lin, 1 )
	call cdputc( buf, lin )

	# Display a new message, if any.
	readone = .false.
	if ( dgrand( msgrand, now ) >= NEWMSG_GRAND )
	    if ( getamsg( MSG_GOD, glastmsg ) )
		{
		call readmsg( MSG_GOD, glastmsg )
		readone = .true.
		msgrand = now
		}
	call cdmove( 1, 1 )
	call cdplay( .true. )
	# Un-read message, if there's a chance it got garbaged.
	if ( readone )
	    if ( iochav( 0 ) )
		glastmsg = modp1( glastmsg - 1, MAXMESSAGES )

	# Get a character with timeout.
	if ( ! iogtimed( ch, 1 ) )
	    next
	switch ( ch )
	    {
	    case 'a':
		call opuadd
		redraw = .true.
	    case 'b':
		if ( confirm( 0 ) )
		    call bigbang
	    case 'd':
		if ( dstatus == DS_LIVE )
		    dstatus = DS_OFF
		else
		    call doomsday
	    case 'e':
		call opuedit
		redraw = .true.
	    case 'f':
		if ( closed )
		    {
		    closed = .false.
		    # Unlock the lockwords (just in case...)
		    PVUNLOCK(lockword)
		    PVUNLOCK(lockmesg)
		    drivstat = DRS_OFF
		    drivpid = 0
		    drivowner(1) = EOS
		    }
		else if ( confirm( 0 ) )
		    closed = .true.
	    case 'h':
		if ( drivstat == DRS_HOLDING )
		    drivstat = DRS_RUNNING
		else
		    drivstat = DRS_HOLDING
	    case 'H':
		call histlist( .true. )
		redraw = .true.
	    case 'i':
		call opinfo( MSG_GOD )
	    case 'I':
		call opinit
		redraw = .true.
	    case 'k':
		call kiss
	    case 'L':
		l = review( MSG_GOD, glastmsg )
	    case 'm':
		call sendmsg( MSG_GOD, .true. )
	    case 'p':
		call oppedit
		redraw = .true.
	    case 'q', 'Q':
		break
	    case 'r':
		call oprobot
	    case 'R':
		call opresign
		redraw = .true.
	    case 's':
		call opstats
		redraw = .true.
	    case 'S':
		call userstats( .true. )
		redraw = .true.
	    case 'T':
		call opteamlist
		redraw = .true.
	    case 'U':
		call userlist( .true. )
		redraw = .true.
	    case 'w':
		call watch
		redraw = .true.
	    case '/':
		call playlist( .true., .false. )
		redraw = .true.
	    case '\':
		call playlist( .true., .true. )
		redraw = .true.
	    case '?':
		call opplanlist
		redraw = .true.
	    case '$':
		call debugplan
		redraw = .true.
	    case '@^L':
		call cdredo
	    case ' ', TERM_NORMAL, EOS:
		# do nothing
	    default:
		call scbeep
	    }
	# Disable messages for awhile.
	call grand( msgrand )
	}

    return

end


###  opinfo - do an operator info command
#
#  SYNOPSIS
#    integer snum
#    call opinfo( snum )
#
subroutine opinfo( snum )
NOIMPLICIT
integer snum

    integer i, j, now(7), alldig
    character ch, cdgetx
    logical l, extra, gplanmatch, safectoi, stmatch
    string pmt "Information on: "
    string huh "I don't understand."
    string nf "Not found."
    include "conqcom2"

    call cdclrl( MSG_LIN1, 2 )

    ch = cdgetx( pmt, MSG_LIN1, 1, TERMS, cbuf, MSGMAXLINE )
    if ( ch == TERM_ABORT )
	{
	call cdclrl( MSG_LIN1, 1 )
	return
	}
    extra = ( ch == TERM_EXTRA )

    call delblanks( cbuf )
    call fold( cbuf )
    if ( cbuf(1) == EOS )
	{
	call cdclrl( MSG_LIN1, 1 )
	return
	}

    if ( cbuf(1) == 's' & alldig( cbuf(2) ) == YES )
	{
	i = 2
	l = safectoi( j, cbuf, i )		# ignore status
	call infoship( j, snum )
	}
    else if ( alldig( cbuf ) == YES )
	{
	i = 1
	l = safectoi( j, cbuf, i )		# ignore status
	call infoship( j, snum )
	}
    else if ( gplanmatch( cbuf, j ) )
	call infoplanet( "", j, snum )
    else if ( stmatch( cbuf, "time", .false. ) )
	{
	call getnow( now )
	call strcpy( "It's ", cbuf )
	call appnumtim( now, cbuf )
	call appchr( '.', cbuf )
	call putmsg( cbuf, MSG_LIN1 )
	call cdmove( MSG_LIN1, 1 )
	}
    else
	{
	call cdmove( MSG_LIN2, 1 )
	call putmsg( huh, MSG_LIN2 )
	}

    return

end


###  opinit - handle the various kinds of initialization
#
#  SYNOPSIS
#    call opinit
#
subroutine opinit
NOIMPLICIT

    integer i, lin, col, icol, length
    character ch, cdgetx, buf(MSGMAXLINE)
    logical confirm
    string pmt "Initialize what: "
    include "conqcom"

    call cdclear
    call cdredo

    lin = 2
    icol = 11
    call cdputc( "Conquest Initialization", lin )

    lin = lin + 3
    col = icol - 2
    call strcpy( "(r)obots", buf )
    i = length( buf )
    call cdputs( buf, lin, col+1 )
    call cdbox( lin-1, col, lin+1, col+i+1 )
    col = col + i + 4
    call cdputs( "<-", lin, col )
    col = col + 4
    call strcpy( "(e)verything", buf )
    i = length( buf )
    call cdputs( buf, lin, col+1 )
    call cdbox( lin-1, col, lin+1, col+i+1 )
    col = col + i + 4
    call cdputs( "->", lin, col )
    col = col + 4
    call strcpy( "(z)ero everything", buf )
    i = length( buf )
    call cdputs( buf, lin, col+1 )
    call cdbox( lin-1, col, lin+1, col+i+1 )

    col = icol + 20
    lin = lin + 3
    call cdput( '|', lin, col )
    lin = lin + 1
    call cdput( 'v', lin, col )
    lin = lin + 3

    col = icol
    call strcpy( "(s)hips", buf )
    i = length( buf )
    call cdputs( buf, lin, col+1 )
    call cdbox( lin-1, col, lin+1, col+i+1 )
    col = col + i + 4
    call cdputs( "<-", lin, col )
    col = col + 4
    call strcpy( "(u)niverse", buf )
    i = length( buf )
    call cdputs( buf, lin, col+1 )
    call cdbox( lin-1, col, lin+1, col+i+1 )
    col = col + i + 4
    call cdputs( "->", lin, col )
    col = col + 4
    call strcpy( "(g)ame", buf )
    i = length( buf )
    call cdputs( buf, lin, col+1 )
    call cdbox( lin-1, col, lin+1, col+i+1 )
    col = col + i + 4
    call cdputs( "->", lin, col )
    col = col + 4
    call strcpy( "(p)lanets", buf )
    i = length( buf )
    call cdputs( buf, lin, col+1 )
    call cdbox( lin-1, col, lin+1, col+i+1 )

    col = icol + 20
    lin = lin + 3
    call cdput( '|', lin, col )
    lin = lin + 1
    call cdput( 'v', lin, col )
    lin = lin + 3

    col = icol + 15
    call strcpy( "(m)essages", buf )
    i = length( buf )
    call cdputs( buf, lin, col+1 )
    call cdbox( lin-1, col, lin+1, col+i+1 )
    col = col + i + 8
    call strcpy( "(l)ockwords", buf )
    i = length( buf )
    call cdputs( buf, lin, col+1 )
    call cdbox( lin-1, col, lin+1, col+i+1 )

    repeat
	{
	lin = MSG_LIN1
	col = 30
	call cdclrl( lin, 1 )
	ch = cdgetx( pmt, lin, col, TERMS, buf, MSGMAXLINE )
	call cdclrl( lin, 1 )
	call cdputs( pmt, lin, col )
	col = col + length( pmt )
	if ( ch == TERM_ABORT | buf(1) == EOS )
	    break
	switch ( buf(1) )
	    {
	    case 'e':
		call cdputs( "everything", lin, col )
		if ( confirm( 0 ) )
		    {
		    call initeverything
		    commonrev = COMMONSTAMP
		    }
	    case 'z':
		call cdputs( "zero everything", lin, col )
		if ( confirm( 0 ) )
		    call zeroeverything
	    case 'u':
		call cdputs( "universe", lin, col )
		if ( confirm( 0 ) )
		    {
		    call inituniverse
		    commonrev = COMMONSTAMP
		    }
	    case 'g':
		call cdputs( "game", lin, col )
		if ( confirm( 0 ) )
		    {
		    call initgame
		    commonrev = COMMONSTAMP
		    }
	    case 'p':
		call cdputs( "planets", lin, col )
		if ( confirm( 0 ) )
		    {
		    call initplanets
		    commonrev = COMMONSTAMP
		    }
	    case 's':
		call cdputs( "ships", lin, col )
		if ( confirm( 0 ) )
		    {
		    call clearships
		    commonrev = COMMONSTAMP
		    }
	    case 'm':
		call cdputs( "messages", lin, col )
		if ( confirm( 0 ) )
		    {
		    call initmsgs
		    commonrev = COMMONSTAMP
		    }
	    case 'l':
		call cdputs( "lockwords", lin, col )
		if ( confirm( 0 ) )
		    {
		    PVUNLOCK(lockword)
		    PVUNLOCK(lockmesg)
		    commonrev = COMMONSTAMP
		    }
	    case 'r':
		call cdputs( "robots", lin, col )
		if ( confirm( 0 ) )
		    {
		    call initrobots
		    commonrev = COMMONSTAMP
		    }
	    default:
		call scbeep
	    }
	}

    return

end


###  oppedit - edit a planet's characteristics
#
#  SYNOPSIS
#    call oppedit
#
subroutine oppedit
NOIMPLICIT

    integer i, j, pnum, lin, col, datacol, modp1
    real x, ctor
    logical gplanmatch, safectoi, iogtimed
    character ch, getcx, buf(MSGMAXLINE)
    include "conqcom"
    data pnum / PNUM_JANUS /
    common / conqopp / pnum

    col = 4
    datacol = col + 28
    repeat
	{
	call cdclear

	# Display the planet.
	i = 10
	j = 63
	call puthing(ptype(pnum), i, j )
	call cdput( chrplanets(ptype(pnum)), i, j )
	call cdputs( pname(1,pnum), i + 1, j + 2 )

	# Display info about the planet.
	lin = 4
	i = pnum
	call cdputs( "(p) - Planet:", lin, col )
	call prints( buf, "%s (%d)", pname(1,i), i )
	call cdputs( buf, lin, datacol )

	lin = lin + 1
	i = pprimary(pnum)
	if ( i == 0 )
	    {
	    lin = lin + 1

	    x = porbvel(pnum)
	    if ( x == 0.0 )
		call cdputs( "(v) - Stationary", lin, col )
	    else
		{
		call cdputs( "(v) - Velocity:", lin, col )
		call prints( buf, "Warp %g", oneplace(x) )
		call cdputs( buf, lin, datacol )
		}
	    }
	else
	    {
	    call cdputs( "(o) - Orbiting:", lin, col )
	    call prints( buf, "%s (%d)", pname(1,i), i )
	    call cdputs( buf, lin, datacol )

	    lin = lin + 1
	    call cdputs( "(v) - Orbit velocity:", lin, col )
	    call prints( buf, "%g degrees/minute", oneplace(porbvel(pnum)) )
	    call cdputs( buf, lin, datacol )
	    }

	lin = lin + 1
	call cdputs( "(r) - Radius:", lin, col )
	call prints( buf, "%g", oneplace(porbrad(pnum)) )
	call cdputs( buf, lin, datacol )

	lin = lin + 1
	call cdputs( "(a) - Angle:", lin, col )
	call prints( buf, "%g", oneplace(porbang(pnum)) )
	call cdputs( buf, lin, datacol )

	lin = lin + 1
	i = ptype(pnum)
	call cdputs( "(t) - Type:", lin, col )
	call prints( buf, "%s (%d)", ptname(1,i), i )
	call cdputs( buf, lin, datacol )

	lin = lin + 1
	i = pteam(pnum)
	if ( pnum <= NUMCONPLANETS )
	    call cdputs( "      Owner team:", lin, col )
	else
	    call cdputs( "(T) - Owner team:", lin, col )
	call prints( buf, "%s (%d)", tname(1,i), i )
	call cdputs( buf, lin, datacol )

	lin = lin + 1
	call cdputs( "(x,y) Position:", lin, col )
	call prints( buf, "%g, %g",oneplace(px(pnum)), oneplace(py(pnum)) )
	call cdputs( buf, lin, datacol )

	lin = lin + 1
	call cdputs( "      Armies:", lin, col )
	call prints( buf, "%d", parmies(pnum) )
	call cdputs( buf, lin, datacol )

	lin = lin + 1
	call cdputs( "      Scanned by:", lin, col )
	buf(1) = '('
	for ( i = 1; i <= NUMTEAMS; i = i + 1 )
	    if ( pscanned(pnum,i) )
		buf(i+1) = chrteams(i)
	    else
		buf(i+1) = '-'
	buf(NUMTEAMS+2) = ')'
	buf(NUMTEAMS+3) = EOS
	call cdputs( buf, lin, datacol )

	lin = lin + 1
	call cdputs( "      Uninhabitable time:", lin, col )
	call prints( buf, "%d", puninhabtime(pnum) )
	call cdputs( buf, lin, datacol )

	lin = lin + 1
	if ( preal(pnum) )
	    call cdputs( "(-) - Visible", lin, col )
	else
	    call cdputs( "(+) - Hidden", lin, col )

	lin = lin + 1
	call cdputs( "(n) - Change planet name", lin, col )

	lin = lin + 1
	call cdputs( "<LF>  increment planet number", lin, col )

	call cdmove( 1, 1 )
	call cdplay( .true. )

	if ( ! iogtimed( ch, 1 ) )
	    next
	switch ( ch )
	    {
	    case 'a':
		# Angle.
		ch = getcx( "New angle? ", MSG_LIN1, 0,
		    TERMS, buf, MSGMAXLINE )
		if ( ch == TERM_ABORT | buf(1) == EOS )
		    next
		call delblanks( buf )
		i = 1
		if ( ! safectoi( j, buf, i ) )
		    next
		i = 1
		x = ctor( buf, i )
		if ( x < 0.0 | x > 360.0 )
		    next
		porbang(pnum) = x
#		case 'A':
#		    # Armies.
#		    ch = getcx( "New number of armies? ",
#			MSG_LIN1, 0, TERMS, buf, MSGMAXLINE )
#		    if ( ch == TERM_ABORT | buf(1) == EOS )
#			next
#		    call delblanks( buf )
#		    i = 1
#		    if ( ! safectoi( j, buf, i ) )
#			next
#		    parmies(pnum) = j
	    case 'n':
		# New planet name.
		ch = getcx( "New name for this planet? ",
		    MSG_LIN1, 0, TERMS, buf, MAXPLANETNAME )
		if ( ch != TERM_ABORT & ( ch == TERM_EXTRA | buf(1) != EOS ) )
		    call stcpn( buf, pname(1,pnum), MAXPLANETNAME )
	    case 'o':
		# New primary.
		ch = getcx( "New planet to orbit? ",
		    MSG_LIN1, 0, TERMS, buf, MAXPLANETNAME )
		if ( ch == TERM_ABORT | buf(1) == EOS )
		    next
		if ( buf(1) == '0' & buf(2) == EOS )
		    pprimary(pnum) = 0
		else if ( gplanmatch( buf, i ) )
		    pprimary(pnum) = i
	    case 'v':
		# Velocity.
		ch = getcx( "New velocity? ",
		    MSG_LIN1, 0, TERMS, buf, MSGMAXLINE )
		if ( ch == TERM_ABORT | buf(1) == EOS )
		    next
		call delblanks( buf )
		i = 1
		if ( ! safectoi( j, buf, i ) )
		    next
		i = 1
		porbvel(pnum) = ctor( buf, i )
	    case 'T':
		# Rotate owner team.
		if ( pnum > NUMCONPLANETS )
		    {
		    repeat
			pteam(pnum) = modp1( pteam(pnum) + 1, NUMALLTEAMS )
		    until ( pteam(pnum) > NUMTEAMS )
		    }
		else
		    call scbeep
	    case 't':
		# Rotate planet type.
		ptype(pnum) = modp1( ptype(pnum) + 1, MAXPLANETTYPES )
	    case 'x':
		# X coordinate.
		ch = getcx( "New X coordinate? ",
		    MSG_LIN1, 0, TERMS, buf, MSGMAXLINE )
		if ( ch == TERM_ABORT | buf(1) == EOS )
		    next
		call delblanks( buf )
		i = 1
		if ( ! safectoi( j, buf, i ) )
		    next
		i = 1
		px(pnum) = ctor( buf, i )
	    case 'y':
		# Y coordinate.
		ch = getcx( "New Y coordinate? ",
		    MSG_LIN1, 0, TERMS, buf, MSGMAXLINE )
		if ( ch == TERM_ABORT | buf(1) == EOS )
		    next
		call delblanks( buf )
		i = 1
		if ( ! safectoi( j, buf, i ) )
		    next
		i = 1
		py(pnum) = ctor( buf, i )
#	    case 's':
#		# Scanned.
#		call cdputs( "Toggle which team? ", MSG_LIN1, 1 )
#		call cdmove( MSG_LIN1, 1 )
#		call cdplay( .true. )
#		ch = cupper( iogchar( ch ) )
#		for ( i = 1; i <= NUMTEAMS; i = i + 1 )
#		    if ( ch == chrteams(i) )
#			{
#			pscanned(pnum,i) = ! pscanned(pnum,i)
#			break
#			}
#	    case 'u':
#		# Uninhabitable minutes
#		ch = getcx( "New uninhabitable minutes? ",
#		    MSG_LIN1, 0, TERMS, buf, MSGMAXLINE )
#		if ( ch == TERM_ABORT | buf(1) == EOS )
#		    next
#		call delblanks( buf )
#		i = 1
#		if ( ! safectoi( j, buf, i ) )
#		    next
#		puninhabtime(pnum) = j
	    case 'p':
		ch = getcx( "New planet to edit? ",
		    MSG_LIN1, 0, TERMS, buf, MAXPLANETNAME )
		if ( ch == TERM_ABORT | buf(1) == EOS )
		    next
		if ( gplanmatch( buf, i ) )
		    pnum = i
	    case 'r':
		# Radius.
		ch = getcx( "New radius? ",
		    MSG_LIN1, 0, TERMS, buf, MSGMAXLINE )
		if ( ch == TERM_ABORT | buf(1) == EOS )
		    next
		call delblanks( buf )
		i = 1
		if ( ! safectoi( j, buf, i ) )
		    next
		i = 1
		porbrad(pnum) = ctor( buf, i )
	    case '+':
		# Now you see it...
		preal(pnum) = .true.
	    case '-':
		# Now you don't
		preal(pnum) = .false.
	    case TERM_EXTRA:
		# Rotate planet number.
		pnum = modp1( pnum + 1, NUMPLANETS )
	    case ' ':
		# do no-thing
	    case '@^L':
		call cdredo
	    case TERM_NORMAL, TERM_ABORT, 'q', 'Q':
		return
	    default:
		call scbeep
	    }
	}

    return

end


###  opplanlist - display the planet list for an operator
#
#  SYNOPSIS
#    call opplanlist
#
subroutine opplanlist
NOIMPLICIT

    character ch
    logical iogtimed

    call cdclear
    repeat
	{
	call planlist( TEAM_NOTEAM )		# we get extra info
	call putpmt( "--- press space when done ---", MSG_LIN2 )
	call cdplay( .true. )
	}
    until ( iogtimed( ch, 1 ) )

end


###  opresign - resign a user
#
#  SYNOPSIS
#    call opresign
#
subroutine opresign
NOIMPLICIT

    integer unum
    logical confirm, gunum
    character ch, cdgetx, buf(MSGMAXLINE)

    call cdclrl( MSG_LIN1, 2 )
    ch = cdgetx( "Resign user: ", MSG_LIN1, 1, TERMS, buf, MSGMAXLINE )
    if ( ch == TERM_ABORT )
	{
	call cdclrl( MSG_LIN1, 1 )
	return
	}
    call delblanks( buf )
    if ( ! gunum( unum, buf ) )
	{
	call cdputs( "No such user.", MSG_LIN2, 1 )
	call cdmove( 1, 1 )
	call cdplay( .false. )
	call sleep( 1.0 )
	}
    else if ( confirm( 0 ) )
	call resign( unum )
    call cdclrl( MSG_LIN1, 2 )

    return

end


###  oprobot - handle gratuitous robot creation
#
#  SYNOPSIS
#    call oprobot
#
subroutine oprobot
NOIMPLICIT

    integer i, j, snum, unum, num, anum
    character ch, cdgetx, buf(MSGMAXLINE)
    logical l, warlike, gunum, newrob, safectoi
    include "conqcom"

    call cdclrl( MSG_LIN1, 2 )
    ch = cdgetx( "Enter username for new robot: ",
	MSG_LIN1, 1, TERMS, buf, MAXUSERNAME )
    if ( ch == TERM_ABORT | buf(1) == EOS )
	{
	call cdclrl( MSG_LIN1, 1 )
	return
	}
    call delblanks( buf )
    if ( ! gunum( unum, buf ) )
	{
	call cdputs( "No such user.", MSG_LIN2, 1 )
	return
	}

    # Defaults.
    num = 1
    warlike = .false.

    if ( ch == TERM_EXTRA )
	{
	ch = cdgetx( "Enter number desired (LF for warlike): ",
	    MSG_LIN2, 1, TERMS, buf, MAXUSERNAME )
	if ( ch == TERM_ABORT )
	    {
	    call cdclrl( MSG_LIN1, 2 )
	    return
	    }
	warlike = ( ch == TERM_EXTRA )
	call delblanks( buf )
	i = 1
	l = safectoi( num, buf, i )
	if ( num <= 0 )
	    num = 1
	}

    anum = 0
    for ( i = 1; i <= num; i = i + 1 )
	{
	if ( ! newrob( snum, unum ) )
	    {
	    call putmsg( "Failed to create robot ship.", MSG_LIN1 )
	    break
	    }

	anum = anum + 1

	# If requested, make the robot war-like.
	if ( warlike )
	    for ( j = 1; j <= NUMTEAMS; j = j + 1 )
		swar(snum,j) = .true.
	}

    # Report the good news.
    call prints( buf, "Automation %s (%s) is now flying ",
	upname(1,unum), uname(1,unum) )
    if ( anum == 1 )
	call appship( snum, buf )
    else
	{
	call appint( anum, buf )
	call appstr( " new ships.", buf )
	}
    call cdclrl( MSG_LIN2, 1 )
    i = MSG_LIN1
    if ( anum != num )
	i = i + 1
    call putmsg( buf, i )

    return

end


###  opstats - display operator statistics
#
#  SYNOPSIS
#    call opstats
#
subroutine opstats
NOIMPLICIT

    integer i, size, lin, col
    character ch, buf(MSGMAXLINE), junk(MSGMAXLINE), timbuf(32)
    logical iogtimed
    real x
    string sfmt "%32s %12s"
    string tfmt "%32s %20s"
    string pfmt "%32s %11g%%"
    include "conqcom"

    col = 8
    repeat
	{
	call cdclear

	lin = 2
	call fmtseconds( ccpuseconds, timbuf )
	call prints( buf, sfmt, "Conquest cpu time:", timbuf )
	call cdputs( buf, lin, col )

	lin = lin + 1
	i = celapsedseconds
	call fmtseconds( i, timbuf )
	call prints( buf, sfmt, "Conquest elapsed time:", timbuf )
	call cdputs( buf, lin, col )

	lin = lin + 1
	if ( i == 0 )
	    x = 0.0
	else
	    x = oneplace( 100.0 * float(ccpuseconds) / float(i) )
	call prints( buf, pfmt, "Conquest cpu usage:", x )
	call cdputs( buf, lin, col )

	lin = lin + 2
	call fmtseconds( dcpuseconds, timbuf )
	call prints( buf, sfmt, "Conqdriv cpu time:", timbuf )
	call cdputs( buf, lin, col )

	lin = lin + 1
	i = delapsedseconds
	call fmtseconds( i, timbuf )
	call prints( buf, sfmt, "Conqdriv elapsed time:", timbuf )
	call cdputs( buf, lin, col )

	lin = lin + 1
	if ( i == 0 )
	    x = 0.0
	else
	    x = oneplace( 100.0 * float(dcpuseconds) / float(i) )
	call prints( buf, pfmt, "Conqdriv cpu usage:", x )
	call cdputs( buf, lin, col )

	lin = lin + 2
	call fmtseconds( rcpuseconds, timbuf )
	call prints( buf, sfmt, "Robot cpu time:", timbuf )
	call cdputs( buf, lin, col )

	lin = lin + 1
	i = relapsedseconds
	call fmtseconds( i, timbuf )
	call prints( buf, sfmt, "Robot elapsed time:", timbuf )
	call cdputs( buf, lin, col )

	lin = lin + 1
	if ( i == 0 )
	    x = 0.0
	else
	    x = oneplace( 100.0 * float(rcpuseconds) / float(i) )
	call prints( buf, pfmt, "Robot cpu usage:", x )
	call cdputs( buf, lin, col )

	lin = lin + 2
	call prints( buf, tfmt, "Last initialize:", inittime )
	call cdputs( buf, lin, col )

	lin = lin + 1
	call prints( buf, tfmt, "Last conquer:", conqtime )
	call cdputs( buf, lin, col )

	lin = lin + 1
	call fmtseconds( playtime, timbuf )
	call prints( buf, sfmt, "Driver time:", timbuf )
	call cdputs( buf, lin, col )

	lin = lin + 1
	call fmtseconds( drivtime, timbuf )
	call prints( buf, sfmt, "Play time:", timbuf )
	call cdputs( buf, lin, col )

	lin = lin + 1
	call prints( buf, tfmt, "Last upchuck:", lastupchuck )
	call cdputs( buf, lin, col )

	lin = lin + 1
	call getdandt( timbuf )
	call prints( buf, tfmt, "Current time:", timbuf )
	call cdputs( buf, lin, col )

	lin = lin + 2
	if ( drivowner(1) != EOS )
	    call prints( junk, "%x (%s), ", drivpid, drivowner )
	else if ( drivpid != 0 )
	    call prints( junk, "%x, ", drivpid )
	else
	    junk(1) = EOS
	call prints( buf, "%sdrivsecs = %03d, drivcnt = %d",
	    junk, drivsecs, drivcnt )
	call cdputs( buf, lin, col )

	lin = lin + 1
	call comsize( size )
	call prints( buf, "%d bytes (out of %d) in the common block.",
	    size, SIZEOF_COMMONBLOCK )
	call cdputs( buf, lin, col )

	lin = lin + 1
	call prints( buf, "Common ident is %d", commonrev )
	if ( commonrev != COMMONSTAMP )
	    {
	    call prints( junk, " (binary ident is %d)", COMMONSTAMP )
	    call appstr( junk, buf )
	    }
	call cdputs( buf, lin, col )

	call cdmove( 1, 1 )
	call cdplay( .true. )
	}
    until ( iogtimed( ch, 1 ) )

    return

end


###  opteamlist - display the team list for an operator
#
#  SYNOPSIS
#    call opteamlist
#
subroutine opteamlist
NOIMPLICIT

    character ch
    logical iogtimed

    call cdclear
    repeat
	{
	call teamlist( -1 )
	call putpmt( "--- press space when done ---", MSG_LIN2 )
	call cdplay( .true. )
	}
    until ( iogtimed( ch, 1 ) )

end


###  opuadd - add a user
#
#  SYNOPSIS
#    call opuadd
#
subroutine opuadd
NOIMPLICIT

    integer i, unum, team, length, rndint
    character ch, cdgetx, cupper
    character buf(MSGMAXLINE), junk(MSGMAXLINE), name(MSGMAXLINE)
    logical gunum, register
    include "conqcom"

    call cdclrl( MSG_LIN1, 2 )
    ch = cdgetx( "Add user: ", MSG_LIN1, 1, TERMS, name, MAXUSERNAME )
    call delblanks( name )
    if ( ch == TERM_ABORT | name(1) == EOS )
	{
	call cdclrl( MSG_LIN1, 1 )
	return
	}
    if ( gunum( unum, name ) )
	{
	call cdputs( "That user is already enrolled.", MSG_LIN2, 1 )
	call cdmove( 1, 1 )
	call cdplay( .false. )
	call sleep( 1.0 )
	call cdclrl( MSG_LIN1, 2 )
	return
	}
    for ( team = 0; team == 0; )
	{
	call strcpy( "Select a team (", junk )
	for ( i = 1; i <= NUMTEAMS; i = i + 1 )
	    call appchr( chrteams(i), junk )
	call appstr( "): ", junk )
	call cdclrl( MSG_LIN1, 1 )
	ch = cdgetx( junk, MSG_LIN1, 1, TERMS, buf, MSGMAXLINE )
	if ( ch == TERM_ABORT )
	    {
	    call cdclrl( MSG_LIN1, 1 )
	    return
	    }
	else if ( ch == TERM_EXTRA & buf(1) == EOS )
	    team = rndint( 1, NUMTEAMS )
	else
	    {
	    ch = cupper( buf(1) )
	    for ( i = 1; i <= NUMTEAMS; i = i + 1 )
		if ( chrteams(i) == ch )
		    {
		    team = i
		    break
		    }
	    }
	}

    buf(1) = EOS
    call apptitle( team, buf )
    call appchr( ' ', buf )
    i = length( buf ) + 1
    call appstr( name, buf )
    buf(i) = cupper( buf(i) )
    buf(MAXUSERPNAME) = EOS
    if ( ! register( name, buf, team, unum ) )
	{
	call cdputs( "Error adding new user.", MSG_LIN2, 1 )
	call cdmove( 1, 1 )
	call cdplay( .false. )
	call sleep( 1.0 )
	}
    call cdclrl( MSG_LIN1, 2 )

    return

end


###  opuedit - edit a user
#
#  SYNOPSIS
#    call opuedit
#
subroutine opuedit
NOIMPLICIT

define(MAXUEDITROWS,MAXOPTIONS+2)
    integer i, unum, row, lin, col, olin, tcol, dcol, lcol, rcol, modp1, length
    character ch, getcx, buf(MSGMAXLINE)
    logical l, left, gunum, safectoi, iogtimed
    include "conqcom"
    common / conqopu / left, row
    data left / .true. /
    data row / 1 /

    call cdclrl( MSG_LIN1, 2 )
    ch = getcx( "Edit which user: ", MSG_LIN1, 0, TERMS, buf, MAXUSERNAME )
    if ( ch == TERM_ABORT )
	{
	call cdclrl( MSG_LIN1, 2 )
	return
	}
    call delblanks( buf )
    if ( ! gunum( unum, buf ) )
	{
	call cdclrl( MSG_LIN1, 2 )
	call cdputs( "Unknown user.", MSG_LIN1, 1 )
	call cdmove( 1, 1 )
	call cdplay( .false. )
	call sleep( 1.0 )
	return
	}
    call cdclear
    call cdclrl( MSG_LIN1, 2 )

    repeat
	{
	call cdclear

	# Do the right side first.
	lin = 1
	tcol = 43
	dcol = 62
	rcol = dcol - 1

	call cdputs( "         Username:", lin, tcol )
	call cdputs( uname(1,unum), lin, dcol )

	lin = lin + 1
	call cdputs( "   Multiple count:", lin, tcol )
	call cdputn( umultiple(unum), 0, lin, dcol )

	lin = lin + 1
	for ( i = 1; i <= MAXOPTIONS; i = i + 1 )
	    {
	    call prints( buf, "%17d:", i )
	    call cdputs( buf, lin+i-1, tcol )
	    if ( uoption(unum,i) )
		call cdput( 'T', lin+i-1, dcol )
	    else
		call cdput( 'F', lin+i-1, dcol )
	    }
	call cdputs( "  Phaser graphics:", lin+OPT_PHASERGRAPHICS-1, tcol )
	call cdputs( "     Planet names:", lin+OPT_PLANETNAMES-1, tcol )
	call cdputs( "       Alarm bell:", lin+OPT_ALARMBELL-1, tcol )
	call cdputs( "  Intruder alerts:", lin+OPT_INTRUDERALERT-1, tcol )
	call cdputs( "      Numeric map:", lin+OPT_NUMERICMAP-1, tcol )
	call cdputs( "            Terse:", lin+OPT_TERSE-1, tcol )
	call cdputs( "       Explosions:", lin+OPT_EXPLOSIONS-1, tcol )

	lin = lin + MAXOPTIONS + 1
	call cdputs( "          Urating:", lin, tcol )
	call cdputr( oneplace(urating(unum)), 0, lin, dcol )

	lin = lin + 1
	call cdputs( "             Uwar:", lin, tcol )
	buf(1) = '('
	for ( i = 1; i <= NUMTEAMS; i = i + 1 )
	    if ( uwar(unum,i) )
		buf(i+1) = chrteams(i)
	    else
		buf(i+1) = '-'
	buf(NUMTEAMS+2) = ')'
	buf(NUMTEAMS+3) = EOS
	call cdputs( buf, lin, dcol )

	lin = lin + 1
	call cdputs( "           Urobot:", lin, tcol )
	if ( urobot(unum) )
	    call cdput( 'T', lin, dcol )
	else
	    call cdput( 'F', lin, dcol )

	# Now the left side.
	lin = 1
	tcol = 3
	dcol = 22
	lcol = dcol - 1

	call cdputs( "             Name:", lin, tcol )
	call cdputs( upname(1,unum), lin, dcol )

	lin = lin + 1
	call cdputs( "             Team:", lin, tcol )
	i = uteam(unum)
	if ( i < 1 | i > NUMTEAMS )
	    call cdputn( i, 0, lin, dcol )
	else
	    call cdputs( tname(1,i), lin, dcol )

	lin = lin + 1
	for ( i = 1; i <= MAXOOPTIONS; i = i + 1 )
	    {
	    call prints( buf, "%17d:", i )
	    call cdputs( buf, lin+i-1, tcol )
	    if ( uooption(unum,i) )
		call cdput( 'T', lin+i-1, dcol )
	    else
		call cdput( 'F', lin+i-1, dcol )
	    }
	call cdputs( "         Multiple:", lin+OOPT_MULTIPLE-1, tcol )
	call cdputs( "     Switch teams:", lin+OOPT_SWITCHTEAMS-1, tcol )
	call cdputs( " Play when closed:", lin+OOPT_PLAYWHENCLOSED-1, tcol )
	call cdputs( "          Disable:", lin+OOPT_SHITLIST-1, tcol )
	call cdputs( "     GOD messages:", lin+OOPT_GODMSG-1, tcol )
	call cdputs( "             Lose:", lin+OOPT_LOSE-1, tcol )
	call cdputs( "        Autopilot:", lin+OOPT_AUTOPILOT-1, tcol )

	lin = lin + MAXOOPTIONS + 1
	call cdputs( "       Last entry:", lin, tcol )
	call cdputs( ulastentry(1,unum), lin, dcol )

	lin = lin + 1
	call cdputs( "  Elapsed seconds:", lin, tcol )
	call fmtseconds( ustats(unum,TSTAT_SECONDS), buf )
	i = dcol + 11 - length( buf )
	call cdputs( buf, lin, i )

	lin = lin + 1
	call cdputs( "      Cpu seconds:", lin, tcol )
	call fmtseconds( ustats(unum,TSTAT_CPUSECONDS), buf )
	i = dcol + 11 - length ( buf )
	call cdputs( buf, lin, i )

	lin = lin + 1

	# Do column 4 of the bottom stuff.
	olin = lin
	tcol = 62
	dcol = 72
	call cdputs( "Maxkills:", lin, tcol )
	call cdputn( ustats(unum,USTAT_MAXKILLS), 0, lin, dcol )

	lin = lin + 1
	call cdputs( "Torpedos:", lin, tcol )
	call cdputn( ustats(unum,USTAT_TORPS), 0, lin, dcol )

	lin = lin + 1
	call cdputs( " Phasers:", lin, tcol )
	call cdputn( ustats(unum,USTAT_PHASERS), 0, lin, dcol )

	# Do column 3 of the bottom stuff.
	lin = olin
	tcol = 35
	dcol = 51
	call cdputs( " Planets taken:", lin, tcol )
	call cdputn( ustats(unum,USTAT_CONQPLANETS), 0, lin, dcol )

	lin = lin + 1
	call cdputs( " Armies bombed:", lin, tcol )
	call cdputn( ustats(unum,USTAT_ARMBOMB), 0, lin, dcol )

	lin = lin + 1
	call cdputs( "   Ship armies:", lin, tcol )
	call cdputn( ustats(unum,USTAT_ARMSHIP), 0, lin, dcol )

	# Do column 2 of the bottom stuff.
	lin = olin
	tcol = 18
	dcol = 29
	call cdputs( " Conquers:", lin, tcol )
	call cdputn( ustats(unum,USTAT_CONQUERS), 0, lin, dcol )

	lin = lin + 1
	call cdputs( "    Coups:", lin, tcol )
	call cdputn( ustats(unum,USTAT_COUPS), 0, lin, dcol )

	lin = lin + 1
	call cdputs( "Genocides:", lin, tcol )
	call cdputn( ustats(unum,USTAT_GENOCIDE), 0, lin, dcol )

	# Do column 1 of the bottom stuff.
	lin = olin
	tcol = 1
	dcol = 10
	call cdputs( "   Wins:", lin, tcol )
	call cdputn( ustats(unum,USTAT_WINS), 0, lin, dcol )

	lin = lin + 1
	call cdputs( " Losses:", lin, tcol )
	call cdputn( ustats(unum,USTAT_LOSSES), 0, lin, dcol )

	lin = lin + 1
	call cdputs( "Entries:", lin, tcol )
	call cdputn( ustats(unum,USTAT_ENTRIES), 0, lin, dcol )

	# Display the stuff
	call cdputc( "Use arrow keys to position, SPACE to modify.", MSG_LIN2 )
	if ( left )
	    i = lcol
	else
	    i = rcol
	call cdput( '+', row, i )
	call cdmove( row, i )
	call cdplay( .true. )

	# Now, get a character and process it.
	if ( ! iogtimed( ch, 1 ) )
	    next
	switch ( ch )
	    {
	    case 'a', 'A', 'h', 'H':
		# Left.
		left = .true.
	    case 'd', 'D', 'l', 'L':
		# Right.
		left = .false.
	    case 'w', 'W', 'k', 'K':
		# Up.
		row = max( row - 1, 1 )
	    case 'x', 'X', 'j', 'J':
		# Down.
		row = min( row + 1, MAXUEDITROWS )
	    case 'q', 'Q', 'y', 'Y':
		# Up and left.
		left = .true.
		row = max( row - 1, 1 )
	    case 'e', 'E', 'u', 'U':
		# Up and right.
		left = .false.
		row = max( row - 1, 1 )
	    case 'z', 'Z', 'b', 'B':
		# Down and left.
		left = .true.
		row = min( row + 1, MAXUEDITROWS )
	    case 'c', 'C', 'n', 'N':
		# Down and right.
		left = .false.
		row = min( row + 1, MAXUEDITROWS )
	    case ' ':
		# Modify the current entry.
		if ( left & row == 1 )
		    {
		    # Pseudonym.
		    call cdclrl( MSG_LIN2, 1 )
		    ch = getcx( "Enter a new pseudonym: ",
			MSG_LIN2, 0, TERMS, buf, MAXUSERPNAME )
		    if ( ch != TERM_ABORT &
		         ( buf(1) != EOS | ch == TERM_EXTRA ) )
			call stcpn( buf, upname(1,unum), MAXUSERPNAME )
		    }
		else if ( ! left & row == 1 )
		    {
		    # Username.
		    call cdclrl( MSG_LIN2, 1 )
		    ch = getcx( "Enter a new username: ",
			MSG_LIN2, 0, TERMS, buf, MAXUSERNAME )
		    if ( ch != TERM_ABORT &
		         ( buf(1) != EOS | ch == TERM_EXTRA ) )
			{
			call delblanks( buf )
			if ( ! gunum( i, buf ) )
			    call stcpn( buf, uname(1,unum), MAXUSERNAME )
			else
			    {
			    call cdclrl( MSG_LIN1, 2 )
			    call cdputc( "That username is already in use.",
				MSG_LIN2 )
			    call cdmove( 1, 1 )
			    call cdplay( .false. )
			    call sleep( 1.0 )
			    }
			}
		    }
		else if ( left & row == 2 )
		    {
		    # Team.
		    uteam(unum) = modp1( uteam(unum) + 1, NUMTEAMS )
		    }
		else if ( ! left & row == 2 )
		    {
		    # Multiple count.
		    call cdclrl( MSG_LIN2, 1 )
		    ch = getcx( "Enter new multiple count: ",
			MSG_LIN2, 0, TERMS, buf, MSGMAXLINE )
		    if ( ch != TERM_ABORT & buf(1) != EOS )
			{
			call delblanks( buf )
			i = 1
			l = safectoi( umultiple(unum), buf, i )
			}
		    }
		else
		    {
		    i = row - 2
		    if ( left )
			if ( i >= 1 & i <= MAXOOPTIONS )
			    uooption(unum,i) = ! uooption(unum,i)
			else
			    call scbeep
		    else
			if ( i >= 1 & i <= MAXOPTIONS )
			    uoption(unum,i) = ! uoption(unum,i)
			else
			    call scbeep
		    }
	    case TERM_NORMAL, TERM_EXTRA, TERM_ABORT:
		break
	    case '@^l':
		call cdredo
	    default:
		call scbeep
	    }
	}

    return

end


###  watch - peer over someone's shoulder
#
#  SYNOPSIS
#    call watch
#
subroutine watch
NOIMPLICIT

    integer i, snum, msgrand, now, modp1, alldig, length, dgrand
    logical l, normal, readone, confirm, safectoi, review
    logical getamsg, iochav, iogtimed
    character ch, tch, buf(MSGMAXLINE), cdgetx
    include "conqcom"
    include "conqcom2"
    string pmt "Watch which ship (<cr> for doomsday)? "
    string nss "No such ship."
    string nf "Not found."
    string help1 "M - message from GOD, L - review messages,"
    string help2 "K - kill this ship, H - this message"

    call cdclrl( MSG_LIN1, 2 )
    tch = cdgetx( pmt, MSG_LIN1, 1, TERMS, buf, MSGMAXLINE )
    if ( tch == TERM_ABORT )
	{
	call cdclrl( MSG_LIN1, 1 )
	return
	}
    if ( length( buf ) == 0 )
	{
	# Doomsday.
	call cdclear
	call cdredo
	repeat
	    {
	    call doomdisplay
	    call cdplay( .true. )
	    }
	until ( iogtimed( ch, 1 ) )
	}
    else
	{
	normal = ( tch != TERM_EXTRA )		# line feed means debugging
	call delblanks( buf )
	if ( alldig( buf ) != YES )
	    {
	    call cdputs( nss, MSG_LIN2, 1 )
	    call cdmove( 1, 1 )
	    call cdplay( .false. )
	    call sleep( 1.0 )
	    return
	    }
	i = 1
	l = safectoi( snum, buf, i )		# ignore status
	if ( snum < 1 | snum > MAXSHIPS )
	    {
	    call cdputs( nss, MSG_LIN2, 1 )
	    call cdmove( 1, 1 )
	    call cdplay( .false. )
	    call sleep( 1.0 )
	    }
#	else if ( sstatus(snum) != SS_LIVE )
#	    {
#	    call cdputs( nf, MSG_LIN2, 1 )
#	    call cdmove( 1, 1 )
#	    call cdplay( .false. )
#	    call sleep( 1.0 )
#	    }
	    else
		{
		credraw = .true.
		call cdclear
		call cdredo
		call grand( msgrand )
		repeat
		    {
		    # Try to display a new message.
		    readone = .false.
		    if ( dgrand( msgrand, now ) >= NEWMSG_GRAND )
			if ( getamsg( MSG_GOD, glastmsg ) )
			    {
			    call readmsg( MSG_GOD, glastmsg )
			    msgrand = now
			    readone = .true.
			    }

		    # Drive the display.
		    if ( normal )
			call display( snum )
		    else
			call debugdisplay( snum )

		    # Un-read message, if there's a chance it got garbaged.
		    if ( readone )
			if ( iochav( 0 ) )
			    glastmsg = modp1( glastmsg - 1, MAXMESSAGES )

		    # Get a character with timeout.
		    if ( ! iogtimed( ch, 1 ) )
			next
		    call cdclrl( MSG_LIN1, 2 )
		    switch ( ch )
			{
			case 'h', 'H':
			    call cdputc( help1, MSG_LIN1 )
			    call cdputc( help2, MSG_LIN2 )
			case 'i':
			    call opinfo( MSG_GOD )
			case 'k':
			    if ( confirm( 0 ) )
				call kill( snum, KB_GOD )
			case 'm':
			    call sendmsg( MSG_GOD, .true. )
			case 'L':
			    l = review( MSG_GOD, glastmsg )
			case '@^L':
			    call cdredo
			default:
			    break
			}
		    # Disable messages for awhile.
		    call grand( msgrand )
		    }
		}
	}

    return

end
