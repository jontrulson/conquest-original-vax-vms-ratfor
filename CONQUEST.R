###############################################################################
#
#                               C O N Q U E S T
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


###  conquest - main program
#
DRIVER(conquest)
NOIMPLICIT

    integer cdcols, cdlins
    logical l, welcome, ioautobroad
    string cpr COPYRIGHT
    string vern VERSION_NUMBER
    string verd VERSION_DATE
    string arr "All rights reserved!"
    include "conqcom2"

    call conqinit			# machine dependent initialization
    call rndini( 0, 0 )			# initialize random numbers
    call cdinit				# set up display environment

    cmaxlin = cdlins( 0 )
    cmaxcol = cdcols( 0 )

    l = ioautobroad( .false. )		# turn OFF automatic broadcast echoing

    csnum = 0				# force menu to get a new ship
    if ( welcome( cunum ) )
	call menu

    l = ioautobroad( .true. )		# turn ON automatic broadcast echoing

    call drpexit			# make the driver go away
    call cdend				# clean up display environment
    call conqend			# machine dependent clean-up

DRETURN

end


###  capentry - captured system entry for a new ship
#
#  SYNOPSIS
#
#    logical flag, capentry
#    integer capentry, snum, system
#    system = capentry( snum, system )
#
logical function capentry( snum, system )
NOIMPLICIT
integer snum, system

    integer i, j, index
    character ch, cupper
    logical owned(NUMTEAMS), iogtimed, stillalive
    include "conqcom"
    include "conqcom2"

    # First figure out which systems we can enter from.
    for ( i = 1; i <= NUMTEAMS; i = i + 1 )
	{
	owned(i) = .false.
	# We must own all three planets in a system.
	for ( j = 1; j <= 3; j = j + 1 )
	    {
	    if ( pteam(teamplanets(i,j)) != steam(snum) )
		next 2
	    }
	owned(i) = .true.
	}
    owned(steam(snum)) = .true.		# always can enter in our system

    # Now count how many systems we can enter from.
    j = 0
    for ( i = 1; i <= NUMTEAMS; i = i + 1 )
	if ( owned(i) )
	    j = j + 1

    # If we can only enter from one, we're done.
    if ( j <= 1 )
	{
	system = steam(snum)
	return ( .true. )
	}

    # Prompt for a decision.
    call strcpy( "Enter which system", cbuf )
    for ( i = 1; i <= NUMTEAMS; i = i + 1 )
	if ( owned(i) )
	    {
	    call appstr( ", ", cbuf )
	    call appstr( tname(1,i), cbuf )
	    }
    # Change first comma to a colon.
    i = index( cbuf, ',' )
    if ( i > 0 )
	cbuf(i) = ':'

    call cdclrl( MSG_LIN1, 2 )
    call cdputc( cbuf, MSG_LIN1 )
    call cdmove( 1, 1 )
    call cdplay( .true. )

    while ( stillalive( csnum ) )
	{
	if ( ! iogtimed( ch, 1 ) )
	    next
	switch  ( ch )
	    {
	    case TERM_NORMAL, TERM_ABORT:
		return ( .false. )
	    case TERM_EXTRA:
		# Enter the home system.
		system = steam(snum)
		return ( .true. )
	    default:
		for ( i = 1; i <= NUMTEAMS; i = i + 1 )
		    if ( chrteams(i) == cupper( ch ) & owned(i) )
			{
			# Found a good one.
			system = i
			return ( .true. )
			}
		# Didn't get a good one; complain and try again.
		call scbeep
		call cdplay( .true. )
	    }
	}

    return ( .false. )			# can get here because of stillalive()

end


###  command - execute a user's command
#
#  SYNOPSIS
#
#    character ch
#    call command( ch )
#
subroutine command( ch )
NOIMPLICIT
character ch

    integer i
    real x
    logical stillalive
    include "conqcom"
    include "conqcom2"

    switch ( ch )
	{
	case '0' - '9', '=':			# set warp factor
	    if ( ch == '=' )
		x = 10.0
	    else
		{
	        i = ch - '0'
	        x = float(i)
		}
	    call dowarp( csnum, x )
	case 'a':				# autopilot
	    if ( uooption(suser(csnum),OOPT_AUTOPILOT) )
		{
		call doautopilot( csnum )
		}
	    else
		{
		goto 1
		}
	case 'A':				# change allocation
	    call doalloc( csnum )
	    call stoptimer
	    if ( stillalive( csnum ) )
		call display( csnum )
	    call settimer
	case 'b':				# beam armies
	    call dobeam( csnum )
	case 'B':				# bombard a planet
	    call dobomb( csnum )
	case 'C':				# cloak control
	    call docloak( csnum )
	case 'd':				# detonate enemy torps
	    call dodet( csnum )
	case 'D':				# detonate own torps
	    call domydet( csnum )
	case 'E':				# emergency distress call
	    call dodistress( csnum )
	case 'f':				# phasers
	    call dophase( csnum )
	case 'F':				# phasers, same direction
	    call dolastphase( csnum )
	case 'h':
	    credraw = .true.
	    call stoptimer
	    call dohelp( csubdcl )
	    if ( stillalive( csnum ) )
		call display( csnum )
	    call settimer
	case 'H':
	    credraw = .true.
	    call stoptimer
	    call histlist( .false. )
	    if ( stillalive( csnum ) )
		call display( csnum )
	    call settimer
	case 'i':				# information
	    call doinfo( csnum )
	case 'I':				# set user options
	    call dooption( csnum, .true. )
	case 'k':				# set course
	    call docourse( csnum )
	case 'K':				# coup
	    call docoup( csnum )
	case 'L':				# review old messages
	    call doreview( csnum )
	case 'm':				# send a message
	    call sendmsg( csnum, soption(csnum,OPT_TERSE) )
	case 'M':				# strategic/tactical map
	    smap(csnum) = ! smap(csnum)
	    call stoptimer
	    call display( csnum )
	    call settimer
	case 'N':				# change pseudonym
	    call pseudo( cunum, csnum )
	case 'o':				# orbit nearby planet
	    call doorbit( csnum )
	case 'P':				# photon torpedo burst
	    call doburst( csnum )
	case 'p':				# photon torpedoes
	    call dotorp( csnum )
	case 'Q':				# self destruct
	    call doselfdest
	case 'R':				# repair mode
	    if ( ! scloaked(csnum) )
		{
		call cdclrl( MSG_LIN1, 2 )
		srmode(csnum) = .true.
		sdwarp(csnum) = 0.0
		}
	    else
		{
		call cdclrl( MSG_LIN2, 1 )
		call putmsg(
		    "You cannot repair while the cloaking device is engaged.",
		    MSG_LIN1 )
		}
	case 't':				# tow
	    call dotow( csnum )
	case 'S':				# more user stats
	    credraw = .true.
	    call stoptimer
	    call userstats( .false. )
	    if ( stillalive( csnum ) )
		call display( csnum )
	    call settimer
	case 'T':				# team list
	    credraw = .true.
	    call stoptimer
	    call doteamlist( steam(csnum) )
	    if ( stillalive( csnum ) )
		call display( csnum )
	    call settimer
	case 'u':				# un-tractor
	    call dountow( csnum )
	case 'U':				# user stats
	    credraw = .true.
	    call stoptimer
	    call userlist( .false. )
	    if ( stillalive( csnum ) )
		call display( csnum )
	    call settimer
	case 'W':				# war and peace
	    call dowar( csnum )
	case '-':				# shields down
	    call doshields( csnum, .false. )
	    call stoptimer
	    call display( csnum )
	    call settimer
	case '+':				# shields up
	    call doshields( csnum, .true. )
	    call stoptimer
	    call display( csnum )
	    call settimer
	case '/':				# player list
	    credraw = .true.
	    call stoptimer
	    call playlist( .false., .false. )
	    if ( stillalive( csnum ) )
		call display( csnum )
	    call settimer
	case '?':				# planet list
	    credraw = .true.
	    call stoptimer
	    call doplanlist( csnum )
	    if ( stillalive( csnum ) )
		call display( csnum )
	    call settimer
	case '$':				# spawn to DCL
	    if ( csubdcl )
		{
		call dosubdcl
		}
	    else
		{
		goto 1
		}
	case '@^L':				# clear and redisplay
	    call cdredo
	case '@^S', '@^Q', EOS:
	    # nothing.
	default:
1	    continue
	    call ioeat
	    call scbeep
	    call putmsg( "Type h for help.", MSG_LIN2 )
	}

    return

end


###  conqds - display background for Conquest
#
#  SYNOPSIS
#    logical multiple, switchteams
#    call conqds( multiple, switchteams )
#
subroutine conqds( multiple, switchteams )
NOIMPLICIT
logical multiple, switchteams

    integer i, length, col, lin, lenc1
    include "conqcom"
    include "conqcom2"
    string c1 " CCC    OOO   N   N   QQQ   U   U  EEEEE   SSSS  TTTTT"
    string c2 "C   C  O   O  NN  N  Q   Q  U   U  E      S        T"
    string c3 "C      O   O  N N N  Q   Q  U   U  EEE     SSS     T"
    string c4 "C   C  O   O  N  NN  Q  Q   U   U  E          S    T"
    string c5 " CCC    OOO   N   N   QQ Q   UUU   EEEEE  SSSS     T"

    # First clear the display.
    call cdclear

    # Display the logo.
    lenc1 = length( c1 )
    col = (cmaxcol-lenc1) / 2
    lin = 2
    call cdputs( c1, lin, col )
    lin = lin + 1
    call cdputs( c2, lin, col )
    lin = lin + 1
    call cdputs( c3, lin, col )
    lin = lin + 1
    call cdputs( c4, lin, col )
    lin = lin + 1
    call cdputs( c5, lin, col )

    # Draw a box around the logo.
    lin = lin + 1
    call cdbox( 1, col-2, lin, col+lenc1+1 )

    lin = lin + 1
    if ( closed )
	call cdputc( "The game is closed.", lin )
    else
	{
	call prints( cbuf, "V%s (%s)  %s",
	    VERSION_NUMBER, VERSION_DATE, COPYRIGHT )
	call cdputc( cbuf, lin )
	}

    lin = lin + 1
    call cdputc( "Options:", lin )

    col = 13
    lin = lin + 2
    i = lin
    call cdputs( "(e) - enter the game", lin, col )
    if ( cnewsfile )
	{
	lin = lin + 1
	call cdputs( "(n) - read the news", lin, col )
	}
    lin = lin + 1
    call cdputs( "(h) - read the help lesson", lin, col )
    lin = lin + 1
    call cdputs( "(S) - more user statistics", lin, col )
    lin = lin + 1
    call cdputs( "(T) - team statistics", lin, col )
    lin = lin + 1
    call cdputs( "(U) - user statistics", lin, col )
    lin = lin + 1
    call cdputs( "(L) - review messages", lin, col )
    lin = lin + 1
    call cdputs( "(W) - set war or peace", lin, col )
    lin = lin + 1
    call cdputs( "(I) - change user options", lin, col )

    col = 48
    lin = i
    call cdputs( "(N) - change your name", lin, col )
    if ( ! multiple )
	{
	lin = lin + 1
	call cdputs( "(r) - resign your commission", lin, col )
	}
    if ( multiple | switchteams )
	{
	lin = lin + 1
	call cdputs( "(s) - switch teams", lin, col )
	}
    lin = lin + 1
    call cdputs( "(H) - user history", lin, col )
    lin = lin + 1
    call cdputs( "(/) - player list", lin, col )
    lin = lin + 1
    call cdputs( "(?) - planet list", lin, col )
    lin = lin + 1
    call cdputs( "(q) - exit the program", lin, col )

    return

end


###  dead - announce to a user that s/he is dead (DOES LOCKING)
#
#  SYNOPSIS
#    integer snum
#    logical leave
#    call dead( snum, leave )
#
subroutine dead( snum, leave )
NOIMPLICIT
integer snum
logical leave

    integer i, j, k, lin, col, kb, entertime, now, dgrand
    character ch, cdgetx, getcx
    logical iogtimed, stillalive
    include "conqcom"
    include "conqcom2"
    string ywkb "You were killed by "

    # (Quickly) clear the screen.
    call cdclear
    call cdredo
    call cdplay( .false. )

    # If something is wrong, don't do anything.
    if ( snum < 1 | snum > MAXSHIPS )
	return

    # If our ships pid is wrong, we are indeed lost.
    if ( spid(snum) != cpid )
	return

    kb = skilledby(snum)

    # Delay while our torps are exploding.
    call grand( entertime )
    i = 0
    while ( dgrand( entertime, now ) < TORPEDOWAIT_GRAND )
	{
	i = 0
	for ( j = 1; j <= MAXTORPS; j = j + 1 )
	    if ( tstatus(snum,j) == TS_DETONATE )
		i = i + 1
	if ( i <= 0 )
	    break
	call sleep( ITER_SECONDS )
	}

    # There aren't supposed to be any torps left.
    if ( i > 0 )
	{
	call strcpy( "dead: ", cbuf )
	call appship( snum, cbuf )
	call appstr( "'s detonating torp count is %d.", cbuf )
	call cerror( MSG_GOD, cbuf, i )
	}

    # Figure out why we died.
    switch ( kb )
	{
	case KB_SELF:
	    call strcpy( "You scuttled yourself.", cbuf )
	case KB_NEGENB:
	    call strcpy( "You were destroyed by the negative energy barrier.",
		cbuf )
	case KB_CONQUER:
	    call strcpy(
		"Y O U   C O N Q U E R E D   T H E   U N I V E R S E ! ! !",
		cbuf )
	case KB_NEWGAME:
	    call strcpy( "N E W   G A M E !", cbuf )
	case KB_EVICT:
	    call strcpy( "Closed for repairs.", cbuf )
	case KB_SHIT:
	    call strcpy( "You are no longer allowed to play.", cbuf )
	case KB_GOD:
	    call strcpy( "You were killed by an act of GOD.", cbuf )
	case KB_DOOMSDAY:
	    call strcpy( "You were eaten by the doomsday machine.", cbuf )
	case KB_GOTDOOMSDAY:
	    call strcpy( "You destroyed the doomsday machine!", cbuf )
	case KB_DEATHSTAR:
	    call strcpy( "You were vaporized by the Death Star.", cbuf )
	case KB_LIGHTNING:
	    call strcpy( "You were destroyed by lightning bolt.", cbuf )
	default:
	    if ( kb >= 1 & kb <= MAXSHIPS )
		{
		call prints( cbuf, "You were kill number %g for %s (",
		    oneplace(skills(kb)), spname(1,kb) )
		call appship( kb, cbuf )
		call appchr( ')', cbuf )
		if ( sstatus(kb) != SS_LIVE )
		    call appstr( ", who also died.", cbuf )
		else
		    call appchr( '.', cbuf )
		}
	    else if ( -kb >= 1 & -kb <= NUMPLANETS )
		{
		call concat( ywkb, pname(1,-kb), cbuf )
		if ( ptype(-kb) == PLANET_SUN )
		    call appstr( "'s solar radiation.", cbuf )
		else
		    call appstr( "'s planetary defenses.", cbuf )
		}
	    else
		{
		# We were unable to determine the cause of death.
		call strcpy( "dead: ", cbuf )
		call appship( snum, cbuf )
		call appstr( " was killed by %d.", cbuf )
		call cerror( MSG_GOD, cbuf, kb )
		call concat( ywkb, "nothing in particular.  (How strange...)",
		    cbuf )
		}
	}

    call cdputc( cbuf, 8 )

    if ( kb == KB_NEWGAME )
	{
	call prints( cbuf, "Universe conquered by %s for the %s team.",
	    conqueror, conqteam )
	call cdputc( cbuf, 10 )
	}
    else if ( kb == KB_SELF )
	{
	i = sarmies(snum)
	if ( i > 0 )
	    {
	    call strcpy( "The ", cbuf )
	    if ( i == 1 )
		call appstr( "army", cbuf )
	    else
		{
		if ( i < 100 )
		    call appnum( i, cbuf )
		else
		    call appint( i, cbuf )
		call appstr( " armies", cbuf )
		}
	    call appstr( " you were carrying ", cbuf )
	    if ( i == 1 )
		call appstr( "was", cbuf )
	    else
		call appstr( "were", cbuf )
	    call appstr( " not amused.", cbuf )
	    call cdputc( cbuf, 10 )
	    }
	}
    else if ( kb > 0 )
	{
	if ( sstatus(kb) == SS_LIVE )
	    {
	    call prints( cbuf, "He had %d%% shields and %d%% damage.",
		round(sshields(kb)), round(sdamage(kb)) )
	    call cdputc( cbuf, 10 )
	    }
	}
    call prints( cbuf, "You got %g this time.", oneplace(skills(snum)) )
    call cdputc( cbuf, 12 )
    call cdmove( 1, 1 )
    call cdplay( .false. )
    if ( ! ( leave & kb == KB_SELF ) & kb != KB_SHIT & kb != KB_EVICT )
	call sleep( 4.0 )

    for ( i = 1; i <= 10 & sstatus(snum) == SS_DYING; i = i + 1 )
	call sleep( 1.0 )
    sstatus(snum) = SS_RESERVED
    ssdfuse(snum) = -TIMEOUT_PLAYER
    skilledby(snum) = 0

    switch ( kb )
	{
	case KB_CONQUER:
	    repeat
		{
		call cdclear
		call cdredo
		ch = cdgetx( "Any last words? ",
		    14, 1, TERMS, lastwords, MAXLASTWORDS )
		call cdclear
		call cdredo
		if ( lastwords(1) != EOS )
		    {
		    call cdputc( "You last words are entered as:", 13 )
		    call prints( cbuf, "%c%s%c", '"', lastwords, '"' )
		    call cdputc( cbuf, 14 )
		    }
		else
		    call cdputc(
			"You have chosen to NOT leave any last words:", 14 )
		ch = getcx( "Press LINEFEED to confirm:", 16, 0,
		    TERMS, cbuf, 10 )
		}
	    until ( ch == TERM_EXTRA )
	case KB_SELF, KB_EVICT, KB_SHIT:
	    # Do nothing special.
	default:
	    call ioeat
	    call putpmt( "--- press space when done ---", MSG_LIN2 )
	    call cdplay( .true. )
	    while ( ! iogtimed( ch, 1 ) & stillalive( csnum ) )
		;
	}
    call cdmove( 1, 1 )

    # Turn off sticky war so we can change war settings from menu().
    for ( i = 1; i <= NUMTEAMS; i = i + 1 )
	srwar(snum,i) = .false.

    return

end


###  dispoption - display options
#
#  SYNOPSIS
#    logical op(MAXOPTIONS)
#    call dispoption( op )
#
subroutine dispoption( op )
NOIMPLICIT
logical op(ARB)

    string l1 "Toggle options, LINEFEED when done: phaser (g)raphics=~ (p)lanet names=~"
    string l2 "(a)larm bells=~ (i)ntruder alerts=~ (n)umeric map=~ (t)erse=~ (e)xplosions=~"

    call cdclrl( MSG_LIN1, 2 )
    call cdputs( l1, MSG_LIN1, 1 )
    call cdputs( l2, MSG_LIN2, 1 )

    if ( op(OPT_PHASERGRAPHICS) )
	call cdput( 'T', MSG_LIN1, 55 )
    else
	call cdput( 'F', MSG_LIN1, 55 )

    if ( op(OPT_PLANETNAMES) )
	call cdput( 'T', MSG_LIN1, 72 )
    else
	call cdput( 'F', MSG_LIN1, 72 )

    if ( op(OPT_ALARMBELL) )
	call cdput( 'T', MSG_LIN2, 15 )
    else
	call cdput( 'F', MSG_LIN2, 15 )

    if ( op(OPT_INTRUDERALERT) )
	call cdput( 'T', MSG_LIN2, 35 )
    else
	call cdput( 'F', MSG_LIN2, 35 )

    if ( op(OPT_NUMERICMAP) )
	call cdput( 'T', MSG_LIN2, 51 )
    else
	call cdput( 'F', MSG_LIN2, 51 )

    if ( op(OPT_TERSE) )
	call cdput( 'T', MSG_LIN2, 61 )
    else
	call cdput( 'F', MSG_LIN2, 61 )

    if ( op(OPT_EXPLOSIONS) )
	call cdput( 'T', MSG_LIN2, 76 )
    else
	call cdput( 'F', MSG_LIN2, 76 )

    return

end


###  doalloc - change weapon/engine allocations
#
#  SYNOPSIS
#    integer snum
#    call doalloc( snum )
#
subroutine doalloc( snum )
NOIMPLICIT
integer snum

    character cdgetx, ch
    integer i, alloc
    logical l, safectoi
    include "conqcom"
    include "conqcom2"
    string pmt "New weapons allocation: (40-60) "

    call cdclrl( MSG_LIN1, 2 )
    ch = cdgetx( pmt, MSG_LIN1, 1, TERMS, cbuf, MSGMAXLINE )
    if ( ch == TERM_EXTRA )
	sweapons(snum) = sengines(snum)
    else if ( ch == TERM_NORMAL )
	{
	i = 1
	l = safectoi( alloc, cbuf, i )			# ignore status
	if ( alloc != 0 )
	    {
	    if ( alloc < 40 )
		alloc = 40
	    else if ( alloc > 60 )
		alloc = 60
	    sweapons(snum) = alloc
	    }
	}

    sengines(snum) = 100 - sweapons(snum)
    call cdclrl( MSG_LIN1, 1 )

    return

end


###  doautopilot - handle the autopilot
#
#  SYNOPSIS
#    integer snum
#    call doautopilot( snum )
#
subroutine doautopilot( snum )
NOIMPLICIT
integer snum

    integer laststat, now, dsecs
    logical iogtimed, stillalive
    character ch, cdgetx
    include "conqcom"
    include "conqcom2"
    string conf "Press LINEFEED to engage autopilot:"

    call cdclrl( MSG_LIN1, 2 )
    if ( cdgetx( conf, MSG_LIN1, 1, TERMS, cbuf, MSGMAXLINE ) != TERM_EXTRA )
	{
	call cdclrl( MSG_LIN1, 1 )
	return
	}

    call putmsg( "Autopilot activated.", MSG_LIN1 )
    srobot(snum) = .true.
    call gsecs( laststat )			# initialize stat timer
    while ( stillalive( csnum ) )
	{
	# Make sure we still control our ship.
	if ( spid(snum) != cpid )
	    break

	# See if it's time to update the statistics.
	if ( dsecs( laststat, now ) >= 15 )
	    {
	    call conqstats( csnum )
	    laststat = now
	    }

	# Get a character.
	if ( ! iogtimed( ch, 1 ) )
	    next
	cmsgok = .false.
	call grand( cmsgrand )
	switch ( ch )
	    {
	    case TERM_ABORT:
		break
	    case '@^L':
		call cdredo
	    default:
		call putmsg( "Press ESCAPE to abort autopilot.", MSG_LIN1 )
		call scbeep
		call cdplay( .true. )
	    }
	cmsgok = .true.
	}
    srobot(snum) = .false.
    saction(snum) = 0

    call cdclrl( MSG_LIN1, 2 )

    return

end


###  dobeam - beam armies up or down (DOES LOCKING)
#
#  SYNOPSIS
#    integer snum
#    call dobeam( snum )
#
subroutine dobeam( snum )
NOIMPLICIT
integer snum

    integer pnum, total, num, upmax, downmax, capacity, beamax, i, alldig
    integer ototal, entertime, now, dgrand, mod
    logical l, oldsshup, dirup, zeroed, conqed
    logical spwar, safectoi, iochav, iogtimed, stillalive
    character ch, clower, cdgetx, buf(MSGMAXLINE)
    real rkills
    include "conqcom"
    include "conqcom2"
    string lastfew "Fleet orders prohibit removing the last three armies."
    string abt "...aborted..."

    srmode(snum) = .false.

    call cdclrl( MSG_LIN1, 2 )

    # Check for allowability.
    if ( swarp(snum) >= 0.0 )
	{
	call putmsg( "We must be orbiting a planet to use the transporter.",
	    MSG_LIN1 )
	return
	}
    pnum = -slock(snum)
    if ( sarmies(snum) > 0 )
	{
	if ( ptype(pnum) == PLANET_SUN )
	    {
	    call putmsg( "Idiot!  Our armies will fry down there!", MSG_LIN1 )
	    return
	    }
	else if ( ptype(pnum) == PLANET_MOON )
	    {
	    call putmsg( "Phoon!  Our armies will suffocate down there!",
		MSG_LIN1 )
	    return
	    }
	else if ( pteam(pnum) == TEAM_GOD )
	    {
	    call putmsg(
	    "GOD->you: YOUR ARMIES AREN'T GOOD ENOUGH FOR THIS PLANET.",
		MSG_LIN1 )
	    return
	    }
	}

    i = puninhabtime(pnum)
    if ( i > 0 )
	{
	call prints( cbuf, "This planet is uninhabitable for %d more minute",
	    i )
	if ( i != 1 )
	    call appchr( 's', cbuf )
	call appchr( '.', cbuf )
	call putmsg( cbuf, MSG_LIN1 )
	return
	}

    if ( pteam(pnum) != steam(snum) &
	 pteam(pnum) != TEAM_SELFRULED &
	 pteam(pnum) != TEAM_NOTEAM )
	if ( ! swar(snum,pteam(pnum)) )
	    {
	    call putmsg( "But we are not at war with this planet!", MSG_LIN1 )
	    return
	    }

    if ( sarmies(snum) == 0 &
	 pteam(pnum) == steam(snum) & parmies(pnum) <= MIN_BEAM_ARMIES )
	{
	call putmsg( lastfew, MSG_LIN1 )
	return
	}

    rkills = oneplace(skills(snum))
    if ( rkills < 1.0 )
	{
	call putmsg(
	    "Fleet orders prohibit beaming armies until you have a kill.",
	    MSG_LIN1 )
	return
	}

    # Figure out what can be beamed.
    downmax = sarmies(snum)
    if ( spwar(snum,pnum) |
	 pteam(pnum) == TEAM_SELFRULED |
	 pteam(pnum) == TEAM_NOTEAM |
	 pteam(pnum) == TEAM_GOD |
	 parmies(pnum) == 0 )
	{
	upmax = 0
	}
    else
	{
	capacity = min( ifix( rkills ) * 2, armylim(steam(snum)) )
	upmax = min( parmies(pnum) - MIN_BEAM_ARMIES, capacity-sarmies(snum) )
	}

    # If there are armies to beam but we're selfwar...
    if ( upmax > 0 & selfwar(snum) & steam(snum) == pteam(pnum) )
	{
	if ( downmax <= 0 )
	    {
	    call strcpy( "The arm", cbuf )
	    if ( upmax == 1 )
		call appstr( "y is", cbuf )
	    else
		call appstr( "ies are", cbuf )
	    call appstr( " reluctant to beam aboard a pirate vessel.", cbuf )
	    call putmsg( cbuf, MSG_LIN1 )
	    return
	    }
	upmax = 0
	}

    # Figure out which direction to beam.
    if ( upmax <= 0 & downmax <= 0 )
	{
	call putmsg( "There is no one to beam.", MSG_LIN1 )
	return
	}
    if ( upmax <= 0 )
	dirup = .false.
    else if ( downmax <= 0 )
	dirup = .true.
    else
	{
	call putmsg( "Beam [up or down] ", MSG_LIN1 )
	call cdplay( .true. )
	while ( stillalive( csnum ) )
	    {
	    if ( ! iogtimed( ch, 1 ) )
		next
	    switch ( clower( ch ) )
		{
		case 'u':
		    dirup = .true.
		    break
		case 'd', TERM_EXTRA:
		    dirup = .false.
		    break
		default:
		    call putmsg( abt, MSG_LIN1 )
		    return
		}
	    }
	}

    if ( dirup )
	beamax = upmax
    else
	beamax = downmax

    # Figure out how many armies should be beamed.
    if ( dirup )
	call strcpy( "up", buf )
    else
	call strcpy( "down", buf )
    call prints( cbuf, "Beam %s [1-%d] ", buf, beamax )
    call cdclrl( MSG_LIN1, 1 )
    ch = cdgetx( cbuf, MSG_LIN1, 1, TERMS, buf, MSGMAXLINE )
    if ( ch == TERM_ABORT )
	{
	call putmsg( abt, MSG_LIN1 )
	return
	}
    else if ( ch == TERM_EXTRA & buf(1) == EOS )
	num = beamax
    else
	{
        call delblanks( buf )
        if ( alldig( buf ) != YES )
	    {
	    call putmsg( abt, MSG_LIN1 )
	    return
	    }
        i = 1
	l = safectoi( num, buf, i )			# ignore status
        if ( num < 1 | num > beamax )
	    {
	    call putmsg( abt, MSG_LIN1 )
	    return
	    }
	}

    # Now we are ready!
    if ( pteam(pnum) > NUMTEAMS )
	# If the planet is not race owned, make it war with us.
        ssrpwar(snum,pnum) = .true.
    else if ( pteam(pnum) != steam(snum) )
	{
	# For a team planet make the war sticky and send an intruder alert.
	srwar(snum,pteam(pnum)) = .true.
	call intrude( snum, pnum )

	## Chance to create a robot here.
	}

    # Lower shields.
    oldsshup = sshup(snum)
    sshup(snum) = .false.

    # Beam.
    total = 0
    ototal = -1				# force an update the first time
    zeroed = .false.
    conqed = .false.

    call grand( entertime )
    repeat
	{
	if ( ! stillalive( csnum ) )
	    return
	if ( iochav( 0 ) )
	    {
	    call putmsg( abt, MSG_LIN1 )
	    break
	    }

	# See if it's time to beam again.
	while ( dgrand( entertime, now ) >= BEAM_GRAND )
	    {
	    entertime = mod( entertime + BEAM_GRAND, 24*60*60*1000 )
	    PVLOCK(lockword)
	    if ( dirup )
		{
		# Beam up.
		if ( parmies(pnum) <= MIN_BEAM_ARMIES )
		    {
		    PVUNLOCK(lockword)
		    call putmsg( lastfew, MSG_LIN1 )
		    break
		    }
		sarmies(snum) = sarmies(snum) + 1
		parmies(pnum) = parmies(pnum) - 1
		}
	    else
		{
		# Beam down.
		sarmies(snum) = sarmies(snum) - 1
		if ( pteam(pnum) == TEAM_NOTEAM | parmies(pnum) == 0 )
		    {
		    call takeplanet( pnum, snum )
		    conqed = .true.
		    }
		else if ( pteam(pnum) != steam(snum) )
		    {
		    parmies(pnum) = parmies(pnum) - 1
		    if ( parmies(pnum) == 0 )
			{
			call zeroplanet( pnum, snum )
			zeroed = .true.
			}
		    }
		else
		    parmies(pnum) = parmies(pnum) + 1
		}
	    PVUNLOCK(lockword)
	    total = total + 1

	    if ( total >= num )
		{
		# Done.
		call cdclrl( MSG_LIN1, 1 )
		break 2
		}
	    }

	if ( ototal != total )
	    {
	    call strcpy( "Beaming ", cbuf )
	    if ( dirup )
		call appstr( "up from ", cbuf )
	    else
		call appstr( "down to ", cbuf )
	    call appstr( pname(1,pnum), cbuf )
	    call appstr( ", ", cbuf )
	    if ( total == 0 )
		call appstr( "no", cbuf )
	    else
		call appint( total, cbuf )
	    call appstr( " arm", cbuf )
	    if ( total == 1 )
		call appchr( 'y', cbuf )
	    else
		call appstr( "ies", cbuf )
	    call appstr( " transported, ", cbuf )
	    call appint( num - total, cbuf )
	    call appstr( " to go.", cbuf )
	    call putmsg( cbuf, MSG_LIN1 )
	    if ( ototal == -1 )
		call cdplay( .true. )		# display the first time
	    ototal = total
	    }

	if ( dirup & parmies(pnum) <= MIN_BEAM_ARMIES )
	    {
	    call putmsg( lastfew, MSG_LIN1 )
	    break
	    }

	call aston
	call sleep( ITER_SECONDS )
	call astoff
	}

    # Restore shields.
    sshup(snum) = oldsshup

    # Try to display the last bombing message.
    call cdplay( .true. )

    if ( conqed )
	{
	call prints( cbuf, "You have conquered %s.", pname(1,pnum) )
	call putmsg( cbuf, MSG_LIN1 )
	}
    else if ( zeroed )
	call putmsg( "Sensors show hostile forces eliminated from the planet.",
	    MSG_LIN1 )

    return

end


###  dobomb - bombard a planet (DOES LOCKING)
#
#  SYNOPSIS
#    integer snum
#    call dobomb( snum )
#
subroutine dobomb( snum )
NOIMPLICIT
integer snum

    integer pnum, i, now, entertime, total, ototal, oparmies, dgrand, mod
    real x, rnd, rnduni, killprob
    logical oldsshup, iochav, stillalive, usefuel
    character cdgetx, buf(MSGMAXLINE)
    include "conqcom"
    include "conqcom2"
    string lastfew "The last few armies are eluding us."
    string abt "...aborted..."

    srmode(snum) = .false.

    call cdclrl( MSG_LIN2, 1 )

    # Check for allowability.
    if ( swarp(snum) >= 0.0 )
	{
	call putmsg( "We must be orbiting a planet to bombard it.", MSG_LIN1 )
	return
	}
    pnum = -slock(snum)
    if ( ptype(pnum) == PLANET_SUN | ptype(pnum) == PLANET_MOON |
         pteam(pnum) == TEAM_NOTEAM | parmies(pnum) == 0 )
	{
        call putmsg( "There is no one there to bombard.", MSG_LIN1 )
	return
	}
    if ( pteam(pnum) == steam(snum) )
	{
	call putmsg( "We can't bomb our own armies!", MSG_LIN1 )
	return
	}
    if ( pteam(pnum) != TEAM_SELFRULED & pteam(pnum) != TEAM_GOD )
        if ( ! swar(snum,pteam(pnum)) )
	    {
	    call putmsg( "But we are not at war with this planet!", MSG_LIN1 )
	    return
	    }

    # Confirm.
    call prints( cbuf, "Press LINEFEED to bombard %s, %d armies:",
	pname(1,pnum), parmies(pnum) )
    call cdclrl( MSG_LIN1, 1 )
    if ( cdgetx( cbuf, MSG_LIN1, 1, TERMS, buf, MSGMAXLINE ) != TERM_EXTRA )
	{
	call cdclrl( MSG_LIN1, 1 )
	return
	}

    # Handle war logic.
    ssrpwar(snum,pnum) = .true.
    if ( pteam(pnum) >= 1 & pteam(pnum) <= NUMTEAMS )
	{
	# For a team planet make the war sticky and send an intruder alert.
	srwar(snum,pteam(pnum)) = .true.
        call intrude( snum, pnum )
	}
    # Planets owned by GOD have a special defense system.
    if ( pteam(pnum) == TEAM_GOD )
	{
	call prints( cbuf, "That was a bad idea, %s...", spname(1,snum) )
	call putmsg( cbuf, MSG_LIN1 )
	call damage( snum,  rnduni( 50.0, 100.0 ), KB_LIGHTNING )
	return
	}

    # Lower shields.
    oldsshup = sshup(snum)
    sshup(snum) = .false.

    # Bombard.
    total = 0
    ototal = -1					# force an update the first time
    oparmies = -1
    call grand( entertime )			# get start time
    repeat
	{
	if ( ! stillalive( csnum ) )
	    return
	if ( iochav( 0 ) )
	    {
	    call putmsg( abt, MSG_LIN1 )
	    break
	    }

	# See if it's time to bomb yet.
	while ( dgrand( entertime, now ) >= BOMBARD_GRAND )
	    {
	    if ( swfuse(snum) > 0 )
		{
		call putmsg( "Weapons are currently overloaded.", MSG_LIN1 )
		break 2
		}
	    x = BOMBARD_FUEL * float(BOMBARD_GRAND) / 1000.0
	    if ( ! usefuel( snum, x, .true. ) )
		{
		call putmsg( "Not enough fuel to bombard.", MSG_LIN1 )
		break 2
		}
	    entertime = mod( entertime + BOMBARD_GRAND, 24*60*60*1000 )
	    killprob = BOMBARD_PROB *
		       weaeff( snum ) *
		       ( parmies(pnum)/100.0 + 0.5 )
	    if ( rnd( 0 ) < killprob )
		{
		PVLOCK(lockword)
		if ( parmies(pnum) <= MIN_BOMB_ARMIES )
		    {
		    # No more armies left to bomb.
		    PVUNLOCK(lockword)
		    call putmsg( lastfew, MSG_LIN1 )
		    break 2
		    }
		parmies(pnum) = parmies(pnum) - 1

		skills(snum) = skills(snum) + BOMBARD_KILLS
		ustats(suser(snum),USTAT_ARMBOMB) =
		    ustats(suser(snum),USTAT_ARMBOMB) + 1
		tstats(steam(snum),TSTAT_ARMBOMB) =
		    tstats(steam(snum),TSTAT_ARMBOMB) + 1
		PVUNLOCK(lockword)
		total = total + 1
		}
	    }

	if ( parmies(pnum) <= MIN_BOMB_ARMIES )
	    {
	    # No more armies left to bomb.
	    call putmsg( lastfew, MSG_LIN1 )
	    break
	    }

	if ( parmies(pnum) != oparmies | ototal != total )
	    {
	    # Either our bomb run total or the population changed.
	    oparmies = parmies(pnum)
	    if ( total == 1 )
		call strcpy( "y", buf )
	    else
		call strcpy( "ies", buf )
	    call prints( cbuf, "Bombing %s, %d arm%s killed, %d left.",
		pname(1,pnum), total, buf, oparmies )
	    call putmsg( cbuf, MSG_LIN1 )
	    if ( ototal == -1 )
		call cdplay( .true. )		# display the first time
	    ototal = total
	    }

	call aston
	call sleep( ITER_SECONDS )
	call astoff
	}

    # Restore shields.
    sshup(snum) = oldsshup

    # Try to display the last bombing message.
    call cdplay( .true. )

    return

end


###  doburst - launch a burst of three torpedoes
#
#  SYNOPSIS
#    integer snum
#    call doburst( snum )
#
subroutine doburst( snum )
NOIMPLICIT
integer snum

    real dir
    logical l, gettarget, launch
    include "conqcom"
    include "conqcom2"

    call cdclrl( MSG_LIN2, 1 )

    if ( scloaked(snum) )
	{
	call putmsg( "The cloaking device is using all available power.",
	    MSG_LIN1 )
	return
	}
    if ( swfuse(snum) > 0 )
	{
	call putmsg( "Weapons are currently overloaded.", MSG_LIN1 )
	return
	}
    if ( sfuel(snum) < TORPEDO_FUEL )
	{
	call putmsg( "Not enough fuel to launch a torpedo.", MSG_LIN1 )
	return
	}

    if ( gettarget( "Torpedo burst: ", MSG_LIN1, 1, dir, slastblast(snum) ) )
	if ( ! launch( snum, dir ) )
	    call putmsg( ">TUBES EMPTY<", MSG_LIN2 )
	else
	    if ( launch( snum, dir ) )
		l = launch( snum, dir )

    call cdclrl( MSG_LIN1, 1 )

    return

end


###  docloak - cloaking device control
#
#  SYNOPSIS
#    integer snum
#    call docloak( snum )
#
subroutine docloak( snum )
NOIMPLICIT
integer snum

    character cdgetx
    logical cloak
    include "conqcom"
    include "conqcom2"
    string pmt "Press LINEFEED to engage cloaking device: "
    string nofuel "Not enough fuel to engage cloaking device."

    call cdclrl( MSG_LIN2, 1 )

    if ( scloaked(snum) )
	{
	scloaked(snum) = .false.
	call putmsg( "Cloaking device disengaged.", MSG_LIN1 )
	return
	}
    if ( swfuse(snum) > 0 )
	{
	call putmsg( "Weapons are currently overloaded.", MSG_LIN1 )
	return
	}
    if ( sfuel(snum) < CLOAK_ON_FUEL )
	{
	call putmsg( nofuel, MSG_LIN1 )
	return
	}

    call cdclrl( MSG_LIN1, 1 )
    if ( cdgetx( pmt, MSG_LIN1, 1, TERMS, cbuf, MSGMAXLINE ) == TERM_EXTRA )
	{
	if ( cloak( snum ) )
	    call putmsg( "Cloaking device engaged.", MSG_LIN2 )
	else
	    call putmsg( nofuel, MSG_LIN2 )
	}
    call cdclrl( MSG_LIN1, 1 )

    return

end


###  docoup - attempt to rise from the ashes (DOES LOCKING)
#
#  SYNOPSIS
#    integer snum
#    call docoup( snum )
#
subroutine docoup( snum )
NOIMPLICIT
integer snum

    integer i, pnum, now, entertime, rndint, dgrand
    real rnd, failprob
    character cdgetx
    logical stillalive
    include "conqcom"
    include "conqcom2"
    string nhp "We must be orbiting our home planet to attempt a coup."
    string conf "Press LINEFEED to try it: "

    call cdclrl( MSG_LIN2, 1 )

    # Check for allowability.
    if ( oneplace( skills(snum) ) < MIN_COUP_KILLS )
	{
	call putmsg(
	    "Fleet orders require three kills before a coup can be attempted.",
	    MSG_LIN1 )
	return
	}
    for ( i = 1; i <= NUMPLANETS; i = i + 1 )
	if ( pteam(i) == steam(snum) & parmies(i) > 0 )
	    {
	    call putmsg( "We don't need to coup, we still have armies left!",
		MSG_LIN1 )
	    return
	    }
    if ( swarp(snum) >= 0.0 )
	{
	call putmsg( nhp, MSG_LIN1 )
	return
	}
    pnum = -slock(snum)
    if ( pnum != homeplanet(steam(snum)) )
	{
        call putmsg( nhp, MSG_LIN1 )
	return
	}
    if ( parmies(pnum) > MAX_COUP_ENEMY_ARMIES )
	{
	call putmsg( "The enemy is still too strong to attempt a coup.",
	    MSG_LIN1 )
	return
	}
    i = puninhabtime(pnum)
    if ( i > 0 )
	{
	call prints( cbuf, "This planet is uninhabitable for %d more minutes.",
	    i )
	call putmsg( cbuf, MSG_LIN1 )
	return
	}

    # Now our team can tell coup time for free.
    tcoupinfo(steam(snum)) = .true.

    i = couptime(steam(snum))
    if ( i > 0 )
	{
	call prints( cbuf, "Our forces need %d more minutes to organize.", i )
	call putmsg( cbuf, MSG_LIN1 )
	return
	}

    # Confirm.
    call cdclrl( MSG_LIN1, 1 )
    if ( cdgetx( conf, MSG_LIN1, 1, TERMS, cbuf, MSGMAXLINE ) != TERM_EXTRA )
	{
	call putmsg( "...aborted...", MSG_LIN1 )
	return
	}

    # Now wait it out...
    call putmsg( "Attempting coup...", MSG_LIN1 )
    call cdplay( .true. )
    call grand( entertime )
    while ( dgrand( entertime, now ) < COUP_GRAND )
	{
	# See if we're still alive.
	if ( ! stillalive( csnum ) )
	    return

	# Sleep (and enable asts so the display will work).
	call aston
	call sleep( ITER_SECONDS )
	call astoff
	}

    call cdclrl( MSG_LIN1, 1 )
    PVLOCK(lockword)
    if ( pteam(pnum) == steam(snum) )
	{
	PVUNLOCK(lockword)
	call putmsg( "Sensors show hostile forces eliminated from the planet.",
	    MSG_LIN2 )
	return
	}

    failprob = parmies(pnum) / MAX_COUP_ENEMY_ARMIES * 0.5 + 0.5
    if ( rnd( 0 ) < failprob )
	{
	# Failed; setup new reorganization time.
        couptime(steam(snum)) = rndint( 5, 10 )
	PVUNLOCK(lockword)
	call putmsg( "Coup unsuccessful.", MSG_LIN2 )
	return
	}

    call takeplanet( pnum, snum )
    parmies(pnum) = rndint( 10, 20 )		# create token coup force
    ustats(suser(snum),USTAT_COUPS) = ustats(suser(snum),USTAT_COUPS) + 1
    tstats(steam(snum),TSTAT_COUPS) = tstats(steam(snum),TSTAT_COUPS) + 1
    PVUNLOCK(lockword)
    call putmsg( "Coup successful!", MSG_LIN2 )

    return

end


###  docourse - set course
#
#  SYNOPSIS
#    integer snum
#    call docourse( snum )
#
subroutine docourse( snum )
NOIMPLICIT
integer snum

    integer i, j, what, sorpnum, xsorpnum, newlock, alldig, token, count
    real dir, appx, appy, mod360, angle, rndnor
    character ch, cdgetx
    logical special, planmatch, safectoi, arrows, findspecial
    include "conqcom"
    include "conqcom2"

    call cdclrl( MSG_LIN1, 2 )

    ch = cdgetx( "Come to course: ", MSG_LIN1, 1, TERMS, cbuf, MSGMAXLINE )
    call delblanks( cbuf )
    if ( ch == TERM_ABORT | cbuf(1) == EOS )
	{
	call cdclrl( MSG_LIN1, 1 )
	return
	}

    newlock = 0					# default to no lock
    call fold( cbuf )

    what = NEAR_ERROR
    if ( alldig( cbuf ) == YES )
	{
	# Raw angle.
        call cdclrl( MSG_LIN1, 1 )
	i = 1
	if ( safectoi( j, cbuf, i ) )
	    {
	    what = NEAR_DIRECTION
	    dir = mod360( float( j ) )
	    }
	}
    else if ( arrows( cbuf, dir ) )
	what = NEAR_DIRECTION
    else if ( special( cbuf, i, token, count ) )
	{
	if ( findspecial( snum, token, count, sorpnum, xsorpnum ) )
	    what = i
	}
    else if ( cbuf(1) == 's' & alldig( cbuf(2) ) == YES )
	{
	# Ship.
	i = 2
	if ( safectoi( sorpnum, cbuf, i ) )
	    what = NEAR_SHIP
	}
    else if ( planmatch( cbuf, sorpnum, .false. ) )
	what = NEAR_PLANET

    switch ( what )
	{
	case NEAR_SHIP:
	    if ( sorpnum < 1 | sorpnum > MAXSHIPS )
		{
		call putmsg( "No such ship.", MSG_LIN2 )
		return
		}
	    if ( sorpnum == snum )
		{
		call cdclrl( MSG_LIN1, 1 )
		return
		}
	    if ( sstatus(sorpnum) != SS_LIVE )
		{
		call putmsg( "Not found.", MSG_LIN2 )
		return
		}
	    if ( scloaked(sorpnum) )
		{
		if ( swarp(sorpnum) <= 0.0 )
		    {
		    call putmsg( "Sensors are unable to lock on.", MSG_LIN2 )
		    return
		    }
		appx = rndnor(sx(sorpnum), CLOAK_SMEAR_DIST)
		appy = rndnor(sy(sorpnum), CLOAK_SMEAR_DIST)
		}
	    else
		{
		appx = sx(sorpnum)
		appy = sy(sorpnum)
		}
	    dir = angle( sx(snum), sy(snum), appx, appy )

	    # Give info if he used LINEFEED.
	    if ( ch == TERM_EXTRA )
		call infoship( sorpnum, snum )
	    else
		call cdclrl( MSG_LIN1, 1 )
	case NEAR_PLANET:
	    dir = angle( sx(snum), sy(snum), px(sorpnum), py(sorpnum) )
	    if ( ch == TERM_EXTRA )
		{
		newlock = -sorpnum
		call infoplanet( "Now locked on to ", sorpnum, snum )
		}
	    else
		call infoplanet( "Setting course for ", sorpnum, snum )
	case NEAR_DIRECTION:
	    call cdclrl( MSG_LIN1, 1 )
	case NEAR_NONE:
	    call putmsg( "Not found.", MSG_LIN2 )
	    return
	default:
	    # This includes NEAR_ERROR.
	    call putmsg( "I don't understand.", MSG_LIN2 )
	    return
	}

    if ( swarp(snum) < 0.0 )		# if orbitting
	swarp(snum) = 0.0		# break orbit
    sdhead(snum) = dir			# set direction first to avoid
    slock(snum) = newlock		#  a race in display()

    return

end


###  dodet - detonate enemy torps
#
#  SYNOPSIS
#    integer snum
#    call dodet( snum )
#
subroutine dodet( snum )
NOIMPLICIT
integer snum

    logical enemydet
    include "conqcom"

    call cdclrl( MSG_LIN2, 1 )

    if ( swfuse(snum) > 0 )
	call putmsg( "Weapons are currently overloaded.", MSG_LIN1 )
    else if ( enemydet( snum ) )
        call putmsg( "detonating...", MSG_LIN1 )
    else
	call putmsg( "Not enough fuel to fire detonators.", MSG_LIN1 )

    return

end


###  dodistress - send an emergency distress call
#
#  SYNOPSIS
#    integer snum
#    call dodistress( snum )
#
subroutine dodistress( snum )
NOIMPLICIT
integer snum

    integer i
    character cdgetx
    include "conqcom"
    include "conqcom2"
    string pmt "Press LINEFEED to send an emergency distress call: "

    call cdclrl( MSG_LIN1, 2 )

    if ( cdgetx( pmt, MSG_LIN1, 1, TERMS, cbuf, MSGMAXLINE ) == TERM_EXTRA )
	{
	call prints( cbuf,
	    "sh=%d, dam=%d, fuel=%d, temp=",
	    round(sshields(snum)),
	    round(sdamage(snum)),
	    round(sfuel(snum)) )
	i = round(swtemp(snum))
	if ( i < 100 )
	    call appint( i, cbuf )
	else
	    call appstr( "**", cbuf )
	call appchr( '/', cbuf )
	i = round(setemp(snum))
	if ( i < 100 )
	    call appint( i, cbuf )
	else
	    call appstr( "**", cbuf )
	i = sarmies(snum)
	if ( i > 0 )
	    {
	    call appstr( ", armies=", cbuf )
	    call appint( i, cbuf )
	    }
	if ( swfuse(snum) > 0 )
	    call appstr( ", -weapons", cbuf )
	if ( sefuse(snum) > 0 )
	    call appstr( ", -engines", cbuf )

	call stormsg( snum, -steam(snum), cbuf )
	}

    call cdclrl( MSG_LIN1, 1 )

    return

end


###  dohelp - display a list of commands
#
#  SYNOPSIS
#    logical subdcl
#    call dohelp( subdcl )
#
subroutine dohelp( subdcl )
NOIMPLICIT
logical subdcl

    integer lin, col, tlin
    character ch
    logical iogtimed, stillalive
    include "conqcom2"

    call cdclear
    call cdputc( "CONQUEST COMMANDS", 1 )

    lin = 4

    # Display the left side.
    tlin = lin
    col = 4
    call cdputs( "0-9,=  set warp factor (= is 10)", tlin, col )
    tlin = tlin + 1
    call cdputs( "A      change w/e allocations", tlin, col )
    tlin = tlin + 1
    call cdputs( "b      beam armies", tlin, col )
    tlin = tlin + 1
    call cdputs( "B      bombard a planet", tlin, col )
    tlin = tlin + 1
    call cdputs( "C      cloaking device", tlin, col )
    tlin = tlin + 1
    call cdputs( "d      detonate enemy torpedoes", tlin, col )
    tlin = tlin + 1
    call cdputs( "D      detonate your own torpedoes", tlin, col )
    tlin = tlin + 1
    call cdputs( "E      send emergency distress call", tlin, col )
    tlin = tlin + 1
    call cdputs( "f      fire phasers", tlin, col )
    tlin = tlin + 1
    call cdputs( "F      fire phasers, same direction", tlin, col )
    tlin = tlin + 1
    call cdputs( "h      this", tlin, col )
    tlin = tlin + 1
    call cdputs( "H      user history", tlin, col )
    tlin = tlin + 1
    call cdputs( "i      information", tlin, col )
    tlin = tlin + 1
    call cdputs( "I      set user options", tlin, col )
    tlin = tlin + 1
    call cdputs( "k      set course", tlin, col )
    tlin = tlin + 1
    call cdputs( "K      try a coup", tlin, col )
    tlin = tlin + 1
    call cdputs( "L      review old messages", tlin, col )
    tlin = tlin + 1
    call cdputs( "m      send a message", tlin, col )
    tlin = tlin + 1
    call cdputs( "M      strategic map toggle", tlin, col )

    # Now do the right side.
    tlin = lin
    col = 44
    call cdputs( "N      change your name", tlin, col )
    tlin = tlin + 1
    call cdputs( "o      come into orbit", tlin, col )
    tlin = tlin + 1
    call cdputs( "p      launch photon torpedoes", tlin, col )
    tlin = tlin + 1
    call cdputs( "P      launch photon torpedo burst", tlin, col )
    tlin = tlin + 1
    call cdputs( "Q      initiate self-destruct", tlin, col )
    tlin = tlin + 1
    call cdputs( "R      enter repair mode", tlin, col )
    tlin = tlin + 1
    call cdputs( "S      more user statistics", tlin, col )
    tlin = tlin + 1
    call cdputs( "t      engage tractor beams", tlin, col )
    tlin = tlin + 1
    call cdputs( "T      team list", tlin, col )
    tlin = tlin + 1
    call cdputs( "u      un-engage tractor beams", tlin, col )
    tlin = tlin + 1
    call cdputs( "U      user statistics", tlin, col )
    tlin = tlin + 1
    call cdputs( "W      set war or peace", tlin, col )
    tlin = tlin + 1
    call cdputs( "-      lower shields", tlin, col )
    tlin = tlin + 1
    call cdputs( "+      raise shields", tlin, col )
    tlin = tlin + 1
    call cdputs( "/      player list", tlin, col )
    tlin = tlin + 1
    call cdputs( "?      planet list", tlin, col )
    if ( subdcl )
	{
	tlin = tlin + 1
	call cdputs( "$      spawn to DCL", tlin, col )
	}
    tlin = tlin + 1
    call cdputs( "^L     refresh the screen", tlin, col )

    call putpmt( "--- press space when done ---", MSG_LIN2 )
    call cdplay( .true. )
    while ( ! iogtimed( ch, 1 ) & stillalive( csnum ) )
	;

    return

end


###  doinfo - do an info command
#
#  SYNOPSIS
#    integer snum
#    call doinfo( snum )
#
subroutine doinfo( snum )
NOIMPLICIT
integer snum

    character ch, cdgetx
    integer i, j, what, sorpnum, xsorpnum, count, token, now(7), alldig
    logical l, extra, special
    logical findspecial, planmatch, stmatch, safectoi
    include "conqcom2"

    call cdclrl( MSG_LIN1, 2 )

    ch = cdgetx( "Information on: ", MSG_LIN1, 1, TERMS, cbuf, MSGMAXLINE )
    if ( ch == TERM_ABORT )
	{
	call cdclrl( MSG_LIN1, 1 )
	return
	}
    extra = ( ch == TERM_EXTRA )

    # Default to what we did last time.
    call delblanks( cbuf )
    call fold( cbuf )
    if ( cbuf(1) == EOS )
	{
	call strcpy( clastinfostr, cbuf )
	if ( cbuf(1) == EOS )
	    {
	    call cdclrl( MSG_LIN1, 1 )
	    return
	    }
	}
    else
	call strcpy( cbuf, clastinfostr )

    if ( special( cbuf, what, token, count ) )
	{
	if ( ! findspecial( snum, token, count, sorpnum, xsorpnum ) )
	    what = NEAR_NONE
	else if ( extra )
	    if ( xsorpnum == 0 )
		what = NEAR_NONE
	    else
		sorpnum = xsorpnum

	if ( what == NEAR_SHIP )
	    call infoship( sorpnum, snum )
	else if ( what == NEAR_PLANET )
	    call infoplanet( "", sorpnum, snum )
	else
	    call putmsg( "Not found.", MSG_LIN2 )
	}
    else if ( cbuf(1) == 's' & alldig( cbuf(2) ) == YES )
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
    else if ( planmatch( cbuf, j, .false. ) )
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
	call putmsg( "I don't understand.", MSG_LIN2 )
	call cdmove( MSG_LIN1, 1 )
	}

    return

end


###  dolastphase - do a fire phasers same direction command
#
#  SYNOPSIS
#    integer snum
#    call dolastphase( snum )
#
subroutine dolastphase( snum )
NOIMPLICIT
integer snum

    logical phaser
    include "conqcom"

    call cdclrl( MSG_LIN1, 1 )

    if ( scloaked(snum) )
	{
	call putmsg( "The cloaking device is using all available power.",
	    MSG_LIN2 )
	return
	}
    if ( swfuse(snum) > 0 )
	{
	call putmsg( "Weapons are currently overloaded.", MSG_LIN1 )
	return
	}
    if ( sfuel(snum) < PHASER_FUEL )
	{
	call putmsg( "Not enough fuel to fire phasers.", MSG_LIN2 )
	return
	}

    if ( phaser( snum, slastphase(snum) ) )
	call cdclrl( MSG_LIN2, 1 )
    else
        call putmsg( ">PHASERS DRAINED<", MSG_LIN2 )

    return

end


###  domydet - detonate your own torps
#
#  SYNOPSIS
#    integer snum
#    call domydet( snum )
#
subroutine domydet( snum )
NOIMPLICIT
integer snum

    integer j
    include "conqcom"

    call cdclrl( MSG_LIN2, 1 )

    call putmsg( "Detonating...", MSG_LIN1 )

    for ( j = 1; j <= MAXTORPS; j = j + 1 )
	call detonate( snum, j )

    return

end


###  dooption - set user options
#
#  SYNOPSIS
#    integer snum
#    logical dodisplay
#    call dooption( snum, dodisplay )
#
subroutine dooption( snum, dodisplay )
NOIMPLICIT
integer snum
logical dodisplay

    integer i, tok
    character ch
    logical top(MAXOPTIONS), sop(MAXOPTIONS), getoption, iogtimed, stillalive
    include "conqcom"
    include "conqcom2"

    # Make some copies of the current ship options.
    for ( i = 1; i <= MAXOPTIONS; i = i + 1)
	{
	sop(i) = soption(snum,i)			# used in case we abort
	top(i) = soption(snum,i)			# used for dispoption()
	}

    while ( stillalive( csnum ) )
	{
	# Display the current options.
	call dispoption( top )
	call cdplay( .true. )

	# Get a character.
	if ( ! iogtimed( ch, 1 ) )
	    next
	switch ( ch )
	    {
	    case TERM_EXTRA:
		# Done fooling around, update the user options.
		for ( i = 1; i <= MAXOPTIONS; i = i + 1)
		    uoption(suser(snum),i) = top(i)
		break
	    case TERM_ABORT:
		# Decided to abort; restore things.
		for ( i = 1; i <= MAXOPTIONS; i = i + 1)
		    soption(snum,i) = sop(i)
		if ( dodisplay )
		    call display( snum )		# update the display
		break
	    default:
		if ( getoption( ch, tok ) )
		    {
		    # Update temporary.
		    top(tok) = ! top(tok)

		    # Copy temporary into ship for display() to use.
		    soption(snum,tok) = top(tok)

		    if ( dodisplay )
			{
			# Force an update.
			call stoptimer
			call display( snum )
			call settimer
			}
		    }
		else
		    call scbeep
	    }
	}

    call cdclrl( MSG_LIN1, 2 )

    return

end


###  doorbit - orbit the ship and print a message
#
#  SYNOPSIS
#    integer snum
#    call doorbit( snum )
#
subroutine doorbit( snum )
NOIMPLICIT
integer snum

    logical findorbit
    integer pnum
    include "conqcom"
    include "conqcom2"

    if ( ( swarp(snum) == ORBIT_CW ) | ( swarp(snum) == ORBIT_CCW ) )
	call infoplanet( "But we are already orbiting ", -slock(snum), snum )
    else if ( ! findorbit( snum, pnum ) )
	{
	call prints( cbuf, "We are not close enough to orbit, %s.",
	    spname(1,snum) )
	call putmsg( cbuf, MSG_LIN1 )
	call cdclrl( MSG_LIN2, 1 )
	}
    else if ( swarp(snum) > MAX_ORBIT_WARP )
	{
	call prints( cbuf, "We are going to fast to orbit, %s.",
	    spname(1,snum) )
	call putmsg( cbuf, MSG_LIN1 )
	call prints( cbuf, "Maximum orbital insertion velocity is warp %g.",
	    oneplace(MAX_ORBIT_WARP) )
	call putmsg( cbuf, MSG_LIN2 )
	}
    else
	{
	call orbit( snum, pnum )
	call infoplanet( "Coming into orbit around ", -slock(snum), snum )
	}

    return

end


###  dophase - do a fire phasers command
#
#  SYNOPSIS
#    integer snum
#    call dophase( snum )
#
subroutine dophase( snum )
NOIMPLICIT
integer snum

    logical gettarget, phaser
    real dir
    include "conqcom"
    include "conqcom2"

    call cdclrl( MSG_LIN2, 1 )
    if ( scloaked(snum) )
	{
	call putmsg( "The cloaking device is using all available power.",
	    MSG_LIN1 )
	return
	}
    if ( swfuse(snum) > 0 )
	{
	call putmsg( "Weapons are currently overloaded.", MSG_LIN1 )
	return
	}
    if ( sfuel(snum) < PHASER_FUEL )
	{
	call putmsg( "Not enough fuel to fire phasers.", MSG_LIN1 )
	return
	}

    if ( gettarget( "Fire phasers: ", MSG_LIN1, 1, dir, slastblast(snum) ) )
	{
	if ( phaser( snum, dir ) )
	    call putmsg( "Firing phasers...", MSG_LIN2 )
	else
	    call putmsg( ">PHASERS DRAINED<", MSG_LIN2 )
	}
    call cdclrl( MSG_LIN1, 1 )

    return

end


###  doplanlist - display the planet list for a ship
#
#  SYNOPSIS
#    integer snum
#    call doplanlist( snum )
#
subroutine doplanlist( snum )
NOIMPLICIT
integer snum

    character ch
    logical iogtimed, stillalive
    include "conqcom"
    include "conqcom2"

    call cdclear
    while ( stillalive( csnum ) )
	{
	call planlist( steam(snum) )
	call putpmt( "--- press space when done ---", MSG_LIN2 )
	call cdplay( .true. )
	if ( iogtimed( ch, 1 ) )
	    break
	}
    return

end


###  doreview - review messages for a ship
#
#  SYNOPSIS
#    integer snum
#    call doreview( snum )
#
subroutine doreview( snum )
NOIMPLICIT
integer snum

    character ch
    logical review, iogtimed, stillalive
    include "conqcom"
    include "conqcom2"

    if ( ! review( snum, slastmsg(snum) ) )
	{
	call putmsg( "There are no old messages.", MSG_LIN1 )
	call putpmt( "--- press space for more ---", MSG_LIN2 )
	call cdplay( .true. )
	while ( ! iogtimed( ch, 1 ) & stillalive( csnum ) )
	    ;
	call cdclrl( MSG_LIN1, 2 )
	}
    return

end


###  doselfdest - execute a self-destruct command
#
#  SYNOPSIS
#    call doselfdest
#
subroutine doselfdest
NOIMPLICIT

    integer i, j, entertime, now, dsecs
    character cdgetx
    logical ch, iogchar, iochav, stillalive
    include "conqcom"
    include "conqcom2"
    string pmt "Press LINEFEED to initiate self-destruct sequence: "

    call cdclrl( MSG_LIN1, 2 )

    if ( cdgetx( pmt, MSG_LIN1, 1, TERMS, cbuf, MSGMAXLINE ) != TERM_EXTRA )
	{
	# Chickened out.
	call cdclrl( MSG_LIN1, 1 )
	return
	}

    # See if we want to exit to dcl.
    cleave = ( 'q' == cbuf(1) | 'Q' == cbuf(1) )
    call cdclrl( MSG_LIN1, 1 )

    # Set up the destruct fuse.
    ssdfuse(csnum) = SELFDESTRUCT_FUSE

    call gsecs( entertime )

    # Force a screen update.
    call stoptimer
    call display( csnum )
    call settimer
    cmsgok = .true.			# messages are ok in the beginning
    while ( ssdfuse(csnum) > 0 )
	{
	ssdfuse(csnum) = SELFDESTRUCT_FUSE - dsecs ( entertime, now )
	# Display new messages until T-minus 3 seconds.
	if ( ssdfuse(csnum) < 3 )
	    cmsgok = .false.
	if ( ! stillalive( csnum ) )
	    {
	    # Died in the process.
	    ssdfuse(csnum) = 0
	    return
	    }
	if ( iochav( 0 ) )
	    {
	    # Got a new character.
	    call grand( cmsgrand )
	    call cdclrl( MSG_LIN1, 2 )
	    if ( iogchar( ch ) == TERM_ABORT )
		{
		ssdfuse(csnum) = 0
		call putmsg( "Self destruct has been canceled.", MSG_LIN1 )
		return
		}
	    else
		{
		call putmsg( "Press ESCAPE to abort self destruct.", MSG_LIN1 )
		call scbeep
		call cdplay( .true. )
		}
	    }
	call aston			# enable asts so the display will work
	call sleep( ITER_SECONDS )
	call astoff
	}
    cmsgok = .false.			# turn off messages

    if ( dstatus == DS_LIVE )
	if ( dist(sx(csnum), sy(csnum), dx, dy) <= DOOMSDAY_KILL_DIST )
	    {
	    dstatus = DS_OFF
	    call stormsg( MSG_DOOM, MSG_ALL, "AIEEEEEEEE!" )
	    call kill( csnum, KB_GOTDOOMSDAY )
	    }
	else
	    call kill( csnum, KB_SELF )
    else
	call kill( csnum, KB_SELF )

    return

end


###  doshields - raise or lower shields
#
#  SYNOPSIS
#    integer snum
#    logical up
#    call doshields( snum, up )
#
subroutine doshields( snum, up )
NOIMPLICIT
integer snum
logical up

    include "conqcom"

    sshup(snum) = up
    if ( up )
	{
	srmode(snum) = .false.
	call putmsg( "Shields raised.", MSG_LIN1 )
	}
    else
	call putmsg( "Shields lowered.", MSG_LIN1 )
    call cdclrl( MSG_LIN2, 1 )

    return

end


###  doteamlist - display the team list for a ship
#
#  SYNOPSIS
#    integer team
#    call doteamlist( team )
#
subroutine doteamlist( team )
NOIMPLICIT
integer team

    character ch
    logical iogtimed, stillalive
    include "conqcom"
    include "conqcom2"

    call cdclear
    while ( stillalive( csnum ) )
	{
	call teamlist( team )
	call putpmt( "--- press space when done ---", MSG_LIN2 )
	call cdplay( .true. )
	if ( iogtimed( ch, 1 ) )
	    break
	}
    return

end


###  dotorp - launch single torpedoes
#
#  SYNOPSIS
#    integer snum
#    call dotorp( snum )
#
subroutine dotorp( snum )
NOIMPLICIT
integer snum

    logical gettarget, launch
    real dir
    include "conqcom"
    include "conqcom2"

    call cdclrl( MSG_LIN2, 1 )

    if ( scloaked(snum) )
	{
	call putmsg( "The cloaking device is using all available power.",
	    MSG_LIN1 )
	return
	}
    if ( swfuse(snum) > 0 )
	{
	call putmsg( "Weapons are currently overloaded.", MSG_LIN1 )
	return
	}
    if ( sfuel(snum) < TORPEDO_FUEL )
	{
	call putmsg( "Not enough fuel to launch a torpedo.", MSG_LIN1 )
	return
	}
    if ( gettarget( "Launch torpedo: ", MSG_LIN1, 1, dir, slastblast(snum) ) )
	if ( ! launch( snum, dir ) )
	    call putmsg( ">TUBES EMPTY<", MSG_LIN2 )

    call cdclrl( MSG_LIN1, 1 )

    return

end


###  dotow - attempt to tow another ship (DOES LOCKING)
#
#  SYNOPSIS
#    integer snum
#    call dotow( snum )
#
subroutine dotow( snum )
NOIMPLICIT
integer snum

    character cdgetx, ch
    integer i, other
    real rnd
    logical l, warsome, safectoi
    include "conqcom"
    include "conqcom2"

    call cdclrl( MSG_LIN1, 2 )
    if ( stowedby(snum) != 0 )
	{
	call strcpy( "But we are being towed by ", cbuf )
	call appship( stowing(snum), cbuf )
	call appchr( '!', cbuf )
	return
	}
    if ( stowing(snum) != 0 )
	{
	call strcpy( "But we're already towing ", cbuf )
	call appship( stowing(snum), cbuf )
	call appchr( '.', cbuf )
	return
	}
    ch = cdgetx( "Tow which ship? ", MSG_LIN1, 1, TERMS, cbuf, MSGMAXLINE )
    call cdclrl( MSG_LIN1, 1 )
    if ( ch == TERM_ABORT )
	return

    i = 1
    l = safectoi( other, cbuf, i )		# ignore status
    cbuf(1) = EOS

    PVLOCK(lockword)
    if ( other < 1 | other > MAXSHIPS )
	call strcpy( "No such ship.", cbuf )
    else if ( sstatus(other) != SS_LIVE )
	call strcpy( "Not found.", cbuf )
    else if ( other == snum )
	call strcpy( "We can't tow ourselves!", cbuf )
    else if ( dist( sx(snum), sy(snum), sx(other), sy(other) ) > TRACTOR_DIST )
	call strcpy( "That ship is out of tractor range.", cbuf )
    else if ( swarp(other) < 0.0 )
	call strcpy( "You can't tow a ship out of orbit.", cbuf )
    else if ( sqrt( ( sdx(snum) - sdx(other) )**2 +
		     ( sdx(snum) - sdx(other) )**2 ) / ( MM_PER_SEC_PER_WARP *
		     ITER_SECONDS ) > MAX_TRACTOR_WARP )
	call prints( cbuf, "That ships relative velocity is higher than %g.",
	    MAX_TRACTOR_WARP )
    else if ( stowing(other) != 0 | stowedby(other) != 0 )
	call strcpy(
	    "There seems to be some interference with the tractor beams...",
		cbuf )
    else
	{
	stowedby(other) = snum
	stowing(snum) = other
	call strcpy( "Tractor beams engaged.", cbuf )
	}
    PVUNLOCK(lockword)
    call putmsg( cbuf, MSG_LIN2 )

    return

end


###  dountow - release a tow (DOES LOCKING)
#
#  SYNOPSIS
#    integer snum
#    call dountow( snum )
#
subroutine dountow( snum )
NOIMPLICIT
integer snum

    integer entertime, now, dgrand
    logical warsome, stillalive
    real rnd
    include "conqcom"
    include "conqcom2"

    call cdclrl( MSG_LIN1, 2 )
    if ( stowedby(snum) != 0 )
	{
	# If we're at war with him or he's at war with us, make it
	#  hard to break free.
	warsome = ( satwar( snum, stowedby(snum) ) )
	if ( warsome )
	    {
	    call grand( entertime )
	    while ( dgrand( entertime, now ) < BREAKAWAY_GRAND )
		{
		if ( ! stillalive( csnum ) )
		    return
		call aston
		call sleep( ITER_SECONDS )
		call astoff
		}
	    }
	if ( warsome & ( rnd( 0 ) > BREAKAWAY_PROB ) )
	    call putmsg( "Attempt to break free failed.", MSG_LIN1 )
	else
	    {
	    call strcpy( "Breaking free from ship ", cbuf )
	    call appship( stowedby(snum), cbuf )
	    PVLOCK(lockword)
	    if ( stowedby(snum) != 0 )
		{
		# Coast to a stop.
		shead(snum) = shead(stowedby(snum))
		swarp(snum) = swarp(stowedby(snum))

		# Release the tow.
		if ( stowing(stowedby(snum)) != 0 )
		    stowing(stowedby(snum)) = 0
		stowedby(snum) = 0
		}
	    PVUNLOCK(lockword)
	    call appchr( '.', cbuf )
	    call putmsg( cbuf, MSG_LIN1 )
	    }
	}
    else if ( stowing(snum) != 0 )
	{
	call strcpy( "Tow released from ship ", cbuf )
	call appship( stowing(snum), cbuf )
	PVLOCK(lockword)
	if ( stowing(snum) != 0 )
	    {
	    # Set other ship coasting.
	    shead(stowing(snum)) = shead(snum)
	    swarp(stowing(snum)) = swarp(snum)

	    # Release the tow.
	    if ( stowedby(stowing(snum)) != 0 )
		stowedby(stowing(snum)) = 0
	    stowing(snum) = 0
	    }
	PVUNLOCK(lockword)
	call appchr( '.', cbuf )
	call putmsg( cbuf, MSG_LIN1 )
	}
    else
	call putmsg( "No tractor beam activity detected.", MSG_LIN1 )

    return

end


###  dowar - declare war or peace
#
#  SYNOPSIS
#    integer snum
#    call dowar( snum )
#
subroutine dowar( snum )
NOIMPLICIT
integer snum

    integer i, entertime, now, dgrand
    logical tuwar(NUMTEAMS), dowait, iogtimed, stillalive
    character ch, clower
    include "conqcom"
    include "conqcom2"

    for ( i = 1; i <= NUMTEAMS; i = i + 1 )
	tuwar(i) = swar(snum,i)

    call cdclrl( MSG_LIN1, 2 )

    call cdputs(
"Press LINEFEED when done, ESCAPE to abort:  Peace: # # # #  War: # # # #",
	MSG_LIN1, 1 )

    while ( stillalive( csnum ) )
	{
	for ( i = 1; i <= NUMTEAMS; i = i + 1 )
	    if ( tuwar(i) )
		{
		call cdput( ' ', MSG_LIN1, 50+i*2 )
		if ( srwar(snum,i) )
		    ch = chrteams(i)
		else
		    ch = clower(chrteams(i))
		call cdput( ch, MSG_LIN1, 64+i*2 )
		}
	    else
		{
		call cdput( clower(chrteams(i)), MSG_LIN1, 50+i*2 )
		call cdput( ' ', MSG_LIN1, 64+i*2 )
		}
	call cdplay( .true. )
	if ( ! iogtimed( ch, 1 ) )
	    next
	ch = clower( ch )
	if ( ch == TERM_ABORT )
	    break
	if ( ch == TERM_EXTRA )
	    {
	    # Now update the war settings.
	    dowait = .false.
	    for ( i = 1; i <= NUMTEAMS; i = i + 1 )
		{
		if ( tuwar(i) & ! swar(snum,i) )
		    dowait = .true.
		uwar(suser(snum),i) = tuwar(i)
		swar(snum,i) = tuwar(i)
		}

	    # Only check for computer delay when flying.
	    if ( sstatus(snum) != SS_RESERVED & dowait )
		{
		# We've set war with at least one team, stall a little.
		call putmsg(
		    "Reprogramming the battle computer, please stand by...",
		    MSG_LIN2 )
		call cdplay( .true. )
		call grand( entertime )
		while ( dgrand( entertime, now ) < REARM_GRAND )
		    {
		    # See if we're still alive.
		    if ( ! stillalive( csnum ) )
			return

		    # Sleep (and enable asts so the display will work).
		    call aston
		    call sleep( ITER_SECONDS )
		    call astoff
		    }
		}
	    break
	    }

	for ( i = 1; i <= NUMTEAMS; i = i + 1 )
	    if ( ch == clower( chrteams(i) ) )
		{
		if ( ! tuwar(i) | ! srwar(snum,i) )
		    {
		    tuwar(i) = ! tuwar(i)
		    next 2
		    }
		break
		}
	call scbeep
	}

    call cdclrl( MSG_LIN1, 2 )

    return

end


###  dowarp - set warp factor
#
#  SYNOPSIS
#    integer snum
#    real warp
#    call dowarp( snum, warp )
#
subroutine dowarp( snum, warp )
NOIMPLICIT
integer snum
real warp

    real mw
    logical usefuel
    include "conqcom"
    include "conqcom2"

    call cdclrl( MSG_LIN2, 1 )

    if ( sdwarp(snum) == 0.0 & warp != 0.0 )
	{
	# See if engines are working.
	if ( sefuse(snum) > 0 )
	    {
	    call putmsg( "Engines are currently overloaded.", MSG_LIN1 )
	    return
	    }

	# No charge if already warp 0.
	if ( ! usefuel( snum, ENGINES_ON_FUEL, .false. ) )
	    {
	    call putmsg( "We don't have enough fuel.", MSG_LIN1 )
	    return
	    }

	# Don't stop repairing if changing to warp 0.
	srmode(snum) = .false.
        }

    # If orbitting, break orbit.
    if ( swarp(snum) < 0.0 )
	{
	swarp(snum) = 0.0
	slock(snum) = 0
	sdhead(snum) = shead(snum)
	}

    # Handle ship limitations.
    sdwarp(snum) = min( warp, warplim(steam(snum)) )

    call prints( cbuf, "Warp %g.", around(sdwarp(snum)) )
    call putmsg( cbuf, MSG_LIN1 )

    # Warn about damage limitations.
    mw = maxwarp( snum )
    if ( around( sdwarp(snum) ) > mw )
	{
	call prints( cbuf, "(Due to damage, warp is currently limited to %g.)",
	    mw )
	call putmsg( cbuf, MSG_LIN2 )
	}

    return

end


###  getoption - decode character into option
#
#  SYNOPSIS
#    logical flag, getoption
#    character ch
#    integer tok
#    flag = getoption( ch, tok )
#
logical function getoption( ch, tok )
NOIMPLICIT
character ch
integer tok

    switch ( ch )
	{
	case 'g':
	    tok = OPT_PHASERGRAPHICS
	case 'p':
	    tok = OPT_PLANETNAMES
	case 'a':
	    tok = OPT_ALARMBELL
	case 'i':
	    tok = OPT_INTRUDERALERT
	case 'n':
	    tok = OPT_NUMERICMAP
	case 't':
	    tok = OPT_TERSE
	case 'e':
	    tok = OPT_EXPLOSIONS
	default:
	    tok = 0
	    return ( .false. )
	}
    return ( .true. )

end


###  gretds - block letter "greetings..."
#
#  SYNOPSIS
#    call gretds
#
subroutine gretds
NOIMPLICIT

    integer col, length
    string g1 " GGG   RRRR   EEEEE  EEEEE  TTTTT   III   N   N   GGG    SSSS"
    string g2 "G   G  R   R  E      E        T      I    NN  N  G   G  S"
    string g3 "G      RRRR   EEE    EEE      T      I    N N N  G       SSS"
    string g4 "G  GG  R  R   E      E        T      I    N  NN  G  GG      S  ..  ..  .."
    string g5 " GGG   R   R  EEEEE  EEEEE    T     III   N   N   GGG   SSSS   ..  ..  .."
    include "conqcom2"

    col = (cmaxcol-length(g5)) / 2
    call cdputs( g1, 1, col )
    call cdputs( g2, 2, col )
    call cdputs( g3, 3, col )
    call cdputs( g4, 4, col )
    call cdputs( g5, 5, col )

    return

end


###  menu - main user menu (DOES LOCKING)
#
#  SYNOPSIS
#    call menu
#
subroutine menu
NOIMPLICIT

    integer i, lin, col, sleepy, countdown, modp1
    character ch
    logical lose, oclosed, confirm, switchteams, multiple, redraw
    logical iogtimed, stillalive
    include "conqcom"
    include "conqcom2"

    # Initialize statistics.
    call initstats( sctime(csnum), setime(csnum) )

    # Log this entry into the Game.
    call loghist( cunum )

    # Set up a few ship characteristics here rather than in initship().
    suser(csnum) = cunum
    steam(csnum) = uteam(cunum)
    spid(csnum) = cpid
    for ( i = 1; i <= MAXOPTIONS; i = i + 1 )
	soption(csnum,i) = uoption(cunum,i)
    for ( i = 1; i <= NUMTEAMS; i = i + 1 )
	{
	srwar(csnum,i) = .false.
	swar(csnum,i) = uwar(cunum,i)
	}
    call stcpn( upname(1,cunum), spname(1,csnum), MAXUSERPNAME )

    # Set up some things for the menu display.
    switchteams = uooption(cunum,OOPT_SWITCHTEAMS)
    multiple = uooption(cunum,OOPT_MULTIPLE)
    oclosed = closed
    cleave = .false.
    redraw = .true.
    sleepy = 0
    countdown = 0

    repeat
	{
	# Make sure things are proper.
	if ( csnum < 1 | csnum > MAXSHIPS )
	    lose = .true.
	else if ( spid(csnum) != cpid )
	    lose = .true.
	else if ( sstatus(csnum) != SS_RESERVED )
	    {
	    call cerror( MSG_GOD, "menu: Ship %d no longer reserved.", csnum )
	    lose = .true.
	    }
	else
	    lose = .false.
	if ( lose )				# again, Jorge?
	    {
	    # We reincarnated or else something bad happened.
	    lin = 7
	    col = 11
	    call cdclear
	    call cdredo
	    call cdputs(
		"Suddenly  a  sinister,  wraithlike  figure appears before you",
		lin, col )
	    lin = lin + 1
	    call cdputs(
		"seeming to float in the air.  In a low,  sorrowful  voice  he",
		lin, col )
	    lin = lin + 1
	    call cdputs(
	       "says, @"Alas, the very nature of the universe has changed, and",
		lin, col )
	    lin = lin + 1
	    call cdputs(
	       "your ship cannot be found.  All must now pass away.@"  Raising",
		lin, col )
	    lin = lin + 1
	    call cdputs(
		"his  oaken  staff  in  farewell,  he fades into the spreading",
		lin, col )
	    lin = lin + 1
	    call cdputs(
		"darkness.  In his place appears a  tastefully  lettered  sign",
		lin, col )
	    lin = lin + 1
	    call cdputs( "reading:", lin, col )
	    lin = lin + 2
	    call cdputc( "INITIALIZATION FAILURE", lin )
	    lin = lin + 2
	    call cdputs(
	       "The darkness becomes all encompassing, and your vision fails.",
		lin, col )
	    call ioeat
	    call cdmove( 1, 1 )
	    call cdplay( .false. )
	    return
	    }

	# Some simple housekeeping.
	if ( multiple != uooption(cunum,OOPT_MULTIPLE) )
	    {
	    multiple = ! multiple
	    redraw = .true.
	    }
	if ( switchteams != ( uooption(cunum,OOPT_SWITCHTEAMS) |
			      ustats(cunum,USTAT_ENTRIES) <= 0 ) )
	    {
	    switchteams = ! switchteams
	    redraw = .true.
	    }
	if ( oclosed != closed )
	    {
	    oclosed = ! oclosed
	    redraw = .true.
	    }
	if ( redraw )
	    {
	    call conqds( multiple, switchteams )
	    redraw = .false.
	    }
	else
	    call cdclrl( MSG_LIN1, 2 )

	call userline( -1, -1, cbuf, .false., .true. )
	call cdputs( cbuf, MSG_LIN1, 1 )
	call userline( cunum, csnum, cbuf, .false., .true. )
	call cdputs( cbuf, MSG_LIN2, 1 )

	call cdmove( 1, 1 )
	call cdplay( .true. )

	# Try to kill the driver if we started one the last time
	#  we played and we've been in the menu long enough.
	if ( countdown > 0 )
	    {
	    countdown = countdown - 1
	    if ( countdown <= 0 )
		call drkill
	    }

	# Reset up the destruct fuse.
	ssdfuse(csnum) = -TIMEOUT_PLAYER

	# Get a character with timeout.
	if ( ! iogtimed( ch, 1 ) )
	    {
	    # We get here if a character hasn't been typed.
	    sleepy = sleepy + 1
	    if ( sleepy > 300 )
		break
	    next
	    }

	# Got a character, zero timeout.
	sleepy = 0
	switch ( ch )
	    {
	    case 'e':
		call play
		if ( childpid != 0 )
		    countdown = 15
		else
		    countdown = 0
		redraw = .true.
	    case 'h':
		call helplesson
		redraw = .true.
	    case 'H':
		call histlist( .false. )
		redraw = .true.
	    case 'I':
		call dooption( csnum, .false. )
	    case 'L':
		call doreview( csnum )
	    case 'n':
		if ( ! cnewsfile )
		    call scbeep
		else
		    {
		    call news
		    redraw = .true.
		    }
	    case 'N':
		call pseudo( cunum, csnum )
	    case 'r':
		if ( multiple )
		    call scbeep
		else
		    {
		    for ( i = 1; i <= MAXSHIPS; i = i + 1 )
			if ( sstatus(i) == SS_LIVE |
			     sstatus(i) == SS_ENTERING )
			    if ( suser(i) == cunum )
				break
		    if ( i <= MAXSHIPS )
			call scbeep
		    else
			{
			call cdclrl( MSG_LIN1, 2 )
			call cdplay( .false. )
			if ( confirm( 0 ) )
			    {
			    call resign( cunum )
			    break
			    }
			}
		    }
	    case 's':
		if ( ! multiple & ! switchteams )
		    call scbeep
		else
		    {
		    steam(csnum) = modp1( steam(csnum)+1, NUMTEAMS )
		    uteam(cunum) = steam(csnum)
		    swar(csnum,steam(csnum)) = .false.
		    uwar(cunum,uteam(cunum)) = .false.
		    }
	    case 'S':
		call userstats( .false. )
		redraw = .true.
	    case 'T':
		call doteamlist( steam(csnum) )
		redraw = .true.
	    case 'U':
		call userlist( .false. )
		redraw = .true.
	    case 'W':
		call dowar( csnum )
		redraw = .true.
	    case 'q', 'Q':
		break
	    case '/':
		call playlist( .false., .false. )
		redraw = .true.
	    case '?':
		call doplanlist( csnum )
		redraw = .true.
	    case '@^L':
		call cdredo
	    case ' ', TERM_NORMAL, '@^S', '@^Q':
		# Do nothing.
	    default:
		call scbeep
	    }
	}
    until ( ! stillalive( csnum ) | cleave )

    # Make our ship available for others to use.
    if ( sstatus(csnum) == SS_RESERVED )
	{
	call conqstats( csnum )
	PVLOCK(lockword)
	ssdfuse(csnum) = 0
	sstatus(csnum) = SS_OFF
	PVUNLOCK(lockword)
	}

    return

end


###  newship - create a new ship for a user (DOES LOCKING)
#
#  SYNOPSIS
#    integer status, newship, unum, snum
#    logical flag, newship
#    flag = newship( unum, snum )
#
logical function newship( unum, snum )
NOIMPLICIT
integer unum, snum

    integer i, j, system, rndint
    real rnduni
    logical fresh, capentry
    character cdgetx
    include "conqcom"
    include "conqcom2"

    PVLOCK(lockword)

    sstatus(snum) = SS_ENTERING			# show intent to fly
    fresh = .true.				# assume we want a fresh ship

    # Count number of his ships flying.
    j = 0
    for ( i = 1; i <= MAXSHIPS; i = i + 1 )
	if ( sstatus(i) == SS_LIVE | sstatus(i) == SS_ENTERING )
	    if ( suser(i) == unum & snum != i )
		j = j + 1
    PVUNLOCK(lockword)

    if ( ! uooption(unum,OOPT_MULTIPLE) )
	{
	# Isn't a multiple; see if we need to reincarnate.
	if ( j > 0 )
	    {
	    # Need to reincarnate.
	    call cdclear
	    call cdredo

	    i = MSG_LIN2/2
	    j = 15
	    call cdputs( "You're already playing on another ship." , i, j )
	    if ( cdgetx( "Press LINEFEED to reincarnate to this ship: ",
		i + 1, j, TERMS, cbuf, MSGMAXLINE ) != TERM_EXTRA )
		{
		sstatus(snum) = SS_RESERVED
		return ( .false. )
		}

	    # Look for a live ship for us to take.
	    PVLOCK(lockword)
	    for ( i = 1; i <= MAXSHIPS; i = i + 1)
		if ( suser(i) == unum & sstatus(i) == SS_LIVE )
		    {
		    fresh = .false.
		    sstatus(snum) = SS_OFF
		    snum = i
		    spid(snum) = cpid
		    sstatus(snum) = SS_ENTERING
		    break
		    }
	    PVUNLOCK(lockword)
	    }
	}
    else
	{
	# Is a multiple.
	if ( j >= umultiple(unum) )
	    {
	    # Flying too many ships
	    sstatus(snum) = SS_RESERVED
	    call cdclear
	    call cdredo
	    i = MSG_LIN2/2
	    call cdputc(
		"I'm sorry, but your playing on too many ships right now.", i )
	    i = i + 1
	    call strcpy( "You are only allowed to fly ", cbuf )
	    j = umultiple(unum)
	    call appint( j, cbuf )
	    call appstr( " ship", cbuf )
	    if ( j != 1 )
		call appchr( 's', cbuf )
	    call appstr( " at one time.", cbuf )
	    call cdputc( cbuf, i )
	    call cdplay( .false. )
	    call sleep( 2.0 )
	    return ( .false. )
	    }
	}

    # Figure out which system to enter.
    if ( fresh )
	{
	system = steam(snum)
	if ( ! capentry( snum, system ) )
	    {
	    sstatus(snum) = SS_RESERVED
	    return ( .false. )
	    }
	}

    PVLOCK(lockword)

    # If necessary, initalize the ship
    if ( fresh )
	{
	call initship( snum, unum )

	# Randomly position the ship near the home sun (or planet).
	if ( pprimary(homeplanet(system)) == homesun(system) )
	    i = homesun(system)
	else
	    i = homeplanet(system)
	call putship( snum, px(i), py(i) )
	sdhead(snum) = rnduni( 0.0, 359.9 )
	shead(snum) = sdhead(snum)
	sdwarp(snum) = float( rndint( 2, 5 ) )		#~~~ this is a kludge
	slock(snum) = -homeplanet(system)
	}

    # Never a robot here.
    srobot(snum) = .false.
    saction(snum) = 0

    # Straighten out the ships deltas.
    call fixdeltas( snum )

    # Finally, turn the ship on.
    sstatus(snum) = SS_LIVE

    PVUNLOCK(lockword)

    return ( .true. )

end


###  play - play the game
#
#  SYNOPSIS
#    call play
#
subroutine play
NOIMPLICIT

    integer laststat, now, dsecs
    character ch
    logical newship, iogtimed, stillalive
    include "conqcom"
    include "conqcom2"

    # Can't carry on without a vessel.
    if ( ! newship( cunum, csnum ) )
	return

    call drstart				# start a driver, if necessary
    ssdfuse(csnum) = 0				# zero self destruct fuse
    call grand( cmsgrand )			# initialize message timer
    cleave = .false.				# assume we won't want to bail
    credraw = .true.				# want redraw first time
    cdisplay = .true.				# ok to display
    cmsgok = .true.				# ok to get messages
    call cdclear				# clear the display
    call cdredo					#  (quickly)
    call display( csnum )			# update the screen manually
    call gsecs( laststat )			# initialize stat timer
    call astoff					# disable before setting timer
    call settimer				# setup for next second

    # While we're alive, field commands and process them.
    while ( stillalive( csnum ) )
	{
	# Make sure we still control our ship.
	if ( spid(csnum) != cpid )
	    break

	# Get a character with one second timeout.
	if ( iogtimed( ch, 1 ) )
	    {
	    cmsgok = .false.
	    call command( ch )
	    call grand( cmsgrand )
	    cmsgok = .true.
	    call cdplay( .true. )
	    }

	# See if it's time to update the statistics.
	if ( dsecs( laststat, now ) >= 15 )
	    {
	    call conqstats( csnum )
	    laststat = now
	    }
	}

    cdisplay = .false.
    call conqstats( csnum )
    call upchuck

    # Asts are still enabled, simply cancel the next screen update.
    call stoptimer
    call aston					# enable asts again

    call dead( csnum, cleave )

    return

end


###  welcome - entry routine
#
#  SYNOPSIS
#    logical flag, welcome
#    integer unum
#    flag = welcome( unum )
#
logical function welcome( unum )
NOIMPLICIT
integer unum

    integer i, team, rndint, length
    logical findship, register, gunum
    character cupper, name(MAXUSERNAME)
    include "conqcom"
    include "conqcom2"
    string sorry1 "I'm sorry, but the game is closed for repairs right now."
    string sorry2 "I'm sorry, but there is no room for a new player right now."
    string sorry3 "I'm sorry, but you are not allowed to play right now."
    string sorryn "Please try again some other time.  Thank you."

    call glname( name )
    if ( ! gunum( unum, name ) )
	{
	# Must be a new player.
        call cdclear
	call cdredo
        if ( closed )
	    {
	    # Can't enroll if the game is closed.
	    call cdputc( sorry1, MSG_LIN2/2 )
	    call cdputc( sorryn, MSG_LIN2/2+1 )
	    call cdmove( 1, 1 )
	    call cdplay( .false. )
            call sleep( 2.0 )
	    return ( .false. )
	    }
	team = rndint( 1, NUMTEAMS )
	cbuf(1) = EOS
	call apptitle( team, cbuf )
	call appchr( ' ', cbuf )
	i = length( cbuf ) + 1
	call appstr( name, cbuf )
	cbuf(i) = cupper( cbuf(i) )
	if ( ! register( name, cbuf, team, unum ) )
	    {
	    call cdputc( sorry2, MSG_LIN2/2 )
	    call cdputc( sorryn, MSG_LIN2/2+1 )
	    call cdmove( 1, 1 )
	    call cdplay( .false. )
	    call sleep( 2.0 )
	    return ( .false. )
	    }
        call gretds
        call strcpy( "You have been selected to command a", cbuf )
        if ( vowel( tname(1,team) ) )
	    call appchr( 'n', cbuf )
        call appchr( ' ', cbuf )
        call appstr( tname(1,team), cbuf )
        call appstr( " starship.", cbuf )
        call cdputc( cbuf, MSG_LIN2/2 )
        call cdputc( "Prepare to be beamed aboard...", MSG_LIN2/2+1 )
	call cdmove( 1, 1 )
        call cdplay( .false. )
        call sleep( 3.0 )
        }

    # Must be special to play when closed.
    if ( closed & ! uooption(unum,OOPT_PLAYWHENCLOSED) )
	{
	call cdclear
	call cdredo
	call cdputc( sorry1, MSG_LIN2/2 )
	call cdputc( sorryn, MSG_LIN2/2+1 )
	call cdmove( 1, 1 )
	call cdplay( .false. )
	call sleep( 2.0 )
	return ( .false. )
	}

    # Can't play if on the shit list.
    if ( uooption(unum,OOPT_SHITLIST) )
        {
	call cdclear
	call cdredo
        call cdputc( sorry3, MSG_LIN2/2 )
        call cdputc( sorryn, MSG_LIN2/2+1 )
	call cdmove( 1, 1 )
        call cdplay( .false. )
        call sleep( 2.0 )
        return ( .false. )
        }

    # Can't play without a ship.
    if ( ! findship( csnum ) )
	{
	call cdclear
	call cdredo
	call cdputc( "I'm sorry, but there are no ships available right now.",
	    MSG_LIN2/2 )
        call cdputc( sorryn, MSG_LIN2/2+1 )
	call cdmove( 1, 1 )
	call cdplay( .false. )
	call sleep( 2.0 )
	return ( .false. )
	}

    return ( .true. )

end
