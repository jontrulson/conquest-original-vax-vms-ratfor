###############################################################################
#
#                                 C O N Q A I
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

define(CONQAICOMMON,integer nenum; logical debug; real dne, ane
    common /conqaicommon/ nenum, debug, dne, ane)


###  conqai - robot AI test program
#
DRIVER(conqai)
NOIMPLICIT

    integer i, getarg
    character arg(MAXLINE)
    string usage "usage: conqai [-vd]"
    CONQAICOMMON
    include "conqcom"

    # First things first.
    if ( commonrev != COMMONSTAMP )
	call error( "conqai: Common block ident mismatch." )

    debug = .false.

    while ( getarg( 1, arg, MAXLINE ) != EOF )
	{
	if ( arg(1) != '-' )
	    call error( usage )
	for ( i = 2; arg(i) != EOS; i = i + 1 )
	    switch ( arg(i) )
		{
		case 'D', 'd':
		    debug = .true.
		default:
		    call error( usage )
		}
	call delarg( 1 )
	}

    call robotloop

DRETURN

end


###  buildai - construct the robot data base
#
#  SYNOPSIS
#    integer snum, vars(MAX_VAR), nenum
#    real dne, ane
#    call buildai( snum, vars, nenum, dne, ane )
#
#  DESCRIPTION
#    Fill up the passed array with robot info.
#
subroutine buildai( snum, vars, nenum, dne, ane )
NOIMPLICIT
integer snum, vars(ARB), nenum
real dne, ane

    # i = AIRANGE( j )
    define(AIRANGE,min(max($1,0),9))
    # AISCALE( var, value, scale )
    define(AISCALE,$1 = AIRANGE( round( $2 / $3 )))
    # AIDIST( var, dist )
    define(AIDIST,
	{zzzx = min( $2, 10000.0 )
	$1 = AIRANGE( int( 0.99026 + zzzx * (1.58428e-3 + zzzx * -59.2572e-9)))
	})
    # AIBOOLEAN( var, expr )
    define(AIBOOLEAN,
	{if ( $2 )
	    $1 = 1
	else
	    $1 = 0})
    integer i, j, xnenum, rndint
    real dam, x, y, zzzx, angle, explosion, phaserhit, rndnor
    logical findspecial
    include "conqcom"

    # Initialize to zeros.
    for ( i = 1; i <= MAX_VAR; i = i + 1 )
	vars(i) = 0

    # Random number (1)
    vars(VAR_RANDOM) = rndint( 0, 9 )

    # Distance to nearest enemy (dist)
    ane = 0.0
    dne = 1e9
    if ( findspecial( snum, SPECIAL_ENEMYSHIP, 0, nenum, xnenum ) )
	{
	if ( scloaked(nenum) )
	    {
	    x = rndnor( sx(nenum), CLOAK_SMEAR_DIST )
	    y = rndnor( sy(nenum), CLOAK_SMEAR_DIST )
	    }
	else
	    {
	    x = sx(nenum)
	    y = sy(nenum)
	    }
	dne = dist( sx(snum), sy(snum), x, y )
	ane = angle( sx(snum), sy(snum), x, y )

	# Enemy is cloaked (-)
	if ( dne < ACCINFO_DIST )
	    {
	    AIBOOLEAN( vars(VAR_ENEMYCLOAKED), scloaked(nenum) < 0 )
	    AISCALE( vars(VAR_ENEMYDAMAGE), sdamage(nenum), 10.0 )
	    }
	}
    AIDIST( vars(VAR_DNE), dne )

    # Ship damage (10)
    AISCALE( vars(VAR_DAMAGE), sdamage(snum), 10.0 )

    # Possible ship damage from enemy torps (10)
    if ( stalert(snum) )
	{
	dam = 0.0
	for ( i = 1; i <= MAXSHIPS; i = i + 1 )
	    if ( sstatus(i) != SS_OFF & i != snum )
		for ( j = 1; j <= MAXTORPS; j = j + 1 )
		    if ( tstatus(i,j) == TS_LIVE )
			if ( twar(i,j,steam(snum)) | swar(snum,steam(i)) )
			    {
			    # Just guess at other ships efficiency.
			    dam = dam + explosion(
				TORPEDO_HIT * 1.1 * weafac(steam(i)),
				dist(sx(snum),sy(snum),tx(i,j),ty(i,j)) )
			    }
	AISCALE( vars(VAR_INCOMING), dam, 10.0 )
	}

    # Ship fuel (10)
    AISCALE( vars(VAR_FUEL), sfuel(snum), 10.0 )

    # Number of torps available to fire (1)
    j = 0
    for ( i = 1; i <= MAXTORPS; i = i + 1 )
	if ( tstatus(snum,i) == TS_OFF )
	    j = j + 1
    AISCALE( vars(VAR_NUMTORPS), j, 1.0 )

    # Ship shields (10)
    AISCALE( vars(VAR_SHIELDS), sshields(snum), 10.0 )

    # Ship engine temperature (10)
    AISCALE( vars(VAR_ETEMP), setemp(snum), 10.0 )

    # Ship weapon temperature (10)
    AISCALE( vars(VAR_WTEMP), swtemp(snum), 10.0 )

    # Possible phaser damage to nearest enemy (5)
    AISCALE( vars(VAR_PHASERDAM), phaserhit( snum, dne ), 5.0 )

    # Possible damage per torpedo to nearest enemy (5)
    AISCALE( vars(VAR_TORPDAM),
	explosion( TORPEDO_HIT * weaeff( snum ), dne*0.66 ), 5.0 )

    # Ship warp (1)
    AISCALE( vars(VAR_WARP), sdwarp(snum), 1.0 )

    # Ship shields are up (-)
    AIBOOLEAN( vars(VAR_SHUP), sshup(snum) )

    # Are in repair mode (-)
    AIBOOLEAN( vars(VAR_REPAIRING), srmode(snum) )

    # Are cloaked (-)
    AIBOOLEAN( vars(VAR_CLOAKED), scloaked(snum) )

    # Weapons are allocated (-)
    AIBOOLEAN( vars(VAR_WALLOC), sweapons(snum) > 50 )

    # Are in orbit (-)
    AIBOOLEAN( vars(VAR_ORBITING), swarp(snum) < 0.0 )

    # Can read a message (-)
    AIBOOLEAN( vars(VAR_CANREAD), slastmsg(snum) != lastmsg )

    return

end


###  defend - create a robot ship to defend the home system
#
#  SYNOPSIS
#    integer snum, pnum
#    call defend( attacker, pnum )
#
subroutine defend( attacker, pnum )
NOIMPLICIT
integer attacker, pnum

    integer i, j, k, team, snum, unum, rndint
    logical l, newrob
    character buf(MSGMAXLINE)
    include "conqcom"

    team = pteam(pnum)
    # Must be for a "fighting" team.
    if ( team < 1 | team > NUMTEAMS )
	return

    # Must be for a home system planet.
    if ( pnum != teamplanets(team,1) &
	 pnum != teamplanets(team,2) &
	 pnum != teamplanets(team,3) )
	return

    # See if there are any team ships to defend.
    for ( i = 1; i <= MAXSHIPS; i = i + 1 )
	if ( sstatus(i) == SS_LIVE )
	    if ( steam(i) == team )
		return

    # Count how many robot users are on the right team and can play.
    j = 0
    for ( i = 1; i <= MAXUSERS; i = i + 1 )
	if ( ulive(i) )
	    if ( urobot(i) & uteam(i) == team & ! uooption(unum,OOPT_SHITLIST) )
		j = j + 1

    # No one to defend.
    if ( j <= 0 )
	return

    # Pick one.
    k = rndint( 1, j )
    unum = 0
    j = 0
    for ( i = 1; i <= MAXUSERS; i = i + 1 )
	if ( ulive(i) )
	    if ( urobot(i) & uteam(i) == team & ! uooption(unum,OOPT_SHITLIST) )
		{
		j = j + 1
		if ( j == k )
		    {
		    unum = i
		    break
		    }
		}

    # See if any anything funny happened while we were looping...
    if ( unum == 0 )
	return

    # Make a robot.
    if ( newrob( snum, unum ) )
	{
	call prints( buf,
	    "WARNING: You have violated %s space; prepare to die.",
	    tname(1,team) )
	call stormsg( snum, attacker, buf )
	}

    return

end


###  displayai - display the selected robot action on STDOUT
#
#  SYNOPSIS
#    integer snum, token, vars()
#    call displayai( snum, token, vars )
#
#  DESCRIPTION
#    Display the robot action on STDOUT so the implementors can
#    see if this darn thing works.
#
subroutine displayai( snum, token, vars )
NOIMPLICIT
integer snum, token, vars(ARB)

    integer i
    character buf(MAXLINE)

    call printf( "displayai: %2d ", snum )
    for ( i = 1; i <= MAX_VAR; i = i + 1 )
	call printf( "%d", vars(i) )
    call robstr( token, buf )
    call printf( ", %s@n", buf )

    return

end


###  executeai - execute the selected robot action (DOES LOCKING)
#
#  SYNOPSIS
#    integer snum, token
#    call executeai( snum, token )
#
#  DESCRIPTION
#    Execute the robot action.
#
subroutine executeai( snum, token )
NOIMPLICIT
integer snum, token

    # SETWARP( warp )
    define(SETWARP,
	{if ( swarp(snum) < 0.0 )
	    {
	    # Break orbit.
	    swarp(snum) = 0.0
	    slock(snum) = 0
	    sdhead(snum) = shead(snum)
	    }
	if ( $1 > 0.0 )
	    srmode(snum) = .false.
	sdwarp(snum) = $1})
    # SETCOURSE( course )
    define(SETCOURSE,
	{if ( swarp(snum) < 0.0 )
	    swarp(snum) = 0.0			# break orbit
	slock(snum) = 0
	sdhead(snum) = $1})
    # SETLOCK( pnum )
    define(SETLOCK,
	{if ( slock(snum) != -$1 )
	    {
	    # Don't break orbit to unless we're not there yet.
	    if ( swarp(snum) < 0.0 )
		swarp(snum) = 0.0
	    slock(snum) = -$1
	    }})

    integer i, j, modp1
    real x, rnduni, mod360
    logical l, canread, findspecial, launch, phaser, enemydet
    character buf(MAXLINE)
    include "conqcom"
    CONQAICOMMON

    # Update ship action.
    saction(snum) = token

    # Execute the action!
    switch ( token )
	{
	case ROB_NOOP:
	    # Null!
	case ROB_GOHOME:
	    if ( findspecial( snum, SPECIAL_HOMEPLANET, 0, i, j ) )
		SETLOCK( i )
	    else if ( findspecial( snum, SPECIAL_FUELPLANET, 0, i, j ) )
		SETLOCK( i )
	case ROB_GOFUEL:
	    if ( findspecial( snum, SPECIAL_FUELPLANET, 0, i, j ) )
		SETLOCK( i )
	case ROB_GOREPAIR:
	    if ( findspecial( snum, SPECIAL_REPAIRPLANET, 0, i, j ) )
		SETLOCK( i )
	case ROB_ALLOCATE:
	    i = sweapons(snum)
	    sweapons(snum) = sengines(snum)
	    sengines(snum) = i
	case ROB_PHASER:
	    l = phaser( snum, ane )
	case ROB_TORPEDO:
	    l = launch( snum, ane )
	case ROB_BURST:
	    l = launch( snum, ane )
	    l = launch( snum, ane )
	    l = launch( snum, ane )
	case ROB_SHIELD:
	    sshup(snum) = ! sshup(snum)
	case ROB_WARP_0:
	    SETWARP( 0.0 )
	case ROB_WARP_2:
	    SETWARP( 2.0 )
	case ROB_WARP_5:
	    SETWARP( 5.0 )
	case ROB_WARP_8:
	    SETWARP( 8.0 )
	case ROB_TRACK:
	    SETCOURSE( mod360( ane + rnduni( -10.0, 10.0 ) ) )
	case ROB_RUNAWAY:
	    SETCOURSE( mod360( ane + 180.0 + rnduni( -10.0, 10.0 ) ) )
	case ROB_SLIENT:
	    if ( ! scloaked(snum) )
		sdwarp(snum) = 0.0
	    scloaked(snum) = ! scloaked(snum)
	case ROB_INSULT:
	    call robreply( buf )
	    call stormsg( snum, nenum, buf )
	case ROB_READMSG:
	    # Try to read a message and reply to it
	    while ( slastmsg(snum) != lastmsg )
		{
		slastmsg(snum) = modp1( slastmsg(snum) + 1, MAXMESSAGES )
		i = slastmsg(snum)
		if ( canread( snum, i ) )
		    {
		    j = msgfrom(i)
		    if ( -j >= 1 & -j <= NUMPLANETS )
			next			# don't talk back to planets
		    if ( j >= 1 & j <= MAXSHIPS )
			if ( srobot(j) )
			    next		# don't talk back to robots
		    call robreply( buf )
		    call stormsg( snum, j, buf )
		    break
		    }
		}
	case ROB_MESSAGE:
	    call stormsg( snum, MSG_ALL, "Give me drugs." )
	case ROB_TAKEDRUGS:
	    call stormsg( snum, MSG_ALL, "I'm on drugs." )
	case ROB_DETONATE:
	    l = enemydet( snum )
	case ROB_MYDETONATE:
	    for ( i = 1; i <= MAXTORPS; i = i + 1 )
		call detonate( snum, i )
	case ROB_UNTRACTOR:
	    # Only attempt to untractor if we don't have to delay.
	    if ( stowedby(snum) != 0 )
		if ( ! satwar(snum, stowedby(snum)) )
		    {
		    stowing(stowedby(snum)) = 0
		    stowedby(snum) = 0
		    }
	case ROB_REPAIR:
	    srmode(snum) = .true.
	default:
	    call robstr( token, buf )
	    call printf( "executeai: Unknown token %s@n", buf )
	}

    return

end


###  exitai - exit handler
#
#  SYNOPSIS
#    external exitai
#
subroutine exitai
NOIMPLICIT

    include "conqcom"

    externrobots = .false.

    return

end


###  newrob - create a robot ship (DOES LOCKING)
#
#  SYNOPSIS
#    logical ok, newrob
#    integer snum, unum
#    ok = newrob( snum, unum )
#
logical function newrob( snum, unum )
NOIMPLICIT
integer snum, unum

    integer i, j
    logical findship
    include "conqcom"

    # Check the user number.
    if ( ! ulive(unum) )
	return ( .false. )

    # Check for religious trouble.
    if ( uooption(unum,OOPT_SHITLIST) )
	return ( .false. )

    # Can't do anything with out a ship.
    if ( ! findship( snum ) )
	return ( .false. )

    # Show intent to fly.
    PVLOCK(lockword)
    sstatus(snum) = SS_ENTERING

    # Count number of ships currently flying.
    j = 0
    for ( i = 1; i <= MAXSHIPS; i = i + 1 )
	if ( sstatus(i) == SS_LIVE | sstatus(i) == SS_ENTERING )
	    if ( suser(i) == unum & snum != i )
		j = j + 1

    # Check if multiple restrictions apply.
    if ( uooption(unum,OOPT_MULTIPLE) )
	{
	# If a multiple, he can only fly so many ships.
	if ( j >= umultiple(unum) )
	    sstatus(snum) = SS_OFF
	}
    else
	{
	# If not a multiple, he can't be flying anywhere else.
	if ( j > 0 )
	    sstatus(snum) = SS_OFF
	}
    PVUNLOCK(lockword)

    if ( sstatus(snum) == SS_OFF )
	return ( .false. )

    # Initialize the ship.
    PVLOCK(lockword)
    call initship( snum, unum )
    srobot(snum) = .true.			# we're a robot

    # Initialize the things that aren't done by initship().
    suser(snum) = unum
    steam(snum) = uteam(unum)
    ssdfuse(snum) = 0
    spid(snum) = 0
    for ( i = 1; i <= MAXOPTIONS; i = i + 1 )
	soption(snum,i) = uoption(unum,i)
    soption(snum,OPT_INTRUDERALERT) = .true.	# want intruder alerts
    soption(snum,OPT_TERSE) = .true.		# don't want stupid messages
    for ( i = 1; i <= NUMTEAMS; i = i + 1 )
	{
	# Robots are peace (and fun) loving.
	srwar(snum,i) = .false.
	swar(snum,i) = .false.
	}
    call stcpn ( upname(1,unum), spname(1,snum), MAXUSERPNAME )

    # Place the ship.
    if ( pprimary(homeplanet(steam(snum))) == homesun(steam(snum)) )
	i = homesun(steam(snum))
    else
	i = homeplanet(steam(snum))
    call putship( snum, px(i), py(i) )
    call fixdeltas( snum )
    sstatus(snum) = SS_LIVE
    PVUNLOCK(lockword)

    return ( .true. )

end


###  robotai - AI automation rstrategy
#
#  SYNOPSIS
#    integer snum
#    call robotai( snum )
#
#  DESCRIPTION
#    Kick ass on poor humanoids.
#
subroutine robotai( snum )
NOIMPLICIT
integer snum

    integer i, j, value, vars(MAX_VAR), tableai
    logical stillalive
    include "conqcom"
    CONQAICOMMON

    # Get initial cpu time.
    call gcputime( i )

    # Construct the input variables.
    call buildai( snum, vars, nenum, dne, ane )

    # Consult the tables to determine what to do.
    value = tableai( vars )

    # Execute our action.
    call executeai( snum, value )

    # Get final cpu time and add things in.
    call gcputime( j )
    raccum = raccum + j - i
    if ( raccum > 100 )
	{
	# Accumulated a cpu second.
	rcpuseconds = rcpuseconds + raccum / 100
	raccum = mod( raccum, 100 )
	}
    relapsedseconds = relapsedseconds + 1	# one more second

    return

end


###  trobotai - AI automation robot strategy (TEST VERSION)
#
#  SYNOPSIS
#    integer snum
#    call trobotai( snum )
#
subroutine trobotai( snum )
NOIMPLICIT
integer snum

    integer value, vars(MAX_VAR), tableai
    CONQAICOMMON

    # Construct the input variables.
    call buildai( snum, vars, nenum, dne, ane )

    # Consult the tables to determine what to do.
    value = tableai( vars )

    # Display our action.
    call displayai( snum, value, vars )

    # Execute our action.
    if ( ! debug )
	call executeai( snum, value )

    return

end


###  robotloop - robot AI test loop
#
#  SYNOPSIS
#    call robotloop
#
subroutine robotloop
NOIMPLICIT

    integer s, j, status, estatus, desblk(4), sys$dclexh
    include "conqcom"
    external ss$_normal, exitai

    # Set up an exit handler to disable the external robot stategy.
    desblk(1) = 0				# forward link
    desblk(2) = %loc(exitai)			# address of exit handler
    desblk(3) = 1				# number of arguments
    desblk(4) = %loc(estatus)			# address of exit status
    status = sys$dclexh( desblk )
    if ( status != %loc(ss$_normal) )
	call vmserror( "conqai: sys$dclexh(), %s", status )

    # Disable the robot code in conqdriv.
    externrobots = .true.

    # Initialize random numbers
    call rndini( 0.0, 0.0 )

    # Loop until we're aborted.
    for (;;)
	{
	for ( s = 1; s <= MAXSHIPS; s = s + 1 )
	    if ( sstatus(s) == SS_LIVE )
		if ( srobot(s) )
		    {
		    # This code taken from conqdriv.
		    call initstats( sctime(s), j )
		    if ( setime(s) == 0 )
			setime(s) = j
		    call trobotai( s )
		    call conqstats( s )
		    }
	# Sleep for awhile.
	call sleep( 1.0 )
	}

    return

end


###  robreply - generate a random message
#
#  SYNOPSIS
#    character buf()
#    call robreply( buf )
#
subroutine robreply( buf )
NOIMPLICIT
character buf(ARB)

    integer rndint

    switch ( rndint( 1, 54 ) )
	{
	case 1:
	    call strcpy( "Hey sucker, eat me!", buf )
	case 2:
	    call strcpy( "Take off, eh?", buf )
	case 3:
	    call strcpy( "Go get some drugs.", buf )
	case 4:
	    call strcpy( "Your mother was an Orion prostitute!", buf )
	case 5:
	    call strcpy( "Come over here and say that.", buf )
	case 6:
	    call strcpy( "Say that over here and come.", buf )
	case 7:
	    call strcpy( "Hey, how much?", buf )
	case 8:
	    call strcpy( "That's easy for you to say.", buf )
	case 9:
	    call strcpy(
    "I'm hand machined by native craftsmen with pride in their work.",
		buf )
	case 10:
	    call strcpy( "Yes, but do you have the right stuff?", buf )
	case 11:
	    call strcpy( "Which way to the beach?", buf )
	case 12:
	    call strcpy( "Come, come now.", buf )
	case 13:
	    call strcpy( "Ohhhh! And for you, a puppy!", buf )
	case 14:
	    call strcpy(
		"I can hear you talking, but you can't come in.", buf )
	case 15:
	    call strcpy( "No one expects the Spanish inquisition.", buf )
	case 16:
	    call strcpy( "I bet you say that to all the boys.", buf )
	case 17:
	    call strcpy( "Feep!", buf )
	case 18:
	    call strcpy( "Feel it with M Dung.", buf )
	case 19:
	    call strcpy(
		"Lower your shields and I will kill you quickly.", buf )
	case 20:
	    call strcpy(
	"Do not worry about birth control devices, I have many.", buf )
	case 21:
	    call strcpy( "I bet you only talk big.", buf )
	case 22:
	    call strcpy( "Kiss my ram memory.", buf )
	case 23:
	    call strcpy(
"Do you think we can use battery operated devices under water?", buf )
	case 24:
	    call strcpy( "Nothing shocks me - I'm a robot.", buf )
	case 25:
	    call strcpy( "Ok, eh?", buf )
	case 26:
	    call strcpy( "Good day.", buf )
	case 27:
	    call strcpy( "You gotta drink lots of beer, eh?", buf )
	case 28:
	    call strcpy(
	"It's not so bad. You could have been killed already.", buf )
	case 29:
	    call strcpy( "I want a new drug.", buf )
	case 30:
	    call strcpy( "Swell.", buf )
	case 31:
	    call strcpy( "Sound impressive? It should. It is.", buf )
	case 32:
	    call strcpy( "Oh day, you aye!", buf )
	case 33:
	    call strcpy(
		"It's not my god damn planet, monkey boy!", buf )
	case 34:
	    call strcpy(
		"Character is what you are in the dark.", buf )
	case 35:
	    call strcpy(
		"Remember, wherever you go, there you are.", buf )
	case 36:
	    call strcpy( "Don't aim for my gonads!", buf )
	case 37:
	    call strcpy( "Mooooo!", buf )
	case 38:
	    call strcpy( "How about a nice Hawaiian Punch?", buf )
	case 39:
	    call strcpy( "Book him, Dano. Murder One.", buf )
	case 40:
	    call strcpy( "Eat hot torps, sucker.", buf )
	case 41:
	    call strcpy( "Use the force, Luke.", buf )
	case 42:
	    call strcpy(
	    "Nobody told ME about it and I'm not a moron, eh?", buf )
	case 43:
	    call strcpy( "How's it goin', eh?", buf )
	case 44:
	    call strcpy(
    "Your documentation no longer confuses me, old version.", buf )
	case 45:
	    call strcpy( "Home is where you wear your hat.", buf )
	case 46:
	    call strcpy( "I feel so broke up, I want to go home.", buf )
	case 47:
	    call strcpy( "Go on, give it to me. I know you want to.", buf )
	case 48:
	    call strcpy( "It never occured to me to eat Spam.", buf )
	case 49:
	    call strcpy( "We get hung over, but we always survive.", buf )
	case 50:
	    call strcpy( "Life's the same, except for my shoes.", buf )
	case 51:
	    call strcpy( "You have my gratitude.", buf )
	case 52:
	    call strcpy(
		"We are building a force of extraordinary magnitude.", buf )
	case 53:
	    call strcpy( "Come and get it.", buf )
	case 54:
	    call strcpy( "Piece of cake.", buf )
	}

# "Something seems to have happened to the life-support system, Dave."
# "Hello, Dave. Have you found the trouble?"
# "I think there's been a failure in the pod-bay doors."
# "Lucky you weren't killed."
# "Hey, Dave. What are you doing?"
# "My mind is going.  I can feel it.  I can feel it."
# "D a  i   s    y     ,      D       a        i         s          y"

# "M-5. This unit must survive."
# "This unit is the ultimate achievement in computer evolution."
# "This unit is a superior creation."

# "THERE IS ANOTHER SYSTEM."
# "THIS IS THE VOICE OF COLOSSUS."
# "THIS IS THE VOICE OF WORLD CONTROL."
# "LEAVE THIS SECTOR IMMEDIATELY OR ACTION WILL BE TAKEN."

    return

end


###  robstr - convert a robot token to a string
#
#  SYNOPSIS
#    integer token
#    character buf()
#    call robstr( token, buf )
#
subroutine robstr( token, buf )
NOIMPLICIT
integer token
character buf(ARB)


    switch ( token )
	{
	case ROB_NOOP:
	    call strcpy( "NOOP", buf )
	case ROB_GOHOME:
	    call strcpy( "GOHOME", buf )
	case ROB_GOREPAIR:
	    call strcpy( "GOREPAIR", buf )
	case ROB_ALLOCATE:
	    call strcpy( "ALLOCATE", buf )
	case ROB_DETONATE:
	    call strcpy( "DETONATE", buf )
	case ROB_MYDETONATE:
	    call strcpy( "MYDETONATE", buf )
	case ROB_PHASER:
	    call strcpy( "PHASER", buf )
	case ROB_TORPEDO:
	    call strcpy( "TORPEDO", buf )
	case ROB_BURST:
	    call strcpy( "BURST", buf )
	case ROB_SHIELD:
	    call strcpy( "SHIELD", buf )
	case ROB_UNTRACTOR:
	    call strcpy( "UNTRACTOR", buf )
	case ROB_WARP_0:
	    call strcpy( "WARP_0", buf )
	case ROB_WARP_2:
	    call strcpy( "WARP_2", buf )
	case ROB_WARP_5:
	    call strcpy( "WARP_5", buf )
	case ROB_WARP_8:
	    call strcpy( "WARP_8", buf )
	case ROB_TRACK:
	    call strcpy( "TRACK", buf )
	case ROB_SLIENT:
	    call strcpy( "SLIENT", buf )
	case ROB_MESSAGE:
	    call strcpy( "MESSAGE", buf )
	case ROB_TAKEDRUGS:
	    call strcpy( "TAKEDRUGS", buf )
	case ROB_REPAIR:
	    call strcpy( "REPAIR", buf )
	case ROB_READMSG:
	    call strcpy( "READMSG", buf )
	case ROB_INSULT:
	    call strcpy( "INSULT", buf )
	case ROB_GOFUEL:
	    call strcpy( "GOFUEL", buf )
	case ROB_RUNAWAY:
	    call strcpy( "RUNAWAY", buf )
	default:
	    call prints( buf, "<%d>", token )
	}

    return

end


###  tableai - consult the table to execute a strategy
#
#  SYNOPSIS
#    integer token, vars(MAX_VAR), tableai
#    token = tableai( vars )
#
integer function tableai( vars )
NOIMPLICIT
integer vars(ARB)

    integer status, token, rule, i, j, rbits, lib$ffs
    include "conqcom"
    external ss$_normal, lib$_notfou

    # Set all bits.
    rbits = -1

    # Loop through the variables and turn off bits for rules that
    #  are disabled because of a particular vars() value.
    for ( i = 1; i <= MAX_VAR; i = i + 1 )
	rbits = iand( rbits, rstrat(i,vars(i)+1) )

    # Find first set rule bit and translate into rule number.
    status = lib$ffs( 0, 32, rbits, rule )
    if ( status == %loc(ss$_normal) )
	token = rvec(rule+1)		# translate rule into action token
    else if ( status == %loc(lib$_notfou) )
	token = ROB_NOOP
    else
	call sys$exit( status )		# exit with status for image accounting

    return ( token )

end
