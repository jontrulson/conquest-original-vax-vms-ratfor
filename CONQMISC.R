###############################################################################
#
#                               C O N Q M I S C
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


###  appkb - append killed by string
#
#  SYNOPSIS
#    integer kb
#    character buf()
#    call appkb( kb, buf )
#
subroutine appkb( kb, buf )
NOIMPLICIT
integer kb
character buf(ARB)

    include "conqcom"

    switch ( kb )
	{
	case KB_SELF:
	    call appstr( "self", buf )
	case KB_NEGENB:
	    call appstr( "negenb", buf )
	case KB_CONQUER:
	    call appstr( "conquer", buf )
	case KB_NEWGAME:
	    call appstr( "newgame", buf )
	case KB_EVICT:
	    call appstr( "evict", buf )
	case KB_SHIT:
	    call appstr( "shit", buf )
	case KB_DOOMSDAY:
	    call appstr( "doomsday", buf )
	case KB_GOTDOOMSDAY:
	    call appstr( "gotdoomsday", buf )
	case KB_GOD:
	    call appstr( "GOD", buf )
	default:
	    if ( kb >= 1 & kb <= MAXSHIPS )
		call appship( kb, buf )
	    else if ( -kb >= 1 & -kb <= NUMPLANETS )
		call appstr( pname(1,-kb), buf )
	    else
		call appint( kb, buf )
	}

    return

end


###  appship - append a ship number to a string
#
#  SYNOPSIS
#    integer snum
#    character str()
#    call appship( snum, str )
#
subroutine appship( snum, str )
NOIMPLICIT
integer snum
character str(ARB)

    integer i
    character ch
    include "conqcom"

    ch = 'S'
    if ( snum >= 1 & snum <= MAXSHIPS )
	{
	i = steam(snum)
	if ( i >= 1 & i <= NUMTEAMS )
	    ch = chrteams(i)
	}

    call appchr( ch, str )
    call appint( snum, str )

    return
end


###  canread - determine if a message is readable
#
#  SYNOPSIS
#    logical ok, canread
#    integer snum, msgnum
#    ok = canread( snum, msgnum )
#
logical function canread( snum, msgnum )
NOIMPLICIT
integer snum, msgnum

    integer from, to
    include "conqcom"

    from = msgfrom(msgnum)
    to = msgto(msgnum)

    # If we're GOD, we can read it.
    if ( snum == MSG_GOD )
	return ( .true. )

    # It's to us.
    if ( to == snum )
	return ( .true. )

    # It's to everybody.
    if ( to == MSG_ALL )
	return ( .true. )

    # Only check these if we're a ship.
    if ( snum >= 1 & snum <= MAXSHIPS )
	{
	# We can only read team messages if we're not self-war.
	if ( ( -to == steam(snum) ) & ! selfwar(snum) )
	    {
	    # Planet alert for our team.
	    if ( -from >= 1 & -from <= NUMPLANETS )
		return ( soption(snum,OPT_INTRUDERALERT) )
	    else
		return ( .true. )
	    }

	# See if we are allowed to read GOD messages.
	if ( to == MSG_GOD | from == MSG_GOD | to == MSG_IMPLEMENTORS )
	    return ( uooption(suser(snum),OOPT_GODMSG) )
	}

    # If we got here, we can't read it.
    return ( .false. )

end


###  clearships - reset ships and torpedoes
#
#  SYNOPSIS
#    call clearships
#
subroutine clearships
NOIMPLICIT

    integer i

    for ( i = 1; i <= MAXSHIPS; i = i + 1 )
	call zeroship( i )

    return

end


###  cvtcoords - convert from internal coords to screen coords
#
#  SYNOPSIS
#    logical inbounds, cvtcoords
#    real cenx, ceny, x, y, scale
#    integer lin, col
#    inbounds = cvtcoords( ceny, ceny, x, y, scale, lin, col )
#
logical function cvtcoords( cenx, ceny, x, y, scale, lin, col )
NOIMPLICIT
real cenx, ceny, x, y, scale
integer lin, col

    include "conqcom"
    include "conqcom2"

    col = round( (cmaxcol-STAT_COLS)/2 + (x-cenx) / scale * WIDTH_FAC ) +
	STAT_COLS

    lin = round( (DISPLAY_LINS/2+1) - (y-ceny) / scale )
    if ( lin < 1 | lin > DISPLAY_LINS | col <= STAT_COLS | col > cmaxcol )
	return ( .false. )

    return ( .true. )

end


###  doomfind - find a planet or ship for the doomsday machine to head for
#
#  SYNOPSIS
#    call doomfind
#
subroutine doomfind
NOIMPLICIT

    integer i
    real taste, tastiness, angle
    include "conqcom"

    tastiness = 0.0
    dlock = -PNUM_MURISAK

    for ( i = 1; i <= NUMPLANETS; i = i + 1 )
	if ( preal(i) )
	    if ( parmies(i) > 0 & pteam(i) != TEAM_NOTEAM )
		{
		taste = parmies(i)*BOMBARD_KILLS / dist(dx, dy, px(i), py(i))
		if ( taste > tastiness )
		    {
		    tastiness = taste
		    dlock = -i
		    }
		}

    for ( i = 1; i <= MAXSHIPS; i = i + 1 )
	if ( sstatus(i) == SS_LIVE )
	    {
	    taste = ( 1.0 +
		      skills(i) * KILLS_KILLS +
		      sarmies(i) * ARMY_KILLS ) / dist(dx, dy, sx(i), sy(i))
	    if ( taste > tastiness )
		{
		tastiness = taste
		dlock = i
		}
	    }

    if ( dlock < 0 )
	dhead = angle( dx, dy, px(-dlock), py(-dlock) )
    else if ( dlock > 0 )
	dhead = angle( dx, dy, sx(dlock), sy(dlock) )

    return

end


###  doomsday - start the doomsday device
#
#  SYNOPSIS
#    call doomsday
#
subroutine doomsday
NOIMPLICIT

    real rnduni
    include "conqcom"

    dhead = rnduni( 0.0, 360.0 )
    dx = DOOMSDAY_START_DIST * cosd(dhead)
    dy = DOOMSDAY_START_DIST * sind(dhead)
    call doomfind
    dstatus = DS_LIVE

    return

end


###  findorbit - find a planet for a ship to orbit
#
#  SYNOPSIS
#    integer snum, pnum
#    logical flag, findorbit
#    flag = findorbit( snum, pnum )
#
logical function findorbit( snum, pnum )
NOIMPLICIT
integer snum, pnum

    integer i
    include "conqcom"

    for ( i = 1; i <= NUMPLANETS; i = i + 1 )
	if ( preal(i) &
	     ( dist( sx(snum), sy(snum), px(i), py(i) ) <= ORBIT_DIST ) )
	    {
	    pnum = i
	    return ( .true. )
	    }
    # Didn't find one.
    pnum = 0
    return ( .false. )

end


###  findship - find a free ship and reserve it (DOES LOCKING)
#
#  SYNOPSIS
#    integer snum
#    logical truth, findship
#    truth = findship( snum )
#
logical function findship( snum )
NOIMPLICIT
integer snum

    integer i
    include "conqcom"

    PVLOCK(lockword)
    snum = 0
    for ( i = 1; i <= MAXSHIPS; i = i + 1 )
	if ( sstatus(i) == SS_OFF )
	    {
	    snum = i
	    call zeroship( snum )
	    sstatus(snum) = SS_RESERVED
	    slastmsg(snum) = LMSG_NEEDINIT
	    ssdfuse(snum) = -TIMEOUT_PLAYER
	    sctime(snum) = 0
	    setime(snum) = 0
	    scacc(snum) = 0
	    seacc(snum) = 0
	    break
	    }
    PVUNLOCK(lockword)

    return ( snum != 0 )

end


###  findspecial - search for nearest some-thing
#
#  SYNOPSIS
#    logical flag, findspecial
#    integer snum, token, count, sorpnum, xsorpnum
#    flag = findspecial( snum, token, count, sorpnum, xsorpnum )
#
logical function findspecial( snum, token, count, sorpnum, xsorpnum )
NOIMPLICIT
integer snum, token, count, sorpnum, xsorpnum

    integer i, a, na, ta, u, nu, tu
    real d, nd, td
    logical valid, peaceful, spwar
    include "conqcom"

    sorpnum = 0					# zero nearest
    xsorpnum = 0				# zero second nearest
    d = 2e20					# distance from nearest
    nd = 3e20					# distance from second nearest
    a = 20000					# armies of nearest
    na = 30000					# armies of second nearest
    u = 20000					# uninhabitable time of nearest
    nu = 20000					# uninhabitable time of next
    switch ( token )
	{
	case SPECIAL_SHIP, SPECIAL_ENEMYSHIP, SPECIAL_TEAMSHIP:
	    # Nearest ship, nearest enemy ship, and nearest team ship.
	    for ( i = 1; i <= MAXSHIPS; i = i + 1 )
		if ( i != snum & sstatus(i) == SS_LIVE )
		    {
		    switch ( token )
			{
			case SPECIAL_ENEMYSHIP:
			    valid = satwar(snum, i)
			case SPECIAL_SHIP:
			    valid = .true.
			case SPECIAL_TEAMSHIP:
			    valid = ( steam(i) == steam(snum) &
				    ! satwar(snum, i) )
			default:
			    return ( .false. )		# this can't happen
			}
		    if ( valid )
			{
			td = dist(sx(snum), sy(snum), sx(i), sy(i))
			if ( td < nd )
			    if ( td < d )
				{
				xsorpnum = sorpnum
				nd = d
				sorpnum = i
				d = td
				}
			    else
				{
				xsorpnum = i
				nd = td
				}
			}
		    }
	case SPECIAL_HOMEPLANET:
	    # Home planet.
	    switch ( steam(snum) )
		{
		case TEAM_FEDERATION:
		    sorpnum = homeplanet(TEAM_FEDERATION)
		case TEAM_ROMULAN:
		    sorpnum = homeplanet(TEAM_ROMULAN)
		case TEAM_KLINGON:
		    sorpnum = homeplanet(TEAM_KLINGON)
		case TEAM_ORION:
		    sorpnum = homeplanet(TEAM_ORION)
		default:
		    return ( .false. )
		}
	case SPECIAL_WEAKPLANET:
	    # Weakest non-team planet.
	    for ( i = 1; i <= NUMPLANETS; i = i + 1 )
		{
		# Only can look for "real" planets.
		if ( ! preal(i) )
		    next
		# Ignore suns and moons.
		if ( ptype(i) == PLANET_SUN | ptype(i) == PLANET_MOON )
		    next
		switch ( token )
		    {
		    case SPECIAL_WEAKPLANET:
			valid = ( pscanned(i,steam(snum)) &
				  pteam(i) != steam(snum) )
		    default:
			return ( .false. )	# this can't happen
		    }
		# Handle army threshold logic.
		if ( valid )
		    switch ( token )
			{
			case SPECIAL_WEAKPLANET:
			    valid = ( parmies(i) >= count )
			default:
			    return ( .false. )	# this can't happen
			}
		if ( valid )
		    {
		    ta = parmies(i)
		    tu = puninhabtime(i)
		    td = dist(sx(snum), sy(snum), px(i), py(i))

		    # Uninhabitable time is of next importance,
		    #  number of armies is of first importantance, and
		    #  distance is of last importance.
		    if ( tu < nu |
			 ( tu == nu & ( ta < na | ( ta == na & td < nd ) ) ) )
			if ( tu < u |
			     ( tu == u & ( ta < a | ( ta == a & td < d ) ) ) )
			    {
			    xsorpnum = sorpnum
			    na = a
			    nu = u
			    nd = d
			    sorpnum = i
			    a = ta
			    u = tu
			    d = td
			    }
			else
			    {
			    xsorpnum = i
			    na = ta
			    nu = tu
			    nd = td
			    }
		    }
		}
	case SPECIAL_ARMYPLANET, SPECIAL_ENEMYPLANET, SPECIAL_FUELPLANET,
	    SPECIAL_PLANET, SPECIAL_REPAIRPLANET, SPECIAL_TEAMPLANET:

	    # Determine if we at peace with all teams.
	    peaceful = .true.
	    for ( i = 1; i <= NUMTEAMS; i = i + 1 )
		if ( swar(snum,i) )
		    {
		    peaceful = .false.
		    break
		    }

	    # Loop through the planets.
	    for ( i = 1; i <= NUMPLANETS; i = i + 1 )
		{
		# Only can look for "real" planets.
		if ( ! preal(i) )
		    next
		# Ignore suns and moons.
		if ( ptype(i) == PLANET_SUN | ptype(i) == PLANET_MOON )
		    next
		switch ( token )
		    {
		    case SPECIAL_ARMYPLANET:
			valid = ( pteam(i) == steam(snum) )
		    case SPECIAL_ENEMYPLANET:
			valid = ( ! pscanned(i,steam(snum)) |
				  ( parmies(i) > 0 &
				  spwar( snum, i ) &
				  ptype(i) != PLANET_MOON ) )
		    case SPECIAL_FUELPLANET:
			valid = ( ( pscanned(i,steam(snum)) | peaceful ) &
				  ! spwar( snum, i ) &
				  parmies(i) > 0 &
				  ptype(i) == PLANET_CLASSM )
		    case SPECIAL_PLANET:
			valid = .true.
		    case SPECIAL_REPAIRPLANET:
			valid = ( ( pscanned(i,steam(snum)) | peaceful ) &
				  ! spwar( snum, i ) &
				  parmies(i) > 0 &
				  ptype(i) != PLANET_MOON )
		    case SPECIAL_TEAMPLANET:
			valid = ( pteam(i) == steam(snum) )
		    default:
			return ( .false. )		# this can't happen
		    }
		# Handle army threshold logic.
		if ( valid )
		    switch ( token )
			{
			case SPECIAL_ARMYPLANET:
			    valid = ( ( parmies(i) - 3 ) >= count )
			case SPECIAL_PLANET, SPECIAL_ENEMYPLANET:
			    valid = ( ! pscanned(i,steam(snum)) |
				      parmies(i) >= count )
			case SPECIAL_FUELPLANET, SPECIAL_REPAIRPLANET,
			    SPECIAL_TEAMPLANET:
			    valid = ( parmies(i) >= count )
			default:
			    return ( .false. )	# this can't happen
			}
		if ( valid )
		    {
		    td = dist(sx(snum), sy(snum), px(i), py(i))
		    if ( td < nd )
			if ( td < d )
			    {
			    xsorpnum = sorpnum
			    nd = d
			    sorpnum = i
			    d = td
			    }
			else
			    {
			    xsorpnum = i
			    nd = td
			    }
		    }
		}
	default:
	    return ( .false. )			# this can't happen
	}

    return ( sorpnum != 0 )

end


###  fixdeltas - update sdx and sdy
#
#  SYNOPSIS
#    integer snum
#    call fixdeltas( snum )
#
subroutine fixdeltas( snum )
NOIMPLICIT
integer snum

    real speed
    include "conqcom"

    speed = swarp(snum) * MM_PER_SEC_PER_WARP * ITER_SECONDS
    sdx(snum) = speed * cosd(shead(snum))
    sdy(snum) = speed * sind(shead(snum))

    return

end


###  gunum - get the user number of the specified user
#
#  SYNOPSIS
#    logical truth, gunum
#    integer unum
#    character lname()
#    truth = gunum( unum, lname )
#
logical function gunum( unum, lname )
NOIMPLICIT
integer unum
character lname(ARB)

    integer i, strcmp
    include "conqcom"

    unum = 0
    for ( i = 1; i <= MAXUSERS; i = i + 1 )
	if ( ulive(i) )
	    if ( strcmp( lname, uname(1,i) ) == 0 )
		{
		unum = i
		return ( .true. )
		}

    return ( .false. )

end


###  histlist - display the last usage list
#
#  SYNOPSIS
#    integer godlike
#    call histlist( godlike )
#
real function histlist( godlike )
NOIMPLICIT
integer godlike

    integer i, j, unum, lin, col, fline, lline, thistptr, modp1
    character ch
    logical iogtimed, stillalive
    include "conqcom"
    include "conqcom2"

    # Do some screen setup.
    call cdclear
    fline = 1
    lline = MSG_LIN1 - 1
    call cdputc( "C O N Q U E S T   U S E R   H I S T O R Y", fline )
    fline = fline + 2

    thistptr = -1			# force an update the first time
    repeat
	{
	if ( ! godlike )
	    if ( ! stillalive( csnum ) )
		break
	if ( thistptr != histptr )
	    {
	    # Need to update the display
	    thistptr = histptr
	    lin = fline
	    col = 8
	    call cdclrl( fline, lline - fline + 1 )
	    i = modp1( thistptr + 1, MAXHISTLOG )	# gag...
	    for ( j = 1; j <= MAXHISTLOG; j = j + 1 )
		{
		i = modp1( i - 1, MAXHISTLOG )
		unum = histunum(i)
		if ( unum < 1 | unum > unum )
		    next
		if ( ! ulive(unum) )
		    next
		call prints( cbuf, "%-12s %s", uname(1,unum), histlog(1,i) )
		call cdputs( cbuf, lin, col )
		lin = lin + 1
		if ( lin > lline )
		    {
		    col = 43
		    lin = fline
		    }
		}
	    }

	call putpmt( "--- press space when done ---", MSG_LIN2 )
	call cdplay( .true. )
	if ( iogtimed( ch, 1 ) )
	    break				# exit loop if we got one
	}

    return

end


###  initeverything - initialize (with extra cheese and tomato) (DOES LOCKING)
#
#  SYNOPSIS
#    call initeverything
#
subroutine initeverything
NOIMPLICIT

    integer i, j
    include "conqcom"

    # Zero EVERYTHING.
    call zeroeverything

    # Twiddle the lockword.
    PVUNLOCK(lockword)
    PVLOCK(lockword)

    # Turn off the universe.
    closed = .true.

    # Zero team stats.
    for ( i = 1; i <= NUMTEAMS; i = i + 1 )
	for ( j = 1; j <= MAXTSTATS; j = j + 1 )
	    tstats(i,j) = 0

    # De-register all users.
    for ( i = 1; i <= MAXUSERS; i = i + 1 )
	ulive(i) = .false.

    celapsedseconds = 0
    ccpuseconds = 0
    delapsedseconds = 0
    dcpuseconds = 0
    relapsedseconds = 0
    rcpuseconds = 0
    raccum = 0

    call stcpn( "never", lastupchuck, DATESIZE )
    call getdandt( inittime )
    call getdandt( conqtime )
    call stcpn( "GOD", conqueror, MAXUSERPNAME )
    call stcpn( "self ruled", conqteam, MAXTEAMNAME )
    call stcpn( "Let there be light...", lastwords, MAXLASTWORDS )

    # Un-twiddle the lockwords.
    PVUNLOCK(lockword)
    PVUNLOCK(lockmesg)

    call initrobots
    call inituniverse

    return

end


###  initgame - initialize the game-permanent variables
#
#  SYNOPSIS
#    call initgame
#
subroutine initgame
NOIMPLICIT

    integer i, j
    include "conqcom"

    # Twiddle the lockword.
    PVUNLOCK(lockword)
    PVLOCK(lockword)

    # Driver.
    drivsecs = 0

    # Doomsday machine.
    dstatus = DS_OFF
    dtype = 0				# should have constants for this
    dx = 0.0
    dy = 0.0
    ddx = 0.0
    ddy = 0.0
    dhead = 0.0
    dlock = 0
    call stcpn( "Doomsday Machine", dname, MAXUSERPNAME )

    # Set up initial armies on planets.
    pteam(PNUM_SOL) = TEAM_NOTEAM
    pteam(PNUM_EARTH) = TEAM_FEDERATION
    pteam(PNUM_TELOS) = TEAM_FEDERATION
    pteam(PNUM_OMEGA) = TEAM_FEDERATION
    pteam(PNUM_SIRIUS) = TEAM_NOTEAM
    pteam(PNUM_ROMULUS) = TEAM_ROMULAN
    pteam(PNUM_REMUS) = TEAM_ROMULAN
    pteam(PNUM_RHO) = TEAM_ROMULAN
    pteam(PNUM_KEJELA) = TEAM_NOTEAM
    pteam(PNUM_KLINGUS) = TEAM_KLINGON
    pteam(PNUM_LEUDUS) = TEAM_KLINGON
    pteam(PNUM_TARSUS) = TEAM_KLINGON
    pteam(PNUM_BETELGEUSE) = TEAM_NOTEAM
    pteam(PNUM_ORION) = TEAM_ORION
    pteam(PNUM_OBERON) = TEAM_ORION
    pteam(PNUM_UMBRIEL) = TEAM_ORION
    pteam(PNUM_MURISAK) = TEAM_NOTEAM
    pteam(PNUM_JANUS) = TEAM_SELFRULED
    pteam(PNUM_SERITIL) = TEAM_SELFRULED
    pteam(PNUM_ELAS) = TEAM_SELFRULED
    pteam(PNUM_SHERMAN) = TEAM_SELFRULED
    pteam(PNUM_CHERON) = TEAM_SELFRULED
    pteam(PNUM_DAKEL) = TEAM_SELFRULED
    pteam(PNUM_OLDAR) = TEAM_SELFRULED
    pteam(PNUM_SARAC) = TEAM_SELFRULED
    pteam(PNUM_EMINIAR) = TEAM_SELFRULED
    pteam(PNUM_VENAR) = TEAM_SELFRULED
    pteam(PNUM_DYNEB) = TEAM_SELFRULED
    pteam(PNUM_XIDEX) = TEAM_SELFRULED
    pteam(PNUM_RIGELB) = TEAM_SELFRULED

    pteam(PNUM_SYRINX) = TEAM_NOTEAM
    pteam(PNUM_SHITFACE) = TEAM_GOD
    pteam(PNUM_HELL) = TEAM_GOD
    pteam(PNUM_JINX) = TEAM_GOD
    pteam(PNUM_LUNA) = TEAM_NOTEAM

    pteam(PNUM_GHOST1) = TEAM_NOTEAM
    pteam(PNUM_GHOST2) = TEAM_NOTEAM
    pteam(PNUM_GHOST3) = TEAM_NOTEAM
    pteam(PNUM_GHOST4) = TEAM_NOTEAM

    pteam(PNUM_SPARE1) = TEAM_NOTEAM

    parmies(PNUM_SOL) = 100
    parmies(PNUM_EARTH) = 50
    parmies(PNUM_TELOS) = 50
    parmies(PNUM_OMEGA) = 50
    parmies(PNUM_SIRIUS) = 100
    parmies(PNUM_ROMULUS) = 50
    parmies(PNUM_REMUS) = 50
    parmies(PNUM_RHO) = 50
    parmies(PNUM_KEJELA) = 100
    parmies(PNUM_KLINGUS) = 50
    parmies(PNUM_LEUDUS) = 50
    parmies(PNUM_TARSUS) = 50
    parmies(PNUM_BETELGEUSE) = 100
    parmies(PNUM_ORION) = 50
    parmies(PNUM_OBERON) = 50
    parmies(PNUM_UMBRIEL) = 50
    parmies(PNUM_MURISAK) = 100
    parmies(PNUM_JANUS) = 25
    parmies(PNUM_SERITIL) = 25
    parmies(PNUM_ELAS) = 25
    parmies(PNUM_SHERMAN) = 25
    parmies(PNUM_CHERON) = 25
    parmies(PNUM_DAKEL) = 25
    parmies(PNUM_OLDAR) = 25
    parmies(PNUM_SARAC) = 25
    parmies(PNUM_EMINIAR) = 25
    parmies(PNUM_VENAR) = 25
    parmies(PNUM_DYNEB) = 25
    parmies(PNUM_XIDEX) = 25
    parmies(PNUM_RIGELB) = 25

    # The rest don't matter since you don't have to conquer them.
    parmies(PNUM_SYRINX) = 100
    parmies(PNUM_SHITFACE) = 256
    parmies(PNUM_HELL) = 128
    parmies(PNUM_JINX) = 512
    parmies(PNUM_LUNA) = 0

    parmies(PNUM_GHOST1) = 0
    parmies(PNUM_GHOST2) = 0
    parmies(PNUM_GHOST3) = 0
    parmies(PNUM_GHOST4) = 0

    parmies(PNUM_SPARE1) = 0

    # Set up the pscanned array so that each team has scanned its own planets.
    for ( i = 1; i <= NUMPLANETS; i = i + 1 )
	{
	puninhabtime(i) = 0		# planets start out inhabitable
	for ( j = 1; j <= NUMTEAMS; j = j + 1 )
	    pscanned(i,j) = .false.
	}

    for ( i = 1; i <= NUMTEAMS; i = i + 1 )
	{
	# Each team has scanned its own planets.
	for ( j = 1; j <= 3; j = j + 1 )
	    pscanned(teamplanets(i,j),i) = .true.

	couptime(i) = 0			# time left to coup starts at zero.
	tcoupinfo(i) = .false.		# don't know coup time
	}

    # Un-twiddle the lockword.
    PVUNLOCK(lockword)

    # Set up the physical universe.
    call initplanets

    return

end


###  initmsgs - initialize the message data structures
#
#  SYNOPSIS
#    call initmsgs
#
subroutine initmsgs
NOIMPLICIT

    integer i
    include "conqcom"

    # Zero the message buffer.
    for ( i = 1; i <= MAXMESSAGES; i = i + 1 )
	{
	msgbuf(1,i) = EOS
	msgfrom(i) = 0
	msgto(i) = 0
	}
    lastmsg = 1
    glastmsg = lastmsg

    return

end


###  initplanets - initialize the planets
#
#  SYNOPSIS
#    call initplanets
#
subroutine initplanets
NOIMPLICIT

    # SETPLANET( name, pnum )
    define(SETPLANET,
	call stcpn( $1, pname(1,$2), MAXPLANETNAME ))
    integer i, rndint
    real rnduni, rndnor, mod360, orbang, orbvel
    include "conqcom"

    # Twiddle the lockword.
    PVUNLOCK(lockword)
    PVLOCK(lockword)

    SETPLANET( "Sol", PNUM_SOL )
    SETPLANET( "Earth", PNUM_EARTH )
    SETPLANET( "Telos", PNUM_TELOS )
    SETPLANET( "Omega", PNUM_OMEGA )
    SETPLANET( "Sirius", PNUM_SIRIUS )
    SETPLANET( "Romulus", PNUM_ROMULUS )
    SETPLANET( "Remus", PNUM_REMUS )
    SETPLANET( "Rho", PNUM_RHO )
    SETPLANET( "Kejela", PNUM_KEJELA )
    SETPLANET( "Klingus", PNUM_KLINGUS )
    SETPLANET( "Leudus", PNUM_LEUDUS )
    SETPLANET( "Tarsus", PNUM_TARSUS )
    SETPLANET( "Betelgeuse", PNUM_BETELGEUSE )
    SETPLANET( "Orion", PNUM_ORION )
    SETPLANET( "Oberon", PNUM_OBERON )
    SETPLANET( "Umbriel", PNUM_UMBRIEL )
    SETPLANET( "Murisak", PNUM_MURISAK )
    SETPLANET( "Janus", PNUM_JANUS )
    SETPLANET( "Seritil", PNUM_SERITIL )
    SETPLANET( "Elas", PNUM_ELAS )
    SETPLANET( "Sherman", PNUM_SHERMAN )
    SETPLANET( "Cheron", PNUM_CHERON )
    SETPLANET( "Dakel", PNUM_DAKEL )
    SETPLANET( "Oldar", PNUM_OLDAR )
    SETPLANET( "Sarac", PNUM_SARAC )
    SETPLANET( "Eminiar", PNUM_EMINIAR )
    SETPLANET( "Venar", PNUM_VENAR )
    SETPLANET( "Dyneb", PNUM_DYNEB )
    SETPLANET( "Xidex", PNUM_XIDEX )
    SETPLANET( "RigelB", PNUM_RIGELB )

    SETPLANET( "Syrinx", PNUM_SYRINX )
    SETPLANET( "Luna", PNUM_LUNA )
    SETPLANET( "Shitface", PNUM_SHITFACE )
    SETPLANET( "Hell", PNUM_HELL )
    SETPLANET( "Jinx", PNUM_JINX )

    SETPLANET( "Ghost 1", PNUM_GHOST1 )
    SETPLANET( "Ghost 2", PNUM_GHOST2 )
    SETPLANET( "Ghost 3", PNUM_GHOST3 )
    SETPLANET( "Ghost 4", PNUM_GHOST4 )
    SETPLANET( "Spare 1", PNUM_SPARE1 )

    ptype(PNUM_SOL) = PLANET_SUN
    ptype(PNUM_EARTH) = PLANET_CLASSM
    ptype(PNUM_TELOS) = PLANET_DEAD
    ptype(PNUM_OMEGA) = PLANET_DEAD
    ptype(PNUM_SIRIUS) = PLANET_SUN
    ptype(PNUM_ROMULUS) = PLANET_CLASSM
    ptype(PNUM_REMUS) = PLANET_DEAD
    ptype(PNUM_RHO) = PLANET_DEAD
    ptype(PNUM_KEJELA) = PLANET_SUN
    ptype(PNUM_KLINGUS) = PLANET_CLASSM
    ptype(PNUM_LEUDUS) = PLANET_DEAD
    ptype(PNUM_TARSUS) = PLANET_DEAD
    ptype(PNUM_BETELGEUSE) = PLANET_SUN
    ptype(PNUM_ORION) = PLANET_CLASSM
    ptype(PNUM_OBERON) = PLANET_DEAD
    ptype(PNUM_UMBRIEL) = PLANET_DEAD
    ptype(PNUM_MURISAK) = PLANET_SUN
    ptype(PNUM_JANUS) = PLANET_CLASSM
    ptype(PNUM_SERITIL) = PLANET_DEAD
    ptype(PNUM_ELAS) = PLANET_CLASSM
    ptype(PNUM_SHERMAN) = PLANET_CLASSM
    ptype(PNUM_CHERON) = PLANET_DEAD
    ptype(PNUM_DAKEL) = PLANET_CLASSM
    ptype(PNUM_OLDAR) = PLANET_DEAD
    ptype(PNUM_SARAC) = PLANET_CLASSM
    ptype(PNUM_EMINIAR) = PLANET_DEAD
    ptype(PNUM_VENAR) = PLANET_CLASSM
    ptype(PNUM_DYNEB) = PLANET_DEAD
    ptype(PNUM_XIDEX) = PLANET_CLASSM
    ptype(PNUM_RIGELB) = PLANET_DEAD
    ptype(PNUM_GHOST1) = PLANET_GHOST
    ptype(PNUM_GHOST2) = PLANET_GHOST
    ptype(PNUM_GHOST3) = PLANET_GHOST
    ptype(PNUM_GHOST4) = PLANET_GHOST

    ptype(PNUM_SYRINX) = PLANET_SUN
    ptype(PNUM_LUNA) = PLANET_MOON
    ptype(PNUM_SHITFACE) = PLANET_DEAD
    ptype(PNUM_HELL) = PLANET_DEAD
    ptype(PNUM_JINX) = PLANET_CLASSM
    ptype(PNUM_SPARE1) = PLANET_GHOST

    call stcpn( "class M planet", ptname(1,PLANET_CLASSM), MAXPTYPENAME )
    call stcpn( "dead planet", ptname(1,PLANET_DEAD), MAXPTYPENAME )
    call stcpn( "sun", ptname(1,PLANET_SUN), MAXPTYPENAME )
    call stcpn( "moon", ptname(1,PLANET_MOON), MAXPTYPENAME )
    call stcpn( "ghost planet", ptname(1,PLANET_GHOST), MAXPTYPENAME )
    call stcpn( "class A planet", ptname(1,PLANET_CLASSA), MAXPTYPENAME )
    call stcpn( "class O planet", ptname(1,PLANET_CLASSO), MAXPTYPENAME )
    call stcpn( "class Z planet", ptname(1,PLANET_CLASSZ), MAXPTYPENAME )

    for ( i = 1; i <= NUMPLANETS; i = i + 1 )
	{
	preal(i) = .true.		# by default, you can see most planets
	porbvel(i) = 0.0
	porbrad(i) = 0
	pprimary(i) = 0
	}

    # Can't see the ghost planets.
    preal(PNUM_GHOST1) = .false.
    preal(PNUM_GHOST2) = .false.
    preal(PNUM_GHOST3) = .false.
    preal(PNUM_GHOST4) = .false.

    # Can't see the play planets either.
    preal(PNUM_SYRINX) = .false.
    preal(PNUM_SHITFACE) = .false.
    preal(PNUM_HELL) = .false.
    preal(PNUM_JINX) = .false.
    preal(PNUM_SPARE1) = .false.

    # Set up the X-Y coordinates of the suns.  Start with Murisak.
    # at the center, then place the other eight suns relative to it,
    # then the radii of the planets.
    px(PNUM_MURISAK) = 0.0
    py(PNUM_MURISAK) = 0.0
    pprimary(PNUM_MURISAK) = 0			# the only "stationary" object
    porbrad(PNUM_MURISAK) = 0.0
    porbang(PNUM_MURISAK) = 0.0
    porbvel(PNUM_MURISAK) = 0.0

    pprimary(PNUM_BETELGEUSE) = PNUM_MURISAK
    porbrad(PNUM_BETELGEUSE) = 11000.0
    porbang(PNUM_BETELGEUSE) = 45.0
    porbvel(PNUM_BETELGEUSE) = 0.0

    pprimary(PNUM_KEJELA) = PNUM_MURISAK
    porbrad(PNUM_KEJELA) = 11000.0
    porbang(PNUM_KEJELA) = 135.0
    porbvel(PNUM_KEJELA) = 0.0

    pprimary(PNUM_SIRIUS) = PNUM_MURISAK
    porbrad(PNUM_SIRIUS) = 11000.0
    porbang(PNUM_SIRIUS) = 225.0
    porbvel(PNUM_SIRIUS) = 0.0

    pprimary(PNUM_SOL) = PNUM_MURISAK
    porbrad(PNUM_SOL) = 11000.0
    porbang(PNUM_SOL) = 315.0
    porbvel(PNUM_SOL) = 0.0

    pprimary(PNUM_GHOST1) = PNUM_MURISAK
    porbrad(PNUM_GHOST1) = 12000.0
    porbang(PNUM_GHOST1) = 0.0
    porbvel(PNUM_GHOST1) = 0.0

    pprimary(PNUM_GHOST2) = PNUM_MURISAK
    porbrad(PNUM_GHOST2) = 12000.0
    porbang(PNUM_GHOST2) = 90.0
    porbvel(PNUM_GHOST2) = 0.0

    pprimary(PNUM_GHOST3) = PNUM_MURISAK
    porbrad(PNUM_GHOST3) = 12000.0
    porbang(PNUM_GHOST3) = 180.0
    porbvel(PNUM_GHOST3) = 0.0

    pprimary(PNUM_GHOST4) = PNUM_MURISAK
    porbrad(PNUM_GHOST4) = 12000.0
    porbang(PNUM_GHOST4) = 270.0
    porbvel(PNUM_GHOST4) = 0.0

    # Murisak's planets.
    pprimary(PNUM_JANUS) = PNUM_MURISAK
    porbrad(PNUM_JANUS) = 2600.0
    pprimary(PNUM_SERITIL) = PNUM_MURISAK
    porbrad(PNUM_SERITIL) = 2600.0
    pprimary(PNUM_ELAS) = PNUM_MURISAK
    porbrad(PNUM_ELAS) = 2600.0
    pprimary(PNUM_SHERMAN) = PNUM_MURISAK
    porbrad(PNUM_SHERMAN) = 2600.0
    pprimary(PNUM_CHERON) = PNUM_MURISAK
    porbrad(PNUM_CHERON) = 2600.0
    # Sol's planets.
    pprimary(PNUM_EARTH) = PNUM_SOL
    porbrad(PNUM_EARTH) = 2600.0
    pprimary(PNUM_TELOS) = PNUM_SOL
    porbrad(PNUM_TELOS) = 2600.0
    pprimary(PNUM_OMEGA) = PNUM_SOL
    porbrad(PNUM_OMEGA) = 2600.0
    # Sirius' planets.
    pprimary(PNUM_ROMULUS) = PNUM_SIRIUS
    porbrad(PNUM_ROMULUS) = 2600.0
    pprimary(PNUM_REMUS) = PNUM_SIRIUS
    porbrad(PNUM_REMUS) = 2600.0
    pprimary(PNUM_RHO) = PNUM_SIRIUS
    porbrad(PNUM_RHO) = 2600.0
    # Kejela's planets.
    pprimary(PNUM_KLINGUS) = PNUM_KEJELA
    porbrad(PNUM_KLINGUS) = 2600.0
    pprimary(PNUM_LEUDUS) = PNUM_KEJELA
    porbrad(PNUM_LEUDUS) = 2600.0
    pprimary(PNUM_TARSUS) = PNUM_KEJELA
    porbrad(PNUM_TARSUS) = 2600.0
    # Betelgeuse's planets.
    pprimary(PNUM_ORION) = PNUM_BETELGEUSE
    porbrad(PNUM_ORION) = 2600.0
    pprimary(PNUM_OBERON) = PNUM_BETELGEUSE
    porbrad(PNUM_OBERON) = 2600.0
    pprimary(PNUM_UMBRIEL) = PNUM_BETELGEUSE
    porbrad(PNUM_UMBRIEL) = 2600.0
    # Side systems.
    pprimary(PNUM_XIDEX) = PNUM_GHOST1
    porbrad(PNUM_XIDEX) = 1150.0
    pprimary(PNUM_RIGELB) = PNUM_GHOST1
    porbrad(PNUM_RIGELB) = 1150.0
    pprimary(PNUM_VENAR) = PNUM_GHOST2
    porbrad(PNUM_VENAR) = 1150.0
    pprimary(PNUM_DYNEB) = PNUM_GHOST2
    porbrad(PNUM_DYNEB) = 1150.0
    pprimary(PNUM_SARAC) = PNUM_GHOST3
    porbrad(PNUM_SARAC) = 1150.0
    pprimary(PNUM_EMINIAR) = PNUM_GHOST3
    porbrad(PNUM_EMINIAR) = 1150.0
    pprimary(PNUM_DAKEL) = PNUM_GHOST4
    porbrad(PNUM_DAKEL) = 1150.0
    pprimary(PNUM_OLDAR) = PNUM_GHOST4
    porbrad(PNUM_OLDAR) = 1150.0

    pprimary(PNUM_SYRINX) = PNUM_MURISAK
    porbrad(PNUM_SYRINX) = 70000.0

    pprimary(PNUM_LUNA) = PNUM_EARTH
    porbrad(PNUM_LUNA) = 1250.0

    pprimary(PNUM_SHITFACE) = PNUM_SYRINX
    porbrad(PNUM_SHITFACE) = 2800.0
    pprimary(PNUM_HELL) = PNUM_SYRINX
    porbrad(PNUM_HELL) = 2100.0
    pprimary(PNUM_JINX) = PNUM_SYRINX
    porbrad(PNUM_JINX) = 2600.0

    pprimary(PNUM_SPARE1) = PNUM_MURISAK
    porbrad(PNUM_SPARE1) = 5000.0

    # Set orbital angles and velocities for planets, and place them.
    # Murisak's planets.
    orbang = rnduni( 0.0, 360.0 )
    orbvel = rndnor( PLANET_ORBIT_FAC, 2.0 ) * ( rndint( 0, 1 ) * 2 - 1 )
    porbang(PNUM_JANUS) = orbang
    porbvel(PNUM_JANUS) = orbvel
    porbang(PNUM_CHERON) = mod360( orbang + 1.0/5.0*360.0 )
    porbvel(PNUM_CHERON) = orbvel
    porbang(PNUM_SHERMAN) = mod360( orbang + 2.0/5.0*360.0 )
    porbvel(PNUM_SHERMAN) = orbvel
    porbang(PNUM_ELAS) = mod360( orbang + 3.0/5.0*360.0 )
    porbvel(PNUM_ELAS) = orbvel
    porbang(PNUM_SERITIL) = mod360( orbang + 4.0/5.0*360.0 )
    porbvel(PNUM_SERITIL) = orbvel
    # Sol's planets.
    orbang = rnduni( 0.0, 360.0 )
    orbvel = rndnor( PLANET_ORBIT_FAC, 2.0 ) * ( rndint( 0, 1 ) * 2 - 1 )
    porbang(PNUM_EARTH) = orbang
    porbvel(PNUM_EARTH) = orbvel
    porbang(PNUM_TELOS) = mod360( orbang + 2.0/3.0*360.0 )
    porbvel(PNUM_TELOS) = orbvel
    porbang(PNUM_OMEGA) = mod360( orbang + 1.0/3.0*360.0 )
    porbvel(PNUM_OMEGA) = orbvel
    # Luna.
    porbvel(PNUM_LUNA) = 12.0 * orbvel
    porbang(PNUM_LUNA) = rnduni( 0.0, 360.0 )
    # Sirius' planets.
    orbang = rnduni( 0.0, 360.0 )
    orbvel = rndnor( PLANET_ORBIT_FAC, 2.0 ) * ( rndint( 0, 1 ) * 2 - 1 )
    porbang(PNUM_ROMULUS) = orbang
    porbvel(PNUM_ROMULUS) = orbvel
    porbang(PNUM_REMUS) = mod360( orbang + 2.0/3.0*360.0 )
    porbvel(PNUM_REMUS) = orbvel
    porbang(PNUM_RHO) = mod360( orbang + 1.0/3.0*360.0 )
    porbvel(PNUM_RHO) = orbvel
    # Kejela's planets.
    orbang = rnduni( 0.0, 360.0 )
    orbvel = rndnor( PLANET_ORBIT_FAC, 2.0 ) * ( rndint( 0, 1 ) * 2 - 1 )
    porbang(PNUM_KLINGUS) = orbang
    porbvel(PNUM_KLINGUS) = orbvel
    porbang(PNUM_LEUDUS) = mod360( orbang + 2.0/3.0*360.0 )
    porbvel(PNUM_LEUDUS) = orbvel
    porbang(PNUM_TARSUS) = mod360( orbang + 1.0/3.0*360.0 )
    porbvel(PNUM_TARSUS) = orbvel
    # Betelgeuse's planets.
    orbang = rnduni( 0.0, 360.0 )
    orbvel = rndnor( PLANET_ORBIT_FAC, 2.0 ) * ( rndint( 0, 1 ) * 2 - 1 )
    porbang(PNUM_ORION) = orbang
    porbvel(PNUM_ORION) = orbvel
    porbang(PNUM_OBERON) = mod360( orbang + 2.0/3.0*360.0 )
    porbvel(PNUM_OBERON) = orbvel
    porbang(PNUM_UMBRIEL) = mod360( orbang + 1.0/3.0*360.0 )
    porbvel(PNUM_UMBRIEL) = orbvel
    # Side systems.
    orbang = rnduni( 0.0, 360.0 )
    orbvel = rndnor( PLANET_ORBIT_FAC, 2.0 ) * ( rndint( 0, 1 ) * 2 - 1 )
    porbang(PNUM_XIDEX) = orbang
    porbvel(PNUM_XIDEX) = orbvel
    porbang(PNUM_RIGELB) = mod360( orbang + 1.0/2.0*360.0 )
    porbvel(PNUM_RIGELB) = orbvel
    orbang = rnduni( 0.0, 360.0 )
    orbvel = rndnor( PLANET_ORBIT_FAC, 2.0 ) * ( rndint( 0, 1 ) * 2 - 1 )
    porbang(PNUM_VENAR) = orbang
    porbvel(PNUM_VENAR) = orbvel
    porbang(PNUM_DYNEB) = mod360( orbang + 1.0/2.0*360.0 )
    porbvel(PNUM_DYNEB) = orbvel
    orbang = rnduni( 0.0, 360.0 )
    orbvel = rndnor( PLANET_ORBIT_FAC, 2.0 ) * ( rndint( 0, 1 ) * 2 - 1 )
    porbang(PNUM_SARAC) = orbang
    porbvel(PNUM_SARAC) = orbvel
    porbang(PNUM_EMINIAR) = mod360( orbang + 1.0/2.0*360.0 )
    porbvel(PNUM_EMINIAR) = orbvel
    orbang = rnduni( 0.0, 360.0 )
    orbvel = rndnor( PLANET_ORBIT_FAC, 2.0 ) * ( rndint( 0, 1 ) * 2 - 1 )
    porbang(PNUM_DAKEL) = orbang
    porbvel(PNUM_DAKEL) = orbvel
    porbang(PNUM_OLDAR) = mod360( orbang + 1.0/2.0*360.0 )
    porbvel(PNUM_OLDAR) = orbvel

    porbvel(PNUM_SYRINX) = 0.1

    # Syrinx's planets.
    orbang = rnduni( 0.0, 360.0 )
    orbvel = rndnor( PLANET_ORBIT_FAC, 2.0 ) * ( rndint( 0, 1 ) * 2 - 1 )
    porbang(PNUM_SHITFACE) = orbang
    porbvel(PNUM_SHITFACE) = orbvel
    porbang(PNUM_HELL) = mod360( orbang + 2.0/3.0*360.0 )
    porbvel(PNUM_HELL) = orbvel
    porbang(PNUM_JINX) = mod360( orbang + 1.0/3.0*360.0 )
    porbvel(PNUM_JINX) = orbvel

    # Spare planets.
    orbang = rnduni( 0.0, 360.0 )
    orbvel = rndnor( PLANET_ORBIT_FAC, 2.0 ) * ( rndint( 0, 1 ) * 2 - 1 )
    porbang(PNUM_SPARE1) = orbang
    porbvel(PNUM_SPARE1) = orbvel

    # Place the planets in their proper orbits.
    for ( i = NUMPLANETS; i >= 1; i = i - 1 )
	if ( pprimary(i) != 0 )
	    {
            px(i) = px(pprimary(i)) + porbrad(i) * cosd(porbang(i))
            py(i) = py(pprimary(i)) + porbrad(i) * sind(porbang(i))
	    }

    # Un-twiddle the lockword.
    PVUNLOCK(lockword)

    # Protect against a system crash here!
    call upchuck

    return

end


###  initrobots - initialize robots
#
#  SYNOPSIS
#    call initrobots
#
subroutine initrobots
NOIMPLICIT

    # SETROBOT( name, pname, team )
    define(SETROBOT,
	{if ( gunum( unum, $1 ) )
	    call stcpn( $2, upname(1,unum), MAXUSERPNAME )
	else if ( register( $1, $2, $3, unum ) )
	    {
	    urobot(unum) = .true.
	    uooption(unum,OOPT_MULTIPLE) = .true.
	    umultiple(unum) = MAXSHIPS}})
    integer i, j, unum
    logical gunum, register
    include "conqdata"				# robot strategy data
    include "conqcom"

    # Create robot guardians.
    SETROBOT( "Romulan", "Colossus", TEAM_ROMULAN )
    SETROBOT( "Orion", "HAL 9000", TEAM_ORION )
    SETROBOT( "Federation", "M-5", TEAM_FEDERATION )
    SETROBOT( "Klingon", "Guardian", TEAM_KLINGON )

    # Copy the strategy table.
    for ( i = 1; i <= MAX_VAR; i = i + 1 )
	for ( j = 1; j <= 10; j = j + 1 )
	    rstrat(i,j) = trstrat(i,j)

    # Copy the action vector.
    for ( i = 1; i <= 32; i = i + 1 )
	rvec(i) = trvec(i)

    externrobots = .false.	# XXX temporary

    return

end


###  initship - initialize a ship for use (DOES MESSAGE LOCKING)
#
#  SYNOPSIS
#    integer snum, unum
#    call initship( snum, unum )
#
# Note: this routine assumes that the ship is reserved.
#
subroutine initship( snum, unum )
NOIMPLICIT
integer snum, unum

    integer i, j
    include "conqcom"

    # sstatus(snum)				# never modified here
    skilledby(snum) = 0
    # suser(snum)				# setup in menu() or newrob()
    # steam(snum)				# setup in menu() or newrob()
    # spid(snum)				# setup in menu() or newrob()
    sx(snum) = 0.0
    sy(snum) = 0.0
    sdx(snum) = 0.0
    sdy(snum) = 0.0
    shead(snum) = 0.0
    sdhead(snum) = 0.0
    swarp(snum) = 0.0
    sdwarp(snum) = 0.0
    slock(snum) = 0
    sshup(snum) = .true.
    sshields(snum) = 100.0
    skills(snum) = 0.0
    sdamage(snum) = 0.0
    sfuel(snum) = 999.0
    swtemp(snum) = 0.0
    setemp(snum) = 0.0
    swfuse(snum) = 0
    sefuse(snum) = 0
    sweapons(snum) = 40
    sengines(snum) = 100 - sweapons(snum)
    sarmies(snum) = 0
    srmode(snum) = .false.
    scloaked(snum) = .false.
    # soption(snum,i)				# setup in menu()
    for ( i = 1; i <= NUMTEAMS; i = i + 1 )
	{
	# srwar(snum,i)				# setup in menu() or newrob()
	# swar(snum,i)				# setup in menu() or newrob()
	sscanned(snum,i) = 0
	}
    for ( i = 1; i <= NUMPLANETS; i = i + 1 )
	ssrpwar(snum,i) = .false.
    # ssdfuse(snum)				# setup in findship()
    PVLOCK(lockmesg)
    if ( slastmsg(snum) == LMSG_NEEDINIT )
	{
	slastmsg(snum) = lastmsg
	salastmsg(snum) = slastmsg(snum)
	}
    PVUNLOCK(lockmesg)
    smap(snum) = .false.
    stowing(snum) = 0
    stowedby(snum) = 0
    slastblast(snum) = 0.0
    slastphase(snum) = 0.0
    spfuse(snum) = 0
    stalert(snum) = .false.
    srobot(snum) = .false.
    saction(snum) = 0
    # spname(1,snum)				# setup in menu() or newrob()

    # Zero torpedos.
    for ( i = 1; i <= MAXTORPS; i = i + 1 )
	{
	tstatus(snum,i) = TS_OFF
	tfuse(snum,i) = 0
	tx(snum,i) = 0.0
	ty(snum,i) = 0.0
	tdx(snum,i) = 0.0
	tdy(snum,i) = 0.0
	tmult(snum,i) = 0.0
	for ( j = 1; j <= NUMTEAMS; j = j + 1 )
	    twar(snum,i,j) = .false.
	}

    # Update user some stats.
    call getdandt( ulastentry(1,unum) )		# time stamp for this entry
    if ( ulastentry(9,unum) == ' ' )		# remove seconds
	call scopy( ulastentry(1,unum), 9, ulastentry(1,unum), 6 )
    ustats(unum,USTAT_ENTRIES) = ustats(unum,USTAT_ENTRIES) + 1
    tstats(steam(snum),TSTAT_ENTRIES) = tstats(steam(snum),TSTAT_ENTRIES) + 1

    return

end


###  inituniverse - initialize (without cheese and tomato) (DOES LOCKING)
#
#  SYNOPSIS
#    call inituniverse
#
subroutine inituniverse
NOIMPLICIT

    integer i
    include "conqcom"

    # Twiddle the lockword.
    PVUNLOCK(lockword)
    PVLOCK(lockword)

    # Turn off the universe.
    closed = .true.

    teamplanets(TEAM_FEDERATION,1) = PNUM_EARTH
    teamplanets(TEAM_FEDERATION,2) = PNUM_TELOS
    teamplanets(TEAM_FEDERATION,3) = PNUM_OMEGA
    teamplanets(TEAM_ROMULAN,1) = PNUM_ROMULUS
    teamplanets(TEAM_ROMULAN,2) = PNUM_REMUS
    teamplanets(TEAM_ROMULAN,3) = PNUM_RHO
    teamplanets(TEAM_KLINGON,1) = PNUM_KLINGUS
    teamplanets(TEAM_KLINGON,2) = PNUM_LEUDUS
    teamplanets(TEAM_KLINGON,3) = PNUM_TARSUS
    teamplanets(TEAM_ORION,1) = PNUM_ORION
    teamplanets(TEAM_ORION,2) = PNUM_OBERON
    teamplanets(TEAM_ORION,3) = PNUM_UMBRIEL

    homeplanet(TEAM_FEDERATION) = PNUM_EARTH
    homeplanet(TEAM_ROMULAN) = PNUM_ROMULUS
    homeplanet(TEAM_KLINGON) = PNUM_KLINGUS
    homeplanet(TEAM_ORION) = PNUM_ORION

    homesun(TEAM_FEDERATION) = PNUM_SOL
    homesun(TEAM_ROMULAN) = PNUM_SIRIUS
    homesun(TEAM_KLINGON) = PNUM_KEJELA
    homesun(TEAM_ORION) = PNUM_BETELGEUSE

    warplim(TEAM_FEDERATION) = 9.0
    warplim(TEAM_ROMULAN) = 8.0
    warplim(TEAM_KLINGON) = 9.0
    warplim(TEAM_ORION) = 10.0

    armylim(TEAM_FEDERATION) = 9
    armylim(TEAM_ROMULAN) = 11
    armylim(TEAM_KLINGON) = 9
    armylim(TEAM_ORION) = 7

    engfac(TEAM_FEDERATION) = 1.0
    engfac(TEAM_ROMULAN) = 0.8
    engfac(TEAM_KLINGON) = 1.0
    engfac(TEAM_ORION) = 1.2

    accelfac(TEAM_FEDERATION) = 1.0
    accelfac(TEAM_ROMULAN) = 0.8
    accelfac(TEAM_KLINGON) = 1.0
    accelfac(TEAM_ORION) = 1.6

    weafac(TEAM_FEDERATION) = 1.0
    weafac(TEAM_ROMULAN) = 1.17
    weafac(TEAM_KLINGON) = 1.0
    weafac(TEAM_ORION) = 0.83

    torpwarp(TEAM_FEDERATION) = 12.0
    torpwarp(TEAM_ROMULAN) = 10.0
    torpwarp(TEAM_KLINGON) = 12.0
    torpwarp(TEAM_ORION) = 14.0

    call stcpn( "Federation", tname(1, TEAM_FEDERATION), MAXTEAMNAME )
    call stcpn( "Romulan", tname(1, TEAM_ROMULAN), MAXTEAMNAME )
    call stcpn( "Klingon", tname(1, TEAM_KLINGON), MAXTEAMNAME )
    call stcpn( "Orion", tname(1, TEAM_ORION), MAXTEAMNAME )
    call stcpn( "self ruled", tname(1, TEAM_SELFRULED), MAXTEAMNAME )
    call stcpn( "non", tname(1, TEAM_NOTEAM), MAXTEAMNAME )
    call stcpn( "GOD", tname(1, TEAM_GOD), MAXTEAMNAME )
    call stcpn( "Empire", tname(1, TEAM_EMPIRE), MAXTEAMNAME )

    chrplanets(PLANET_CLASSM) = 'M'
    chrplanets(PLANET_DEAD) = 'D'
    chrplanets(PLANET_SUN) = 'S'
    chrplanets(PLANET_MOON) = 'm'
    chrplanets(PLANET_GHOST) = 'G'
    chrplanets(PLANET_CLASSA) = 'A'
    chrplanets(PLANET_CLASSO) = 'O'
    chrplanets(PLANET_CLASSZ) = 'Z'

    chrteams(TEAM_FEDERATION) = 'F'
    chrteams(TEAM_ROMULAN) = 'R'
    chrteams(TEAM_KLINGON) = 'K'
    chrteams(TEAM_ORION) = 'O'
    chrteams(TEAM_SELFRULED) = '-'
    chrteams(TEAM_NOTEAM) = ' '
    chrteams(TEAM_GOD) = 'G'
    chrteams(TEAM_EMPIRE) = 'E'

    chrtorps(TEAM_FEDERATION) = '*'
    chrtorps(TEAM_ROMULAN) = '@@'
    chrtorps(TEAM_KLINGON) = '+'
    chrtorps(TEAM_ORION) = '.'

    # Initialize driver variables.
    drivcnt = 0
    drivowner(1) = EOS

    # Initialize user history stuff.
    histptr = 1
    for ( i = 1; i <= MAXHISTLOG; i = i + 1 )
	{
	histunum(i) = -1
	histlog(1,i) = EOS
	}

    # Un-twiddle the lockword.
    PVUNLOCK(lockword)

    call initgame
    call clearships
    call initmsgs

    return

end


###  intrude - possibly send an intruder alert
#
#  SYNOPSIS
#    integer snum, pnum
#    call intrude( snum, pnum )
#
subroutine intrude( snum, pnum )
NOIMPLICIT
integer snum, pnum

    character buf(MSGMAXLINE)
    string atta " attacking"
    string armeq ", armies="
    include "conqcom"

    if ( preal(pnum) &
	 pteam(pnum) != TEAM_SELFRULED &
	 pteam(pnum) != TEAM_NOTEAM )
	if ( snum == MSG_DOOM )
	    {
	    call strcpy( dname, buf )
	    call upper( dname )
	    call appstr( atta, buf )
	    call appstr( armeq, buf )
	    call appint( parmies(pnum), buf )
	    call stormsg( -pnum, -pteam(pnum), buf )
	    }
	else if ( swar(snum,pteam(pnum)) )
	    {
	    call strcpy( "INTRUDER ALERT - ", buf )
	    call appship( snum, buf )
	    call appstr( atta, buf )
	    call appstr( armeq, buf )
	    call appint( parmies(pnum), buf )
	    call stormsg( -pnum, -pteam(pnum), buf )
	    call defend( snum, pnum )
	    }

    return

end


###  loghist - log this entry into the Game (DOES LOCKING)
#
#  SYNOPSIS
#    integer unum
#    call loghist( unum )
#
real function loghist( unum )
NOIMPLICIT
integer unum

    integer modp1
    include "conqcom"

    PVLOCK(lockword)
    histptr = modp1( histptr + 1, MAXHISTLOG )

    call getdandt( histlog(1,histptr) )	# time stamp for this entry
    if ( histlog(9,histptr) == ' ' )		# remove seconds
	call scopy( histlog(1,histptr), 9, histlog(1,histptr), 6 )
    histunum(histptr) = unum
    PVUNLOCK(lockword)

end


###  newarp - handle ship acceleration.
#
#  SYNOPSIS
#    real warp, newarp, dwarp
#    integer snum
#    warp = newarp( snum, dwarp )
#
real function newarp( snum, dwarp )
NOIMPLICIT
integer snum
real dwarp

    real x, acc
    include "conqcom"

    x = dwarp - swarp(snum)
    acc = accelfac(steam(snum)) * engeff( snum ) * ITER_SECONDS
    if ( acc >= abs( x ) )
	return ( dwarp )			# close enough (or equal)
    else if ( x < 0 )
	return ( swarp(snum) - acc )

    return ( swarp(snum) + acc )

end


###  phoon - calculate the phase of a moon
#
#  SYNOPSIS
#    integer phase, phoon, pnum
#    phase = phoon( pnum )
#
integer function phoon( pnum )
NOIMPLICIT
integer pnum

    integer i, j, ph
    real mod360
    include "conqcom"

    # Suns simply don't have phases.
    if ( ptype(pnum) == PLANET_SUN )
	return ( PHOON_NO )

    # You have to orbit some-thing to have a phase.
    i = pprimary(pnum)
    if ( i == 0 )
	return ( PHOON_NO )

    # Your primary must be a non-sun that is real.
    if ( ptype(i) == PLANET_SUN | ! preal(i) )
	return ( PHOON_NO )

    # Your primary has to orbit a (real) sun to have a phase.
    j = pprimary(i)
    if ( j == 0 )
	return ( PHOON_NO )
    if ( ptype(j) != PLANET_SUN | ! preal(j) )
	return ( PHOON_NO )

    # Things are cool, now calculate the phase.
    ph = int( mod360( porbang(pnum) - porbang(i) - 45.0 ) / 90.0 )

    # The number calculated is in the range 0 to 3, and works fine
    # if the moon is orbiting counter clockwise. If it is orbiting
    # in the other direction, we must swap the first and last quarters.
    if ( porbvel(pnum) < 0.0 )
	switch ( ph )
	    {
	    case PHOON_FIRST:
		ph = PHOON_LAST
	    case PHOON_LAST:
		ph = PHOON_FIRST
	    }
    return ( ph )

end


###  planmatch - check if a string matches a planet name
#
#  SYNOPSIS
#    integer planmatch, pnum, godlike
#    character str()
#    logical status, godlike
#    status = planmatch( str, pnum, godlike )
#
logical function planmatch( str, pnum, godlike )
NOIMPLICIT
character str(ARB)
integer pnum
logical godlike

    logical stmatch
    include "conqcom"

    if ( godlike )
	{
	for ( pnum = 1; pnum <= NUMPLANETS; pnum = pnum + 1 )
	    if ( stmatch( str, pname(1,pnum), .false. ) )
		return ( .true. )
	}
    else
	{
	for ( pnum = 1; pnum <= NUMPLANETS; pnum = pnum + 1 )
	    if ( preal(pnum) )
		if ( stmatch( str, pname(1,pnum), .false. ) )
		    return ( .true. )
	}

    return ( .false. )

end


###  puthing - put an object on the display
#
#  SYNOPSIS
#    integer what, lin, col
#    call puthing( what, lin, col )
#
subroutine puthing( what, lin, col )
NOIMPLICIT
integer what, lin, col

    integer i, j, tlin, tcol
    character buf(6,3)
    include "conqcom2"

    switch ( what )
	{
	case PLANET_SUN:
	    call strcpy( " \|/ ", buf(1,1) )
	    call strcpy( "-- --", buf(1,2) )
	    call strcpy( " /|\ ", buf(1,3) )
	case PLANET_CLASSM, PLANET_CLASSA, PLANET_CLASSO,
	    PLANET_CLASSZ, PLANET_DEAD, PLANET_GHOST:
	    call strcpy( " .-. ", buf(1,1) )
	    call strcpy( "(   )", buf(1,2) )
	    call strcpy( " `-' ", buf(1,3) )
	case PLANET_MOON:
	    call strcpy( "     ", buf(1,1) )
	    call strcpy( " ( ) ", buf(1,2) )
	    call strcpy( "     ", buf(1,3) )
	case THING_EXPLOSION:
	    call strcpy( " %%% ", buf(1,1) )
	    call strcpy( "%%%%%", buf(1,2) )
	    call strcpy( " %%% ", buf(1,3) )
	case THING_DEATHSTAR:
	    call strcpy( "/===\", buf(1,1) )
	    call strcpy( "===O=", buf(1,2) )
	    call strcpy( "\===/", buf(1,3) )
	default:
	    call strcpy( " ??? ", buf(1,1) )
	    call strcpy( "?????", buf(1,2) )
	    call strcpy( " ??? ", buf(1,3) )
	}

    for ( j = 1; j <= 3; j = j + 1 )
	{
	tlin = lin + j - 2
	if ( tlin >= 1 & tlin <= DISPLAY_LINS )
	    for ( i = 1; i <= 5; i = i + 1 )
		{
		tcol = col + i - 3
		if ( tcol > STAT_COLS & tcol <= cmaxcol )
		    if ( buf(i,j) != ' ' )
			call cdput( buf(i,j), tlin, tcol )
		}
	}

    return

end


###  putship - place a ship in the universe
#
#  SYNOPSIS
#    integer snum
#    real basex, basey
#    call putship( snum, basex, basey )
#
subroutine putship( snum, basex, basey )
NOIMPLICIT
integer snum
real basex, basey

    integer i, j
    real smear, rndnor
    include "conqcom"

    smear = ENTRY_SMEAR_DIST
    for ( j = 1; j <= 64; j = j + 1 )
	{
	# If we're doing poorly, expand the area of entry.
	if ( mod( j, 16 ) == 0 )
	    smear = smear * 1.2
	sx(snum) = rndnor( basex, smear )
	sy(snum) = rndnor( basey, smear )
	for ( i = 1; i <= MAXSHIPS; i = i + 1 )
	    if ( sstatus(i) == SS_LIVE )
		if ( satwar( i, snum ) & i != snum &
	    dist( sx(snum), sy(snum), sx(i), sy(i) ) <= ENTRY_ENEMY_DIST )
		    next 2
	# If we got here, then the position isn't near an enemy ship.
	return
	}

    # If we got here, we couldn't find a "good" position,
    #  so just report the error and let it slide.
    call cerror( MSG_GOD, "putship: Failed retry maximum on ship %d", snum )

    return

end


###  readmsg - display a message
#
#  SYNOPSIS
#    integer snum, msgnum
#    call readmsg( snum, msgnum )
#
subroutine readmsg( snum, msgnum )
NOIMPLICIT
integer snum, msgnum

    integer i
    character buf(MSGMAXLINE)
    include "conqcom"

    # Format who the message is from.
    i = msgfrom(msgnum)
    if ( i >= 1 & i <= MAXSHIPS )
	{
	buf(1) = EOS
	call appship( i, buf )
	}
    else if ( -i >= 1 & -i <= NUMPLANETS )
	call strcpy( pname(1,-i), buf )
    else switch ( i )
	{
	case MSG_NOONE:
	    call strcpy( "nobody", buf )
	case MSG_GOD:
	    call strcpy( "GOD", buf )
	case MSG_DOOM:
	    call concat( "The ", dname, buf )
	case MSG_OUTSIDE:
	    buf(1) = EOS
	case MSG_COMP:
	    call strcpy( "Comp", buf )
	default:
	    call strcpy( "???", buf )
	}

    call appstr( "->", buf )

    # Format who the message is to.
    i = msgto(msgnum)
    if ( i == snum )
	call appstr( "you", buf )
    else if ( i >= 1 & i <= MAXSHIPS )
	call appship( i, buf )
    else if ( -i >= 1 & -i <= NUMTEAMS )
	call appchr( chrteams(-i), buf )
    else switch ( i )
	{
	case MSG_NOONE:
	    call appstr( "nobody", buf )
	case MSG_GOD:
	    call appstr( "GOD", buf )
	case MSG_ALL:
	    call appstr( "ALL", buf )
	case MSG_IMPLEMENTORS:
	    call appstr( "IMPs", buf )
	default:
	    call appstr( "???", buf )
	}

    call appstr( ": ", buf )
    call appstr( msgbuf(1,msgnum), buf )

    call putmsg( buf, MSG_LIN1 )
    call cdclrl( MSG_LIN2, 1 )

    return

end


###  sendmsg - prompt the user for a message and send it
#
#  SYNOPSIS
#    integer from
#    call sendmsg( from, terse )
#
subroutine sendmsg( from, terse )
NOIMPLICIT
integer from
logical terse

    integer i, j, alldig, to, length
    character ch, cdgetx, cdgetp, buf(MSGMAXLINE), msg(MESSAGE_SIZE)
    logical l, editing, mailimps, safectoi
    string mto "Message to: "
    string nf "Not found."
    string huh "I don't understand."
    common / conqmsg / to, msg
    data to / MSG_NOONE /
    data msg(1) / EOS /
    include "conqcom"
    include "conqcom2"

    # First, find out who we're sending to.
    call cdclrl( MSG_LIN1, 2 )
    ch = cdgetx( mto, MSG_LIN1, 1, TERMS, buf, MSGMAXLINE )
    if ( ch == TERM_ABORT )
	{
	call cdclrl( MSG_LIN1, 1 )
	return
	}

    # LINEFEED means use the target and text from the last message.
    editing = ( ch == TERM_EXTRA & buf(1) == EOS )
    if ( editing )
	{
	# Make up a default string using the last target.
	if ( to >= 1 & to <= MAXSHIPS )
	    call prints( buf, "%d", to )
	else if ( -to >= 1 & -to <= NUMTEAMS )
	    call strcpy( tname(1,-to), buf )
	else switch ( to )
	    {
	    case MSG_ALL:
		call strcpy( "All", buf )
	    case MSG_GOD:
		call strcpy( "GOD", buf )
	    case MSG_IMPLEMENTORS:
		call strcpy( "Implementors", buf )
	    default:
		buf(1) = EOS
	    }

	# Prompt for the target again with the default.
	if ( cdgetp( mto, MSG_LIN1, 1, TERMS, buf, MSGMAXLINE ) == TERM_ABORT )
	    {
	    call cdclrl( MSG_LIN1, 1 )
	    return
	    }
	}

    # Got a target, parse it.
    call delblanks( buf )
    call upper( buf )
    if ( alldig( buf ) == YES )
	{
	# All digits means a ship number.
	i = 1
	l = safectoi( j, buf, i )		# ignore status
	if ( j < 1 | j > MAXSHIPS )
	    {
	    call putmsg( "No such ship.", MSG_LIN2 )
	    return
	    }
	if ( sstatus(j) != SS_LIVE )
	    {
	    call putmsg( nf, MSG_LIN2 )
	    return
	    }
	to = j
	}
    else switch ( buf(1) )
	{
	case 'A':
	    to = MSG_ALL
	case 'G':
	    to = MSG_GOD
	case 'I':
	    to = MSG_IMPLEMENTORS
	default:
	    # Check for a team character.
	    for ( i = 1; i <= NUMTEAMS; i = i + 1 )
		if ( buf(1) == chrteams(i) )
		    break
	    if ( i > NUMTEAMS )
		{
		call putmsg( huh, MSG_LIN2 )
		return
		}
	    to = -i
	}

    # Now, construct a header for the selected target.
    call strcpy( "Message to ", buf )
    if ( to >= 1 & to <= MAXSHIPS )
	{
	if ( sstatus(to) != SS_LIVE )
	    {
	    call putmsg( nf, MSG_LIN2 )
	    return
	    }
	call appship( to, buf )
	call appchr( ':', buf )
	}
    else if ( -to >= 1 & -to <= NUMTEAMS )
	{
	call appstr( tname(1,-to), buf )
	call appstr( "s:", buf )
	}
    else switch ( to )
	{
	case MSG_ALL:
	    call appstr( "everyone:", buf )
	case MSG_GOD:
	    call appstr( "GOD:", buf )
	case MSG_IMPLEMENTORS:
	    call appstr( "The Implementors:", buf )
	default:
	    call putmsg( huh, MSG_LIN2 )
	    return
	}

    if ( ! terse )
	call appstr( " (ESCAPE to abort)", buf )

    call putmsg( buf, MSG_LIN1 )
    call cdclrl( MSG_LIN2, 1 )

    if ( ! editing )
	msg(1) = EOS

    if ( to == MSG_IMPLEMENTORS )
	i = MSGMAXLINE
    else
	i = MESSAGE_SIZE

    if ( cdgetp( ">", MSG_LIN2, 1, TERMS, msg, i ) != TERM_ABORT )
	if ( to != MSG_IMPLEMENTORS )
	    call stormsg( from, to, msg )
	else
	    {
	    # Handle a message to the Implementors.
	    call strcpy( "Communique from ", buf )
	    if ( from >= 1 & from <= MAXSHIPS )
		{
		call appstr( spname(1,from), buf )
		call appstr( " on board ", buf )
		call appship( from, buf )
		}
	    else if ( from == MSG_GOD )
		call appstr( "GOD", buf )
	    else
		{
		call appchr( '(', buf )
		call appint( from, buf )
		call appchr( ')', buf )
		}
	    call stormsg( from, MSG_IMPLEMENTORS, msg ) # let GOD read it also
	    if ( ! mailimps( buf, msg ) )
		call stormsg( MSG_OUTSIDE, from, msg )	# store error message
	    }

    call cdclrl( MSG_LIN1, 2 )

    return

end


###  sortplanets - sort planets by name
#
#  SYNOPSIS
#    integer sv(NUMPLANETS)
#    call sortplanets( sv )
#
#	sv - sort vector of planet names
#
# This routine ASSUMES that the sort vector is initialized,
# for the reason that it is fastest to sort a list that is
# already sorted.
#
subroutine sortplanets( sv )
NOIMPLICIT
integer sv(ARB)

    integer i, j, pnum, np, strcmp
    integer osv(NUMPLANETS)
    include "conqcom"

    # Copy the old sort vector.
    for ( pnum = 1; pnum <= NUMPLANETS; pnum = pnum + 1 )
	osv(pnum) = sv(pnum)

    # Sort planet names into sv() in an insertion sort.
    np = 0		# start with no planets
    for ( pnum = 1; pnum <= NUMPLANETS; pnum = pnum + 1 )
	{
	for ( i = 1; i <= np; i = i + 1  )
	    if ( strcmp( pname(1,sv(i)), pname(1,osv(pnum)) ) > 0 )
		{
		for ( j = np; j >= i; j = j - 1 )
		    sv(j+1) = sv(j)
		break
		}
	sv(i) = osv(pnum)
	np = np + 1
	}

    return

end


###  spwar - test whether a ship is at war with a planet
#
#  SYNOPSIS
#    logical atwar, spwar
#    integer snum, pnum
#    atwar = spwar( snum, pnum )
#
logical function spwar( snum, pnum )
NOIMPLICIT
integer snum, pnum

    include "conqcom"

    if ( ! preal(pnum) )
	return ( .false. )		# can't be at war unless it's real
    else if ( ptype(pnum) == PLANET_SUN )
	return ( .true. )		# always at war with suns
    if ( ptype(pnum) == PLANET_MOON )
	return ( .false. )		# never at war with moons
    else if ( parmies(pnum) <= 0 )
	return ( .false. )		# can't have war without armies
    else switch ( pteam(pnum) )
	{
	case TEAM_FEDERATION, TEAM_ROMULAN, TEAM_KLINGON, TEAM_ORION:
	    if ( pteam(pnum) == steam(snum) )
		return ( .false. )
	    else
		return ( swar(snum,pteam(pnum)) )
	default:
	    return ( ssrpwar(snum,pnum) )
	}

    return ( .true. )			# can't get here...

end


###  stillalive - determine if a ship is still alive
#
#  SYNOPSIS
#    logical flag, stillalive
#    integer snum
#    flag = stillalive( snum )
#
logical function stillalive( snum )
NOIMPLICIT
integer snum

    include "conqcom"

    # Look for religious trouble or the "closed" sign in the window.
    if ( uooption(suser(snum),OOPT_SHITLIST) )
	{
	if ( sstatus(snum) == SS_LIVE )
	    call kill( snum, KB_SHIT )
	return ( .false. )
	}
    if ( closed & ! uooption(suser(snum),OOPT_PLAYWHENCLOSED) )
	{
	if ( sstatus(snum) == SS_LIVE )
	    call kill( snum, KB_EVICT )
	return ( .false. )
	}

    if ( sstatus(snum) == SS_RESERVED | sstatus(snum) == SS_ENTERING )
	return ( .true. )

    return ( sstatus(snum) == SS_LIVE )

end


###  stormsg - store a message in the message buffer (DOES LOCKING)
#
#  SYNOPSIS
#    integer from, to
#    character msg()
#    call stormsg( from, to, msg )
#
subroutine stormsg( from, to, msg )
NOIMPLICIT
integer from, to
character msg(ARB)

    integer nlastmsg, modp1, i
    include "conqcom"

    PVLOCK(lockmesg)
    nlastmsg = modp1( lastmsg+1, MAXMESSAGES )
    call stcpn( msg, msgbuf(1,nlastmsg), MESSAGE_SIZE )
    msgfrom(nlastmsg) = from
    msgto(nlastmsg) = to
    lastmsg = nlastmsg

    # Remove allowable last message restrictions.
    for ( i = 1; i<=MAXSHIPS; i = i + 1 )
	if ( nlastmsg == salastmsg(i) )
	    salastmsg(i) = LMSG_READALL
    PVUNLOCK(lockmesg)

    return

end


###  usefuel - consume a quantity of matter-antimatter devices
#
#  SYNOPSIS
#    integer snum
#    real fuel
#    logical ok, usefuel, weapon
#    ok = usefuel( snum, fuel, weapon )
#
logical function usefuel( snum, fuel, weapon )
NOIMPLICIT
integer snum
real fuel
logical weapon

    real rnd, rnduni
    integer rndint
    include "conqcom"

    if ( fuel <= 0.0 )
	return ( .false. )
    if ( weapon )
	{
	if ( swfuse(snum) > 0 )
	    return ( .false. )
	}
    else
	{
	if ( sefuse(snum) > 0 )
	    {
	    sdwarp(snum) = 0.0
	    return ( .false. )
	    }
	}
    sfuel(snum) = sfuel(snum) - fuel
    if ( sfuel(snum) < 0.0 )
	{
	# When you're out of gas, you're out of fun...
	sfuel(snum) = 0.0
	scloaked(snum) = .false.
	sdwarp(snum) = 0.0
	return ( .false. )
	}
    else if ( sfuel(snum) > 999.0 )
	sfuel(snum) = 999.0

    # Temperature.
    if ( weapon )
	{
	swtemp(snum) = swtemp(snum) + fuel * TEMPFUEL_FAC / weaeff ( snum )
	if ( swtemp(snum) < 0.0 )
	    swtemp(snum) = 0.0
	else if ( swtemp(snum) >= 100.0 )
	    if ( rnd( 0 ) < WEAPON_DOWN_PROB )
		{
		swfuse(snum) = rndint( MIN_DOWN_FUSE, MAX_DOWN_FUSE )
		if ( ! soption(snum,OPT_TERSE) )
		    call stormsg( MSG_COMP, snum, "Weapons overload." )
		}
	}
    else
	{
	setemp(snum) = setemp(snum) + fuel * TEMPFUEL_FAC / engeff( snum )
	if ( setemp(snum) < 0.0 )
	    setemp(snum) = 0.0
	else if ( setemp(snum) >= 100.0 )
	    if ( rnd( 0 ) < ENGINE_DOWN_PROB )
		{
		sefuse(snum) = rndint( MIN_DOWN_FUSE, MAX_DOWN_FUSE )
		if ( ! soption(snum,OPT_TERSE) )
		    call stormsg( MSG_COMP, snum, "Engines super-heated." )
		}
	}

    return ( .true. )

end


###  zeroeverything - initialize the common block to zeros
#
#  SYNOPSIS
#    call zeroeverything
#
subroutine zeroeverything
NOIMPLICIT

    include "conqcom"

    call cdfill( 0, commonrev, SIZEOF_COMMONBLOCK )

    return

end


###  zeroship - turn OFF a ship and its torpedos
#
#  SYNOPSIS
#    integer snum
#    call zeroship( snum )
#
subroutine zeroship( snum )
NOIMPLICIT
integer snum

    integer i, j
    include "conqcom"

    sstatus(snum) = SS_OFF
    skilledby(snum) = 0
    suser(snum) = 0
    steam(snum) = 0
    spid(snum) = 0
    sx(snum) = 0.0
    sy(snum) = 0.0
    sdx(snum) = 0.0
    sdy(snum) = 0.0
    shead(snum) = 0.0
    sdhead(snum) = 0.0
    swarp(snum) = 0.0
    sdwarp(snum) = 0.0
    slock(snum) = 0
    sshup(snum) = .false.
    sshields(snum) = 0.0
    skills(snum) = 0.0
    sdamage(snum) = 0.0
    sfuel(snum) = 0.0
    setemp(snum) = 0.0
    swtemp(snum) = 0.0
    swfuse(snum) = 0
    sefuse(snum) = 0
    sweapons(snum) = 0
    sengines(snum) = 0
    sarmies(snum) = 0
    srmode(snum) = .false.
    scloaked(snum) = .false.
    for ( i = 1; i <= MAXOPTIONS; i = i + 1 )
	soption(snum,i) = .false.
    for ( i = 1; i <= NUMTEAMS; i = i + 1 )
	{
	srwar(snum,i) = .false.
	swar(snum,i) = .false.
	sscanned(snum,i) = 0
	}
    for ( i = 1; i <= NUMPLANETS; i = i + 1 )
	ssrpwar(snum,i) = .false.
    ssdfuse(snum) = 0
    slastmsg(snum) = 0
    salastmsg(snum) = 0
    smap(snum) = .false.
    stowing(snum) = 0
    stowedby(snum) = 0
    slastblast(snum) = 0.0
    slastphase(snum) = 0.0
    spfuse(snum) = 0
    stalert(snum) = .false.
    srobot(snum) = .false.
    saction(snum) = 0
    for ( i = 1; i <= SIZEUSERPNAME; i = i + 1 )
	spname(i,snum) = EOS
    sctime(snum) = 0
    setime(snum) = 0
    scacc(snum) = 0
    seacc(snum) = 0

    for ( i = 1; i <= MAXTORPS; i = i + 1 )
	{
	tstatus(snum,i) = TS_OFF
	tfuse(snum,i) = 0
	tx(snum,i) = 0.0
	ty(snum,i) = 0.0
	tdx(snum,i) = 0.0
	tdy(snum,i) = 0.0
	tmult(snum,i) = 0.0
	for ( j = 1; j <= NUMTEAMS; j = j + 1 )
	    twar(snum,i,j) = .false.
	}

    return

end
