###############################################################################
#
#                               C O N Q D R I V
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

define(SUBMIN_SECONDS,5)	# seconds between planet movement
define(MINUTE_SECONDS,60)
define(FIVEMINUTE_SECONDS,300)


###  conqdriv - main program (DOES LOCKING)
#
program conqdriv
NOIMPLICIT

    integer s, i, j, pid, drivtenths, ship(MAXSHIPS)
    integer dsecs, modp1, rndint, strcmp, getdcl
    integer ctime, etime, cacc, eacc
    logical force, isagod
    character buf(ARGBUFSIZE)
    string vern VERSION_NUMBER
    string verd VERSION_DATE
    include "conqcom"

    # First things first.
    if ( commonrev != COMMONSTAMP )
	call sys$exit

    call initstats( ctime, etime )
    cacc = 0
    eacc = 0

    PVLOCK(lockword)
    call gsecs( drivtime )		# prevent driver timeouts
    call gsecs( playtime )

    # Look for the force flag.
    i = getdcl( buf )
    call fold( buf )
    if ( strcmp( buf, "-f" ) == 0 )
	{
	call glname( buf )
	force = isagod( buf )
	}
    else
	force = .false.

    if ( ! force )
	{
	# Make sure we're supposed to be starting.
	if ( drivstat != DRS_RESTART )
	    {
	    PVUNLOCK(LOCKWORD)
	    call sys$exit
	    }

	if ( drivpid != 0 )
	    {
	    PVUNLOCK(LOCKWORD)
	    call sys$exit
	    }
	}
    else
	{
	# Kill the other driver.
	if ( drivstat == DRS_RUNNING )
	    {
	    drivstat = DRS_KAMIKAZE
	    i = TIMEOUT_DRIVER * ITER_TENTHS
	    while ( drivstat != DRS_OFF & i > 0 )
		{
		call sleep( ITER_SECONDS )
		i = i - ITER_TENTHS
		}
	    }
	}

    drivstat = DRS_STARTING		# show intent of becoming "the" driver
    call rndini( 0, 0 )			# init random numbers
    call getpid( pid )			# store our pid
    drivpid = pid
    call glname( drivowner )		# store our username

    # Start within bounds.
    drivsecs = modp1( drivsecs, FIVEMINUTE_SECONDS )

    # Special hack to cause the one second fuse to expire upon entry.
    drivtenths = 10

    drivstat = DRS_RUNNING

    PVUNLOCK(LOCKWORD)

    if ( force )
	call cerror( MSG_GOD,
	    "My Lord, driver %x reporting. I have assumed control.", pid )

    for ( s = 1; s <= MAXSHIPS; s = s + 1 )
	ship(s) = s

    while ( pid == drivpid & drivstat != DRS_KAMIKAZE )
	{
	if ( drivtenths >= 10 )
	    {
	    # Finished a second.
	    drivtenths = 0

	    # Check for player timeout.
	    if ( dsecs( playtime, drivtime ) >= TIMEOUT_PLAYER )
		{
		drivpid = 0
		drivstat = DRS_OFF
		drivowner(1) = EOS
		call upchuck
		break
		}

	    if ( drivstat == DRS_RUNNING )
		{
		# Randomize ship ordering.
		for ( s = 1; s <= MAXSHIPS; s = s + 1 )
		    {
		    i = rndint( 1, MAXSHIPS )
		    j = ship(i)
		    ship(i) = ship(s)
		    ship(s) = j
		    }

		# Do the big things first to sync the small things.
		drivsecs = modp1( drivsecs + 1, FIVEMINUTE_SECONDS )
		if ( mod( drivsecs, FIVEMINUTE_SECONDS ) == 0 )
		    call fivemindrive( ship )
		if ( mod( drivsecs, MINUTE_SECONDS ) == 0 )
		    call mindrive( ship )
		if ( mod( drivsecs, SUBMIN_SECONDS ) == 0 )
		    {
		    call submindrive
		    call upstats( ctime, etime, cacc, eacc,
			dcpuseconds, delapsedseconds )
		    }
		call secdrive( ship )

		# Update the common block every minute.
		if ( mod( drivsecs, MINUTE_SECONDS ) == 0 )
		    call upchuck
		}
	    }
	if ( drivstat == DRS_RUNNING )
	    call iterdrive( ship )
	call sleep( ITER_SECONDS )
	drivtenths = drivtenths + ITER_TENTHS
	}

    # See if we should turn off.
    if ( drivstat == DRS_KAMIKAZE )
	{
	drivpid = 0
	drivstat = DRS_OFF
	drivowner(1) = EOS
	}

    # Make last minute driver stats update.
    call upstats( ctime, etime, cacc, eacc, dcpuseconds, delapsedseconds )

end


###  iterdrive - drive the universe one iteration
#
#  SYNOPSIS
#    integer ship(MAXSHIPS)
#    call iterdrive( ship )
#
subroutine iterdrive( ship )
NOIMPLICIT
integer ship(ARB)

    integer s, t, i, j, k
    real h, ad, x, dis, ht, explosion, angle, subang
    real acdist, newarp
    character buf(MSGMAXLINE)
    include "conqcom"

    # Drive the ships.
    for ( s = 1; s <= MAXSHIPS; s = s + 1 )
	{
	i = ship(s)
	if ( sstatus(i) == SS_LIVE )
	    {
	    # Phaser fuses.
	    if ( spfuse(i) > 0 )
		spfuse(i) = max( 0, spfuse(i) - ITER_TENTHS )

	    # Turning.
	    if ( swarp(i) >= 0.0 & sdhead(i) != shead(i) )
		{
		h = shead(i)
		ad = subang( h, sdhead(i) )
		x = max( 210.0 - swarp(i)*20.0/engeff( i ), 2.0 ) * ITER_SECONDS
		if ( abs( ad ) <= x )
		    shead(i) = sdhead(i)
		else if ( ad < 0.0 )
		    shead(i) = h + x
		else
		    shead(i) = h - x

		call fixdeltas( i )
		}

	    # Movement.
	    j = stowedby(i)
	    if ( j != 0 )
		{
		# Being towed; slowly (grossly?) align with our tower.
		swarp(i) = 0.0
		sdwarp(i) = 0.0
		h = angle( sx(i), sy(i), sx(j), sy(j) )
		ad = subang( h, shead(j) )
		if ( ad < 0.0 )
		    h = h - max( ad, -10.0*ITER_SECONDS )
		else
		    h = h - min( ad, +10.0*ITER_SECONDS )

		sx(i) = sx(j) + TOW_DIST * cosd(h+180.0)
		sy(i) = sy(j) + TOW_DIST * sind(h+180.0)
		}
	    else if ( swarp(i) >= 0.0 )
		{
		# Cruising.
		x = min( sdwarp(i), maxwarp( i ) )
		if ( swarp(i) != x )
		    {
		    swarp(i) = newarp( i, x )
		    call fixdeltas( i )
		    }

		sx(i) = sx(i) + sdx(i)
		sy(i) = sy(i) + sdy(i)

		# If we're locked onto a planet but not orbiting it see if
		# we are close enough to orbit.
		j = -slock(i)
		if ( j > 0 )
		    {
		    # Make sure the planet is still real.
		    if ( ! preal(j) )
			slock(i) = 0
		    else if ( swarp(i) >= 0.0 )
			{
			# Still moving; if we're going slow enough to orbit,
			#  check if we're close enough to do so. Otherwise,
			#  check to see if it's time to slow down yet.
			dis = dist( sx(i), sy(i), px(j), py(j) )
			if ( swarp(i) <= MAX_ORBIT_WARP )
			    {
			    # Going slow enough to orbit.
			    if ( dis <= ORBIT_DIST )
				{
				# Close enough to orbit.
				call orbit( i, j )
				if ( ! soption(i,OPT_TERSE) )
				    {
				    call prints( buf,
					"Coming into orbit around %s.",
					pname(1,j) )
				    call stormsg( MSG_COMP, i, buf )
				    }
				}
			    }
			else if ( ( dis - ORBIT_DIST ) <=
			    acdist( swarp(i), MAX_ORBIT_WARP,
			    accelfac(steam(i)) * engeff( i ) ) )
			    {
			    # Time to slow down.
			    if ( sdwarp(i) > MAX_ORBIT_WARP )
				{
				sdwarp(i) = MAX_ORBIT_WARP
				if ( ! soption(i,OPT_TERSE) )
				    {
				    # "WARNING, WILL ROBINSON!!"
				    call prints( buf,
		    "Approaching %s - commencing orbital insertion maneuver.",
					pname(1,j) )
				    call stormsg( MSG_COMP, i, buf )
				    }
				}
			    }
			}
		    }
		}
	    }
	}

    # Drive the torps.
    for ( s = 1; s <= MAXSHIPS; s = s + 1 )
	{
	i = ship(s)
	if ( sstatus(i) != SS_OFF )
	    {
	    for ( j = 1; j <= MAXTORPS; j = j + 1 )
	        if ( tstatus(i,j) == TS_LIVE )
	            {
		    # Movement.
		    tx(i,j) = tx(i,j) + tdx(i,j)
		    ty(i,j) = ty(i,j) + tdy(i,j)
		    }
	        else if ( tstatus(i,j) == TS_DETONATE )
		    {
		    # Detonate.
		    tfuse(i,j) = FIREBALL_FUSE
		    tstatus(i,j) = TS_FIREBALL
		    for ( t = 1; t <= MAXSHIPS; t = t + 1 )
			{
			k = ship(t)
			if ( sstatus(k) == SS_LIVE & k != i )
			    if ( twar(i,j,steam(k)) | swar(k,steam(i)) )
				{
				ht = explosion( TORPEDO_HIT * tmult(i,j),
				    dist( tx(i,j), ty(i,j), sx(k), sy(k) ) )
				if ( ht > 0.0 )
				    call hit( k, ht, i )
				}
			}
		    }
	    }
	}

    # Drive the planet eater.
    if ( dstatus == DS_LIVE )
	{
	dx = dx + ddx
	dy = dy + ddy
	}

    return

end


###  secdrive - drive the one-second interval items (DOES LOCKING)
#
#  SYNOPSIS
#    integer ship(MAXSHIPS)
#    call secdrive( ship )
#
subroutine secdrive( ship )
NOIMPLICIT
integer ship(ARB)

    integer s, t, i, j, k, rndint, pnum, ctemp
    real mod360, dis, angle, repair, inc, dec
    real rnduni, rnd, rndnor, x, warp
    logical talert(MAXSHIPS), spwar, l, usefuel
    character buf(MSGMAXLINE)
    include "conqcom"

    for ( s = 1; s <= MAXSHIPS; s = s + 1 )
	{
	i = ship(s)
	if ( sstatus(i) == SS_OFF )
	    next

	if ( sstatus(i) != SS_LIVE )
	    {
	    # Turn off timed out ships.
	    if ( ssdfuse(i) < 0 )
		{
		# This code may be too safe...
		PVLOCK(lockword)
		if ( sstatus(i) != SS_LIVE & ssdfuse(i) < 0 )
		    {
		    ssdfuse(i) = ssdfuse(i) + 1
		    if ( ssdfuse(i) == 0 )
			{
			ssdfuse(i) = 0
			skilledby(i) = 0
			sstatus(i) = SS_OFF
			}
		    }
		PVUNLOCK(LOCKWORD)
		next
		}
	    }

	# The ship is alive; see if we've been kicked out.
	if ( uooption(suser(i),OOPT_SHITLIST) )
	    {
	    call kill( i, KB_SHIT )
	    next
	    }
	if ( closed )
	    if ( ! uooption(suser(i),OOPT_PLAYWHENCLOSED) )
		{
		call kill( i, KB_EVICT )
		next
		}

	# The ship is still alive.
	if ( srobot(i) )
	    if ( ! externrobots )
		call robotai( i )

	# Ship movement again.
	warp = swarp(i)
	pnum = -slock(i)
	if ( pnum >= 1 & pnum <= NUMPLANETS )
	    if ( warp < 0.0 )
		{
		# Orbiting.
		if ( warp == ORBIT_CW )
		    {
		    # Orbiting clockwise.
		    shead(i) = mod360( shead(i) - ORBIT_FAC )
		    sx(i) = px(pnum) + ORBIT_DIST * cosd(shead(i)+90.0)
		    sy(i) = py(pnum) + ORBIT_DIST * sind(shead(i)+90.0)
		    }
		else if ( warp == ORBIT_CCW )
		    {
		    # Orbiting counter-clockwise.
		    shead(i) = mod360( shead(i) + ORBIT_FAC )
		    sx(i) = px(pnum) + ORBIT_DIST * cosd(shead(i)-90.0)
		    sy(i) = py(pnum) + ORBIT_DIST * sind(shead(i)-90.0)
		    }
		}
	    else
		{
		# Cruising, locked on; update ship's desired heading.
		sdhead(i) = angle(sx(i), sy(i), px(pnum), py(pnum))
		}

	# Ships - Teams.
	for ( j = 1; j <= NUMTEAMS; j = j + 1 )
	    if ( sscanned(i,j) > 0 )
		sscanned(i,j) = sscanned(i,j) - 1

	# Ships, planets and suns scans.
	for ( j = 1; j <= NUMPLANETS; j = j + 1 )
	    if ( preal(j) )
		{
		# Do we scan the planet?
		dis = dist( sx(i), sy(i), px(j), py(j) )
		if ( dis <= PLANET_DIST )
		    {
		    k = steam(i)
		    if ( k >= 1 & k <= NUMTEAMS )
			pscanned(j,k) = .true.

		    # Planet armies (and suns) get to do damage here.
		    if ( spwar( i,j ) & parmies(j) > 0 )
			call hit( i,
			   rndnor( PLANET_HIT + parmies(j) * ARMY_HIT, 1.0 ),
			   -j )
		    }

		# Does the planet scan us?
		if ( j <= NUMPLANETS )
		    if ( dis <= ACCINFO_DIST )
			if ( ! scloaked(i) )
			    {
			    k = pteam(j)
			    if ( k >= 1 & k <= NUMTEAMS )
				sscanned(i,k) = SCANNED_FUSE
			    }
		}

	# Planet eater.
	if ( dstatus == DS_LIVE )
	    if ( dist( sx(i), sy(i), dx, dy ) <= DOOMSDAY_DIST )
		call hit( i, rndnor( DOOMSDAY_HIT, 1.0 ), KB_DOOMSDAY )

	# Negative energy barrier.
	if ( abs( sx(i) ) >= NEGENB_DIST | abs(sy(i)) >= NEGENB_DIST )
	    if ( abs( sx(i) ) <= NEGENBEND_DIST &
		 abs( sy(i) ) <= NEGENBEND_DIST )
		call hit( i, NEGENB_HIT, KB_NEGENB )

	# Shields.
	if ( sshields(i) < 100.0 )
	    {
	    # Shields repair twice as fast when they're down.
	    x = SHIELD_FAC
	    if ( ! sshup(i) )
		x = x * 2.0
	    sshields(i) = min( 100.0, sshields(i) + x )
	    }

	# Repair.
	repair = REPAIR_FAC
	if ( srmode(i) )
	    {
	    scloaked(i) = .false.
	    if ( swarp(i) >= 0.0 )
		sdwarp(i) = 0.0
	    repair = repair * RMODE_REPAIR_MULT
	    }
	if ( swarp(i) < 0.0 )			# orbiting
	    if ( ! spwar( i,-slock(i) ) )	# a friendly
		if ( parmies(-slock(i)) > 0 )	# populated planet
		    repair = repair * PLANET_REPAIR_MULT
	sdamage(i) = sdamage(i) - repair
	if ( sdamage(i) < 0.0 )
	    {
	    sdamage(i) = 0.0
	    if ( srmode(i) )
		srmode(i) = .false.
	    }

	# Weapons/engines down fuses.
	if ( swfuse(i) > 0 )
	    {
	    swfuse(i) = swfuse(i) - 1
	    if ( swfuse(i) <= 0 )
		if ( ! soption(i,OPT_TERSE) )
		    call stormsg( MSG_COMP, i, "Weapons are back on-line." )
	    }
	if ( sefuse(i) > 0 )
	    {
	    sefuse(i) = sefuse(i) - 1
	    sdwarp(i) = 0.0
	    if ( sefuse(i) <= 0 )
		if ( ! soption(i,OPT_TERSE) )
		    call stormsg( MSG_COMP, i, "Engines are now functional." )
	    }

	# Fuel.
	inc = FUEL_FAC
	dec = 0.0
	if ( swarp(i) < 0.0 )
	    {
	    # You get fuel for orbiting friendly, populated class M,
	    #  with shields down.
	    if ( ! sshup(i) | srmode(i) )
		if ( ptype(-slock(i)) == PLANET_CLASSM )
		    if ( ! spwar( i,-slock(i) ) )
			if ( parmies(-slock(i)) > 0 )
			    inc = inc * MPLANET_FUEL_MULT
	    }
	else
	    {
	    # Cruising.
	    if ( sshup(i) )
		dec = dec + sdwarp(i) * FUELWARP_FAC * FUELSHIELDS_MULT
	    else
		dec = dec + sdwarp(i) * FUELWARP_FAC
	    dec = dec + sdwarp(i) * sarmies(i) * FUELWARPARMY_FAC
	    if ( stowing(i) != 0 )
		{
		if ( sshup(stowing(i)) )
		    dec = dec + sdwarp(i) * FUELWARP_FAC * FUELSHIELDS_MULT
		else
		    dec = dec + sdwarp(i) * FUELWARP_FAC
		dec = dec + sdwarp(i) * sarmies(stowing(i)) * FUELWARPARMY_FAC
		}
	    }
	# Cloaking.
	if ( scloaked(i) )
	    {
	    srmode(i) = .false.
	    dec = dec + CLOAK_FUEL
	    if ( swarp(i) > 0.0 )
		dec = dec + sdwarp(i) * CLOAK_WARP_FUEL
	    }
	sfuel(i) = min( 999.0, sfuel(i) + inc )
	if ( dec > 0.0 )
	    l = usefuel( i, dec, .false. )

	# Cool-down.
	swtemp(i) = max( 0.0, swtemp(i) - WEAPON_COOL_FAC )
	setemp(i) = max( 0.0, setemp(i) - ENGINE_COOL_FAC )
	}

    # Torp alert logic.
    for ( i = 1; i <= MAXSHIPS; i = i + 1 )
	talert(i) = .false.
    for ( s = 1; s <= MAXSHIPS; s = s + 1 )
	{
	i = ship(s)
	if ( sstatus(i) != SS_OFF )
	    {
            # Torpedoes.
	    for ( j = 1; j <= MAXTORPS; j = j + 1 )
	        if ( tstatus(i,j) != TS_OFF )
	            {
	            # Torpedo fuses.
		    tfuse(i,j) = tfuse(i,j) - 1
		    if ( tfuse(i,j) <= 0 )
			{
		        if ( tstatus(i,j) == TS_LIVE )
			    call detonate( i, j )
		        else if ( tstatus(i,j) == TS_FIREBALL )
			    tstatus(i,j) = TS_OFF
			}
		    else
			{
			if ( tstatus(i,j) == TS_LIVE )
			    {
			    # Proximity check.
			    for ( t = 1; t <= MAXSHIPS; t = t + 1 )
				{
				k = ship(t)
				if ( sstatus(k) == SS_LIVE & k != i )
				    if ( twar(i,j,steam(k)) | swar(k,steam(i)) )
					{
				        dis = distf( tx(i,j), ty(i,j),
					    sx(k), sy(k) )
				        if ( dis <= TORPEDO_PROX )
					    {
					    call detonate( i, j )
					    break
					    }
					else if ( dis <= ALERT_DIST )
					    talert(k) = .true.
					}
				}
			    }
			if ( tstatus(i,j) == TS_LIVE )
			    {
			    # Proximity check for the doomsday machine.
			    if ( dstatus == DS_LIVE )
				if ( distf( tx(i,j), ty(i,j), dx, dy ) <=
				     TORPEDO_PROX )
					    {
					    call detonate( i, j )
					    break
					    }
			    }
			}
		    }
	    }
	}
    # Finish up torp alert logic.
    for ( i = 1; i <= MAXSHIPS; i = i + 1 )
	stalert(i) = talert(i)

    # Planet eater.
    if ( dstatus == DS_LIVE )
	{
	if ( dlock < 0 )
	    {
	    # Planet.
	    if ( distf( dx, dy, px(-dlock), py(-dlock) ) <= DOOMSDAY_DIST )
	        {
		# Decrement armies.
		if ( rnd( 0 ) <= 0.1 )
		    call intrude( MSG_DOOM, -dlock )
	        PVLOCK(lockword)
	        parmies(-dlock) = parmies(-dlock) - 1
	        if ( parmies(-dlock) <= 0 )
	            {
		    puninhabtime(-dlock) = rndint( MIN_UNINHAB_MINUTES,
						    MAX_UNINHAB_MINUTES )
	            call zeroplanet( -dlock, 0 )
	            call doomfind
	            }
	        PVUNLOCK(LOCKWORD)
	        }
	    }
	else if ( dlock > 0 )
	    {
	    # Ship.
	    if ( sstatus(dlock) != SS_LIVE )
		call doomfind
	    else if ( distf( dx, dy, sx(dlock), sy(dlock) ) <= DOOMSDAY_DIST )
	        swarp(dlock) = 0.0	# clever doomsday tractors
	    }

	# Update heading.
	if ( dlock < 0 )
	    dhead = angle( dx, dy, px(-dlock), py(-dlock) )
	else if ( dlock > 0 )
	    dhead = angle( dx, dy, sx(dlock), sy(dlock) )
	ddx = DOOMSDAY_WARP * MM_PER_SEC_PER_WARP * ITER_SECONDS * cosd(dhead)
	ddy = DOOMSDAY_WARP * MM_PER_SEC_PER_WARP * ITER_SECONDS * sind(dhead)
	}

    return

end


###  submindrive - drive the sub-minute interval items
#
#  SYNOPSIS
#    call submindrive
#
subroutine submindrive
NOIMPLICIT

    integer i
    real speed, mod360
    include "conqcom"

    for ( i = NUMPLANETS; i >= 1; i = i - 1 )
	{
	# Advance porbang().
	if ( pprimary(i) != 0 )
	    {
	    porbang(i) = mod360( porbang(i) + porbvel(i) *
		SUBMIN_SECONDS / 60.0 )
            px(i) = px(pprimary(i)) + porbrad(i) * cosd(porbang(i))
            py(i) = py(pprimary(i)) + porbrad(i) * sind(porbang(i))
	    }
	else if ( porbvel(i) != 0.0 )
	    {
	    # Special hack for planets to move in a straight line.
	    speed = porbvel(i) * MM_PER_SEC_PER_WARP * SUBMIN_SECONDS
	    px(i) = px(i) + speed * cosd(porbang(i))
	    py(i) = py(i) + speed * sind(porbang(i))
	    }
	}

    return

end


###  mindrive - drive the one-minute interval items
#
#  SYNOPSIS
#    call mindrive
#
subroutine mindrive
NOIMPLICIT

    integer i
    real rnd
    include "conqcom"

    for ( i = 1; i <= NUMTEAMS; i = i + 1 )
	{
	# Decrement couptime().
	if ( couptime(i) > 0 )
	    couptime(i) = couptime(i) - 1
	}

    for ( i = 1; i <= NUMPLANETS; i = i + 1 )
	{
	# Decrement puninhabtime().
	if ( puninhabtime(i) > 0 )
	    puninhabtime(i) = puninhabtime(i) - 1
	}

    if ( dstatus == DS_LIVE )
	call doomfind
    else if ( rnd( 0 ) < DOOMSDAY_PROB )
	call doomsday

    return

end


###  fivemindrive - drive the five-minute interval items (DOES LOCKING)
#
#  SYNOPSIS
#    call fivemindrive
#
subroutine fivemindrive
NOIMPLICIT

    integer i, thresh, rndint
    real rnd
    include "conqcom"

    # Drive the planets.
    PVLOCK(lockword)
    for ( i = 1; i <= NUMPLANETS; i = i + 1 )
	if ( parmies(i) > 0 & pteam(i) != TEAM_GOD )
	    if ( parmies(i) < SPARSE_THRESH )
		{
		if ( rnd( 0 ) <= SPARSE_REPOP_PROB )
		    parmies(i) = parmies(i) + 1
		}
	    else
		{
		if ( ptype(i) == PLANET_CLASSM )
		    thresh = MALTHUS_M_THRESH
		else
		    thresh = MALTHUS_D_THRESH

		if ( parmies(i) >= thresh & rnd( 0 ) <= MALTHUS_PROB )
		    parmies(i) = max( round( parmies(i) * rnd( 0 ) ), 1 )
		else
		    parmies(i) = parmies(i) +
			rndint( REPOP_LOWER_BOUND, REPOP_UPPER_BOUND )
		}
    PVUNLOCK(LOCKWORD)

    return

end
