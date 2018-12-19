###############################################################################
#
#                                 C O N Q L B
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


###  chalkup - perform kills accoutinng
#
#  SYNOPSIS
#    integer snum
#    call chalkup( snum )
#
#  Note: This routines ASSUMES you have the common locked before you call it.
#
subroutine chalkup( snum )
NOIMPLICIT
integer snum

    integer i, unum, team
    real x, w, l, m
    include "conqcom"

    unum = suser(snum)
    team = steam(snum)

    # Update wins.
    ustats(unum,USTAT_WINS) = ustats(unum,USTAT_WINS) + ifix(skills(snum))
    tstats(team,TSTAT_WINS) = tstats(team,TSTAT_WINS) + ifix(skills(snum))

    # Update max kills.
    i = ifix( skills(snum) )
    if ( i > ustats(unum,USTAT_MAXKILLS) )
	ustats(unum,USTAT_MAXKILLS) = i

    # Update rating.
    l = ustats(unum,USTAT_LOSSES)
    if ( l == 0 )
	l = 1
    w = ustats(unum,USTAT_WINS)
    m = ustats(unum,USTAT_MAXKILLS)
    urating(unum) = ( w / l ) + ( m / 4.0 )
    x = w - l
    if ( x >= 0.0 )
	urating(unum) = urating(unum) + x ** ( 1.0 / 3.0 )
    else
	urating(unum) = urating(unum) - (- x ) ** ( 1.0 / 3.0 )

    return

end


###  cloak - attempt to engage the cloaking device
#
#  SYNOPSIS
#    logical didit, cloak
#    integer snum
#    didit = cloak( snum )
#
logical function cloak( snum )
NOIMPLICIT
integer snum

    logical usefuel
    include "conqcom"

    srmode(snum) = .false.
    if ( ! usefuel( snum, CLOAK_ON_FUEL, .false. ) )
	return ( .false. )
    scloaked(snum) = .true.
    return ( .true. )

end


###  damage - damage a ship
#
#  SYNOPSIS
#    integer snum, kb
#    real dam
#    call damage( snum, dam, kb )
#
subroutine damage( snum, dam, kb )
NOIMPLICIT
integer snum
real dam
integer kb

    real mw
    include "conqcom"

    sdamage(snum) = sdamage(snum) + dam
    if ( sdamage(snum) >= 100.0 )
	call kill( snum, kb )
    else
	{
	mw = maxwarp( snum )
	sdwarp(snum) = min( sdwarp(snum), mw )
	}

    return

end


###  detonate - blow up a torpedo (DOES LOCKING)
#
#  SYNOPSIS
#    integer snum, tnum
#    call detonate( snum, tnum )
#
subroutine detonate( snum, tnum )
NOIMPLICIT
integer snum, tnum

    include "conqcom"

    PVLOCK(lockword)
    if ( tstatus(snum,tnum) == TS_LIVE )
	tstatus(snum,tnum) = TS_DETONATE
    PVUNLOCK(lockword)

    return

end


###  display - do one update of a ships screen
#
#  SYNOPSIS
#    integer snum
#    call display( snum )
#
subroutine display( snum )
NOIMPLICIT
integer snum

    integer i, j, k, idx, lin, col, datacol, minenemy, minsenemy
    integer modp1, length, strcmp
    integer linofs(8), colofs(8)
    character ch, buf(MSGMAXLINE)
    logical cvtcoords, beep, lsmap
    real x, scale, cenx, ceny, dis, mindis, minsdis, fl, cd, sd
    static real zzskills, zzswarp
    static character zzbuf(MSGMAXLINE)
    static integer zzsshields, zzcshields, zzshead, zzsfuel, zzcfuel
    static integer zzsweapons, zzsengines, zzsdamage, zzcdamage, zzsarmies
    static integer zzsetemp, zzswtemp, zzctemp, zzstowedby, zzssdfuse
    static real prevsh, prevdam
    include "conqcom"
    include "conqcom2"
    string dirch "-/|\-/|\"
    data linofs /  0, -1, -1, -1,  0,  1,  1,  1 /
    data colofs /  1,  1,  0, -1, -1, -1,  0,  1 /
    data prevsh / 0.0 /, prevdam / 100.0 /

    if ( credraw )
	{
	call cdclear
	lin = DISPLAY_LINS + 1
	call cdline( 1, STAT_COLS, lin, STAT_COLS )
	call cdline( lin, 1, lin, cmaxcol )
	}
    else
	call cdclra( 1, STAT_COLS + 1, DISPLAY_LINS, cmaxcol )

    beep = .false.
    mindis = 1.0e6
    minsdis = 1.0e6
    minenemy = 0
    minsenemy = 0
    lsmap = smap(snum)

    if ( lsmap )
	{
	scale = MAP_FAC
	cenx = 0.0
	ceny = 0.0
	}
    else
	{
	scale = SCALE_FAC
	cenx = sx(snum)
	ceny = sy(snum)
	}

    if ( closed )
	call cdputs( "CLOSED", 1, STAT_COLS+(cmaxcol-STAT_COLS-6)/2+1 )
    else if ( srobot(snum) )
	call cdputs( "ROBOT", 1, STAT_COLS+(cmaxcol-STAT_COLS-5)/2+1 )

    # Display the planets and suns.
    for ( i = NUMPLANETS; i >= 1; i = i - 1 )
	{
	if ( ! preal(i) )
	    next
	if ( ! cvtcoords( cenx, ceny, px(i), py(i), scale, lin, col ) )
	    next
	if ( lsmap )
	    {
	    # Strategic map.
	    # Can't see moons.
	    if ( ptype(i) == PLANET_MOON )
		next
	    # If it's a sun or we any planet we haven't scanned...
	    if ( ptype(i) == PLANET_SUN | ! pscanned(i,steam(snum)) )
		call cdput( chrplanets(ptype(i)), lin, col )
	    else
		{
		# Pick a planet owner character.
		if ( parmies(i) <= 0 | pteam(i) < 1 | pteam(i) > NUMTEAMS )
		    ch = '-'
		else
		    ch = chrtorps(pteam(i))

		# Display the planet; either it's numeric or it's not.
		if ( soption(snum,OPT_NUMERICMAP) )
		    {
		    call prints( buf, "%c%d%c", ch, parmies(i), ch )
		    call cdputs( buf, lin, col+2-length( buf ) )
		    }
		else if ( pscanned(i,steam(snum)) )
		    {
		    buf(1) = ch
		    buf(2) = chrplanets(ptype(i))
		    buf(3) = ch
		    buf(4) = EOS
		    call cdputs( buf, lin, col-1 )
		    }
		}

	    # If necessary, display the planet name.
	    if ( soption(snum,OPT_PLANETNAMES) )
		{
		buf(1) = pname(1,i)
		buf(2) = pname(2,i)
		buf(3) = pname(3,i)
		buf(4) = EOS
		call cdputs( buf, lin, col+2 )
		}
	    }
	else
	    {
	    # Tactical map.
	    call puthing( ptype(i), lin, col )
	    call cdput( chrplanets(ptype(i)), lin, col )
	    if ( soption(snum,OPT_PLANETNAMES) )
		if ( lin+1 <= DISPLAY_LINS )
		    call cdputs( pname(1,i), lin+1, col+2 )
	    }
	}

    # Display the planet eater.
    if ( dstatus == DS_LIVE )
	if ( ! lsmap )
	    if ( cvtcoords( cenx, ceny, dx, dy, scale, lin, col ) )
		{
		beep = .true.
		sd = sind(dhead)
		cd = cosd(dhead)
		# Draw the body.
		for ( fl = -DOOMSDAY_LENGTH/2.0;
		      fl < DOOMSDAY_LENGTH/2.0;
		      fl = fl + 50.0 )
		    if ( cvtcoords( cenx, ceny, dx+fl*cd, dy+fl*sd, scale, lin, col ) )
			call cdput( '#', lin, col )
		# Draw the head.
		if ( cvtcoords( cenx, ceny, dx+DOOMSDAY_LENGTH/2.0*cd,
					     dy+DOOMSDAY_LENGTH/2.0*sd,
					     scale, lin, col ) )
		    call cdput( '*', lin, col )
		}

    # Display phaser graphics.
    if ( ! lsmap & spfuse(snum) > 0 )
	if ( soption(snum, OPT_PHASERGRAPHICS) )
	    {
	    sd = sind(slastphase(snum))
	    cd = cosd(slastphase(snum))
	    ch = dirch(modp1( round( slastphase(snum) + 22.5 ) / 45 + 1, 8 ))
	    for ( fl = 0; fl <= PHASER_DIST; fl = fl + 50.0 )
		if ( cvtcoords( cenx, ceny,
				 sx(snum)+fl*cd, sy(snum)+fl*sd,
				 scale, lin, col ) )
		    call cdput( ch, lin, col )
	    }

    # Display the ships.
    for ( i = 1; i <= MAXSHIPS; i = i + 1 )
	if ( sstatus(i) != SS_OFF )
	    {
	    # Display the torps if we're not strategic.
	    if ( ! lsmap )
		{
		# First display exploding torps.
		if ( soption(snum, OPT_EXPLOSIONS) )
		    for ( j = 1; j <= MAXTORPS; j = j + 1 )
			if ( tstatus(i,j) == TS_FIREBALL )
			    if ( cvtcoords( cenx, ceny, tx(i,j), ty(i,j),
				scale, lin, col) )
				call puthing( THING_EXPLOSION, lin, col )
		# Now display the live torps.
		for ( j = 1; j <= MAXTORPS; j = j + 1 )
		    if ( tstatus(i,j) == TS_LIVE | tstatus(i,j) == TS_DETONATE )
			if ( cvtcoords( cenx, ceny, tx(i,j), ty(i,j),
			    scale, lin, col ) )
			    call cdput( chrtorps(steam(i)), lin, col )
		}
	    # Display the ships.
	    if ( sstatus(i) == SS_LIVE )
		{
		# It's alive.
		dis = dist( sx(snum), sy(snum), sx(i), sy(i) )

		# Here's where ship to ship accurate information is "gathered".
		if ( dis < ACCINFO_DIST & ! scloaked(i) & ! selfwar( snum ) )
		    sscanned(i,steam(snum)) = SCANNED_FUSE

		# Check for nearest enemy and nearest scanned enemy.
		if ( satwar( i, snum ) )
		    if ( i != snum )
			{
			if ( dis < mindis )
			    {
			    # New nearest enemy.
			    mindis = dis
			    minenemy = i
			    }
			if ( dis < minsdis )
			    if ( ! selfwar( snum ) )
				if ( sscanned(i,steam(snum)) > 0 )
				    {
				    # New nearest scanned enemy.
				    minsdis = dis
				    minsenemy = i
				    }
			}

		# There is potential for un-cloaked ships and ourselves.
		if ( ! scloaked(i) | i == snum )
		    {
		    # ... especially if he's in the bounds of our current
		    #  display (either tactical or strategic map)
		    if ( cvtcoords( cenx, ceny, sx(i), sy(i),
			scale, lin, col ) )
			{
			# He's on the screen.
			# We can see him if one of the following is true:
			#
			#  - We are not looking at our strategic map
			#  - We're mutually at peace
			#  - Our team has scanned him and we're not self-war
			#  - He's within accurate scanning range
			#
			if ( ( ! lsmap ) |
			( ! satwar(i, snum) ) |
			( sscanned(i,steam(snum)) & ! selfwar(snum) ) |
			( dis <= ACCINFO_DIST ) )
			    {
			    if ( ( i == snum ) & scloaked(snum) )
				ch = CHAR_CLOAKED
			    else
				ch = chrteams(steam(i))
			    call cdput( ch, lin, col )
			    call cdputn( i, 0, lin, col + 2 )
			    idx = modp1( round( shead(i) + 22.5 ) / 45 + 1, 8 )
			    j = lin+linofs(idx)
			    k = col+colofs(idx)
			    if ( j >= 1 & j <= DISPLAY_LINS & k > STAT_COLS &
				 k <= cmaxcol )
				call cdput( dirch(idx), j, k )
			    }
			}
		    if ( snum == i )
			{
			# If it's our ship and we're off the screen, fake it.
			if ( lin < 1 )
			    lin = 1
			else
			    lin = min( lin, DISPLAY_LINS )
			if ( col < STAT_COLS + 1 )
			    col = STAT_COLS + 1
			else
			    col = min( col, cmaxcol )
			call cdmove( lin, col )
			}
		    }
		} # it's alive
	    } # for each ship

    # Construct alert status line.
    if ( credraw )
	zzbuf(1) = EOS
    buf(1) = EOS
    if ( minenemy != 0 | stalert(snum) )
	{
	if ( mindis <= PHASER_DIST )
	    {
	    # Nearest enemy is very close.
	    call strcpy( "RED ALERT ", buf )
	    beep = .true.
	    }
	else if ( mindis < ALERT_DIST )
	    {
	    # Nearest enemy is close.
	    call strcpy( "Alert ", buf )
	    beep = .true.
	    }
	else if ( stalert(snum) )
	    {
	    # Nearby torpedos.
	    call strcpy( "Torp alert", buf )
	    minenemy = 0			# disable nearby enemy code
	    beep = .true.
	    }
	else if ( mindis < YELLOW_DIST )
	    {
	    # Near an enemy.
	    call strcpy( "Yellow alert ", buf )
	    }
	else if ( minsenemy != 0 )
	    {
	    # An enemy near one of our ships or planets.
	    minenemy = minsenemy		# for cloaking code below
	    call strcpy( "Yellow alert ", buf )
	    }
	else
	    minenemy = 0

	if ( minenemy != 0 )
	    {
	    call appship( minenemy, buf )
	    if ( scloaked(minenemy) )
		call appstr( " (CLOAKED)", buf )
	    }
	}
    if ( strcmp( buf, zzbuf ) != 0 )
	{
	lin = DISPLAY_LINS + 1
	call cdline( lin, STAT_COLS, lin, cmaxcol )
	if ( buf(1) != EOS )
	    {
	    col = (cmaxcol-STAT_COLS-length(buf))/2+STAT_COLS+1
	    call cdputs( buf, DISPLAY_LINS+1, col )
	    }
	call strcpy( buf, zzbuf )
	}

    # Build and display the status info as necessary.
    lin = 1
    col = 1
    datacol = col + 14

    # Shields.
    if ( sshields(snum) < prevsh )
	beep = .true.
    prevsh = sshields(snum)

    if ( credraw )
	{
	zzsshields = -9
	zzcshields = ' '
	}
    i = round( sshields(snum) )
    if ( ! sshup(snum) | srmode(snum) )
	i = -1
    if ( i != zzsshields )
	{
	call cdclra( lin, datacol, lin, STAT_COLS-1 )
	if ( i == -1 )
	    call cdputs( "DOWN", lin, datacol )
	else
	    {
	    call prints( buf, "%d", i )
	    call cdputs( buf, lin, datacol )
	    }
	zzsshields = i
	}

    if ( i < 60 )
	j = 'S'
    else
	j = 's'
    if ( j != zzcshields )
	{
	if ( j == 'S' )
	    call cdputs( "SHIELDS =", lin, col )
	else
	    call cdputs( "shields =", lin, col )
	zzcshields = j
	}

    # Kills.
    lin = lin + 2
    if ( credraw )
	{
	call cdputs( "kills =", lin, col )
	zzskills = -20.0
	}
    x = oneplace(skills(snum))
    if ( x != zzskills )
	{
	call cdclra( lin, datacol, lin, STAT_COLS-1 )
	call prints( buf, "%g", x )
	call cdputs( buf, lin, datacol )
	zzskills = x
	}

    # Warp.
    lin = lin + 2
    if ( credraw )
	{
	call cdputs( "warp =", lin, col )
	zzswarp = 92.0			# "Warp 92 Mr. Sulu."
	}
    x = oneplace(swarp(snum))
    if ( x != zzswarp )
	{
	call cdclra( lin, datacol, lin, STAT_COLS-1 )
	if ( x >= 0 )
	    {
	    call prints( buf, "%g", x )
	    call cdputs( buf, lin, datacol )
	    }
	else
	    call cdput( 'o', lin, datacol )
	zzswarp = x
	}

    # Heading.
    lin = lin + 2
    if ( credraw )
	{
	call cdputs( "heading =", lin, col )
	zzshead = 999
	}
    i = slock(snum)
    if ( i >= 0 | swarp(snum) < 0.0 )
	i = round( shead(snum) )
    if ( i != zzshead )
	{
	call cdclra( lin, datacol, lin, STAT_COLS-1 )
	if ( -i >= 1 & -i <= NUMPLANETS )
	    call prints( buf, "%.3s", pname(1,-i) )
	else
	    call prints( buf, "%d", i )
	call cdputs( buf, lin, datacol )
	zzshead = i
	}

    # Fuel.
    lin = lin + 2
    if ( credraw )
	{
	zzsfuel = -99
	zzcfuel = ' '
	}
    i = round( sfuel(snum) )
    if ( i != zzsfuel )
	{
	call cdclra( lin, datacol, lin, STAT_COLS-1 )
	call prints( buf, "%d", i )
	call cdputs( buf, lin, datacol )
	zzsfuel = i
	}

    if ( i < 200 )
	j = 'F'
    else
	j = 'f'
    if ( j != zzcfuel )
	{
	if ( j == 'F' )
	    call cdputs( "FUEL =", lin, col )
	else if ( j == 'f' )
	    call cdputs( "fuel =", lin, col )
	zzcfuel = j
	}

    # Allocation.
    lin = lin + 2
    if ( credraw )
	{
	call cdputs( "w/e =", lin, col )
	zzsweapons = -9
	zzsengines = -9
	}
    i = sweapons(snum)
    j = sengines(snum)
    if ( swfuse(snum) > 0 )
	i = 0
    if ( sefuse(snum) > 0 )
	j = 0
    if ( i != zzsweapons | j != zzsengines )
	{
	call cdclra( lin, datacol, lin, STAT_COLS-1 )
	buf(1) = EOS
	if ( i == 0 )
	    call appstr( "**", buf )
	else
	    call appint( i, buf )
	call appchr( '/', buf )
	if ( j == 0 )
	    call appstr( "**", buf )
	else
	    call appint( j, buf )
	call cdputs( buf, lin, datacol )
	zzsweapons = i
	}

    # Temperature.
    lin = lin + 2
    if ( credraw )
	{
	zzswtemp = 0
	zzsetemp = 0
	zzctemp = ' '
	}
    i = round( swtemp(snum) )
    j = round( setemp(snum) )
    if ( i > 100 )
	i = 100
    if ( j > 100 )
	j = 100
    if ( i != zzswtemp | j != zzsetemp )
	{
	call cdclra( lin, datacol, lin, STAT_COLS-1 )
	if ( i != 0 | j != 0 )
	    {
	    buf(1) = EOS
	    if ( i >= 100 )
		call appstr( "**", buf )
	    else
		call appint( i, buf )
	    call appchr( '/', buf )
	    if ( j >= 100 )
		call appstr( "**", buf )
	    else
		call appint( j, buf )
	    call cdputs( buf, lin, datacol )
	    }
	zzswtemp = i
	zzsetemp = j
	}

    if ( i == 0 & j == 0 )
	j = ' '
    else if ( i >= 80 | j >= 80 )
	j = 'T'
    else
	j = 't'
    if ( j != zzctemp )
	{
	call cdclra( lin, col, lin, datacol-1 )
	if ( j == 't' )
	    call cdputs( "temp =", lin, col )
	else if ( j == 'T' )
	    call cdputs( "TEMP =", lin, col )
	zzctemp = j
	}

    # Damage/repair.
    if ( sdamage(snum) > prevdam )
	beep = .true.
    prevdam = sdamage(snum)

    lin = lin + 2
    if ( credraw )
	{
	zzsdamage = -9
	zzcdamage = ' '
	}
    i = round( sdamage(snum) )
    if ( i != zzsdamage )
	{
	call cdclra( lin, datacol, lin, STAT_COLS-1 )
	if ( i > 0 )
	    {
	    call prints( buf, "%d", i )
	    call cdputs( buf, lin, datacol )
	    }
	zzsdamage = i
	}

    if ( srmode(snum) )
	j = 'r'
    else if ( i >= 50 )
	j = 'D'
    else if ( i > 0 )
	j = 'd'
    else
	j = ' '
    if ( j != zzcdamage )
	{
	call cdclra( lin, col, lin, datacol-1 )
	if ( j == 'r' )
	    call cdputs( "REPAIR, dmg =", lin, col )
	else if ( j == 'd' )
	    call cdputs( "damage =", lin, col )
	else if ( j == 'D' )
	    call cdputs( "DAMAGE =", lin, col )
	zzcdamage = j
	}

    # Armies.
    lin = lin + 2
    if ( credraw )
	zzsarmies = -666
    i = sarmies(snum)
    if ( i == 0 )
	i = -saction(snum)
    if ( i != zzsarmies )
	{
	call cdclra( lin, col, lin, STAT_COLS-1 )
	if ( i > 0 )
	    {
	    call cdputs( "armies =", lin, col )
	    call prints( buf, "%d", i )
	    call cdputs( buf, lin, datacol )
	    }
	else if ( i < 0 )
	    {
	    call cdputs( "action =", lin, col )
	    call robstr( -i, buf )
	    call cdputs( buf, lin, datacol )
	    }
	zzsarmies = i
	}

    # Tractor beams.
    lin = lin + 2
    if ( credraw )
	zzstowedby = 0
    i = stowedby(snum)
    if ( i == 0 )
	i = -stowing(snum)
    if ( i != zzstowedby )
	{
	call cdclra( lin, col, lin, datacol-1 )
	if ( i == 0 )
	    buf(1) = EOS
	else if ( i < 0 )
	    {
	    call strcpy( "towing ", buf )
	    call appship( -i, buf )
	    }
	else if ( i > 0 )
	    {
	    call strcpy( "towed by ", buf )
	    call appship( i, buf )
	    }
	call cdputs( buf, lin, col )
	zzstowedby = i
	}

    # Self destruct fuse.
    lin = lin + 2
    if ( credraw )
	zzssdfuse = -9
    if ( scloaked(snum) )
	i = -1
    else
	i = max( 0, ssdfuse(snum) )
    if ( i != zzssdfuse )
	{
	call cdclra( lin, col, lin, STAT_COLS-1 )
	if ( i > 0 )
	    {
	    call prints( buf, "DESTRUCT MINUS %-3d", i )
	    call cdputs( buf, lin, col )
	    }
	else if ( i == -1 )
	    call cdputs( "CLOAKED", lin, col )
	zzssdfuse = i
	}

    if ( beep )
	if ( soption(snum,OPT_ALARMBELL) )
	    call scbeep

    call cdplay( .true. )

    credraw = .false.

    return

end


###  enemydet - detonate enemy torpedos
#
#  SYNOPSIS
#    logical didit, enemydet
#    integer snum
#    didit = enemydet( snum )
#
logical function enemydet( snum )
NOIMPLICIT
integer snum

    integer i, j
    logical usefuel
    include "conqcom"

    # Stop repairing.
    srmode(snum) = .false.

    if ( ! usefuel( snum, DETONATE_FUEL, .true. ) )
	return ( .false. )

    for ( i = 1; i <= MAXSHIPS; i = i + 1 )
	if ( sstatus(i) != SS_OFF & i != snum )
	    for ( j = 1; j <= MAXTORPS; j = j + 1 )
		if ( tstatus(i,j) == TS_LIVE )
		    if ( twar(i,j,steam(snum)) | swar(snum,steam(i)) )
			if ( dist( sx(snum), sy(snum), tx(i,j), ty(i,j) ) <=
			     DETONATE_DIST )
			    call detonate( i, j )

    return ( .true. )

end


###  hit - hit a ship
#
#  SYNOPSIS
#    integer snum, kb
#    real ht
#    call hit( snum, ht, kb )
#
subroutine hit( snum, ht, kb )
NOIMPLICIT
integer snum
real ht
integer kb

    include "conqcom"

    if ( ht > 0.0 )
	if ( sshup(snum) & ! srmode(snum) )
	    if ( ht > sshields(snum) )
		{
		call damage( snum, ht-sshields(snum), kb )
		sshields(snum) = 0.0
		}
	    else
		sshields(snum) = sshields(snum) - ht
	else
	    call damage( snum, ht, kb )

    return
end


###  ikill - ikill a ship
#
#  SYNOPSIS
#    integer snum, kb
#    call ikill( snum, kb )
#
#  Note: This routines ASSUMES you have the common locked before you call it.
#
subroutine ikill( snum, kb )
NOIMPLICIT
integer snum, kb

    integer i, unum, team, kunum, kteam
    real tkills
    include "conqcom"

    # Only procede if the ship is alive
    if ( sstatus(snum) != SS_LIVE )
	return

    # The ship is alive; kill it.
    skilledby(snum) = kb
    sstatus(snum) = SS_DYING

    unum = suser(snum)
    team = steam(snum)

    # Detonate all torpedos.
    for ( i = 1; i <= MAXTORPS; i = i + 1 )
	if ( tstatus(snum,i) == TS_LIVE )
	    tstatus(snum,i) = TS_DETONATE

    # Release any tows.
    if ( stowing(snum) != 0 )
	stowedby(stowing(snum)) = 0
    if ( stowedby(snum) != 0 )
	stowing(stowedby(snum)) = 0

    # Zero team scan fuses.
    for ( i = 1; i <= NUMTEAMS; i = i + 1 )
	sscanned(snum,i) = 0

    if ( kb == KB_CONQUER )
	skills(snum) = skills(snum) + CONQUER_KILLS
    else if ( kb == KB_GOTDOOMSDAY )
	skills(snum) = skills(snum) + DOOMSDAY_KILLS
    else if ( kb > 0 )				# if a ship did the killing
	{
	kunum = suser(kb)
	kteam = steam(kb)
	tkills = 1.0 + skills(snum) * KILLS_KILLS
	if ( sarmies(snum) > 0 )
	    {
	    # Keep track of carried armies killed - they are special.
	    tkills = tkills + sarmies(snum) * ARMY_KILLS
	    ustats(kunum,USTAT_ARMSHIP) =
		ustats(kunum,USTAT_ARMSHIP) + sarmies(snum)
	    tstats(kteam,TSTAT_ARMSHIP) =
		tstats(kteam,TSTAT_ARMSHIP) + sarmies(snum)
	    }

	# Kills accounting.
	if ( sstatus(kb) == SS_LIVE )
	    skills(kb) = skills(kb) + tkills
	else
	    {
	    # Have to do some hacking when our killer is dead.
	    ustats(kunum,USTAT_WINS) =
		ustats(kunum,USTAT_WINS) - ifix(skills(kb))
	    tstats(kteam,TSTAT_WINS) =
		tstats(kteam,TSTAT_WINS) - ifix(skills(kb))
	    skills(kb) = skills(kb) + tkills
	    call chalkup( kb )
	    }

	# Sticky war logic.
	if ( ! swar(snum,kteam) )
	    {
	    swar(kb,team) = .true.
	    srwar(kb,team) = .true.
	    }
	}

    # Kills accounting.
    call chalkup( snum )
    if ( kb != KB_SELF & kb != KB_CONQUER & kb != KB_NEWGAME &
	 kb != KB_EVICT & kb != KB_SHIT & kb != KB_GOD )
	{
	# Update losses.
	ustats(unum,USTAT_LOSSES) = ustats(unum,USTAT_LOSSES) + 1
	tstats(team,TSTAT_LOSSES) = tstats(team,TSTAT_LOSSES) + 1
	}

    if ( ! srobot(snum) | spid(snum) != 0 )
	{
	sstatus(snum) = SS_DEAD
	ssdfuse(snum) = -TIMEOUT_PLAYER		# setup dead timeout timer
	}
    else
	{
	sstatus(snum) = SS_OFF			# turn robots off
	# We'd like to remove this next line so that you could
	# use conqoper to see what killed him, but then robots
	# show up on the debugging playlist...
	skilledby(snum) = 0
	}

    return

end


###  infoplanet - write out information about a planet
#
#  SYNOPSIS
#    character str()
#    integer pnum, snum
#    call infoplanet( str, pnum, snum )
#
subroutine infoplanet( str, pnum, snum )
NOIMPLICIT
character str(ARB)
integer pnum, snum

    integer i, j, phoon, length
    logical godlike, canscan, spwar
    character buf(MSGMAXLINE*2), junk(MSGMAXLINE)
    real x, y, angle
    include "conqcom"

    # Check range of the passed planet number.
    if ( pnum < 1 | pnum > NUMPLANETS )
	{
	call putmsg( "No such planet.", MSG_LIN1 )
	call cdclrl( MSG_LIN2, 1 )
	call cdmove( MSG_LIN1, 1 )
	call cerror( MSG_GOD, "infoplanet: Called with invalid pnum (%d).",
	    pnum )
	return
	}

    # GOD is too clever.
    godlike = ( snum < 1 | snum > MAXSHIPS )

    # In some cases, report hostilities.
    junk(1) = EOS
    if ( ptype(pnum) == PLANET_CLASSM | ptype(pnum) == PLANET_DEAD )
	if ( ! godlike )
	    if ( pscanned(pnum,steam(snum)) & spwar( snum, pnum ) )
		call appstr( " (hostile)", junk )

    # Things that orbit things that orbit have phases.
    switch ( phoon( pnum ) )
	{
	case PHOON_FIRST:
	    call appstr( " (first quarter)", junk )
	case PHOON_FULL:
	    call appstr( " (full)", junk )
	case PHOON_LAST:
	    call appstr( " (last quarter)", junk )
	case PHOON_NEW:
	    call appstr( " (new)", junk )
	case PHOON_NO:
	    # Do no-thing.
	default:
	    call appstr( " (weird)", junk )
	}

    if ( godlike )
	{
	x = 0.0
	y = 0.0
	}
    else
	{
	x = sx(snum)
	y = sy(snum)
	}
    call prints( buf, "%s%s, a %s%s, range %d, direction %d",
	str,
	pname(1,pnum),
	ptname(1,ptype(pnum)),
	junk,
	round( dist( x, y, px(pnum), py(pnum) ) ),
	round( angle( x, y, px(pnum), py(pnum) ) ) )

    if ( godlike )
	canscan = .true.
    else
	canscan = pscanned(pnum,steam(snum))

    junk(1) = EOS
    if ( ptype(pnum) != PLANET_SUN & ptype(pnum) != PLANET_MOON )
	{
	if ( ! canscan )
	    call strcpy( "with unknown occupational forces", junk )
	else
	    {
	    i = parmies(pnum)
	    if ( i == 0 )
		{
		j = puninhabtime(pnum)
		if ( j > 0 )
		    call prints( junk, "uninhabitable for %d more minutes", j )
		else
		    call strcpy( "with NO armies", junk )
		}
	    else
		{
		call prints( junk, "with %d %s arm", i, tname(1,pteam(pnum)) )
		if ( i == 1 )
		    call appstr( "y", junk )
		else
		    call appstr( "ies", junk )
		}
	    }

	# Now see if we can tell about coup time.
	if ( godlike )
	    canscan = .false.			# GOD can use teaminfo instead
	else
	    canscan = ( pnum == homeplanet(steam(snum)) &
			tcoupinfo(steam(snum)) )
	if ( canscan )
	    {
	    j = couptime(steam(snum))
	    if ( j > 0 )
		{
		if ( junk(1) != EOS )
		    call appstr( ", ", junk )
		call appint( j, junk )
		call appstr( " minutes until coup time", junk )
		}
	    }
	}

    if ( junk(1) == EOS )
	call appchr( '.', buf )
    else
	{
	call appchr( ',', buf )
	call appchr( '.', junk )
	}

    # Now output the info. Break the stuff in buf across two lines
    #  (if necessary) and force the stuff in junk (the number of
    #  armies for planets) to be all on the second line.
    i = length( buf )				# length of first part
    j = 69					# desired maximum length
    if ( i <= j )
	{
	# The first part is small enough.
	call putmsg( buf, MSG_LIN1 )
	if ( junk(1) != EOS )
	    call putmsg( junk, MSG_LIN2 )
	else
	    call cdclrl( MSG_LIN2, 1 )
	}
    else
	{
	# Break it into two lines.
	i = j + 1
	while ( buf(i) != ' ' & i > 1 )
	    i = i - 1
	call appchr( ' ', buf )
	call appstr( junk, buf )
	buf(i) = EOS				# terminate at blank
	call putmsg( buf, MSG_LIN1 )
	call putmsg( buf(i+1), MSG_LIN2 )
	}

    call cdmove( MSG_LIN1, 1 )
    return

end


###  infoship - write out information about a ship
#
#  SYNOPSIS
#    integer snum, scanner
#    call infoship( snum, scanner )
#
subroutine infoship( snum, scanner )
NOIMPLICIT
integer snum, scanner

    integer i, status
    character cupper, junk(MSGMAXLINE)
    real x, y, dis, angle, kills, appx, appy, rndnor
    logical godlike, canscan
    include "conqcom"
    include "conqcom2"

    godlike = ( scanner < 1 | scanner > MAXSHIPS )
    call cdclrl( MSG_LIN2, 1 )
    if ( snum < 1 | snum > MAXSHIPS )
	{
	call putmsg( "No such ship.", MSG_LIN1 )
	call cdmove( MSG_LIN1, 1 )
	return
	}
    status = sstatus(snum)
    if ( ! godlike & status != SS_LIVE )
	{
	call putmsg( "Not found.", MSG_LIN1 )
	call cdmove( MSG_LIN1, 1 )
	return
	}
    cbuf(1) = EOS
    call appship( snum, cbuf )
    if ( snum == scanner )
	{
	# Silly Captain...
	call appstr( ": That's us, silly!", cbuf )
	call putmsg( cbuf, MSG_LIN1 )
	call cdmove( MSG_LIN1, 1 )
	return
	}
    # Scan another ship.
    if ( godlike )
	{
	x = 0.0
	y = 0.0
	}
    else
	{
	x = sx(scanner)
	y = sy(scanner)
	}
    if ( scloaked(snum) )
	{
	appx = rndnor(sx(snum), CLOAK_SMEAR_DIST)
	appy = rndnor(sy(snum), CLOAK_SMEAR_DIST)
	}
    else
	{
	appx = sx(snum)
	appy = sy(snum)
	}
    dis = dist( x, y, appx, appy )
    if ( godlike )
	canscan = .true.
    else
	{
	# Help out the driver with this scan.
	if ( (dis < ACCINFO_DIST & ! scloaked(snum)) & ! selfwar(scanner) )
	    sscanned(snum, steam(scanner)) = SCANNED_FUSE

	# Decide if we can do an acurate scan.
	canscan = ( (dis < ACCINFO_DIST & ! scloaked(snum)) |
		( (sscanned(snum, steam(scanner)) > 0) & ! selfwar(scanner) ) )
	}

    call appstr( ": ", cbuf )
    if ( spname(1,snum) != EOS )
	{
	call appstr( spname(1,snum), cbuf )
	call appstr( ", ", cbuf )
	}
    kills = oneplace( skills(snum) )
    if ( kills == 0.0 )
	call appstr( "no", cbuf )
    else
	{
	call prints( junk, "%1g", kills )
	call appstr( junk, cbuf )
	}
    call appstr( " kill", cbuf )
    if ( kills != 1.0 )
	call appchr( 's', cbuf )
    if ( dis < ACCINFO_DIST & scloaked(snum) )
	call appstr( " (CLOAKED) ", cbuf )
    else
	call appstr( ", ", cbuf )
    if ( godlike )
	{
	call appsstatus( status, cbuf )
	call appchr( '.', cbuf )
	}
    else if ( swar(snum,steam(scanner)) )
	call appstr( "at WAR.", cbuf )
    else
	call appstr( "at peace.", cbuf )
    call putmsg( cbuf, MSG_LIN1 )

    if ( ! scloaked(snum) | swarp(snum) > 0.0 )
	call prints( cbuf, "Range %d, direction %d",
	    round( dis ), round( angle( x, y, appx, appy ) ) )
    else
	cbuf(1) = EOS
    if ( canscan )
	{
	if ( cbuf(1) != EOS )
	    call appstr( ", ", cbuf )
	call appstr( "shields ", cbuf )
	if ( sshup(snum) & ! srmode(snum) )
	    call appint( round( sshields(snum) ), cbuf )
	else
	    call appstr( "DOWN", cbuf )
	i = round( sdamage(snum) )
	if ( i > 0 )
	    {
	    if ( cbuf(1) != EOS )
		call appstr( ", ", cbuf )
	    call prints( junk, "damage %d", i )
	    call appstr( junk, cbuf )
	    }
	i = sarmies(snum)
	if ( i > 0 )
	    {
	    call prints( junk, ", with %d arm", i )
	    call appstr( junk, cbuf )
	    if ( i == 1 )
		call appchr( 'y', cbuf )
	    else
		call appstr( "ies", cbuf )
	    }
	}
    if ( cbuf(1) != EOS )
	{
	cbuf(1) = cupper( cbuf(1) )
	call appchr( '.', cbuf )
	call putmsg( cbuf, MSG_LIN2 )
	}

    call cdmove( MSG_LIN1, 1 )
    return

end


###  kill - kill a ship (DOES LOCKING)
#
#  SYNOPSIS
#    integer snum, kb
#    call kill( snum, kb )
#
subroutine kill( snum, kb )
NOIMPLICIT
integer snum, kb

    include "conqcom"

    # Simply call the internal routine.
    PVLOCK(lockword)
    call ikill( snum, kb )
    PVUNLOCK(lockword)

    return

end


###  launch - create a new torpedo for a ship (DOES LOCKING)
#
#  SYNOPSIS
#    integer launch, snum
#    logical flag, launch
#    real dir
#    flag = launch( snum, dir )
#
logical function launch( snum, dir )
NOIMPLICIT
integer snum
real dir

    integer i, j
    real speed, adir, rndnor
    logical usefuel
    include "conqcom"

    # Stop repairing.
    srmode(snum) = .false.

    # Remember this important direction.
    slastblast(snum) = dir

    # Find a free torp.
    PVLOCK(lockword)
    for ( i = 1; i <= MAXTORPS; i = i + 1 )
	if ( tstatus(snum,i) == TS_OFF )
	    {
	    # Found one.
	    tstatus(snum,i) = TS_LAUNCHING
	    PVUNLOCK(lockword)
	    # Use fuel.
	    if ( ! usefuel( snum, TORPEDO_FUEL, .true. ) )
		{
		tstatus(snum,i) = TS_OFF
		return ( .false. )
		}
	    # Initialize it.
	    tfuse(snum,i) = TORPEDO_FUSE
	    tx(snum,i) = rndnor( sx(snum), 100.0 )
	    ty(snum,i) = rndnor( sy(snum), 100.0 )
	    speed = torpwarp(steam(snum)) * MM_PER_SEC_PER_WARP * ITER_SECONDS
	    adir = rndnor( dir, 2.0 )
	    tdx(snum,i) = speed * cosd(adir)
	    tdy(snum,i) = speed * sind(adir)
	    tmult(snum,i) = weaeff( snum )
	    for ( j = 1; j <= NUMTEAMS; j = j + 1 )
		twar(snum,i,j) = swar(snum,j)
	    tstatus(snum,i) = TS_LIVE

	    # Update stats.
	    PVLOCK(lockword)
	    ustats(suser(snum),USTAT_TORPS) =
		ustats(suser(snum),USTAT_TORPS) + 1
	    tstats(steam(snum),TSTAT_TORPS) =
		tstats(steam(snum),TSTAT_TORPS) + 1
	    PVUNLOCK(lockword)
	    return ( .true. )
	    }

    PVUNLOCK(lockword)

    return ( .false. )

end


###  orbit - place a ship into orbit around a planet
#
#  SYNOPSIS
#    integer snum, pnum
#    call orbit( snum, pnum )
#
subroutine orbit( snum, pnum )
NOIMPLICIT
integer snum, pnum

    real angle, beer, mod360
    include "conqcom"

    slock(snum) = -pnum
    sdwarp(snum) = 0.0

    # Find bearing to planet.
    beer = angle( sx(snum), sy(snum), px(pnum), py(pnum) )
    if ( shead(snum) < ( beer - 180.0 ) )
	beer = beer - 360.0

    # Check beer head to determine orbit direction.
    if ( beer <= shead(snum) )
	{
	swarp(snum) = ORBIT_CW
	shead(snum) = mod360( beer + 90.0 )
	}
    else
	{
	swarp(snum) = ORBIT_CCW
	shead(snum) = mod360( beer - 90.0 )
	}

    return

end


###  phaser - fire phasers (bug fry!!) (DOES LOCKING)
#
#  SYNOPSIS
#    logical didit, phaser
#    integer snum
#    real dir
#    didit = phaser( snum, dir )
#
logical function phaser( snum, dir )
NOIMPLICIT
integer snum
real dir

    integer k
    real dis, ang, angle, phaserhit
    logical usefuel
    include "conqcom"

    # Set up last weapon direction.
    slastblast(snum) = dir

    # Stop repairing.
    srmode(snum) = .false.

    # See if ok to fire.
    if ( spfuse(snum) > 0 )
	return ( .false. )

    # Try to use fuel for this shot.
    if ( ! usefuel( snum, PHASER_FUEL, .true. ) )
	return ( .false. )

    # Update stats.
    PVLOCK(lockword)
    ustats(suser(snum),USTAT_PHASERS) = ustats(suser(snum),USTAT_PHASERS) + 1
    tstats(steam(snum),TSTAT_PHASERS) = tstats(steam(snum),TSTAT_PHASERS) + 1
    PVUNLOCK(lockword)

    # Set up last fired direction.
    slastphase(snum) = dir

    # Start phaser fuse.
    spfuse(snum) = PHASER_TENTHS

    # See what we can hit.
    for ( k = 1; k <= MAXSHIPS; k = k + 1 )
	if ( sstatus(k) == SS_LIVE & k != snum )
	    if ( satwar( snum, k ) )
		{
		dis = dist( sx(snum), sy(snum), sx(k), sy(k) )
		if ( dis <= PHASER_DIST )
		    {
		    ang = angle( sx(snum), sy(snum), sx(k), sy(k) )
		    if ( ang > 315.0 )
			ang = ang - 360.0
		    if ( abs( dir - ang ) <= PHASER_SPREAD )
			call hit( k, phaserhit( snum, dis ), snum )
		    }
		}

    return ( .true. )

end


###  phaserhit - determine phaser damage
#
#  SYNOPSIS
#    integer snum
#    real hit, phaserhit, dis
#    hit = phaserhit( snum, dis )
#
real function phaserhit( snum, dis )
NOIMPLICIT
integer snum
real dis

    include "conqcom"

    return ( - dis / PHASER_DIST + 1.0 ) * PHASER_HIT * weaeff( snum )

end


###  planlist - list planets
#
#  SYNOPSIS
#    integer team
#    call planlist( team )
#
subroutine planlist( team )
NOIMPLICIT
integer team

    integer i, j, lin, col, olin, sv(NUMPLANETS), pnum
    logical init
    character ch, junk(10)
    string hd "planet      type team armies          planet      type team armies"
    data init / .false. /
    include "conqcom"
    include "conqcom2"

    if ( ! init )
	{
	for ( i = 1; i <= NUMPLANETS; i = i + 1 )
	    sv(i) = i
	init = .true.
	}
    call sortplanets( sv )

    lin = 1
    call cdputc( "P L A N E T   L I S T", lin )
    call strcpy( hd, cbuf )
    lin = lin + 2
    call cdputc( cbuf, lin )
    for ( i = 1; cbuf(i) != EOS; i = i + 1 )
	if ( cbuf(i) != ' ' )
	    cbuf(i) = '-'
    lin = lin + 1
    call cdputc( cbuf, lin )
    lin = lin + 1
    olin = lin
    col = 7
    for ( j = 1; j <= 3; j = j + 1 )
	{
	for ( i = 1; i <= NUMPLANETS; i = i + 1 )
	    {
	    pnum = sv(i)

	    # Don't display unless it's real.
	    if ( ! preal(pnum) )
		next

	    # We want planets, moons, and suns.
	    switch ( j )
		{
		case 1:
		    if ( ptype(pnum) == PLANET_SUN |
			 ptype(pnum) == PLANET_MOON)
			next
		case 2:
		    if ( ptype(pnum) != PLANET_MOON )
			next
		case 3:
		    if ( ptype(pnum) != PLANET_SUN )
			next
		}

	    # Figure out who owns it and count armies.
	    ch =  chrteams(pteam(pnum))
	    call prints( junk, "%d", parmies(pnum) )

	    # Then modify based on scan information.
	    if ( team != TEAM_NOTEAM )
		if ( ! pscanned(pnum,team) )
		    {
		    ch = '?'
		    call strcpy( "?", junk )
		    }

	    # Suns and moons are displayed as unowned.
	    if ( ptype(pnum) == PLANET_SUN | ptype(pnum) == PLANET_MOON )
		ch = ' '

	    # Don't display armies for suns unless we're special.
	    if ( ptype(pnum) == PLANET_SUN )
		if ( team != TEAM_NOTEAM )
		    junk(1) = EOS

	    # Moons aren't supposed to have armies.
	    if ( ptype(pnum) == PLANET_MOON )
		if ( team != TEAM_NOTEAM )
		    junk(1) = EOS
		else if ( parmies(pnum) == 0 )
		    junk(1) = EOS

	    call prints( cbuf, "%-13s %-4c %-3c  %4s",
		pname(1,pnum), chrplanets(ptype(pnum)), ch, junk )

	    call cdputs( cbuf, lin, col )

	    lin = lin + 1
	    if ( lin == MSG_LIN1 )
		{
		lin = olin
		col = 45
		}
	    }
	}

    return

end


###  playlist - list ships
#
#  SYNOPSIS
#    logical godlike, doall
#    call playlist( godlike, doall )
#
subroutine playlist( godlike, doall )
NOIMPLICIT
logical godlike, doall

    integer i, unum, status, kb, lin, col, length
    integer fline, lline, fship
    logical stillalive, iogtimed
    character ch, sbuf(5)
    include "conqcom"
    include "conqcom2"

    # Do some screen setup.
    call cdclear
    call strcpy( "ship name          pseudonym              kills", cbuf )
    col = ( cmaxcol - length( cbuf ) ) / 2
    lin = 2
    call cdputs( cbuf, lin, col )

    for ( i = 1; cbuf(i) != EOS; i = i + 1 )
	if ( cbuf(i) != ' ' )
	    cbuf(i) = '-'
    lin = lin + 1
    call cdputs( cbuf, lin, col )

    fline = lin + 1				# first line to use
    lline = MSG_LIN1				# last line to use
    fship = 1					# first user in uvec

    repeat
	{
	if ( ! godlike )
	    if ( ! stillalive( csnum ) )
		break
	i = fship
	call cdclrl( fline, lline - fline + 1 )
	lin = fline
	while ( i <= MAXSHIPS & lin <= lline )
	    {
	    status = sstatus(i)
	    kb = skilledby(i)
	    if ( status == SS_LIVE |
		     ( doall & ( status != SS_OFF | kb != 0 ) ) )
		{
		sbuf(1) = EOS
		call appship( i, sbuf )
		unum = suser(i)
		if ( unum >= 1 & unum <= MAXUSERS )
		    call prints( cbuf, "%-4s %-13.13s %-21.21s %6g",
			sbuf, uname(1,unum), spname(1,i), oneplace(skills(i)) )
		else
		    call prints( cbuf, "%-4s %13c %21c %6c", sbuf,
			' ', ' ', ' ' )
		if ( doall & kb != 0 )
		    {
		    call appchr( ' ', cbuf )
		    call appchr( ' ', cbuf )
		    call appkb( kb, cbuf )
		    }
		call cdputs( cbuf, lin, col )
		if ( doall & status != SS_LIVE )
		    {
		    cbuf(1) = EOS
		    call appsstatus( status, cbuf )
		    call cdputs( cbuf, lin, col - 2 - length( cbuf ) )
		    }
		}
	    i = i + 1
	    lin = lin + 1
	    }
	if ( i > MAXSHIPS )
	    {
	    # We're displaying the last page.
	    call putpmt( "--- press space when done ---", MSG_LIN2 )
	    call cdplay( .true. )
	    if ( iogtimed( ch, 1 ) )
		{
		if ( ch == TERM_EXTRA )
		    fship = 1			# move to first page
		else
		    break
		}
	    }
	else
	    {
	    # There are ships left to display.
	    call putpmt( "--- press space for more ---", MSG_LIN2 )
	    call cdplay( .true. )
	    if ( iogtimed( ch, 1 ) )
		{
		if ( ch == TERM_EXTRA )
		    fship = 1			# move to first page
		else if ( ch == ' ' )
		    fship = i			# move to next page
		else
		    break
		}
	    }
	}

    return

end


###  pseudo - change an user's pseudonym
#
#  SYNOPSIS
#    integer unum, snum
#    call pseudo( unum, snum )
#
subroutine pseudo( unum, snum )
NOIMPLICIT
integer unum, snum

    character ch, getcx, buf(MSGMAXLINE)
    include "conqcom"

    call cdclrl( MSG_LIN1, 2 )
    call strcpy( "Old pseudonym: ", buf )
    if ( snum >= 1 & snum <= MAXSHIPS )
	call appstr( spname(1,snum), buf )
    else
	call appstr( upname(1,unum), buf )
    call cdputc( buf, MSG_LIN1 )
    ch = getcx( "Enter a new pseudonym: ",
	MSG_LIN2, -4, TERMS, buf, MAXUSERPNAME )
    if ( ch != TERM_ABORT & ( buf(1) != EOS | ch == TERM_EXTRA ) )
	{
	call stcpn( buf, upname(1,unum), MAXUSERPNAME )
	if ( snum >= 1 & snum <= MAXSHIPS )
	    call stcpn( buf, spname(1,snum), MAXUSERPNAME )
	}
    call cdclrl( MSG_LIN1, 2 )

    return

end


###  register - register a new user (DOES LOCKING)
#
#  SYNOPSIS
#    character lname(), rname()
#    integer team, unum
#    logical flag, register
#    flag = register( lname, rname, team, unum )
#
logical function register( lname, rname, team, unum )
NOIMPLICIT
character lname(ARB), rname(ARB)
integer team, unum

    integer i, j
    include "conqcom"

    PVLOCK(lockword)
    for ( i = 1; i <= MAXUSERS; i = i + 1 )
	if ( ! ulive(i) )
	    {
	    ulive(i) = .true.
	    PVUNLOCK(lockword)
	    urating(i) = 0.0
	    uteam(i) = team
	    urobot(i) = .false.
	    umultiple(i) = 2			# but the option bit is off

	    for ( j = 1; j <= MAXUSTATS; j = j + 1 )
		ustats(i,j) = 0

	    for ( j = 1; j <= NUMTEAMS; j = j + 1 )
		uwar(i,j) = .true.
	    uwar(i,uteam(i)) = .false.

	    for ( j = 1; j <= MAXOPTIONS; j = j + 1 )
		uoption(i,j) = .true.
	    uoption(i,OPT_INTRUDERALERT) = .false.
	    uoption(i,OPT_NUMERICMAP) = .false.
	    uoption(i,OPT_TERSE) = .false.

	    for ( j = 1; j <= MAXOOPTIONS; j = j + 1 )
		uooption(i,j) = .false.

	    call stcpn( "never", ulastentry(1,i), DATESIZE )
	    call stcpn( lname, uname(1,i), MAXUSERNAME )
	    call stcpn( rname, upname(1,i), MAXUSERPNAME )
	    unum = i
	    return ( .true. )
	    }

    PVUNLOCK(lockword)

    return ( .false. )

end


###  resign - remove a user from the user list (DOES LOCKING)
#
#  SYNOPSIS
#    integer unum
#    call resign( unum )
#
subroutine resign( unum )
NOIMPLICIT
integer unum

    integer i
    include "conqcom"

    PVLOCK(lockword)
    if ( unum >= 1 & unum <= MAXUSERS )
	{
	ulive(unum) = .false.
	for ( i = 1; i <= MAXHISTLOG; i = i + 1 )
	    if ( unum == histunum(i) )
		{
		histunum(i) = -1
		histlog(1,i) = EOS
		}
	}
    PVUNLOCK(lockword)

    return

end


###  review - review old messages
#
#  SYNOPSIS
#    logical flag, review
#    integer snum, slm
#    flag = review( snum, slm )
#
logical function review( snum, slm )
NOIMPLICIT
integer snum, slm

    integer i, msg, lastone, modp1
    logical canread, more, didany
    include "conqcom"

    didany = .false.

    lastone = modp1( lastmsg+1, MAXMESSAGES )
    if ( snum >= 1 & snum <= MAXSHIPS )
	{
	if ( slastmsg(snum) == LMSG_NEEDINIT )
	    return ( .false. )				# none to read
	i = salastmsg(snum)
	if ( i != LMSG_READALL )
	    lastone = i
	}

    call cdclrl( MSG_LIN1, 1 )

    for ( msg = slm; msg != lastone; msg = modp1( msg-1, MAXMESSAGES ) )
	if ( canread( snum, msg ) | snum == msgfrom(msg) )
	    {
	    call readmsg( snum, msg )
	    didany = .true.
	    call cdplay( .true. )
	    if ( ! more( "" ) )
		break
	    }

    call cdclrl( MSG_LIN1, 2 )

    return ( didany )

end


###  takeplanet - take a planet (DOES SPECIAL LOCKING)
#
#  SYNOPSIS
#    integer pnum, snum
#    call takeplanet( pnum, snum )
#
#  Note: This routines ASSUMES you have the common locked before you call it.
#
subroutine takeplanet( pnum, snum )
NOIMPLICIT
integer pnum, snum

    integer i
    character buf(MSGMAXLINE)
    include "conqcom"

    pteam(pnum) = steam(snum)
    parmies(pnum) = 1
    skills(snum) = skills(snum) + PLANET_KILLS
    ustats(suser(snum),USTAT_CONQPLANETS) =
	ustats(suser(snum),USTAT_CONQPLANETS) + 1
    tstats(steam(snum),TSTAT_CONQPLANETS) =
	tstats(steam(snum),TSTAT_CONQPLANETS) + 1
    call prints( buf, "All hail the liberating %s armies.  Thanks, ",
	tname(1,steam(snum)) )
    call appship( snum, buf )
    call appchr( '!', buf )

    # Check whether the universe has been conquered.
    for ( i = 1; i <= NUMCONPLANETS; i = i + 1 )
	if ( ptype(i) == PLANET_CLASSM | ptype(i) == PLANET_DEAD )
	    if ( pteam(i) != steam(snum) | ! preal(i) )
		{
		# No.
		call stormsg( -pnum, -steam(snum), buf )
		return
		}
    # Yes!
    call getdandt( conqtime )
    call stcpn( spname(1,snum), conqueror, MAXUSERPNAME )
    lastwords(1) = EOS
    ustats(suser(snum),USTAT_CONQUERS) = ustats(suser(snum),USTAT_CONQUERS) + 1
    tstats(steam(snum),TSTAT_CONQUERS) = tstats(steam(snum),TSTAT_CONQUERS) + 1
    call stcpn( tname(1,steam(snum)), conqteam, MAXTEAMNAME )
    call ikill( snum, KB_CONQUER )
    for ( i = 1; i <= MAXSHIPS; i = i + 1 )
	if ( sstatus(i) == SS_LIVE )
	    call ikill( i, KB_NEWGAME )

    PVUNLOCK(lockword)
    call initgame
    PVLOCK(lockword)

    return

end


###  teamlist - list team statistics
#
#  SYNOPSIS
#    integer team
#    call teamlist( team )
#
subroutine teamlist( team )
NOIMPLICIT
integer team

    integer i, j, lin, col, ctime, etime
    logical godlike, showcoup
    character buf(MSGMAXLINE), timbuf(20,5)
    real x(5)
    string dfmt "%15s %12d %12d %12d %12d %12d"
    string pfmt "%15s %11g%% %11g%% %11g%% %11g%% %11g%%"
    string sfmt "%15s %12s %12s %12s %12s %12s"
    include "conqcom"
    include "conqcom2"

    godlike = ( team < 1 | team > NUMTEAMS )
    col = 1

    lin = 1
    call prints( buf, "Statistics since %s:", inittime )
    call cdputc( buf, lin )

    lin = lin + 1
    call prints( buf, "Universe last conquered at %s", conqtime )
    call cdputc( buf, lin )

    lin = lin + 1
    call prints( buf, "by %s for the %s team.", conqueror, conqteam )
    call cdputc( buf, lin )

    lin = lin + 1
    call cdclrl( lin, 1 )
    if ( lastwords(1) != EOS )
	{
	call prints( buf, "%c%s%c", '"', lastwords, '"' )
	call cdputc( buf, lin )
	}

    lin = lin + 2
    call prints( buf, sfmt, " ",
	tname(1,1), tname(1,2), tname(1,3), tname(1,4), "Totals" )
    call cdputs( buf, lin, col )

    lin = lin + 1
    for ( i = 1; buf(i) != EOS; i = i + 1 )
	if ( buf(i) != ' ' )
	    buf(i) = '-'
    call cdputs( buf, lin, col )

    lin = lin + 1
    call prints( buf, dfmt, "Conquers",
	tstats(1,TSTAT_CONQUERS), tstats(2,TSTAT_CONQUERS),
	tstats(3,TSTAT_CONQUERS), tstats(4,TSTAT_CONQUERS),
	tstats(1,TSTAT_CONQUERS) + tstats(2,TSTAT_CONQUERS) +
	tstats(3,TSTAT_CONQUERS) + tstats(4,TSTAT_CONQUERS) )
    call cdputs( buf, lin, col )

    lin = lin + 1
    call prints( buf, dfmt, "Wins",
	tstats(1,TSTAT_WINS), tstats(2,TSTAT_WINS),
	tstats(3,TSTAT_WINS), tstats(4,TSTAT_WINS),
	tstats(1,TSTAT_WINS) + tstats(2,TSTAT_WINS) +
	tstats(3,TSTAT_WINS) + tstats(4,TSTAT_WINS) )
    call cdputs( buf, lin, col )

    lin = lin + 1
    call prints( buf, dfmt, "Losses",
	tstats(1,TSTAT_LOSSES), tstats(2,TSTAT_LOSSES),
	tstats(3,TSTAT_LOSSES), tstats(4,TSTAT_LOSSES),
	tstats(1,TSTAT_LOSSES) + tstats(2,TSTAT_LOSSES) +
	tstats(3,TSTAT_LOSSES) + tstats(4,TSTAT_LOSSES) )
    call cdputs( buf, lin, col )

    lin = lin + 1
    call prints( buf, dfmt, "Ships",
	tstats(1,TSTAT_ENTRIES), tstats(2,TSTAT_ENTRIES),
	tstats(3,TSTAT_ENTRIES), tstats(4,TSTAT_ENTRIES),
	tstats(1,TSTAT_ENTRIES) + tstats(2,TSTAT_ENTRIES) +
	tstats(3,TSTAT_ENTRIES) + tstats(4,TSTAT_ENTRIES) )
    call cdputs( buf, lin, col )

    lin = lin + 1
    etime = tstats(1,TSTAT_SECONDS) + tstats(2,TSTAT_SECONDS) +
	tstats(3,TSTAT_SECONDS) + tstats(4,TSTAT_SECONDS)
    call fmtseconds( tstats(1,TSTAT_SECONDS), timbuf(1,1) )
    call fmtseconds( tstats(2,TSTAT_SECONDS), timbuf(1,2) )
    call fmtseconds( tstats(3,TSTAT_SECONDS), timbuf(1,3) )
    call fmtseconds( tstats(4,TSTAT_SECONDS), timbuf(1,4) )
    call fmtseconds( etime, timbuf(1,5) )
    call prints( buf, sfmt, "Time",
	timbuf(1,1), timbuf(1,2), timbuf(1,3), timbuf(1,4), timbuf(1,5) )
    call cdputs( buf, lin, col )

    lin = lin + 1
    ctime = tstats(1,TSTAT_CPUSECONDS) + tstats(2,TSTAT_CPUSECONDS) +
	tstats(3,TSTAT_CPUSECONDS) + tstats(4,TSTAT_CPUSECONDS)
    call fmtseconds( tstats(1,TSTAT_CPUSECONDS), timbuf(1,1) )
    call fmtseconds( tstats(2,TSTAT_CPUSECONDS), timbuf(1,2) )
    call fmtseconds( tstats(3,TSTAT_CPUSECONDS), timbuf(1,3) )
    call fmtseconds( tstats(4,TSTAT_CPUSECONDS), timbuf(1,4) )
    call fmtseconds( ctime, timbuf(1,5) )
    call prints( buf, sfmt, "Cpu time",
	timbuf(1,1), timbuf(1,2), timbuf(1,3), timbuf(1,4), timbuf(1,5) )
    call cdputs( buf, lin, col )

    lin = lin + 1
    for ( i = 1; i <= 4; i = i + 1 )
	{
	j = tstats(i,TSTAT_SECONDS)
	if ( j <= 0 )
	    x(i) = 0.0
	else
	    x(i) = oneplace( 100.0 *
			      float(tstats(i,TSTAT_CPUSECONDS)) / float(j) )
	}
    if ( etime <= 0 )
	x(5) = 0.0
    else
	x(5) = oneplace(100.0 * float(ctime) / float(etime))
    call prints( buf, pfmt, "Cpu usage", x(1), x(2), x(3), x(4), x(5) )
    call cdputs( buf, lin, col )

    lin = lin + 1
    call prints( buf, dfmt, "Phaser shots",
	tstats(1,TSTAT_PHASERS), tstats(2,TSTAT_PHASERS),
	tstats(3,TSTAT_PHASERS), tstats(4,TSTAT_PHASERS),
	tstats(1,TSTAT_PHASERS) + tstats(2,TSTAT_PHASERS) +
	tstats(3,TSTAT_PHASERS) + tstats(4,TSTAT_PHASERS) )
    call cdputs( buf, lin, col )

    lin = lin + 1
    call prints( buf, dfmt, "Torps fired",
	tstats(1,TSTAT_TORPS), tstats(2,TSTAT_TORPS),
	tstats(3,TSTAT_TORPS), tstats(4,TSTAT_TORPS),
	tstats(1,TSTAT_TORPS) + tstats(2,TSTAT_TORPS) +
	tstats(3,TSTAT_TORPS) + tstats(4,TSTAT_TORPS) )
    call cdputs( buf, lin, col )

    lin = lin + 1
    call prints( buf, dfmt, "Armies bombed",
	tstats(1,TSTAT_ARMBOMB), tstats(2,TSTAT_ARMBOMB),
	tstats(3,TSTAT_ARMBOMB), tstats(4,TSTAT_ARMBOMB),
	tstats(1,TSTAT_ARMBOMB) + tstats(2,TSTAT_ARMBOMB) +
	tstats(3,TSTAT_ARMBOMB) + tstats(4,TSTAT_ARMBOMB) )
    call cdputs( buf, lin, col )

    lin = lin + 1
    call prints( buf, dfmt, "Armies captured",
	tstats(1,TSTAT_ARMSHIP), tstats(2,TSTAT_ARMSHIP),
	tstats(3,TSTAT_ARMSHIP), tstats(4,TSTAT_ARMSHIP),
	tstats(1,TSTAT_ARMSHIP) + tstats(2,TSTAT_ARMSHIP) +
	tstats(3,TSTAT_ARMSHIP) + tstats(4,TSTAT_ARMSHIP) )
    call cdputs( buf, lin, col )

    lin = lin + 1
    call prints( buf, dfmt, "Planets taken",
	tstats(1,TSTAT_CONQPLANETS), tstats(2,TSTAT_CONQPLANETS),
	tstats(3,TSTAT_CONQPLANETS), tstats(4,TSTAT_CONQPLANETS),
	tstats(1,TSTAT_CONQPLANETS) + tstats(2,TSTAT_CONQPLANETS) +
	tstats(3,TSTAT_CONQPLANETS) + tstats(4,TSTAT_CONQPLANETS) )
    call cdputs( buf, lin, col )

    lin = lin + 1
    call prints( buf, dfmt, "Coups",
	tstats(1,TSTAT_COUPS), tstats(2,TSTAT_COUPS),
	tstats(3,TSTAT_COUPS), tstats(4,TSTAT_COUPS),
	tstats(1,TSTAT_COUPS) + tstats(2,TSTAT_COUPS) +
	tstats(3,TSTAT_COUPS) + tstats(4,TSTAT_COUPS) )
    call cdputs( buf, lin, col )

    lin = lin + 1
    call prints( buf, dfmt, "Genocides",
	tstats(1,TSTAT_GENOCIDE), tstats(2,TSTAT_GENOCIDE),
	tstats(3,TSTAT_GENOCIDE), tstats(4,TSTAT_GENOCIDE),
	tstats(1,TSTAT_GENOCIDE) + tstats(2,TSTAT_GENOCIDE) +
	tstats(3,TSTAT_GENOCIDE) + tstats(4,TSTAT_GENOCIDE) )
    call cdputs( buf, lin, col )

    for ( i = 1; i <= 4; i = i + 1 )
	if ( couptime(i) == 0 )
	    timbuf(1,i) = EOS
	else
	    call prints( timbuf(1,i), "%d", couptime(i) )

    if ( ! godlike )
	for ( i = 1; i <= 4; i = i + 1 )
	    if ( team != i )
		call strcpy( "-", timbuf(1,i) )
	    else if ( ! tcoupinfo(i) & timbuf(1,i) != EOS )
		call strcpy( "?", timbuf(1,i) )

    timbuf(1,5) = EOS

    lin = lin + 1
    call prints( buf, sfmt, "Coup time",
	timbuf(1,1), timbuf(1,2), timbuf(1,3), timbuf(1,4), timbuf(1,5) )
    call cdputs( buf, lin, col )

    return

end


###  userline - format user statistics
#
#  SYNOPSIS
#    integer unum, snum
#    character buf()
#    logical showgods, showteam
#    call userline( unum, snum, buf, showgods, showteam )
#
# Special hack: If snum is valid, the team and pseudonym are taken from
# the ship instead of the user.
#
subroutine userline( unum, snum, buf, showgods, showteam )
NOIMPLICIT
integer unum, snum
character buf(ARB)
logical showgods, showteam

    integer i, team
    character ch, ch2, junk(MSGMAXLINE), timstr(20), name(MAXUSERPNAME)
    logical isagod
string hd "name          pseudonym           team skill  wins  loss mxkls  ships      time"
    include "conqcom"

    if ( unum < 1 | unum > MAXUSERS )
	{
	call strcpy( hd, buf )
	return
	}
    if ( ! ulive(unum) )
	{
	buf(1) = EOS
	return
	}

    ch2 = ' '
    if ( showgods )
	{
	for ( i = 1; i <= MAXOOPTIONS; i = i + 1)
	    if ( uooption(unum,i) )
		{
		ch2 = '+'
		break
		}
	if ( ch2 != '+' )
	    if ( isagod( uname(1,unum) ) )
		ch2 = '+'
	}

    # If we were given a valid ship number, use it's information.
    if ( snum >= 1 & snum <= MAXSHIPS )
	{
	call strcpy( spname(1,snum), name )
	team = steam(snum)
	}
    else
	{
	call strcpy( upname(1,unum), name )
	team = uteam(unum)
	}

    # Figure out which team he's on.
    if ( uooption(unum,OOPT_MULTIPLE) & ! showteam )
	ch = 'M'
    else
	ch = chrteams(team)

    call prints( junk, "%-12s %c%-21s %c %6g",
	uname(1,unum),
	ch2,
	name,
	ch,
	oneplace(urating(unum)) )

    call fmtminutes( ( ustats(unum,USTAT_SECONDS) + 30 ) / 60, timstr )

    call prints( buf, "%s %5d %5d %5d %6d %9s",
	junk,
	ustats(unum,USTAT_WINS),
	ustats(unum,USTAT_LOSSES),
	ustats(unum,USTAT_MAXKILLS),
	ustats(unum,USTAT_ENTRIES),
	timstr )

    return

end


###  userlist - display the user list
#
#  SYNOPSIS
#    call userlist( godlike )
#
subroutine userlist( godlike )
NOIMPLICIT
logical godlike

    integer i, j, unum, nu, uvec(MAXUSERS), fuser, fline, lline, lin
    character ch
    logical iogtimed, stillalive
    include "conqcom"
    include "conqcom2"

    # Sort user numbers into uvec() in an insertion sort on urating().
    nu = 0
    for ( unum = 1; unum <= MAXUSERS; unum = unum + 1 )
	if ( ulive(unum) )
	    {
	    for ( i = 1; i <= nu; i = i + 1 )
		if ( urating(uvec(i)) < urating(unum) )
		    {
		    for ( j = nu; j >= i; j = j - 1 )
			uvec(j+1) = uvec(j)
		    break
		    }
	    uvec(i) = unum
	    nu = nu + 1
	    }

    # Do some screen setup.
    call cdclear
    lin = 1
    call cdputc( "U S E R   L I S T", lin )

    lin = lin + 2
    call userline( -1, -1, cbuf, .false., .false. )
    call cdputs( cbuf, lin, 1 )

    for ( j = 1; cbuf(j) != EOS; j = j + 1 )
	if ( cbuf(j) != ' ' )
	    cbuf(j) = '-'
    lin = lin + 1
    call cdputs( cbuf, lin, 1 )

    fline = lin + 1				# first line to use
    lline = MSG_LIN1				# last line to use
    fuser = 1					# first user in uvec

    repeat
	{
	if ( ! godlike )
	    if ( ! stillalive( csnum ) )
		break
	i = fuser
	call cdclrl( fline, lline - fline + 1 )
	lin = fline
	while ( i <= nu & lin <= lline )
	    {
	    call userline( uvec(i), -1, cbuf, godlike, .false. )
	    call cdputs( cbuf, lin, 1 )
	    i = i + 1
	    lin = lin + 1
	    }
	if ( i > nu )
	    {
	    # We're displaying the last page.
	    call putpmt( "--- press space when done ---", MSG_LIN2 )
	    call cdplay( .true. )
	    if ( iogtimed( ch, 1 ) )
		{
		if ( ch == TERM_EXTRA )
		    fuser = 1			# move to first page
		else
		    break
		}
	    }
	else
	    {
	    # There are users left to display.
	    call putpmt( "--- press space for more ---", MSG_LIN2 )
	    call cdplay( .true. )
	    if ( iogtimed( ch, 1 ) )
		if ( ch == TERM_EXTRA )
		    fuser = 1			# move to first page
		else if ( ch == ' ' )
		    fuser = i			# move to next page
		else
		    break
	    }
	}

    return

end


###  userstats - display the user list
#
#  SYNOPSIS
#    call userstats( godlike )
#
subroutine userstats( godlike )
NOIMPLICIT
logical godlike

    integer i, j, unum, nu, uvec(MAXUSERS), fuser, fline, lline, lin
    character ch
    logical iogtimed, stillalive
    string hd "name         cpu  conq coup geno  taken bombed/shot  shots  fired    last entry"
    include "conqcom"
    include "conqcom2"

    # Sort user numbers into uvec() in an insertion sort on urating().
    nu = 0
    for ( unum = 1; unum <= MAXUSERS; unum = unum + 1 )
	if ( ulive(unum) )
	    {
	    for ( i = 1; i <= nu; i = i + 1 )
		if ( urating(uvec(i)) < urating(unum) )
		    {
		    for ( j = nu; j >= i; j = j - 1 )
			uvec(j+1) = uvec(j)
		    break
		    }
	    uvec(i) = unum
	    nu = nu + 1
	    }

    # Do some screen setup.
    call cdclear
    lin = 1
    call cdputc( "M O R E   U S E R   S T A T S", lin )

    lin = lin + 2
    call cdputs( "planets  armies    phaser  torps", lin, 34 )

    call strcpy( hd, cbuf )
    lin = lin + 1
    call cdputs( cbuf, lin, 1 )

    for ( j = 1; cbuf(j) != EOS; j = j + 1 )
	if ( cbuf(j) != ' ' )
	    cbuf(j) = '-'
    lin = lin + 1
    call cdputs( cbuf, lin, 1 )

    fline = lin + 1				# first line to use
    lline = MSG_LIN1				# last line to use
    fuser = 1					# first user in uvec

    repeat
	{
	if ( ! godlike )
	    if ( ! stillalive( csnum ) )
		break
	i = fuser
	call cdclrl( fline, lline - fline + 1 )
	lin = fline
	while ( i <= nu & lin <= lline )
	    {
	    call statline( uvec(i), cbuf )
	    call cdputs( cbuf, lin, 1 )
	    i = i + 1
	    lin = lin + 1
	    }
	if ( i > nu )
	    {
	    # We're displaying the last page.
	    call putpmt( "--- press space when done ---", MSG_LIN2 )
	    call cdplay( .true. )
	    if ( iogtimed( ch, 1 ) )
		{
		if ( ch == TERM_EXTRA )
		    fuser = 1			# move to first page
		else
		    break
		}
	    }
	else
	    {
	    # There are users left to display.
	    call putpmt( "--- press space for more ---", MSG_LIN2 )
	    call cdplay( .true. )
	    if ( iogtimed( ch, 1 ) )
		{
		if ( ch == TERM_EXTRA )
		    fuser = 1			# move to first page
		else if ( ch == ' ' )
		    fuser = i			# move to next page
		else
		    break
		}
	    }
	}

    return

end


###  statline - format a user stat line
#
#  SYNOPSIS
#    integer unum
#    character buf()
#    call statline( unum, buf )
#
subroutine statline( unum, buf )
NOIMPLICIT
integer unum
character buf(ARB)

    integer i, length
    character ch, junk(MSGMAXLINE), percent(MSGMAXLINE)
    include "conqcom"

    if ( unum < 1 | unum > MAXUSERS )
	{
	buf(1) = EOS
	return
	}
    if ( ! ulive(unum) )
	{
	buf(1) = EOS
	return
	}

    if ( ustats(unum,USTAT_SECONDS) == 0 )
	call strcpy( "- ", percent )
    else
	{
	i = 1000 * ustats(unum,USTAT_CPUSECONDS) / ustats(unum,USTAT_SECONDS)
	call prints( percent, "%d%%", (i + 5) / 10 )
	}

    call prints( junk, "%-12s %4s %4d %4d %4d",
	uname(1,unum),
	percent,
	ustats(unum,USTAT_CONQUERS),
	ustats(unum,USTAT_COUPS),
	ustats(unum,USTAT_GENOCIDE) )

    call prints( buf, "%s %6d %6d %4d %6d %6d",
	junk,
	ustats(unum,USTAT_CONQPLANETS),
	ustats(unum,USTAT_ARMBOMB),
	ustats(unum,USTAT_ARMSHIP),
	ustats(unum,USTAT_PHASERS),
	ustats(unum,USTAT_TORPS) )

    # Convert zero counts to dashes.
    ch = EOS
    for ( i = 9; buf(i) != EOS; i = i + 1 )
	{
	if ( buf(i) == '0' )
	    if ( ch == ' ' )
		if ( buf(i+1) == ' ' | buf(i+1) == EOS )
		    buf(i) = '-'
	ch = buf(i)
	}

    call prints( junk, " %13.13s", ulastentry(1,unum) )
    call appstr( junk, buf )

    return

end


###  zeroplanet - zero a planet (DOES SPECIAL LOCKING)
#
#  SYNOPSIS
#    integer pnum, snum
#    call zeroplanet( pnum, snum )
#
#  NOTE
#    This routines ASSUMES you have the common area locked before you call it.
#
subroutine zeroplanet( pnum, snum )
NOIMPLICIT
integer pnum, snum

    integer oteam, i, rndint
    include "conqcom"

    oteam = pteam(pnum)
    pteam(pnum) = TEAM_NOTEAM
    parmies(pnum) = 0

    # Make the planet not scanned.
    for ( i = 1; i <= NUMTEAMS; i = i + 1 )
	pscanned(pnum,i) = .false.

    if ( oteam != TEAM_SELFRULED & oteam != TEAM_NOTEAM )
	{
	# Check whether that was the last planet owned by the vanquished.
	for ( i = 1; i <= NUMPLANETS; i = i + 1 )
	    if ( pteam(i) == oteam )
		return
	# Yes.
	couptime(oteam) = rndint( MIN_COUP_MINUTES, MAX_COUP_MINUTES )
	tcoupinfo(oteam) = .false.		# lost coup info
	if ( snum >= 1 & snum <= MAXSHIPS )
	    {
	    ustats(suser(snum),USTAT_GENOCIDE) =
		ustats(suser(snum),USTAT_GENOCIDE) + 1
	    tstats(steam(snum),TSTAT_GENOCIDE) =
		tstats(steam(snum),TSTAT_GENOCIDE) + 1
	    }
	}

    return

end
