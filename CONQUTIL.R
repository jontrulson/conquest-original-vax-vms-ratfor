###############################################################################
#
#                               C O N Q U T I L
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
include "sclb"				# used only by pagefile()


###  acdist - figure distance traveled while changing velocities
#
#  SYNOPSIS
#    real dis, curvel, newvel, acc
#    dis = acdist( curvel, newvel, acc )
#
#  DESCRIPTION
#    These formulas works for de-acceleration only.
#
real function acdist( curvel, newvel, acc )
NOIMPLICIT
real curvel, newvel, acc

    real t
#
#    The following is a box approximation that takes into account
#    the way the driver moves ships and so gives the *exact* distance.
#
#    v = curvel - newvel
#    n = v / ( ITER_SECONDS * acc ) + 1.0
#    d = ( n * ITER_SECONDS + ( v / 2.0 + newvel ) * MM_PER_SEC_PER_WARP )
#
#    A "faster", but less accurate formula (when compared to the
#    way the driver moves ships) is as follows:
#
    t = ( curvel - newvel ) / acc
    return ( ( curvel * t - 0.5 * acc * t * t ) * MM_PER_SEC_PER_WARP )

end


###  angle - compute the angle between two points
#
#  SYNOPSIS
#    real ang, angle, fromx, fromy, tox, toy
#    ang = angle( fromx, fromy, tox, toy )
#
real function angle( fromx, fromy, tox, toy )
NOIMPLICIT
real fromx, fromy, tox, toy

    real mod360

    if ( fromx == tox & fromy == toy )
	return ( 0.0 )

    return ( mod360( rtod( atan2( toy - fromy, tox - fromx ) ) ) )

end


###  appint - append an integer to a string
#
#  SYNOPSIS
#    integer int
#    character str()
#    call appint( i, str )
#
subroutine appint( i, str )
NOIMPLICIT
integer i
character str(ARB)

    integer j, itoc
    character buf(20)

    j = itoc( i, buf, 20 )
    call appstr( buf, str )

    return

end


###  appnum - append a number in English
#
#  SYNOPSIS
#    integer num
#    character buf()
#    call appnum( num, buf )
#
# Note: This routine only works for the number less than 100.
#
subroutine appnum( num, buf )
NOIMPLICIT
integer num
character buf(ARB)

    integer i, j

    i = num
    if ( i >= 100 )
	{
	call appstr( "big num", buf )
	return
	}
    if ( i >= 20 )
	{
	j = i/10
	switch ( j )
	    {
	    case 2:
		call appstr( "twenty", buf )
	    case 3:
		call appstr( "thirty", buf )
	    case 4:
		call appstr( "forty", buf )
	    case 5:
		call appstr( "fifty", buf )
	    case 6:
		call appstr( "sixty", buf )
	    case 7:
		call appstr( "seventy", buf )
	    case 8:
		call appstr( "eighty", buf )
	    case 9:
		call appstr( "ninety", buf )
	    default:
		call appstr( "???", buf )
	    }
	i = i - j*10
	if ( i == 0 )
	    return
	call appchr( '-', buf )
	}

    switch ( i )
	{
	case 0:
	    call appstr( "zero", buf )
	case 1:
	    call appstr( "one", buf )
	case 2:
	    call appstr( "two", buf )
	case 3:
	    call appstr( "three", buf )
	case 4:
	    call appstr( "four", buf )
	case 5:
	    call appstr( "five", buf )
	case 6:
	    call appstr( "six", buf )
	case 7:
	    call appstr( "seven", buf )
	case 8:
	    call appstr( "eight", buf )
	case 9:
	    call appstr( "nine", buf )
	case 10:
	    call appstr( "ten", buf )
	case 11:
	    call appstr( "eleven", buf )
	case 12:
	    call appstr( "twelve", buf )
	case 13:
	    call appstr( "thirteen", buf )
	case 14:
	    call appstr( "fourteen", buf )
	case 15:
	    call appstr( "fifteen", buf )
	case 16:
	    call appstr( "sixteen", buf )
	case 17:
	    call appstr( "seventeen", buf )
	case 18:
	    call appstr( "eighteen", buf )
	case 19:
	    call appstr( "nineteen", buf )
	}

    return

end


###  appnumtim - append English formated time and date
#
#  SYNOPSIS
#   call appnumtim( now, buf )
#
#	now - integer time vector as returned by getnow(2)
#	buf - character string to receive time
#
integer function appnumtim( now, buf )
NOIMPLICIT
integer now(ARB)
character buf(ARB)

    integer hour, wkday
    logical am

    am = .true.				# assume morning
    hour = now(4)
    if ( hour == 0 )
	hour = 12			# midnight
    else if ( hour == 12 )
	am = .false.			# afternoon
    else if ( hour > 12 )
	{
	hour = hour - 12
	am = .false.			# afternoon
	}
    switch ( wkday( now(2), now(3), now(1) ) )
	{
	case 1:
	    call appstr( "Sunday", buf )
	case 2:
	    call appstr( "Monday", buf )
	case 3:
	    call appstr( "Tuesday", buf )
	case 4:
	    call appstr( "Wednesday", buf )
	case 5:
	    call appstr( "Thursday", buf )
	case 6:
	    call appstr( "Friday", buf )
	case 7:
	    call appstr( "Saturday", buf )
	default:
	    call appstr( "???", buf )
	}
    call appstr( ", ", buf )
    switch ( now(2) )
	{
	case 1:
	    call appstr( "January", buf )
	case 2:
	    call appstr( "February", buf )
	case 3:
	    call appstr( "March", buf )
	case 4:
	    call appstr( "April", buf )
	case 5:
	    call appstr( "May", buf )
	case 6:
	    call appstr( "June", buf )
	case 7:
	    call appstr( "July", buf )
	case 8:
	    call appstr( "August", buf )
	case 9:
	    call appstr( "September", buf )
	case 10:
	    call appstr( "October", buf )
	case 11:
	    call appstr( "November", buf )
	case 12:
	    call appstr( "December", buf )
	default:
	    call appstr( "???", buf )
	}
    call appchr( ' ', buf )
    call appint( now(3), buf )		# day of month
    call appstr( ", at ", buf )
    call appnum( hour, buf )		# hour
    call appchr( ' ', buf )
    if ( now(5) == 0 )			# minute
	call appstr( "o'clock", buf )
    else
	{
	if ( now(5) < 10 )
	    call appstr( "o ", buf )
	call appnum( now(5), buf )
	}
    call appchr( ' ', buf )
    if ( am )
	call appstr( "ante", buf )
    else
	call appstr( "post", buf )
    call appstr( " meridiem", buf )

    return

end


###  appsstatus - append ship status string
#
#  SYNOPSIS
#    integer status
#    character buf()
#    call appsstatus( status, buf )
#
subroutine appsstatus( status, buf )
NOIMPLICIT
integer status
character buf(ARB)

    switch ( status )
	{
	case SS_OFF:
	    call appstr( "off", buf )
	case SS_ENTERING:
	    call appstr( "entering", buf )
	case SS_LIVE:
	    call appstr( "live", buf )
	case SS_DYING:
	    call appstr( "dying", buf )
	case SS_DEAD:
	    call appstr( "dead", buf )
	case SS_RESERVED:
	    call appstr( "reserved", buf )
	default:
	    call appint( status, buf )
	}
    return

end


###  apptitle - append a team oriented title
#
#  SYNOPSIS
#    integer team
#    character buf()
#    call apptitle( team, buf )
#
subroutine apptitle( team, buf )
NOIMPLICIT
integer team
character buf(ARB)

    switch ( team )
	{
	case TEAM_FEDERATION:
	    call appstr( "Captain", buf )
	case TEAM_ROMULAN:
	    call appstr( "Centurion", buf )
	case TEAM_ORION:
	    call appstr( "Commander", buf )
	case TEAM_KLINGON:
	    call appstr( "Kommander", buf )
	}

    return

end


###  arrows - interpret arrow keys
#
#  SYNOPSIS
#    logical flag, arrows
#    character str()
#    real dir
#    flag = arrows( str, dir )
#
logical function arrows( str, dir )
NOIMPLICIT
character str(ARB)
real dir

    integer i, idx, index
    real ndir1, ndir2, ndir, mod360
    string arrs "dewqazxc"

    # Special hack preventing "ea" and "da" from being recognized as arrows.
    # "ea" is reserved for Earth and "da" for Dakel.
    if ( str(1) == 'e' & str(2) == 'a' )
	return ( .false. )
    if ( str(1) == 'd' & str(2) == 'a' )
	return ( .false. )

    dir = 0.0
    for ( i = 1; str(i) != EOS; i = i + 1 )
	{
	idx = index( arrs, str(i) )
	if ( idx == 0 )
	    return ( .false. )
	ndir1 = (idx-1) * 45.0
	ndir2 = ndir1 - 360.0
	if ( abs( dir - ndir1 ) < abs( dir - ndir2 ) )
	    ndir = ndir1
	else
	    ndir = ndir2
	dir = ( dir*(i-1) + ndir ) / i
	}

    dir = mod360( dir )

    return ( .true. )

end


###  cerror - conquest error message
#
#  SYNOPSIS
#    integer to, status
#    character fmt()
#    call cerror( to, fmt, status )
#
subroutine cerror( to, fmt, status )
NOIMPLICIT
integer to
character fmt(ARB)
integer status

    character buf(MSGMAXLINE)

    call prints( buf, fmt, status )
    call stormsg( MSG_OUTSIDE, to, buf )

    return

end


###  confirm - ask the user to confirm a dangerous action
#
#  SYNOPSIS
#    logical ok, confirm
#    ok = confirm( 0 )
#
logical function confirm( dummy )
NOIMPLICIT
integer dummy

    character ch, getcx, buf(MSGMAXLINE)

    call cdclrl( MSG_LIN2, 1 )
    ch = getcx( "Are you sure? ", MSG_LIN2, 0, TERMS, buf, MSGMAXLINE )
    call cdclrl( MSG_LIN2, 1 )
    if ( ch == TERM_ABORT )
	return ( .false. )
    if ( buf(1) == 'y' | buf(1) == 'Y' )
	return ( .true. )

    return ( .false. )

end


###  delblanks - remove all blanks from a string
#
#  SYNOPSIS
#    character str()
#    call delblanks( str )
#
subroutine delblanks( str )
NOIMPLICIT
character str(ARB)

    integer i, j

    for ( i = 1; str(i) != EOS; )
	if ( str(i) == ' ' )
	    for ( j = i; str(j) != EOS; j = j + 1 )
		str(j) = str(j+1)
	else
	    i = i + 1

    return

end


###  dgrand - delta time for thousands
#
#  SYNOPSIS
#    integer i, dgrand, s, n
#    i = dgrand( s, n )
#
integer function dgrand( s, n )
NOIMPLICIT
integer s, n

    integer tn, ts

    # Save s in case it and n are the same variable.
    ts = s

    # Get thousands since midnight.
    call grand( tn )
    n = tn

    # Calculate the difference.
    if ( tn < ts )
	tn = tn + 24 * 60 * 60 * 1000		# crossed midnight

    return ( tn - ts )

end


###  dsecs - delta time for seconds
#
#  SYNOPSIS
#    integer i, dsecs, s, n
#    i = dsecs( s, n )
#
integer function dsecs( s, n )
NOIMPLICIT
integer s, n

    integer tn, ts

    # Save s in case it and n are the same variable.
    ts = s

    # Get seconds since midnight.
    call gsecs( tn )
    n = tn

    # Calculate the difference.
    if ( tn < ts )
	tn = tn + 24 * 60 * 60		# crossed midnight

    return ( tn - ts )

end


###  explosion - hits based on distance
#
#  SYNOPSIS
#    real newhits, explosion, basehits, dis
#    newhits = explosion( basehits, dis )
#
real function explosion( basehits, dis )
NOIMPLICIT
real basehits, dis

    if ( dis > PHASER_DIST )
	return ( 0.0 )
    return ( basehits / ( ( EXPLOSION_FALLOFF - 1.0 ) *
	max( dis - EXPLOSION_RADIUS, 0.0 ) / PHASER_DIST + 1.0 ) -
	basehits / EXPLOSION_FALLOFF * dis / PHASER_DIST )

end


###  fmtminutes - format a minutes string
#
#  SYNOPSIS
#   call fmtminutes( itime, buf )
#
#	itime - integer time in minutes
#	buf - character string to receive time
#
integer function fmtminutes( itime, buf )
NOIMPLICIT
integer itime
character buf(ARB)

    integer i, days, hours, minutes
    character junk(32)
    logical minus

    if ( itime < 0 )
	{
	minus = .true.
	i = -itime
	}
    else
	{
	minus = .false.
	i = itime
	}

    minutes = mod( i, 60 )		# minutes
    i = i / 60
    hours = mod( i, 24 )		# hours
    days = i / 24			# days

    if ( minus )
	if ( days > 0 )
	    days = -days
	else if ( hours > 0 )
	    hours = -hours
	else
	    minutes = -minutes

    # Format time.
    call prints( junk, "%d %2d:%02d", days, hours, minutes )

    # Skip the junk and find the beginning.
    for ( i = 1; junk(i) == ' ' | junk(i) == ':' | junk(i) == '0'; i = i + 1 )
	;

    # Store in return buffer.
    call scopy( junk, i, buf, 1 )

    return

end


###  fmtseconds - format a seconds string
#
#  SYNOPSIS
#   call fmtseconds( itime, buf )
#
#	itime - integer time in seconds
#	buf - character string to receive time
#
integer function fmtseconds( itime, buf )
NOIMPLICIT
integer itime
character buf(ARB)

    integer i, days, hours, minutes, seconds
    character junk(32)
    logical minus

    if ( itime < 0 )
	{
	minus = .true.
	i = -itime
	}
    else
	{
	minus = .false.
	i = itime
	}

    seconds = mod( i, 60 )		# seconds
    i = i / 60
    minutes = mod( i, 60 )		# minutes
    i = i / 60
    hours = mod( i, 24 )		# hours
    days = i / 24			# days

    if ( minus )
	if ( days > 0 )
	    days = -days
	else if ( hours > 0 )
	    hours = -hours
	else if ( minutes > 0 )
	    minutes = -minutes
	else
	    seconds = -seconds

    # Format time.
    call prints( junk, "%d %2d:%02d:%02d", days, hours, minutes, seconds )

    # Skip the junk and find the beginning.
    for ( i = 1; junk(i) == ' ' | junk(i) == ':' | junk(i) == '0'; i = i + 1 )
	;

    # Store in return buffer.
    call scopy( junk, i, buf, 1 )

    return

end


###  getamsg - find the next readable message
#
#  SYNOPSIS
#    logical gotone, getamsg
#    integer snum, msg
#    gotone = getamsg( snum, msg )
#
logical function getamsg( snum, msg )
NOIMPLICIT
integer snum, msg

    integer modp1
    logical canread, grabmsg
    include "conqcom"

    repeat
	{
	while ( msg != lastmsg )
	    {
	    msg = modp1( msg + 1, MAXMESSAGES )
	    # If we can read it, only do so if it's not from us to GOD.
	    if ( canread( snum, msg ) )
		return ( snum != msgfrom(msg) | MSG_GOD != msgto(msg) )
	    }
	# We didn't read a message; try for a terminal broadcast.
	#  message from the real world.
	}
    until ( ! grabmsg( snum ) )

    return ( .false. )

end


###  getcx - prompt for a string, centered
#
#  SYNOPSIS
#    character pmt(),
#    integer lin, offset
#    character terms(), buf()
#    integer len
#    tch = getcx( pmt, lin, offset, terms, buf, len )
#
character function getcx( pmt, lin, offset, terms, buf, len )
NOIMPLICIT
character pmt(ARB)
integer lin, offset
character terms(ARB), buf(ARB)
integer len

    integer i, length
    character cdgetx
    include "conqcom2"

    i = ( cmaxcol - length( pmt ) ) / 2 + offset
    if ( i <= 0 )
	i = 1
    return ( cdgetx( pmt, lin, i, terms, buf, len ) )

end


###  getdandt - get the date and time into a string
#
#  SYNOPSIS
#    character buf()
#    call getdandt( buf )
#
subroutine getdandt( buf )
NOIMPLICIT
character buf(ARB)

    integer now(7)
    character junk(5)

    call getnow( now )
    switch ( now(2) )
	{
	case 1:
	    call strcpy( "Jan", junk )
	case 2:
	    call strcpy( "Feb", junk )
	case 3:
	    call strcpy( "Mar", junk )
	case 4:
	    call strcpy( "Apr", junk )
	case 5:
	    call strcpy( "May", junk )
	case 6:
	    call strcpy( "Jun", junk )
	case 7:
	    call strcpy( "Jul", junk )
	case 8:
	    call strcpy( "Aug", junk )
	case 9:
	    call strcpy( "Sep", junk )
	case 10:
	    call strcpy( "Oct", junk )
	case 11:
	    call strcpy( "Nov", junk )
	case 12:
	    call strcpy( "Dec", junk )
	default:
	    call strcpy( "???", junk )
	}
    call prints( buf, "%2d:%02d:%02d %02d%s%02d",
	now(4), now(5), now(6), now(3), junk, mod( now(1), 100 ) )

    return

end


###  gettarget - get a target angle from the user
#
#  SYNOPSIS
#    character pmt()
#    integer lin, col
#    real dir
#    logical flag, gettarget
#    flag = gettarget( pmt, lin, col, dir )
#
logical function gettarget( pmt, lin, col, dir, default )
NOIMPLICIT
character pmt(ARB)
integer lin, col
real dir, default

    integer i, j, alldig
    logical arrows, safectoi
    real mod360
    character ch, cdgetx, buf(MSGMAXLINE)

    call cdclrl( lin, 1 )
    ch = cdgetx( pmt, lin, col, TERMS, buf, MSGMAXLINE )
    if ( ch == TERM_ABORT )
	return ( .false. )

    call delblanks( buf )
    call fold( buf )
    if ( buf(1) == EOS )
	{
	# Default.
	dir = default
	return ( .true. )
	}
    if ( alldig( buf ) == YES )
	{
	i = 1
	if ( ! safectoi( j, buf, i ) )
	    return ( .false. )
	dir = mod360( float( j ) )
	return ( .true. )
	}
    if ( arrows( buf, dir ) )
	return ( .true. )

    return ( .false. )

end


###  grabmsg - grab a broadcast message from the outside world, if any
#
#  SYNOPSIS
#    logical flag, grabmsg
#    integer snum
#    flag = grabmsg( snum )
#
logical function grabmsg( snum )
NOIMPLICIT
integer snum

    logical iogbroadcast
    character buf(MAXLINE)		# this one needs to be big

    if ( iogbroadcast( buf ) )
	{
	call iofmtstr( buf )
        call stormsg( MSG_OUTSIDE, snum, buf )
	return ( .true. )
	}

    return ( .false. )

end


###  grand - thousands since midnight
#
#  SYNOPSIS
#    integer h
#    call grand( h )
#
subroutine grand( h )
NOIMPLICIT
integer h

    integer now(7)

    call getnow( now )
    h = ( ( ( now(4) * 60 ) + now(5) ) * 60 + now(6) ) * 1000 + now(7)

    return

end


###  gsecs - seconds since midnight
#
#  SYNOPSIS
#    integer s
#    call gsecs( s )
#
subroutine gsecs( s )
NOIMPLICIT
integer s

    integer now(7)

    call getnow( now )
    s = ( ( now(4) * 60 ) + now(5) ) * 60 + now(6)

    return

end


###  mod360 - modularize a real number to 0.0 <= r < 360.0
#
#  SYNOPSIS
#    real mr, mod360, r
#    mr = mod360( r )
#
real function mod360( r )
NOIMPLICIT
real r

    real mr

    mr = r

    while ( mr < 0.0 )
	mr = mr + 360.0

    return ( mod( mr, 360.0 ) )

end


###  modp1 - modulus plus one
#
#  SYNOPSIS
#    integer mi, modp1, i, modulus
#    mi = modp1( i, modulus )
#
integer function modp1( i, modulus )
NOIMPLICIT
integer i, modulus

    integer m

    m = i
    while ( m < 1 )
	m = m + modulus

    return ( mod( m-1, modulus ) + 1 )

end


###  more - wait for the user to type a space
#
#  SYNOPSIS
#    character pmt()
#    logical spacetyped, more
#    spacetyped = more( pmt )
#
logical function more( pmt )
NOIMPLICIT
character pmt(ARB)

    character ch, iogchar
    string pml "--- press space for more ---"

    if ( pmt(1) != EOS )
	call putpmt( pmt, MSG_LIN2 )
    else
	call putpmt( pml, MSG_LIN2 )

    call cdplay( .true. )
    ch = iogchar( ch )
    return ( ch == ' ' )

end


###  pagefile - page through a file
#
#  SYNOPSIS
#    character file(), errmsg()
#    logical ignorecontroll, eatblanklines
#    call pagefile( file, errmsg, ignorecontroll, eatblanklines )
#
subroutine pagefile( file, errmsg, ignorecontroll, eatblanklines )
NOIMPLICIT
character file(ARB), errmsg(ARB)
logical ignorecontroll, eatblanklines

    integer i, j, o, status, fd, pause, open, type, lin, getlin, ctoi, index
    integer length
    character lastch, cdnulfont, cdsngfont
    character nfont, afont, rfont, ufont, ofont(MSGMAXLINE)
    character ibuf(MAXLINE), obuf(MSGMAXLINE)
    logical x, more, lastblank, iochav
    string sdone "--- press space when done ---"

    fd = open( file, READ )
    if ( fd == ERR )
	{
	call cdclear
	call cdredo
	call cdputc( errmsg, MSG_LIN2/2 )
	x = more( sdone )
	return
	}

    nfont = cdnulfont( 0 )
    rfont = cdsngfont( ATTRIBUTE_REVERSE )
    ufont = cdsngfont( ATTRIBUTE_UNDERLINE )
    if ( ufont == nfont )
	ufont = rfont

    lastblank = .true.				# skip leading blank lines
    repeat
	{
	call cdclear
	lin = 0
	repeat
	    {
	    # Get the next line, bail if none left.
	    status = getlin( ibuf, fd )
	    if ( status == EOF )
		break

	    # Kill trailing newline.
	    i = length( ibuf )
	    if ( i > 0 )
		if ( ibuf(i) == '@n' )
		    ibuf(i) = EOS

	    # Blank eating logic.
	    if ( eatblanklines )
		{
	        # Munch multiple blank lines.
	        if ( ibuf(1) == EOS )
		    {
		    if ( lastblank )
		        next
		    else
		        lastblank = .true.
		    }
	        else
		    lastblank = .false.
		}

	    # Step to the next line.
	    lin = lin + 1

	    # Copy the line, handling special characters.
	    for ( o = 1; o < MSGMAXLINE; o = o + 1 )
		ofont(o) = nfont
	    lastch = ' '
	    pause = -1
	    i = 1
	    o = 1
	    while ( o < MSGMAXLINE )
		switch ( ibuf(i) )
		    {
		    case EOS:
			break
		    case '@^h':
			# Handle boldface and underlined letters.
			if ( o > 1 )
			    o = o - 1
			if ( lastch == '_' )
			    ofont(o) = ufont
			else if ( type( lastch ) == LETTER )
			    ofont(o) = rfont
			lastch = ibuf(i)
			i = i + 1
		    case '@^l':
			i = i + 1		# step over ^l
			lastch = ' '
			if ( ! ignorecontroll )
			    {
			    pause = ctoi( ibuf, i )
			    break
			    }
		    case '@t':
			j = ((o-1)/8+1)*8+1
			for ( ; o <= j; o = o + 1 )
			    obuf(o) = ' '
			lastch = ibuf(i)
			i = i + 1
		    default:
			# Skip junk.
			if ( ibuf(i) >= ' ' & ibuf(i) <= '~' )
			    {
			    lastch = ibuf(i)
			    obuf(o) = ibuf(i)
			    o = o + 1
			    }
			else
			    lastch = ' '
			i = i + 1
		    }
	    obuf(o) = EOS

	    # Now display the line.
	    afont = nfont
	    call cdfont( afont )
	    for ( o = 1; obuf(o) != EOS; o = o + 1 )
		if ( afont != ofont(o) )
		    {
		    afont = ofont(o)
		    call cdfont( afont )
		    call cdput( obuf(o), lin, o )
		    }
		else if ( obuf(o) != ' ' )
		    call cdput( obuf(o), lin, o )

	    # Make sure we're in the normal font.
	    call cdfont( nfont )

	    # If we've displayed enough lines, bail.
	    }
	until ( lin >= MSG_LIN2-1 | pause >= 0 )

	if ( status == EOF )
	    {
	    x = more( sdone )
	    break
	    }
	if ( pause > 0 )
	    {
	    call cdplay( .true. )
	    if ( ! iochav( 0 ) )
		call sleep( float(pause) / 100.0 )
	    }
	else
	    {
	    if ( ! more( "--- press space for more ---" ) )
		break
	    }
	}

    # Make sure we're in the normal font.
    call cdfont( nfont )

    return

end


###  putmsg - display a message on the bottom of the user's screen
#
#  SYNOPSIS
#    character msg()
#    integer line
#    call putmsg( msg, line )
#
subroutine putmsg( msg, line )
NOIMPLICIT
character msg(ARB)
integer line

    call cdclrl( line, 1 )
    call cdputs( msg, line, 1 )

    return

end


###  putpmt - display a prompt
#
#  SYNOPSIS
#    character pmt()
#    integer line
#    call putpmt( pmt, line )
#
subroutine putpmt( pmt, line )
NOIMPLICIT
character pmt(ARB)
integer line

    integer i, dcol, pcol, length
    include "conqcom2"

    i = length( pmt )
    dcol = ( cmaxcol - i ) / 2
    pcol = dcol + i
    call cdclrl( line, 1 )
    call cdputs( pmt, line, dcol )
    call cdmove( line, pcol )

    return

end


###  safectoi - character to integer conversion with overflow protection
#
#  SYNOPSIS
#    logical flag, safectoi
#    integer num, ptr
#    character buf()
#    flag = safectoi( num, buf ptr )
#
logical function safectoi( num, buf, ptr )
NOIMPLICIT
integer num
character buf(ARB)
integer ptr

    integer i, length, ctoi


    i = length( buf ) - ptr - 1

    # If the number is the same size as the biggest integer,
    #  assume that it is too big.
    if ( i >= MAXINTEGER_LENGTH )
	{
	num = MAXINTEGER_VALUE
	return ( .false. )
	}

    # No problem, get the number.
    num = ctoi( buf, ptr )

    return ( .true. )

end


###  special - check if a string is a valid "special" specifier
#
#  SYNOPSIS
#    character str()
#    integer what, token, count
#    logical flag, special
#    flag = special( str, what, token, count )
#
logical function special( str, what, token, count )
NOIMPLICIT
character str(ARB)
integer what, token, count

    integer i, type
    logical l, stmatch, safectoi
    character buf(20)

    what = NEAR_ERROR
    token = SPECIAL_NOTSPECIAL
    count = 0

    # Reject obvious losers.
    if ( str(1) != 'n' & str(1) != 'w' & str(1) != 'h' )
	return ( .false. )

    call stcpn( str, buf, 20 )			# need a private copy

    # Find threshold count; cleverly, the default will be zero when using ctoi.
    for ( i = 1; buf(i) != EOS & type( buf(i) ) != DIGIT; i = i + 1 )
	;
    buf(i) = EOS				# ditch numeric part
    l =  safectoi( count, str, i )		# ignore status

    if ( stmatch( buf, "nes", .false. ) )	# this one must be first
	{
	what = NEAR_SHIP
	token = SPECIAL_ENEMYSHIP
	}
    else if ( stmatch( buf, "nfp", .false. ) )
	{
	what = NEAR_PLANET
	token = SPECIAL_FUELPLANET
	}
    else if ( stmatch( buf, "nep", .false. ) )
	{
	what = NEAR_PLANET
	token = SPECIAL_ENEMYPLANET
	}
    else if ( stmatch( buf, "ns", .false. ) )
	{
	what = NEAR_SHIP
	token = SPECIAL_SHIP
	}
    else if ( stmatch( buf, "np", .false. ) )
	{
	what = NEAR_PLANET
	token = SPECIAL_PLANET
	}
    else if ( stmatch( buf, "nts", .false. ) )
	{
	what = NEAR_SHIP
	token = SPECIAL_TEAMSHIP
	}
    else if ( stmatch( buf, "nap", .false. ) )
	{
	what = NEAR_PLANET
	token = SPECIAL_ARMYPLANET
	if ( count <= 0 )
	    count = 1
	}
    else if ( stmatch( buf, "wp", .false. ) )
	{
	what = NEAR_PLANET
	token = SPECIAL_WEAKPLANET
	}
    else if ( stmatch( buf, "ntp", .false. ) )
	{
	what = NEAR_PLANET
	token = SPECIAL_TEAMPLANET
	}
    else if ( stmatch( buf, "nrp", .false. ) )
	{
	what = NEAR_PLANET
	token = SPECIAL_REPAIRPLANET
	}
    else if ( stmatch( buf, "hp", .false. ) )
	{
	what = NEAR_PLANET
	token = SPECIAL_HOMEPLANET
	}
    else
	return ( .false. )			# string simply isn't special

    return ( .true. )

end


###  stcpn - copy a string with a size limit
#
#  SYNOPSIS
#    character from(), to()
#    integer tosize
#    call stcpn( from, to, tosize )
#
subroutine stcpn( from, to, tosize )
NOIMPLICIT
character from(ARB), to(ARB)
integer tosize

    integer i

    i = 0
    repeat
	{
	i = i + 1
	if ( i >= tosize )
	    break
	to(i) = from(i)
	}
    until ( from(i) == EOS )

    to(tosize) = EOS

    return

end


###  stmatch - check whether two strings match or not
#
#  SYNOPSIS
#    logical matched, stmatch, casesensitive
#    character str1(), str2()
#    matched = stmatch( str1, str2, casesensitive )
#
logical function stmatch( str1, str2, casesensitive )
NOIMPLICIT
character str1(ARB), str2(ARB)
logical casesensitive

    integer i
    character clower

    if ( casesensitive )
        for ( i = 1; str1(i) == str2(i) & str1(i) != EOS; i = i + 1 )
	    ;
    else
        for ( i = 1;
              clower(str1(i)) == clower(str2(i)) & str1(i) != EOS;
	      i = i + 1 )
	    ;

    if ( i == 1 )
	if ( str1(1) == EOS & str2(1) == EOS )
	    return ( .true. )
	else
	    return ( .false. )
    else if ( str1(i) == EOS | str2(i) == EOS )
	return ( .true. )

    return ( .false. )

end


###  subang - find smallest difference between angles.
#
#  SYNOPSIS
#    real h, subang, a1, a2
#    h = subang( a1, a2 )
#
real function subang( a1, a2 )
NOIMPLICIT
real a1, a2

    real x

    x = a1 - a2
    while ( x > 180.0 )
	x = x - 360.0
    while ( x < -180.0 )
	x = x + 360.0

    return ( x )

end
