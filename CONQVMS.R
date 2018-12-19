###############################################################################
#
#                               C O N Q V M S
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

define(JPI$_OWNER,16%00000303)		# owner pid token for drcheck()
define(JPI$_PRIB,16%00000309)		# base priority token for conqinit()
define(JPI$_CPUTIM,16%00000407)		# cpu time token for gcputime()


###  astoff - disable asts
#
#  SYNOPSIS
#    call astoff
#
subroutine astoff
NOIMPLICIT

    call sys$setast( %val(0) )		# disable asts
    return

end


###  aston - enable asts
#
#  SYNOPSIS
#    call aston
#
subroutine aston
NOIMPLICIT

    call sys$setast( %val(1) )		# enable asts
    return

end


###  astservice - ast service routine for conquest
#
#  SYNOPSIS
#    call astservice
#
# This routine gets called from a sys$setimr ast. Normally, it outputs
# one screen update and then sets up another timer request.
#
subroutine astservice
NOIMPLICIT

    integer now, msg, modp1, dgrand
    logical getamsg, stillalive, iochav, readone
    include "conqcom"
    include "conqcom2"

    # Don't do anything if we're not supposed to.
    if ( ! cdisplay )
	return

    # Don't do anything if we're dead.
    if ( ! stillalive( csnum ) )
	return

    call drcheck				# handle driver logic

    # See if we can display a new message.
    readone = .false.
    if ( cmsgok )
	if ( dgrand( cmsgrand, now ) >= NEWMSG_GRAND )
	    if ( getamsg( csnum, slastmsg(csnum) ) )
		{
		call readmsg( csnum, slastmsg(csnum) )
		cmsgrand = now
		readone = .true.
		}

    # Perform one ship display update.
    call display( csnum )

    # Un-read the message if there's a chance it got garbaged.
    if ( readone )
	if ( iochav( 0 ) )
	    slastmsg(csnum) = modp1( slastmsg(csnum) - 1, MAXMESSAGES )

    # Schedule for next time.
    call settimer

    return

end


###  comsize - return size of the common block (in bytes)
#
#  SYNOPSIS
#    integer size
#    call comsize( size )
#
subroutine comsize( size )
NOIMPLICIT
integer size

    include "conqcom"

    size = %loc(glastmsg) - %loc(commonrev) + 4

    return

end


###  conqend - machine dependent clean-up
#
#  SYNOPSIS
#    call conqend
#
subroutine conqend
NOIMPLICIT

    call gamend					# clean up game environment

    return

end


###  conqinit - machine dependent initialization
#
#  SYNOPSIS
#    call conqinit
#
subroutine conqinit
NOIMPLICIT

    integer i, lib$get_ef, t_getbpri, strcmp, gdespri
    logical gdial, gprio, gcron
    external c_conq_fdial, c_conq_fprio, c_conq_despri
    external c_conq_badttys, c_conq_antigods, c_conq_conquest
    external c_conq_gamcron, c_conq_newsfile, c_conq_fsubdcl, ss$_normal
    character gamcron(FILENAMESIZE)
    include "conqcom"
    include "conqcom2"

    # First things first.
    if ( commonrev != COMMONSTAMP )
	call error( "conquest: Common block ident mismatch." )

    # Get priority for use when spawning.
    if ( t_getbpri( cpriority ) != %loc(ss$_normal) )
	call error( "conqinit: Failed to get base priority" )

    # Get an event flag for the ast timer.
    if ( lib$get_ef( ctimflag ) != %loc(ss$_normal) )
	call error( "conqinit: Failed to allocate event flag" )

    # Set up game environment.
    gdial = ( %loc(c_conq_fdial) == YES )
    gprio = ( %loc(c_conq_fprio) == YES )
    gdespri = %loc(c_conq_despri)

    # Figure out which gamcron file to use (and if we're gonna use one).
    if ( %loc(c_conq_gamcron) == 0 )
	{
	gcron = .true.
	call gamcronfile( gamcron )
	}
    else
	{
	call strcpy( c_conq_gamcron, gamcron )
	gcron = ( gamcron(1) != 0 )
	}

    call gamlinit( gdial, gprio, gcron, gdespri, c_conq_antigods,
	c_conq_badttys, c_conq_conquest, gamcron )

    # Other house keeping.
    call getpid( cpid )
    csubdcl = ( %loc(c_conq_fsubdcl) == YES )
    cnewsfile = ( strcmp( c_conq_newsfile, "" ) != 0 )

    # Zero process id of our child (since we don't have one yet).
    childpid = 0

    # Zero last time drcheck() was called.
    clastime = 0

    # Haven't scanned anything yet.
    clastinfostr(1) = EOS

    return

end


###  conqstats - handle cpu and elapsed statistics (DOES LOCKING)
#
#  SYNOPSIS
#    integer snum
#    call conqstats( snum )
#
subroutine conqstats( snum )
NOIMPLICIT
integer snum

    integer unum, team, cadd, eadd
    include "conqcom"

    cadd = 0
    eadd = 0
    call upstats( sctime(snum), setime(snum), scacc(snum), seacc(snum),
	cadd, eadd )

    # Add in the new amounts.
    PVLOCK(lockword)
    if ( spid(snum) != 0 )
	{
	# Update stats for a humanoid ship.
	unum = suser(snum)
	ustats(unum,USTAT_CPUSECONDS) = ustats(unum,USTAT_CPUSECONDS) + cadd
	ustats(unum,USTAT_SECONDS) = ustats(unum,USTAT_SECONDS) + eadd
	team = uteam(unum)
	tstats(team,TSTAT_CPUSECONDS) = tstats(team,TSTAT_CPUSECONDS) + cadd
	tstats(team,TSTAT_SECONDS) = tstats(team,TSTAT_SECONDS) + eadd
	ccpuseconds = ccpuseconds + cadd
	celapsedseconds = celapsedseconds + eadd
	}
    PVUNLOCK(lockword)

    return

end


###  dosubdcl - spawn a DCL subprocess
#
#  SYNOPSIS
#    call dosubdcl
#
subroutine dosubdcl
NOIMPLICIT

    integer status, opriority, tccabort, lib$spawn, sys$setpri
    character cdgetx
    logical l, ioautobroad, stillalive
    include "conqcom2"
    include "cexith"				# need to get at ccabort
    string pmt "Press LINEFEED to spawn to DCL: "
    external ss$_normal

    if ( sys$setpri( , , %val(cpriority), opriority ) != %loc(ss$_normal) )
	call cerror( MSG_GOD, "Dosubdcl(): Error setting base priority.", 0 )

    call cdclrl( MSG_LIN1, 2 )

    if ( cdgetx( pmt, MSG_LIN1, 1, TERMS, cbuf, MSGMAXLINE ) == TERM_EXTRA )
	{
	call stoptimer				# turn off the timer ast
	call cdclear
	call cdredo
	call cdmove( 1, 1 )
	call cdplay( .false. )
	l = ioautobroad( .true. )		# turn ON auto broadcast echoing
	tccabort = ccabort			# save old ^C setting
	ccabort = NO				# ignore while spawned
	call ioend				# only have to turn off iolb

	call puts( "Your ship is now on automatic pilot.@n" )
	call puts( "To resume command, type @"logout@".@n" )

	call aston				# enable asts while spawned
	status = lib$spawn( )			# really default case...
	call astoff
	call ioinit
	ccabort = tccabort			# restore ^C setting

	l = ioautobroad( .false. )		# turn OFF again
	call cdclear
	call cdredo
	credraw = .true.
	if ( stillalive( csnum ) )
	    call display( csnum )
	call settimer				# start the timer ast again
	}
    call cdclrl( MSG_LIN1, 1 )

    if ( sys$setpri( , , %val(opriority), ) != %loc(ss$_normal) )
	call cerror( MSG_GOD, "Dosubdcl(): Error resetting base priority.", 0 )

	return

end


###  drcheck - make sure the driver is still around (DOES LOCKING)
#
#  SYNOPSIS
#    call drcheck
#
subroutine drcheck
NOIMPLICIT

    integer apid, ppid, dsecs, modp1, sys$getjpi
    integer*2 list(8)
    include "conqcom"
    include "conqcom2"
    equivalence (apid, list(3))
    data list / 4, JPI$_OWNER, 2*0, 2*0, 2*0 /
    external ss$_normal

    # If we haven't been getting cpu time in recent history, do no-thing.
    if ( dsecs( clastime, clastime ) > TIMEOUT_DRCHECK )
	return

    if ( dsecs( drivtime, playtime ) > TIMEOUT_DRIVER )
	{
	if ( childpid != 0 )
	    {
	    # We own the driver. See if it's still there.
	    apid = %loc(ppid)
	    if ( sys$getjpi( , childpid, , list, , , ) == %loc(ss$_normal) )
		if ( ppid == cpid )
		    {
		    # He's still alive and belongs to us.
		    call gsecs( drivtime )
		    return
		    }
		else
		    call cerror( MSG_GOD, "drcheck: Wrong ppid %x.", ppid )

	    # If we got here, something was wrong; disown the child.
	    childpid = 0
	    }

	PVLOCK(lockword)
	if ( dsecs( drivtime, playtime ) > TIMEOUT_DRIVER )
	    {
	    call drcreate
	    drivcnt = modp1( drivcnt + 1, 1000 )
	    call cerror( MSG_GOD, "Driver timeout #%d.", drivcnt )
	    }
	PVUNLOCK(lockword)
	}
    call drstart

    return

end


###  drcreate - create a new driver process
#
#  SYNOPSIS
#    call drcreate
#
subroutine drcreate
NOIMPLICIT

    integer i, pid, status, idsc(2), ndsc(2), modp1, sys$creprc
    integer*2 epid(2)
    character name(FILENAMESIZE)
    include "conqcom"
    include "conqcom2"
    equivalence (cpid, epid)
    external ss$_normal, c_conq_conqdriv

    call gsecs( drivtime )			# prevent driver timeout
    drivpid = 0					# zero current driver pid
    drivstat = DRS_RESTART			# driver state to restart

    call dscbld( idsc, c_conq_conqdriv )
    i = epid(1)					# need bottom short word
    call prints( name, "CONQDRIV_%04x", i )
    call upper( name )
    call dscbld( ndsc, name )

    status = sys$creprc( pid, idsc, , , , , , ndsc, %val(cpriority), , , )
    # Check for errors creating the process.
    if ( status == %loc(ss$_normal) )
	childpid = pid				# remember this number
    else
	{
	# We failed this time.
	drivstat = DRS_OFF
	call cerror( MSG_GOD, "drcreate: sys$creprc(), 0x%x", status )
	}

    return

end


###  drkill - make the driver go away if we started it (DOES LOCKING)
#
#  SYNOPSIS
#    call drkill
#
subroutine drkill
NOIMPLICIT

    integer i
    include "conqcom"
    include "conqcom2"

    if ( childpid != 0 )
	if ( childpid == drivpid & drivstat == DRS_RUNNING )
	    {
	    PVLOCK(lockword)
	    if ( childpid == drivpid & drivstat == DRS_RUNNING )
		drivstat = DRS_KAMIKAZE
	    PVUNLOCK(lockword)
	    }

    return

end


###  drpexit - make the driver go away if we started it
#
#  SYNOPSIS
#    call drpexit
#
subroutine drpexit
NOIMPLICIT

    integer i
    include "conqcom"
    include "conqcom2"

    if ( childpid != 0 )
	{
	# We may well have started the driver.
	call drkill
	for ( i = 1; childpid == drivpid & i <= 50; i = i + 1 )
	    call sleep( 0.1 )
	if ( childpid == drivpid )
	    call cerror( MSG_GOD,
		"drpexit(): Driver didn't exit; pid = %08x", childpid )
	}

    return

end


###  drstart - Start a new driver if necessary (DOES LOCKING)
#
#  SYNOPSIS
#    call drstart
#
subroutine drstart
NOIMPLICIT

    include "conqcom"

    if ( drivstat == DRS_OFF )
	{
	PVLOCK(lockword)
	if ( drivstat == DRS_OFF )
	    call drcreate
	PVUNLOCK(lockword)
	}
    return

end


###  gcputime - get cpu time
#
#  SYNOPSIS
#    integer cpu
#    call gcputime( cpu )
#
#  DESCRIPTION
#    The total cpu time (in hundreths) for the current process is returned.
#
subroutine gcputime( cpu )
NOIMPLICIT
integer cpu

    integer status, flag, acpu, lib$get_ef, sys$getjpi, sys$clref, sys$waitfr
    integer*2 jpilst(8)
    external ss$_normal
    data jpilst / 4, JPI$_CPUTIM, 6*0 /
    equivalence (jpilst(3), acpu)

    acpu = %loc(cpu)
    status = sys$getjpi( , , , jpilst, , , )
    if ( status != %loc(ss$_normal) )
	call vmserror( "conqvms$gcputime(): sys$getjpi %s", status )

    return

end


###  helplesson - verbose help
#
#  SYNOPSIS
#    call helplesson
#
subroutine helplesson
NOIMPLICIT

    character buf(MSGMAXLINE)
    external c_conq_helpfile

    call prints( buf, "%s: Can't open.", c_conq_helpfile )
    call pagefile( c_conq_helpfile, buf, .true., .true. )

    return

end


###  initstats - statistics setup
#
#  SYNOPSIS
#    integer ctemp, etemp
#    call initstats( ctemp, etemp )
#
subroutine initstats( ctemp, etemp )
NOIMPLICIT
integer ctemp, etemp

    call gcputime( ctemp )
    call grand( etemp )

    return

end


###  isagod - determine if a user is a god or not
#
#  SYNOPSIS
#    logical flag, isagod
#    character name()
#    flag = isagod( name )
#
#	name - username
#	flag - .true. or .false.
#
logical function isagod( name )
NOIMPLICIT
character name(ARB)

    logical gamtname
    external c_conq_gods

    return ( gamtname( name, c_conq_gods, .false. ) )

end


###  mail - send a one liner mail message (TOOLS mail version)
#
#  SYNOPSIS
#    logical sendok, mail
#    character names(), subject(), msg()
#    sendok = mail( names, subject, msg )
#
# Note: The buffer msg() will contain an error message if .false. is
# returned by this routine.
#
logical function mail( names, subject, msg )
NOIMPLICIT
character names(ARB), subject(ARB), msg(ARB)

    call strcpy( "Mail not available", msg )

    return ( .false. )

end


###  mailimps - send a one liner mail message to the Implementors
#
#  SYNOPSIS
#    logical sendok, mailimps
#    character subject(), msg()
#    sendok = mailimps( subject, msg )
#
#	subject - the subject of the message
#	msg - the message
#	sendok - .true. if the message was sent, else .false.
#
# Note: The buffer msg() will contain an error message if .false. is
# returned by this routine.
#
logical function mailimps( subject, msg )
NOIMPLICIT
character subject(ARB), msg(ARB)

    logical mail
    string mailaddr MAILADDR

    if ( mailaddr(1) == EOS )
	{
	call strcpy( "It is not possible to contact the Implementors.", msg )
	return ( .false. )
	}
    return ( mail( MAILADDR, subject, msg ) )

end


###  news - list current happenings
#
#  SYNOPSIS
#    call news
#
subroutine news
NOIMPLICIT

    external c_conq_newsfile

    call pagefile( c_conq_newsfile, "No news is good news.", .true., .true. )

    return

end


###  settimer - set timer to call display()
#
#  SYNOPSIS
#    call csetimer
#
subroutine settimer
NOIMPLICIT

    integer t(2), status, sys$setimr
    data t / -10000000, -1 /			# one second
    include "conqcom2"
    external ss$_normal, astservice

    status = sys$setimr( %val(ctimflag), t, astservice, ctimid )
    if ( status != %loc(ss$_normal) )
	call cerror( csnum, "Error starting timer, status is 0x%x", status )

    return

end


###  stoptimer - cancel timer
#
#  SYNOPSIS
#    call stoptimer
#
subroutine stoptimer
NOIMPLICIT

    integer status, sys$cantim
    include "conqcom2"
    external ss$_normal

    cdisplay = .false.
    status = sys$cantim( ctimid, )
    if ( status != %loc(ss$_normal) )
	call cerror( csnum, "Error canceling timer, status is 0x%x", status )

    call aston
    # Pending asts will flush here.
    call astoff
    cdisplay = .true.

    return

end


###  upchuck - update the common block to disk.
#
#  SYNOPSIS
#    call upchuck
#
subroutine upchuck
NOIMPLICIT

    integer status, sys$updsec, sys$waitfr, inaddr(2), retaddr(2)
    integer*2 iosb(4)
    external ss$_normal
    include "conqcom"

    inaddr(1) = %loc(closed)			# starting address
    inaddr(2) = %loc(glastmsg) + 4		# last address
    retaddr(1) = 0
    retaddr(2) = 0

    PVLOCK(lockword)
    status = sys$updsec( inaddr, retaddr, , , %val(0), iosb, , )
    if ( status == %loc(ss$_normal) )
	{
	status = sys$waitfr( %val(0) )
	call getdandt( lastupchuck )
	}
    else
	call cerror( MSG_GOD, "upchuck(): sys$updsec(), 0x%x", status )
    PVUNLOCK(lockword)

    return

end


###  upstats - update statistics
#
#  SYNOPSIS
#    integer ctemp, etemp, caccum, eaccum, ctime, etime
#    call upstats( ctemp, etemp, caccum, eaccum, ctime, etime )
#
subroutine upstats( ctemp, etemp, caccum, eaccum, ctime, etime )
NOIMPLICIT
integer ctemp, etemp, caccum, eaccum, ctime, etime

    integer i, j, now, dgrand

    # Update cpu time.
    call gcputime( i )
    caccum = caccum + i - ctemp
    ctemp = i
    if ( caccum > 100 )
	{
	# Accumulated a cpu second.
	ctime = ctime + caccum / 100
	caccum = mod( caccum, 100 )
	}

    # Update elapsed time.
    eaccum = eaccum + dgrand( etemp, now )
    if ( eaccum > 1000 )
	{
	# A second elapsed.
	etemp = now
	etime = etime + eaccum / 1000
	eaccum = mod( eaccum, 1000 )
	}

    return

end
