# ionlb - VMS/VMS machine dependent terminal I/O library

# ver  date   who remarks
# --- ------- --- ------------------------------------------------------
# 09a 04Mar85 cal .Upgraded to VMS V4.
#                  Reformatted.
#                  Modified to use pasthru.
#                  Improved terminal/mailbox broadcast code.
#                  Added iovmserror().
#                  Added exit handler for ioend()
# 08d 14Jun85 cal .Fixed data overrun bug in iogtimed().
# 08c 17Sep84 cal .Modified to not include "tslb". Renamed include from
#                  "ionlb.c" to "ioncom".
# 08b 08Aug84 cal .Added iopbuf().
# 08a 16Jul84 cal .Renamed ioget() to iogchar(), iotget() to iogtimed(), and
#                  ioput() to iopchar(). Removed iomxname() and made code
#                  inline to ioinit(). Removed iobuffered(). Moved iofmtstr()
#                  to ioplb. Turned iogtimed() into a logical function and
#                  added a time limit argument. Trimmed revision history.
# 07i 22Jun84 cal
#              &
#             vp  .Added iopeek().
# 07h 22May84 cal .Modified iogbroadcast() to strip NULLs from the received
#                  terminal broadcast message.
#
# description
#
#    Iolb is designed to provide raw-mode I/O to and from the user's
#    terminal. Raw-mode I/O is the simplest thing possible: when the user
#    types a character, it is read by the program; when the program writes
#    a character, it is sent to the terminal. No line-by-line buffering,
#    no converting tabs to spaces, spaces to tabs, lower case to upper and
#    upper to lower, no ignoring some characters and intercepting others...
#    nothing but simple read-a-character, write-a-character. Is that asking
#    too much?
#
#    Apparently. On many computer systems in use today, it is IMPOSSIBLE
#    to implement raw I/O. On most systems, it is very difficult, it usually
#    requires some assembly language hacking, and is usually inefficient.
#    This annoying situation is not really anybody's fault. When today's
#    systems were being designed, the phrases "real-time", "screen oriented"
#    and "interactive" did not yet have any referents. We can only hope that
#    the systems unable to be adapted will die out from lack of use.
#
#    The low-level routines in this package (ioinit(), ioend(), iochav(),
#    iogchar(), and iopchar()) are system-dependent of course, but the rest
#    should be quite portable.
#
#    This library contains the non-portable low level part of this package.
#

# Random defines
define(MAXOBUFSIZE,1000)	# maximum size of output buffer (.5sec@19.2Kb)
define(MXSIZE,200)		# size of mailbox
define(MXMUL,5)			# number of message outstanding
define(MXPROT,0)		# protection mask for mailbox

# Terminal mailbox message offsets
define(MSG_W_TYPE,1)		# location of message type
define(MSG_W_UNIT,2)		# location of unit number of sender
define(MSG_B_DEVCNT,5)		# location of size of sender device name
define(MSG_S_DEVNAM,6)		# location of sender device name string
define(MSG_W_MSGCNT,21)		# location of size of broadcast message
define(MSG_S_MSGBUF,23)		# location of broadcast message string


### ioautobroad - examine and set the automatic broadcast mode
#
# synopsis
#
#    logical oldmode, newmode, ioautobroad
#    oldmode = ioautobroad( newmode )
#
# description
#
#    This routine allows the user to examine and change the automatic
#    broadcast mode. As a side effect, the terminal modes are changed.
#    If the user initially had terminal broadcasts disabled, then we
#    make sure that they as well as mailbox broadcasts are disabled.
#    Otherwise, we enable terminal broadcasts and diable mailbox
#    broadcasts or visa versa. In all cases, we set the terminal modes.
#    This allows use of this routine from ioinit() for the initial
#    broadcast setup.
#
logical function ioautobroad( auto )
NOIMPLICIT
logical auto

    integer status, sys$qiow
    logical tautobroad
    include "ioncom"
    external ss$_normal, io$_setmode, tt$m_nobrdcst, tt2$m_brdcstmbx

    tautobroad = autobroad			# save old setting
    autobroad = auto

    if ( nobroadcast )
	{
	newchar(2) = newchar(2) | %loc(tt$m_nobrdcst)
	newchar(3) = newchar(3) & ! %loc(tt2$m_brdcstmbx)
	}
    else if ( autobroad )
	{
	newchar(2) = newchar(2) & ! %loc(tt$m_nobrdcst)
	newchar(3) = newchar(3) & ! %loc(tt2$m_brdcstmbx)
	}
    else
	{
	newchar(2) = newchar(2) | %loc(tt$m_nobrdcst)
	newchar(3) = newchar(3) | %loc(tt2$m_brdcstmbx)
	}

    # Set modes.
    status = sys$qiow( , %val(chan), %val(%loc(io$_setmode)), , , ,
	newchar, %val(12), , , , )
    if ( status != %loc(ss$_normal) )
	call iovmserror( "iolb$ioautobroad: sys$qiow(), %s", status )

    return ( tautobroad )

end


### iochav - test whether a character is available to be read or not
#
# synopsis
#
#    logical avail, iochav
#    avail = iochav( 0 )
#
# note
#
#    This routine does \not/ call iopeek() because of speed considerations.
#    The two routines are meant to virtually be copies of one another and
#    should be modified in parallel.
#
logical function iochav( dummy )
NOIMPLICIT
integer dummy

    integer status, sys$qiow
    integer*2 mode(4)
    external io$_sensemode, io$m_typeahdcnt, ss$_normal
    include "ioncom"

    status = sys$qiow( %val(ievfl), %val(chan),
	%val(%loc(io$_sensemode)|%loc(io$m_typeahdcnt) ), , , ,
	mode, , , , , )
    if ( status != %loc(ss$_normal) )
	call iovmserror( "iolb$iochav: sys$qiow(), %s", status )

    return ( mode(1) != 0 )

end


### ioend - terminate the terminal I/O package
#
# synopsis
#
#    call ioend
#
subroutine ioend
NOIMPLICIT

    integer sys$qiow, status, sys$canexh, sys$dassgn, lib$free_ef
    byte iosb(8)
    external ss$_normal, ss$_nohandler, io$_setmode
    include "ioncom"
    
    # Cancel the exit handler.
    status = sys$canexh( desblk )
    if ( status != %loc(ss$_normal) & status != %loc(ss$_nohandler) )
	call iovmserror( "iolb$ioend: sys$canexh(), %s", status )

    # The channel can be zero only if there is no cleanup necessary.
    if ( chan == 0 )
	return

    # Flush any remaining messages.
    call iomsgflush

    # Destroy the broadcast mailbox.
    call iomxkill

    # Flush any remaining output.
    call ioflush

    # Reset terminal modes.
    status = sys$qiow( , %val(chan), %val(%loc(io$_setmode)), iosb, , ,
	oldchar, %val(12), , , , )
    if ( status != %loc(ss$_normal) )
	call iovmserror( "iolb$ioend: sys$qiow(), %s", status )

    # De-assign the terminal channel.
    status = sys$dassgn( %val(chan) )
    if ( status != %loc(ss$_normal) )
	call iovmserror( "iolb$ioend: sys$dassgn(), %s", status )

    # Deallocate event flags.
    if ( lib$free_ef( ievfl ) != %loc( ss$_normal ) |		# input flag
	 lib$free_ef( cevfl ) != %loc( ss$_normal ) |		# iogchar() flag
	 lib$free_ef( oevfl ) != %loc( ss$_normal ) |		# output flag
	 lib$free_ef( mevfl ) != %loc( ss$_normal ) )		# mailbox flag
	call error( "iolb$ioend: lib$free_ef() failed" )

    # End of cleanup; zero channel number.
    chan = 0

    return

end


### ioflush - flush the output buffer
#
# synopsis
#
#    call ioflush
#
subroutine ioflush
NOIMPLICIT

    integer status, sys$qiow
    include "ioncom"
    external ss$_normal, io$_writelblk

    status = sys$qiow( %val(oevfl), %val(chan),
	%val(%loc(io$_writelblk)), , , , obuf, %val(obufp), , %val(0), , )
    if ( status != %loc(ss$_normal) )
	call iovmserror( "iolb$ioflush: sys$qiow(), %s", status )
    obufp = 0

    return

end


### ioinit - initialize the terminal I/O routines
#
# synopsis
#
#    call ioinit
#
# description
#
#    This routine must be called before any others in iolb. It does
#    the mondo-initialization required for a library such high quality.
#
#    Of course, the side effects are massive.
#
subroutine ioinit
NOIMPLICIT

    integer i, status, estatus, cps, tdsc(2), mxdsc(2)
    integer sys$crembx, sys$assign, sys$qiow, sys$dclexh, lib$get_ef
    integer*2 pid(2)
    byte iosb(8)
    logical ljunk, ioautobroad
    character tname(FILENAMESIZE), mxname(FILENAMESIZE)
    include "ioncom"
    external ss$_normal, ss$_bufferovf, io$_sensemode, tt$m_noecho
    external tt$m_mbxdsabl, tt$m_ttsync, tt$m_nobrdcst, tt2$m_xon
    external tt2$m_pasthru, ioend
    external tt$c_baud_50, tt$c_baud_75, tt$c_baud_300, tt$c_baud_110
    external tt$c_baud_134, tt$c_baud_150, tt$c_baud_600, tt$c_baud_1200
    external tt$c_baud_1800, tt$c_baud_2000, tt$c_baud_2400, tt$c_baud_3600
    external tt$c_baud_4800, tt$c_baud_7200, tt$c_baud_9600, tt$c_baud_19200

    # Allocate necessary event flags.
    if ( lib$get_ef( ievfl ) != %loc( ss$_normal ) |	# input flag
	 lib$get_ef( cevfl ) != %loc( ss$_normal ) |	# iogchar() flag
	 lib$get_ef( oevfl ) != %loc( ss$_normal ) |	# output flag
	 lib$get_ef( mevfl ) != %loc( ss$_normal ) )	# mailbox flag
	call error( "iolb$ioinit: lib$get_ef() failed" )

    # Generate a unique mailbox name and create it.
    call getpid( pid )
    i = pid(1)					# need bottom short word
    call prints( mxname, "%04x_IOLB", i )
    call upper( mxname )
    call dscbld( mxdsc, mxname )
    status = sys$crembx( , mxchan, %val(MXSIZE), %val(MXSIZE*MXMUL),
	%val(MXPROT), , mxdsc )
    if ( status != %loc(ss$_normal) )
	call iovmserror( "iolb$ioinit: sys$crembx(), %s", status )

    # Assign a channel to the terminal.
    call termin( tname )
    call upper( tname )
    call dscbld( tdsc, tname )
    status = sys$assign( tdsc, chan, , mxdsc )
    if ( status != %loc(ss$_normal) )
	{
	chan = 0				# disable ioend() cleanup
	call iovmserror( "iolb$ioinit: sys$assign(), %s", status )
	}

    # Get modes.
    status = sys$qiow( , %val(chan), %val(%loc(io$_sensemode)), iosb, , ,
	oldchar, %val(12), , , , )
    if ( status != %loc(ss$_normal) )
	{
	chan = 0				# disable ioend() cleanup
	call iovmserror( "iolb$ioinit: sys$qiow(), %s", status )
	}

    # Figure out if the user wants broadcasts.
    nobroadcast = ( ( oldchar(2) & %loc(tt$m_nobrdcst) ) != 0 )

    # Construct the new characteristics. We always turn off echo, disable
    # unsolicited input or hangup mailbox messages, turn off ttsync, clear the
    # xon state (in case the terminal is the ^S state), and turn on pasthru.
    newchar(1) = oldchar(1)
    newchar(2) = oldchar(2) | %loc(tt$m_noecho) | %loc(tt$m_mbxdsabl)
    newchar(2) = newchar(2) & ! %loc(tt$m_ttsync)
    newchar(3) = oldchar(3) | %loc(tt2$m_xon) | %loc(tt2$m_pasthru)
    
    # Handle broadcast characteristics (ioautobroad() always does a set mode)
    autobroad = .false.			# initialize to something meaningful
    ljunk = ioautobroad( .true. )	# default to autobroad on

    # Initialize output buffer.
    if ( iosb(3) == %loc(tt$c_baud_50) )
	cps = 5
    else if ( iosb(3) == %loc(tt$c_baud_75) )
	cps = 8
    else if ( iosb(3) == %loc(tt$c_baud_110) )
	cps = 10
    else if ( iosb(3) == %loc(tt$c_baud_134) )
	cps = 13
    else if ( iosb(3) == %loc(tt$c_baud_150) )
	cps = 15
    else if ( iosb(3) == %loc(tt$c_baud_300) )
	cps = 30
    else if ( iosb(3) == %loc(tt$c_baud_600) )
	cps = 60
    else if ( iosb(3) == %loc(tt$c_baud_1200) )
	cps = 120
    else if ( iosb(3) == %loc(tt$c_baud_1800) )
	cps = 180
    else if ( iosb(3) == %loc(tt$c_baud_2000) )
	cps = 200
    else if ( iosb(3) == %loc(tt$c_baud_2400) )
	cps = 240
    else if ( iosb(3) == %loc(tt$c_baud_3600) )
	cps = 360
    else if ( iosb(3) == %loc(tt$c_baud_4800) )
	cps = 480
    else if ( iosb(3) == %loc(tt$c_baud_7200) )
	cps = 720
    else if ( iosb(3) == %loc(tt$c_baud_9600) )
	cps = 960
    else if ( iosb(3) == %loc(tt$c_baud_19200) )
	cps = 1920
    else
	cps = 400		# default
    obufsize = min0 ( cps / 2, MAXOBUFSIZE )
    obufp = 0

    # Initialize terminal broadcast mailbox read.
    call iomxread

    # Set up an exit handler to cleanup if things go astray.
    desblk(1) = 0				# forward link
    desblk(2) = %loc(ioend)			# address of exit handler
    desblk(3) = 0				# number of arguments
    desblk(4) = %loc(estatus)			# address of exit status
    status = sys$dclexh( desblk )
    if ( status != %loc(ss$_normal) )
	call iovmserror( "iolb$ioinit: sys$dclexh(), %s", status )

    return

end


### iopeek - test whether a character is available and return it if present
#
# synopsis
#
#    logical avail, iopeek
#    character ch
#    avail = iopeek( ch )
#
# note
#
#    Iochav() does \not/ call this routine because of speed considerations.
#    The two routines are meant to virtually be copies of one another and
#    should be modified in parallel.
#
logical function iopeek( ch )
NOIMPLICIT
character ch

    integer status, sys$qiow
    logical gotit
    integer*2 mode
    character tbufinfo(8)
    equivalence (mode, tbufinfo)
    external io$_sensemode, io$m_typeahdcnt, ss$_normal
    include "ioncom"

    status = sys$qiow( %val(ievfl), %val(chan),
	%val( %loc(io$_sensemode) + %loc(io$m_typeahdcnt) ), , , ,
	mode, , , , , )
    if ( status != %loc(ss$_normal) )
	call iovmserror( "iolb$iopeek: sys$qiow(), %s", status )

    gotit = ( mode != 0 )

    if ( gotit )
	ch = tbufinfo(3)

    return ( gotit )

end


### iopbuf - write out a number of characters
#
# synopsis
#
#    integer n
#    character buf()
#    call iopbuf( n, buf )
#
subroutine iopbuf( n, buf )
NOIMPLICIT
integer n
character buf(ARB)

    integer i
    include "ioncom"

    i = 0
    while ( i < n )
	{
	obufp = obufp + 1
	i = i + 1
	obuf(obufp) = buf(i)
	if ( obufp >= obufsize )
	    call ioflush
	}

    return

end


### iopchar - write a character out to the terminal
#
# synopsis
#
#    character ch
#    call iopchar( ch )
#
subroutine iopchar( ch )
NOIMPLICIT
character ch

    include "ioncom"

    obufp = obufp + 1
    obuf(obufp) = ch
    if ( obufp >= obufsize )
	call ioflush

    return

end


### iogbroadcast - get a broadcast message if there is one
#
# synopsis
#
#    logical gotone, iogbroadcast
#    character buf()
#    gotone = iogbroadcast( buf )
#
# description
#
#    On the VAX, there is an esoteric way to capture terminal broadcast
#    messages in a mailbox and make then syncronous with your program.
#
#    This is tubular.
#
logical function iogbroadcast( buf )
NOIMPLICIT
character buf(ARB)

    integer i, j, k
    integer*2 mxtype, mxmsgcnt, mxunit
    character mxdevnam(MSG_W_MSGCNT-MSG_S_DEVNAM-1)
    character mxmsgbuf(MXSIZE-MSG_S_MSGBUF-1)
    byte mxdevcnt
    equivalence (mxbuf(MSG_W_TYPE), mxtype)
    equivalence (mxbuf(MSG_W_UNIT), mxunit)
    equivalence (mxbuf(MSG_B_DEVCNT), mxdevcnt)
    equivalence (mxbuf(MSG_S_DEVNAM), mxdevnam)
    equivalence (mxbuf(MSG_W_MSGCNT), mxmsgcnt)
    equivalence (mxbuf(MSG_S_MSGBUF), mxmsgbuf)
    external ss$_normal, msg$_trmbrdcst
    include "ioncom"

    # See if a message has arrived yet.
    if ( mxiosb(1) == 0 )
	{
	buf(1) = EOS
	return ( .false. )
	}

    # Check for weirdness.
    i = mxiosb(1)
    if ( i != %loc(ss$_normal) )
	call iovmserror( "ionlb$iogbroadcast: sys$qio(), %s", i )

    # Should only get terminal broadcast messages.
    i = mxtype
    if ( i != %loc(msg$_trmbrdcst) )
	call ioerror( "ionlb$iogbroadcast: Bad message type, 0x%x", i )

    # The system broadcast service is really quite stupid. If you
    # use it with default carriage control, it can put ascii NULLs
    # into the broadcast buffer. Copy the message, skipping NULLs.
    i = 1
    k = mxmsgcnt
    j = 1
    while ( i <= k )
	{
	if ( mxmsgbuf(i) != 0 )
	    {
	    buf(j) = mxmsgbuf(i)
	    j = j + 1
	    }
	i = i + 1
	}
    buf(j) = EOS
    call iomxread				# post another mailbox read

    return ( .true. )

end


### iogchar - get a character
#
# synopsis
#
#    character ch, iogchar
#    ch = iogchar( ch )
#
character function iogchar( ch )
NOIMPLICIT
character ch

    integer status, sys$qiow
    external io$_readlblk, ss$_normal
    include "ioncom"

    # This is a good place to flush the output buffer.
    call ioflush

##    call sys$setast(%val(1))		# enable asts
    # iogchar() and iogtimed() use separate event flags...
    status = sys$qiow( %val(cevfl), %val(chan), %val(%loc(io$_readlblk)),
	, , , ch, %val(1), , %val(0), , )
##    call sys$setast(%val(0))		# disable asts
    if ( status != %loc(ss$_normal) )
	call iovmserror( "iolb$iogchar: sys$qiow(), %s", status )

    return ( ch )

end


### iogtimed - get a character with timeout
#
# synopsis
#
#    logical gotone, iogtimed
#    character ch
#    integer seconds
#    gotone = iogtimed( ch, seconds )
#
logical function iogtimed( ch, seconds )
NOIMPLICIT
character ch
integer seconds

    integer status, sys$qiow
    integer*2 iosb(4)
    external io$_readlblk, io$m_timed, ss$_normal, ss$_timeout, ss$_dataoverun
    include "ioncom"

    # This is a good place to flush the output buffer.
    call ioflush

##    call sys$setast(%val(1))		# enable asts
    # iogchar() and iogtimed() use separate event flags...
    status = sys$qiow( %val(cevfl), %val(chan),
	%val(%loc(io$_readlblk) + %loc(io$m_timed)), iosb, , ,
	ch, %val(1), %val(seconds), %val(0), , )
##    call sys$setast(%val(0))		# disable asts

    if ( status == %loc(ss$_normal) )
	status = iosb(1)
    if ( status == %loc(ss$_timeout) | status == %loc(ss$_dataoverun) )
	return ( .false. )
    if ( status != %loc(ss$_normal) )
	call iovmserror( "iolb$iogtimed: sys$qiow(), %s", status )

    return ( .true. )

end


### iomsgflush - (internal) flush any broadcast messages
#
# synopsis
#
#    call iomsgflush
#
subroutine iomsgflush
NOIMPLICIT

    logical iogbroadcast
    character buf(MAXLINE)

    while ( iogbroadcast( buf ) )
	call iopstr( buf )

    return

end


### iomxkill - (internal) deassign the broadcast mailbox
#
# synopsis
#
#    call iomxkill
#
subroutine iomxkill
NOIMPLICIT

    integer status, sys$dassgn
    external ss$_normal
    include "ioncom"

    status = sys$dassgn( %val(mxchan) )
    if ( status != %loc(ss$_normal) )
	call iovmserror( "iolb$iomxkill: sys$dassgn(), %s", status )

    return

end


### iomxread - (internal) set up for a read from the mailbox
#
# synopsis
#
#    call iomxread
#
subroutine iomxread
NOIMPLICIT

    integer status, sys$qio
    external ss$_normal, io$_readvblk
    include "ioncom"

    mxiosb(1) = 0			# clear now, poll later...
    status = sys$qio( %val(mevfl), %val(mxchan),
	%val(%loc(io$_readvblk)),
	mxiosb, , , mxbuf, %val(MXSIZE), , , , )
    if ( status != %loc(ss$_normal) )
	call iovmserror( "iolb$iomxread: sys$qio(), %s", status )

    return

end


### iovmserror - (internal) report fatal VMS iolb error
#
# synopsis
#
#    character str()
#    integer status
#    call iovmserror( fmt, status )
#
#	fmt - format specifier containing "%s"
#	status - VMS status return
#
subroutine iovmserror( fmt, status )
NOIMPLICIT
character fmt(ARB)
integer status

    call ioend
    call vmserror( fmt, status )

    return					# not reached

end
