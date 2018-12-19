# ioplb - machine independent terminal I/O library

# ver  date   who remarks
# --- ------- --- ------------------------------------------------------
# 08a 12May85 cal .Reformatted.
# 07B 08Aug84 cal .Modified iopstr() to use iopbuf().
# 07A 16Jul84 cal .Renamed iomunch() to iofmtstr(), iogetq() to iogquick(),
#                  ioputs() to iopstr(), ioputx() to iopline(), and iogetz()
#                  to iogstr(). Removed iocant() and ioyn(). Rewrote ioerror()
#                  to use fprintf(). Added features to iogstr(). Modified to
#                  not include "ctrlsyms". Added NOIMPLICITs. Trimmed revision
#                  history.
# 06D 25Mar84 vp  .Added iogetz().
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
#    This library contains the portable part of this io package.
#


###  iobeep - (internal) ring the bell
#
# synopsis
#
#    call iobeep
#
# description
#
#    This routine is just for lazy folks.
#
subroutine iobeep
NOIMPLICIT

    call iopchar( '@^g' )

    return

end


###  iocrlf - write out a carriage return-linefeed
#
# synopsis
#
#    call iocrlf
#
# description
#
#    Another routine for lazy folks.
#
subroutine iocrlf
NOIMPLICIT

    call iopchar( '@^m' )
    call iopchar( '@^j' )

    return

end


###  ioeat - swallow any input that has come so far
#
# synopsis
#
#    call ioeat
#
# description
#
#    This routines flushes the type ahead buffer. That is, it reads any
#    characters that are available, but no more.
#
subroutine ioeat
NOIMPLICIT

    logical iochav
    character ch, iogchar

    while ( iochav( 0 ) )
        ch = iogchar( ch )

    return

end


###  ioerror - report a fatal iolb error
#
# synopsis
#
#    character fmt()
#    integer status
#    call ioerror( fmt, status )
#
#	fmt - format specifier string
#	status - value to output using fmt
#
# description
#
#    Outputs an error message using the passed format string and status
#    and then terminates program.
#
subroutine ioerror( fmt, status )
NOIMPLICIT
character fmt(ARB)
integer status

    integer i, length

    call ioend
    call fprintf( ERROUT, fmt, status )
    i = length( fmt )
    if ( i > 0 )
	if ( fmt(i) != '@n' )
	    call putch( '@n', ERROUT )
    call error( "" )

    return

end


###  iofmtstr - format message
#
# synopsis
#
#    character buf()
#    call iofmtstr( buf )
#
# description
#
#    The passed buffer is massaged; carriage returns and line feeds
#    are turned into blanks. Multiple blanks and tabs are compressed.
#    Evil control characters are translated to visible sequences.
#    The length of the string may change length.
#
subroutine iofmtstr( buf )
NOIMPLICIT
character buf(ARB)

    integer i, j, len, length
    character lastc

    len = length( buf )
    lastc = ' '				# this causes loss of leading blanks
    i = 1
    while ( i <= len )
	{
	if ( buf(i) == '@^j' | buf(i) == '@^m' | buf(i) == '@^i' )
	    buf(i) = ' '
	if ( buf(i) == ' ' & lastc == ' ' )
	    {
	    # Skip multiple blanks.
	    # Copy characters back one element (including the EOS).
	    for ( j = i; j <= len; j = j + 1 )
		buf(j) = buf(j+1)
	    len = len - 1
	    next
	    }

	if ( buf(i) < ' ' )
	    {
	    # Handle control characters.
	    # Copy characters forward one element (including the EOS).
	    len = len + 1
	    for ( j = len; j > i; j = j - 1 )
		buf(j+1) = buf(j)
	    buf(i+1) = buf(i) + '@@'
	    buf(i) = '^'
	    i = i + 1
	    }
	else if ( buf(i) == '@^?' )
	    {
	    # Handle delete.
	    # Copy characters forward one element (including the EOS).
	    for ( j = len + 1; j > i; j = j - 1 )
		buf(j+1) = buf(j)
	    len = len + 1
	    buf(i) = '^'
	    i = i + 1
	    buf(i) = '?'
	    }
	lastc = buf(i)
	i = i + 1
	}

    if ( len > 0 )
	if ( buf(len) == ' ' )
	    buf(len) = EOS			# clean up tailing blank

    return

end


###  iogquick - get a character if one is available
#
# synopsis
#
#    logical gotone, iogquick
#    character ch
#    gotone = iogquick( ch )
#
# description
#
#    If a character is available, it is read, placed in "ch" and .true.
#    is returned, otherwise .false. is returned.
#
logical function iogquick( ch )
NOIMPLICIT
character ch

    logical iochav
    character iogchar

    if ( iochav( 0 ) )
        {
        ch = iogchar( ch )
	return ( .true. )
        }

    return ( .false. )

end


###  iogstr - get a string
#
# synopsis
#
#    logical gotsome, iogstr
#    character str()
#    integer maxlen
#    gotsome = iogstr( str, maxlen )
#
# description
#
#    This routine attempts to get a line of input. If it is successful,
#    a line is returned in "str" and .true. is returned. A maximum of
#    "maxlen" minus one characters will be put into "str", although
#    more may have been read. The bell is rung when the user attempts
#    to enter more characters than fit in "str". The returned string
#    is terminated with an EOS character.
#
#    Characters are read up to and including the newline (which is not
#    returned in "str"). The user can erase characters with either
#    delete or backspace. Words may be erased with ^W. The entire input
#    line may be killed with ^U or ^X. Other control characters that are
#    input are echoed as visible sequences. The character ^V escapes the
#    special meaning of any character that follows. Other control
#    characters that are input are echoed as visible sequences.
#
#    If the user attempts to delete more characters than have been input,
#    .false. is returned.
#
logical function iogstr( str, maxlen )
NOIMPLICIT
character str(ARB)
integer maxlen

define(iscontrol,($1<' '|$1=='@^?'))
define(IOESCAPE,'@^v')				# the code allows for this
						# to be either a control or
						# regular character

    integer len
    logical lastescape, thisescape
    character ch, iogchar
    string bsb "@^h @^h"

    len = 0
    thisescape = .false.
    repeat
        {
	lastescape = thisescape
	thisescape = ( iogchar( ch ) == IOESCAPE )
	if ( ! lastescape )
	    {
	    switch ( ch )
		{
		case '@^?', '@^h':
		    # Delete a character.
		    if ( len <= 0 )
			{
			# Tried to delete past beginning of line.
			str(1) = EOS
			return ( .false. )
			}
		    if ( str(len) == ' ' )
			call iopchar( '@^h' )
		    else
			{
			call iopstr( bsb )
			if ( iscontrol( str(len) ) )
			    call iopstr( bsb )
			}
		    len = len - 1
		    next
		case '@^u', '@^x':
		    # Kill line.
		    for ( ; len > 0; len = len - 1 )
			if ( str(len) == ' ' )
			    call iopchar( '@^h' )
			else
			    {
			    call iopstr( bsb )
			    if ( iscontrol( str(len) ) )
				call iopstr( bsb )
			    }
		    next
		case '@^w':
		    # Delete word; first, back up over blanks, then non-blanks.
		    for ( ; len > 0 & str(len) == ' '; len = len - 1 )
			call iopchar( '@^h' )
		    for ( ; len > 0 & str(len) != ' '; len = len - 1 )
			{
			call iopstr( bsb )
			if ( iscontrol( str(len) ) )
			    call iopstr( bsb )
			}
		    next
		case '@^m', '@^j':
		    # All done.
		    str(len+1) = EOS
		    break
		}
	    }

	# We got here if the character didn't find a case in the switch.
	# See if it fits in the buffer.
	if ( len + 1 < maxlen )
	    {
	    if ( ! thisescape | lastescape )
		{
		len = len + 1
		str(len) = ch
		}
	    if ( iscontrol( ch ) )
		{
		# Processing a control character.
		if ( lastescape )
		    {
		    call iopchar( '@^h' )
		    if ( ! iscontrol( IOESCAPE ) )
			call iopchar( '^' )
		    }
		else
		    call iopchar( '^' )

		if ( ch == '@^?' )
		    call iopchar( '?' )
		else
		    call iopchar( ch + '@@' )
		}
	    else
		{
		# Processing a non-control character.
		if ( lastescape )
		    {
		    if ( iscontrol( IOESCAPE ) )
			call iopstr( bsb )
		    call iopchar( '@^h' )
		    }
		call iopchar( ch )
		}
	    }
	else
	    call iobeep

	# Escaped character logic.
	if ( lastescape )
	    thisescape = .false.
        }

    return ( .true. )

end


###  iopline - write a string, translate newlines into carriage return-linefeed
#
# synopsis
#
#    character str()
#    call iopline( str )
#
# description
#
#    Characters, up to the EOS, are written out. Newlines are translated into
#    carriage return-linefeed.
#
subroutine iopline( str )
NOIMPLICIT
character str(ARB)

    integer i

    for ( i = 1; str(i) != EOS; i = i + 1 )
        if ( str(i) == '@n' )
            call iocrlf
        else
            call iopchar( str(i) )

    return

end


###  iopstr - write a string
#
# synopsis
#
#    character str()
#    call iopstr( str )
#
# description
#
#    Characters, up to the EOS, are written out.
#
subroutine iopstr( str )
NOIMPLICIT
character str(ARB)

    integer length

    call iopbuf( length( str ), str )

    return

end


###  iospaces - write out a number of spaces
#
# synopsis
#
#    integer n
#    call iospaces( n )
#
# description
#
#    A routine for slightly lazy folks. Outputs "n" spaces.
#
subroutine iospaces( n )
NOIMPLICIT
integer n

    integer i

    for ( i = n; i > 0; i = i - 1 )
        call iopchar( ' ' )

    return

end
