###############################################################################
#
#                              C D 2 L B
#                       Craig's Display Library
#
###############################################################################
#
# Modification History
# ------------ -------
# 03m 11Sep84 cal .Renamed include files "cdlb" and "cdlb.c" to "cddef" and
#                  "cdcom". Removed some unused variables.
# 03l 16Jul84 cal .Modified to use new iolb routine names.
# 03k 08Jun84 cal .Changed cdend() to position the cursor.
# 03j 20Apr84 cal .Added requirements of minimum terminal capabilities to
#                  cdinit().
# 03i 11Mar84 cal .Fixed bug in cdgetp() that precluded clear to end of line
#                  from being used.
# 03h 22Feb84 cal .Added cdgetp() which gets a string with a default and
#                  modified cdgetx() to use it.
# 03g 14Dec83 cal .Modified cdclra() to accept points in any order. Removed
#                  definition of NOIMPLICIT. Removed several unused variables.
# 03f 23Sep83 cal .Added a portable version of cdfill() and cdmovtc() and
#                  removed fast macro conditional code. Changed cdinit() to
#                  not call cdmaketc(). Added cdbox(). Changed to use
#                  scdisp(). Modified cdgetx() to accept carriage return as
#                  a valid terminator if no terminators are specified.
#                  Speeded up loop in cdplay(). Modified cdline() to accept
#                  points in any order.
# 03e 10Sep83 cal .Modified cdplay() to keep track of the last line it
#                  updated. Renamed fillch(), movtchs(), and maktbl()
#                  to cdfill(), cdmovtc(), and cdmaketc(). Removed cdget(),
#                  cdgetk(), and cdpeek() since they duplicate scget(),
#                  scgetk(), scpeek(). Removed cdlredo() since cdlb is not
#                  for hardcopy terminals. Reformatted routine headers.
# 03d 05Sep83 cal .Created cdsetfont(), which contains some code moved
#                  out from cdcput(). Modified all routines that deal with
#                  fonts to always treat them as characters.
# 03c 14Jul83 cal .Ratfix.
# 03b 10Jul83 cal .Modified to not use cursor right, but rather put
#                  out the character that is really on the screen.
# 03a 29May83 cal .Rewritten to go like hell
#
###############################################################################


include "sclb"
include "sclb.i"
include "cddef"

define(MSGMAXLINE,90)			# used for screen formatting
define(NORMAL_FONT,0)			# normal font (must be 0)
#define(DEBUG_CDLB,)			# uncomment this to get debug code

# abbreviations
define(T,.true.)
define(F,.false.)

# Macro to calculate relative cursor movement costs.
#
#    cost = cdcrel(olin, ocol, nlin, ncol)
#    ( colcost(ncol-ocol) + lincost(nlin-olin) )
#
define(cdcrel,(colcost($4-$2)+lincost($3-$1)))

# Special characters (for building boxes).
define(LSCH_MIN,1)
define(LSCH_MAX,15)

define(LSCH_D,1)
define(LSCH_L,2)
define(LSCH_LD,3)
define(LSCH_U,4)
define(LSCH_UD,5)
define(LSCH_UL,6)
define(LSCH_ULD,7)
define(LSCH_R,8)
define(LSCH_RD,9)
define(LSCH_RL,10)
define(LSCH_RLD,11)
define(LSCH_RU,12)
define(LSCH_RUD,13)
define(LSCH_RUL,14)
define(LSCH_RULD,15)


### cdacel - clear to end of line (internal)
#
# synopsis
#
#    integer lin, col
#    call cdacel ( lin, col )
#
#	lin - line number
#	col - column number
#
subroutine cdacel ( lin, col )
NOIMPLICIT
integer lin, col

    integer j

    include "cdcom"

    # Actually clear to end of line.
    call cdgoto ( lin, col )
    call sccel

    # Fix up internal image.
    j = maxcol - col + 1				# number of characters
    call cdfill ( ' ', aimage(col,lin), j )
    call cdfill ( NORMAL_FONT, afont(col,lin), j )

    changed(lin) = .true.
    alinend(lin) = 0
    firstcol(lin) = min0 ( firstcol(lin), col )

    return

end


### cdandfont - make font containing anything in both of two fonts
#
# synopsis
#
#    character font, cdandfont, font1, font2
#    font = cdandfont ( font1, font2 )
#
#	font1 - a font
#	font2 - a font
#	font - returned font
#
character function cdandfont ( font1, font2 )
NOIMPLICIT
character font1, font2

    return ( font1 & font2 )

end


### cdaput - put character on actual image (internal)
#
# synopsis
#
#    character ch, f
#    integer lin, col
#    call cdaput ( ch, f, lin, col )
#
#	ch - character to output
#	f - font for character
#	lin - line number
#	col - line column
#
subroutine cdaput ( ch, f, lin, col )
NOIMPLICIT
character ch, f
integer lin, col

    include "cdcom"
    include "sclb.c"

    # Move cursor.
    call cdgoto ( lin, col )

    # Put the character out.
    call cdcput ( ch, f )

    # Update image of screen.
    aimage(col,lin) = ch
    afont(col,lin) = f
    changed(lin) = .true.
    alinend(lin) = max0 ( alinend(lin), col )
    firstcol(lin) = min0 ( firstcol(lin), col )

    # Compute new cursor position.
    if ( acol < maxcol )
	acol = acol + 1
    else
	{
	# At last character of line.
	if ( scautonl )
	    {
	    acol = 1
	    alin = alin + 1
	    }
	}

    return

end


### cdbox - draw a box
#
# synopsis
#
#    integer lin1, col1, lin2, col2
#    call cdbox ( lin1, col1, lin2, col2 )
#
#	lin1 - first line number
#	col1 - first column number
#	lin2 - last line number
#	col2 - last column number
#
# description
#
#    This routine draws a box defined by the two points.
#
subroutine cdbox ( lin1, col1, lin2, col2 )
NOIMPLICIT
integer lin1, col1, lin2, col2

    call cdline ( lin1, col1, lin1, col2 )	# right
    call cdline ( lin1, col2, lin2, col2 )	# down
    call cdline ( lin2, col2, lin2, col1 )	# left
    call cdline ( lin2, col1, lin1, col1 )	# up

    return

end


### cdcput - output a character (internal)
#
# synopsis
#
#    character ch, f
#    call cdcput ( ch, f )
#
#	ch - character to output
#       f - font for character
#
subroutine cdcput ( ch, f )
NOIMPLICIT
character ch, f

    include "cdcom"

    # Set up correct font.
    if ( f != afontc )
	call cdsetfont ( f )

    # Put character on screen.
    switch ( ch )
	{
	case LSCH_D:
	    call scgraphics ( LSEG_D )
	case LSCH_L:
	    call scgraphics ( LSEG_L )
	case LSCH_LD:
	    call scgraphics ( LSEG_LD )
	case LSCH_U:
	    call scgraphics ( LSEG_U )
	case LSCH_UD:
	    call scgraphics ( LSEG_UD )
	case LSCH_UL:
	    call scgraphics ( LSEG_UL )
	case LSCH_ULD:
	    call scgraphics ( LSEG_ULD )
	case LSCH_R:
	    call scgraphics ( LSEG_R )
	case LSCH_RD:
	    call scgraphics ( LSEG_RD )
	case LSCH_RL:
	    call scgraphics ( LSEG_RL )
	case LSCH_RLD:
	    call scgraphics ( LSEG_RLD )
	case LSCH_RU:
	    call scgraphics ( LSEG_RU )
	case LSCH_RUD:
	    call scgraphics ( LSEG_RUD )
	case LSCH_RUL:
	    call scgraphics ( LSEG_RUL )
	case LSCH_RULD:
	    call scgraphics ( LSEG_RULD )
	case CDLB_PILLOW:
	    # Pillow character.
	    call scpillow
	default:
	    # Put normal character on screen.
	    call scput ( ch )
	}

    return

end


### cdclear - clear the desired image
#
# synopsis
#
#    call cdclear
#
subroutine cdclear
NOIMPLICIT

    include "cdcom"

    # Clear the desired image.
    call cdclra ( 1, 1, maxlin, maxcol )

    # Force normal font.
    call cdsetfont ( NORMAL_FONT )

    return

end


### cdclra - clear rectangular area in desired image
#
# synopsis
#
#    integer l1, c1, l2, c2
#    call cdclra ( l1, c1, l2, c2 )
#
#	l1 - first line to clear
#	c1 - first column to clear
#	l2 - last line to clear
#	c2 - last column to clear
#
# description
#
#    This routine clears (to blanks in the normal font) all characters
#    in the desired image contained within the rectangle defined by
#    the two points.
#
subroutine cdclra ( l1, c1, l2, c2 )
NOIMPLICIT
integer l1, c1, l2, c2

    integer i, j, rfc, rlc, rfl, rll

    include "cdcom"

    rfc = max0 ( 1, min0 ( c1, c2 ) )
    rlc = min0 ( maxcol, max0 ( c1, c2 ) )

    # Calculate length.
    j = rlc - rfc + 1

    rfl = max0 ( 1, min0 ( l1, l2 ) )
    rll = min0 ( maxlin, max0 ( l1, l2 ) )

    for ( i = rfl ; i <= rll ; i = i + 1 )
	{
	call cdfill ( ' ', dimage(rfc,i), j )
	call cdfill ( NORMAL_FONT, dfont(rfc,i), j )
	changed(i) = .true.

	# Update beginning and end of line info.
	if ( rlc >= dlinend(i) )
	    dlinend(i) = min0 ( dlinend(i), rfc - 1 )
	firstcol(i) = min0 ( firstcol(i), rfc )
	}

    return

end


### cdclrl - clear lines in desired image
#
# synopsis
#
#    integer f, n
#    call cdclrl ( f, n )
#
#	f - the first line to clear
#	n - the number of lines to clear
#
subroutine cdclrl ( f, n )
NOIMPLICIT
integer f, n

    integer i

    include "cdcom"

    i = f + n - 1
    if ( i > 0 )
	call cdclra ( f, 1, i, maxcol )

    return

end


### cdcols - return the number of columns for this terminal
#
# synopsis
#
#    integer cols, cdcols
#    cols = cdcols ( 0 )
#
#	cols - number of columns
#
integer function cdcols ( dummy )
NOIMPLICIT
integer dummy

    include "cdcom"

    return ( maxcol )

end


### cddefont - decode font character (internal)
#
# synopsis
#
#     character f
#     logical att(MAX_ATTRIBUTES)
#     call cddefont ( f, att )
#
#	f - font character
#	att - character attributes vector
#
subroutine cddefont ( f, att )
NOIMPLICIT
character f
logical att(MAX_ATTRIBUTES)

    integer i

    logical ht ( 64 )
    logical ut ( 64 )
    logical rt ( 64 )
    logical bt ( 64 )
    logical gt ( 64 )
    logical at ( 64 )

    data ht /F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,
             F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T/
    data ut /F,F,T,T,F,F,T,T,F,F,T,T,F,F,T,T,F,F,T,T,F,F,T,T,F,F,T,T,F,F,T,T,
             F,F,T,T,F,F,T,T,F,F,T,T,F,F,T,T,F,F,T,T,F,F,T,T,F,F,T,T,F,F,T,T/
    data rt /F,F,F,F,T,T,T,T,F,F,F,F,T,T,T,T,F,F,F,F,T,T,T,T,F,F,F,F,T,T,T,T,
             F,F,F,F,T,T,T,T,F,F,F,F,T,T,T,T,F,F,F,F,T,T,T,T,F,F,F,F,T,T,T,T/
    data bt /F,F,F,F,F,F,F,F,T,T,T,T,T,T,T,T,F,F,F,F,F,F,F,F,T,T,T,T,T,T,T,T,
             F,F,F,F,F,F,F,F,T,T,T,T,T,T,T,T,F,F,F,F,F,F,F,F,T,T,T,T,T,T,T,T/
    data gt /F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,
             F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T/
    data at /F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,
             T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T/


    i = f + 1

    att ( ATTRIBUTE_HIGHLIGHT ) = ht ( i )
    att ( ATTRIBUTE_UNDERLINE ) = ut ( i )
    att ( ATTRIBUTE_REVERSE ) = rt ( i )
    att ( ATTRIBUTE_BLINK ) = bt ( i )
    att ( ATTRIBUTE_GRAPHICS ) = gt ( i )
    att ( ATTRIBUTE_ALTERNATE ) = at ( i )

    return

end


### cdend - end display environment
#
# synopsis
#
#    call cdend
#
# description
#
#    This must be the last routine called from cdlb.
#
subroutine cdend
NOIMPLICIT

    include "cdcom"
    include "sclb.c"

    if ( sccikey != CANT & sccokey != CANT )
	call scokey

    if ( sccmove != CANT )
	if ( alin != maxlin | acol != 1 )
	    call scmove ( maxlin, 1 )

    call scend

    return

end


### cdfill - fill buffer (portable version)
#
# synopsis
#
#    character ch, buf()
#    integer count
#    call cdfill ( ch, buf, count )
#
#	ch - character to fill with
#       buf - buffer to fill
#       count - fill count
#
subroutine cdfill ( ch, buf, count )
NOIMPLICIT
character ch, buf(ARB)
integer count

    integer i

    for ( i = 1; i <= count; i = i + 1 )
	buf(i) = ch

end


### cdfont - set desired font
#
# synopsis
#
#    character f
#    call cdfont ( f )
#
#	f - new desired font
#
subroutine cdfont ( f )
NOIMPLICIT
character f

    include "cdcom"

    dfontc = f

    return

end


### cdgetn - read a number from the terminal
#
# synopsis
#
#    character pmt()
#    integer status, cdgetn, num, lin, col
#    status = cdgetn ( pmt, lin, col, num )
#
#	pmt - prompt string
#	lin - line number
#	col - column number
#       num - number read
#       status - OK if a number was read, else ERR
#
integer function cdgetn ( pmt, lin, col, num )
NOIMPLICIT
character pmt(ARB)
integer num, lin, col

    character buf(MSGMAXLINE)
    integer i, cdgets, ctoi

    if ( cdgets ( pmt, lin, col, buf, MSGMAXLINE ) == ERR )
	return ( ERR )

    i = 1
    num = ctoi ( buf, i )
    return ( OK )

end


### cdgets - read a line from the terminal
#
# synopsis
#
#    character pmt(), str()
#    integer status, cdgets, maxlen, lin, col
#    status = cdgets ( pmt, lin, col, str, maxlen )
#
#	pmt - prompt string
#	lin - line number to display prompt
#	col - column number to display prompt
#	str - returned string
#       maxlen - size of str
#
# description
#
#    A line is read from the terminal and returned in str. A maximum of
#    maxlen characters will be put into the string, although more may be
#    read. Characters are read up to a newline or linefeed.
#
#    You can erase characters with either delete or backspace. You can
#    erase the line with either control-x or control-u. You can erase
#    the last word with control-w. You can redisplay the screen with
#    control-l.
#
integer function cdgets ( pmt, lin, col, str, maxlen )
NOIMPLICIT
character pmt(ARB), str(ARB)
integer maxlen, lin, col

    integer length
    character cdgetx
    character termch
    string terms "@^m@^j"

    termch = cdgetx ( pmt, lin, col, terms, str, maxlen )

    if ( length ( str ) == 0 )
	cdgets = ERR
    else
	cdgets = OK

    return

end


###############################################################################
#
### cdgetx - read a line from the terminal with special terminators
#
# synopsis
#
#    character pmt(), terms(), str()
#    integer maxlen, lin, col
#    character termch, cdgetx
#    termch = cdgetx ( pmt, lin, col, terms, str, maxlen )
#
#	pmt - prompt string
#	lin - line number to display prompt
#	col - column number to display prompt
#       terms - string of acceptable line terminators
#	str - returned string
#       maxlen - size of str
#	termch - the terminating character
#
# description
#
#    This routine prompts for input from the terminal using ``pmt''. The
#    input string is returned in ``str''. A maximum of maxlen characters
#    will be put into the string, although more may be read. Characters
#    are read up to one of the terminator characters in the string
#    ``terms''. If ``terms'' is an empty string, carriage return is
#    allowed as a terminator.
#
#    You can erase characters with either delete or backspace. You can
#    erase the line with either control-x or control-u. You can erase
#    the last word with control-w. You can redisplay the screen with
#    control-l.
#
#    The terminating character is returned as value.
#
character function cdgetx ( pmt, lin, col, terms, str, maxlen )
NOIMPLICIT
character pmt(ARB), terms(ARB), str(ARB)
integer maxlen, lin, col

    character cdgetp

    str(1) = EOS
    return ( cdgetp ( pmt, lin, col, terms, str, maxlen ) )

end


###############################################################################
#
### cdgetp - read a line from the terminal with special terminators
#
# synopsis
#
#    character pmt(), terms(), str()
#    integer maxlen, lin, col
#    character termch, cdgetx
#    termch = cdgetp ( pmt, lin, col, terms, str, maxlen )
#
#	pmt - prompt string
#	lin - line number to display prompt
#	col - column number to display prompt
#       terms - string of acceptable line terminators
#	str - passed and returned string
#       maxlen - size of str
#	termch - the terminating character
#
# description
#
#    This routine prompts for input from the terminal. The prompt ``pmt''
#    and initial contents of ``str'' and displayed and then input is read
#    and the result is returned in ``str''. This means that a default value
#    of str can be provided. This routine is otherwise similar to cdgetx().
#
character function cdgetp ( pmt, lin, col, terms, str, maxlen )
NOIMPLICIT
character pmt(ARB), terms(ARB), str(ARB)
integer maxlen, lin, col

    integer i, len, icol, scol, imaxlen, length, index
    character ch, iogchar
    logical scdisp
    include "cdcom"
    include "sclb.c"

    call cdputs ( pmt, lin, col )
    scol = col + length ( pmt )
    call cdputs ( str, lin, scol )
    len = length ( str )
    icol = scol + len
    if ( scautonl & lin == maxlin )
	i = maxcol - 1
    else
	i = maxcol
    imaxlen = min0 ( maxlen, maxcol - scol + 1 )

    repeat
        {
	str( min0 ( len+1, imaxlen ) ) = EOS
        call cdmove ( lin, icol )
	call cdplay ( .true. )
        ch = iogchar ( ch )
	if ( terms(1) != EOS )
	    if ( index ( terms, ch ) != 0 )
		break
	    else if ( ch == '@^m' )
		break
        if ( ch == '@^h' | ch == '@^?' )
            {
	    # Delete one character.
            if ( len > 0 )
                {
                len = len - 1
		icol = icol - 1
		call cdclra ( lin, icol, lin, icol )
                }
            }
        else if ( ch == '@^w' )
            {
	    # Delete the last word.
            if ( len > 0 )
                {
		# Back up over blanks.
		i = icol				# remember the end
		while ( len > 0 )
		    if ( str(len) == ' ' )
			{
			icol = icol - 1
			len = len - 1
			}
		    else
			break

		# Back up over non-blanks.
		while ( len > 0 )
		    if ( str(len) == ' ' )
			break
		    else
			{
			icol = icol - 1
			len = len - 1
			}

		# Clear things in the actual image, if necessary.
		if ( icol < i )
		    call cdclra ( lin, icol, lin, i - 1 )
                }
            }
        else if ( ch == '@^u' | ch == '@^x' )
            {
            if ( len > 0 )
                {
		call cdclra ( lin, scol, lin, icol - 1 )
		icol = scol
		len = 0
                }
            }
        else if ( ch == '@^l' )
	    call cdredo
	else if ( ! scdisp ( ch ) )
	    call iobeep
	else if ( len + 1 < imaxlen )
	    {
	    len = len + 1
	    str(len) = ch
	    call cdput ( ch, lin, icol )
	    icol = icol + 1
	    }
	else
	    call iobeep
        }

    str( min0 ( len+1, imaxlen ) ) = EOS

    return ( ch )

end


### cdgfont - get the current font
#
# synopsis
#
#    character font, cdgfont
#    font = cdgfont ( 0 )
#
#	font - the current font
#
character function cdgfont ( dummy )
NOIMPLICIT
integer dummy

    include "cdcom"

    return ( dfontc )

end


### cdgoto - move the cursor optimally (internal)
#
# synopsis
#
#    integer lin, col
#    call cdgoto ( lin, col )
#
#	lin - line number
#	col - column number
#
# description
#
#    This routine chooses a cursor movement strategy by comparing
#    character costs. Currently, it uses the following strategies:
#
#	- absolute cursor addressing
#	- relative cursor movement
#	- a carriage return plus relative movement
#	- a cursor-home plus relative movement
#
#    Special note: relative cursor movement to the right usually
#    costs only one character per position moved, even on terminals
#    whose cursor-right commands are more than one character long.
#    This is because movement to the right is accomplished by re-typing
#    the characters that are already in the positions being moved over.
#    The exception to this is when the character has attributes other
#    than the current ones set.
#
subroutine cdgoto ( lin, col )
NOIMPLICIT
integer lin, col

    integer i, strat, cost, thiscost
    include "cdcom"
    include "sclb.c"

    # 'A' means absolute movement.
    # 'R' means relative cursor movement.
    # 'N' means newline plus relative movement.
    # 'H' means home plus relative movement.

    # There's a good chance we're already where we want to be.
    if ( lin == alin & col == acol )
	return

    # The default moving strategy is absolute cursor positioning.
    strat = 'A'
    cost = sccmove

    # Compute and compare the cost of relative cursor positioning.
    thiscost = cdcrel(alin, acol, lin, col)
    if ( thiscost <= cost )    # Could be <, but <= makes it more interesting.
        {
        strat = 'R'
        cost = thiscost
        }

    if ( acol != 1 )		# If we are not already in the first column...
        {
        # Compute the cost of a CR followed by relative positioning.
        thiscost = sccbol + cdcrel(alin, 1, lin, col)
        if ( thiscost <= cost )
            {
            strat = 'N'
            cost = thiscost
            }
	if ( alin != 1 )	# If we are not already at home...
	    {
	    # Compute the cost of a home followed by relative positioning.
	    thiscost = scchome + cdcrel(1, 1, lin, col)
	    if ( thiscost <= cost )
		{
		strat = 'H'
		cost = thiscost
		}
	    }
        }

    # Now execute whatever strategy was decided on.
    if ( strat == 'A' )
        call scmove ( lin, col )
    else
        {
	# Some form of relative movement.
        if ( strat == 'N' )
            {
            call scbol
            acol = 1
            }
        else if ( strat == 'H' )
            {
            call schome
            alin = 1
            acol = 1
            }
        if ( col < acol )
            call scleft ( acol-col )
        else if ( col > acol )
	    {
	    # Put out characters from the actual image to move.
	    # the cursor right, if we're in the correct font.
	    # Otherwise, use relative cursor movement.
	    for ( i = acol; i < col; i = i + 1 )
		if ( afontc == afont(i,alin) )
		    call cdcput ( aimage(i,alin), afont(i,alin) )
		else
		    call scright ( 1 )
	    }

        if ( lin < alin )
            call scup ( alin-lin )
        else if ( lin > alin )
            call scdown ( lin-alin )
        }
    alin = lin
    acol = col

    return
end


### cdicll - clear lines of internal image (internal)
#
# synopsis
#
#    integer f, l
#    call cdicll ( f, l )
#
#	f - the first line in the area to clear
#	l - the last line in the area to clear
#
subroutine cdicll ( f, l )
NOIMPLICIT
integer f, l

    integer i
    include "cdcom"

    for ( i = f ; i <= l ; i = i + 1 )
	{
	call cdfill ( ' ', aimage(1,i), maxcol )
	call cdfill ( NORMAL_FONT, afont(1,i), maxcol )
	changed(i) = .true.
	alinend(i) = 0
	firstcol(i) = 1
	}

    return

end


### cdinit - initialize cdlb
#
# synopsis
#
#    call cdinit
#
# description
#
#    This routine must be called before all others in cdlb.
#
subroutine cdinit
NOIMPLICIT

    integer i
    logical scdisp
    integer ich
    character ch
    equivalence (ich, ch)
    include "cdcom"
    include "sclb.c"

    # Initialize screen library (this MUST be done first).
    call scinit

    # Impose minimum terminal requirements.
    if ( schardcopy )
	{
	call scend
	call error ( "cdinit: You cannot run from a hardcopy terminal." )
	}
    if ( sccmove == CANT & ( scchome == CANT | sccleft == CANT |
	  sccright == CANT | sccup == CANT | sccdown == CANT ) )
	{
	call scend
	call error (
	    "cdinit: Either direct or relative cursor addressing is required." )
	}
    if ( sccclear == CANT )
	{
	call scend
	call error ( "cdinit: Your terminal doesn't have clear screen." )
	}

    if ( sccikey != CANT  &  sccokey != CANT )
	call scikey

    # Set up relative movement cost tables.
    for ( i = -MAXSCCOLS; i <= 0; i = i + 1 )
	colcost(i) = -i * sccleft
    # For movement to the right, we'll just put out the character
    # that is already there.
    for ( i = 1; i <= MAXSCCOLS; i = i + 1 )
	colcost(i) = i

    for ( i = -MAXSCLINS; i <= 0; i = i + 1 )
	lincost(i) = -i * sccup
    for ( i = 1; i <= MAXSCLINS; i = i + 1 )
	lincost(i) = i * sccdown

    # Initialize the character translation table.
    for ( ich = 0; ich < 256; ich = ich + 1 )
	{
	if ( scdisp ( ch ) )
	    trntbl(ich+1) = ch
	else
	    trntbl(ich+1) = CDLB_PILLOW
	}

    # Calculate usable screen dimensions.
    maxcol = min0 ( scpcols, MAXSCCOLS )
    maxlin = min0 ( scplins, MAXSCLINS )

    afontc = NORMAL_FONT
    call cdredo

    dfontc = NORMAL_FONT
    call cdclear

    # Start out with all lines matching.
    for ( i = 1 ; i <= maxlin ; i = i + 1 )
	{
	changed(i) = .false.
	firstcol(i) = maxcol + 1
	}

    dlin = 1
    dcol = 1

    return

end


### cdline - draw horizontal or vertical line
#
# synopsis
#
#    integer lin1, col1, lin2, col2
#    call cdline ( lin1, col1, lin2, col2 )
#
#	lin1 - first line number
#	col1 - first column number
#	lin2 - last line number
#	col2 - last column number
#
# description
#
#    This routine draws either a horizontal or a vertical line between
#    between the two screen positions. Bad parameters are ignored.
#
subroutine cdline ( lin1, col1, lin2, col2 )
NOIMPLICIT
integer lin1, col1, lin2, col2

    integer i

    if ( lin1 == lin2 )
	{
	# A horizontal line.
	if ( col1 < col2 )
	    {
	    # Left to right.
	    call cdlsr ( lin1, col1 )
	    for ( i = col1 + 1 ; i < col2 ; i = i + 1 )
		call cdlsrl ( lin1, i )
	    call cdlsl ( lin1, col2 )
	    }
	else if ( col1 > col2 )
	    {
	    # Right to left.
	    call cdlsr ( lin1, col2 )
	    for ( i = col2 + 1 ; i < col1 ; i = i + 1 )
		call cdlsrl ( lin1, i )
	    call cdlsl ( lin1, col1 )
	    }
	}
    else if ( col1 == col2 )
	{
	# A vertical line.
	if ( lin1 < lin2 )
	    {
	    # Top down.
	    call cdlsd ( lin1, col1 )
	    for ( i = lin1 + 1 ; i < lin2 ; i = i + 1 )
		call cdlsud ( i, col1 )
	    call cdlsu ( lin2, col1 )
	    }
	else if ( lin1 > lin2 )
	    {
	    # Bottom up.
	    call cdlsd ( lin2, col1 )
	    for ( i = lin2 + 1 ; i < lin1 ; i = i + 1 )
		call cdlsud ( i, col1 )
	    call cdlsu ( lin1, col1 )
	    }
	}

    return

end


### cdlins - return the number of lines for this terminal
#
# synopsis
#
#    integer lin, cdlins
#    lin = cdlins ( 0 )
#
#	lin - number of lines for this terminal
#
integer function cdlins ( dummy )
NOIMPLICIT
integer dummy

    include "cdcom"

    return ( maxlin )

end


### cdlsd - add lower part of axis (internal)
#
# synopsis
#
#    integer lin, col
#    call cdlsd ( lin, col )
#
#	lin - line number
#	col - column number
#
# description
#
#   Adds the lower part of the y axis at the specified position.
#
subroutine cdlsd ( lin, col )
NOIMPLICIT
integer lin, col

    character ch
    include "cdcom"

    if ( 1 <= lin  &  lin <= maxlin  &
         1 <= col  &  col <= maxcol )
	{
	switch ( dimage ( col, lin ) )
	    {
	    default:
		ch = LSCH_D
	    case LSCH_D:
		ch = LSCH_D
	    case LSCH_L:
		ch = LSCH_LD
	    case LSCH_LD:
		ch = LSCH_LD
	    case LSCH_U:
		ch = LSCH_UD
	    case LSCH_UD:
		ch = LSCH_UD
	    case LSCH_UL:
		ch = LSCH_ULD
	    case LSCH_ULD:
		ch = LSCH_ULD
	    case LSCH_R:
		ch = LSCH_RD
	    case LSCH_RD:
		ch = LSCH_RD
	    case LSCH_RL:
		ch = LSCH_RLD
	    case LSCH_RLD:
		ch = LSCH_RLD
	    case LSCH_RU:
		ch = LSCH_RUD
	    case LSCH_RUD:
		ch = LSCH_RUD
	    case LSCH_RUL:
		ch = LSCH_RULD
	    case LSCH_RULD:
		ch = LSCH_RULD
	    }

	dimage(col,lin) = ch
	dfont(col,lin) = NORMAL_FONT
	changed(lin) = .true.
	dlinend(lin) = max0 ( dlinend(lin), col )
	firstcol(lin) = min0 ( firstcol(lin), col )
	}

    return

end


### cdlsl - add left part of axis (internal)
#
# synopsis
#
#    integer lin, col
#    call cdlsl ( lin, col )
#
#	lin - line number
#	col - column number
#
# description
#
#   Adds the left part of the x axis at the specified position.
#
subroutine cdlsl ( lin, col )
NOIMPLICIT
integer lin, col

    character ch
    include "cdcom"

    if ( 1 <= lin  &  lin <= maxlin  &
         1 <= col  &  col <= maxcol )
	{
	switch ( dimage ( col, lin ) )
	    {
	    default:
		ch = LSCH_L
	    case LSCH_D:
		ch = LSCH_LD
	    case LSCH_L:
		ch = LSCH_L
	    case LSCH_LD:
		ch = LSCH_LD
	    case LSCH_U:
		ch = LSCH_UL
	    case LSCH_UD:
		ch = LSCH_ULD
	    case LSCH_UL:
		ch = LSCH_UL
	    case LSCH_ULD:
		ch = LSCH_ULD
	    case LSCH_R:
		ch = LSCH_RL
	    case LSCH_RD:
		ch = LSCH_RLD
	    case LSCH_RL:
		ch = LSCH_RL
	    case LSCH_RLD:
		ch = LSCH_RLD
	    case LSCH_RU:
		ch = LSCH_RUL
	    case LSCH_RUD:
		ch = LSCH_RULD
	    case LSCH_RUL:
		ch = LSCH_RUL
	    case LSCH_RULD:
		ch = LSCH_RULD
	    }

	dimage(col,lin) = ch
	dfont(col,lin) = NORMAL_FONT
	changed(lin) = .true.
	dlinend(lin) = max0 ( dlinend(lin), col )
	firstcol(lin) = min0 ( firstcol(lin), col )
	}

    return

end


### cdlsr - add right part of axis (internal)
#
# synopsis
#
#    integer lin, col
#    call cdlsr ( lin, col )
#
#	lin - line number
#	col - column number
#
# description
#
#   Adds the right part of the x axis at the specified position.
#
subroutine cdlsr ( lin, col )
NOIMPLICIT
integer lin, col

    character ch
    include "cdcom"

    if ( 1 <= lin  &  lin <= maxlin  &
         1 <= col  &  col <= maxcol )
	{
	switch ( dimage ( col, lin ) )
	    {
	    default:
		ch = LSCH_R
	    case LSCH_D:
		ch = LSCH_RD
	    case LSCH_L:
		ch = LSCH_RL
	    case LSCH_LD:
		ch = LSCH_RLD
	    case LSCH_U:
		ch = LSCH_RU
	    case LSCH_UD:
		ch = LSCH_RUD
	    case LSCH_UL:
		ch = LSCH_RUL
	    case LSCH_ULD:
		ch = LSCH_RULD
	    case LSCH_R:
		ch = LSCH_R
	    case LSCH_RD:
		ch = LSCH_RD
	    case LSCH_RL:
		ch = LSCH_RL
	    case LSCH_RLD:
		ch = LSCH_RLD
	    case LSCH_RU:
		ch = LSCH_RU
	    case LSCH_RUD:
		ch = LSCH_RUD
	    case LSCH_RUL:
		ch = LSCH_RUL
	    case LSCH_RULD:
		ch = LSCH_RULD
	    }

	dimage(col,lin) = ch
	dfont(col,lin) = NORMAL_FONT
	changed(lin) = .true.
	dlinend(lin) = max0 ( dlinend(lin), col )
	firstcol(lin) = min0 ( firstcol(lin), col )
	}

    return

end


### cdlsrl - put horizontal line character (internal)
#
# synopsis
#
#    integer lin, col
#    call cdlsrl ( lin, col )
#
#	lin - line number
#	col - column number
#
# description
#
#   Puts the horizontal line character at the specified position.
#
subroutine cdlsrl ( lin, col )
NOIMPLICIT
integer lin, col

    character ch
    include "cdcom"

    if ( 1 <= lin  &  lin <= maxlin  &
         1 <= col  &  col <= maxcol )
	{
	switch ( dimage ( col, lin ) )
	    {
	    default:
		ch = LSCH_RL
	    case LSCH_D:
		ch = LSCH_RLD
	    case LSCH_L:
		ch = LSCH_RL
	    case LSCH_LD:
		ch = LSCH_RLD
	    case LSCH_U:
		ch = LSCH_RUL
	    case LSCH_UD:
		ch = LSCH_RULD
	    case LSCH_UL:
		ch = LSCH_RUL
	    case LSCH_ULD:
		ch = LSCH_RULD
	    case LSCH_R:
		ch = LSCH_RL
	    case LSCH_RD:
		ch = LSCH_RLD
	    case LSCH_RL:
		ch = LSCH_RL
	    case LSCH_RLD:
		ch = LSCH_RLD
	    case LSCH_RU:
		ch = LSCH_RUL
	    case LSCH_RUD:
		ch = LSCH_RULD
	    case LSCH_RUL:
		ch = LSCH_RUL
	    case LSCH_RULD:
		ch = LSCH_RULD
	    }

	dimage(col,lin) = ch
	dfont(col,lin) = NORMAL_FONT
	changed(lin) = .true.
	dlinend(lin) = max0 ( dlinend(lin), col )
	firstcol(lin) = min0 ( firstcol(lin), col )
	}

    return

end


### cdlsu - add upper part of axis (internal)
#
# synopsis
#
#    integer lin, col
#    call cdlsu ( lin, col )
#
#	lin - line number
#	col - column number
#
# description
#
#   Adds the upper part of the y axis at the specified position.
#
subroutine cdlsu ( lin, col )
NOIMPLICIT
integer lin, col

    character ch
    include "cdcom"

    if ( 1 <= lin  &  lin <= maxlin  &
	 1 <= col  &  col <= maxcol )
	{
	switch ( dimage ( col, lin ) )
	    {
	    default:
		ch = LSCH_U
	    case LSCH_D:
		ch = LSCH_UD
	    case LSCH_L:
		ch = LSCH_UL
	    case LSCH_LD:
		ch = LSCH_ULD
	    case LSCH_U:
		ch = LSCH_U
	    case LSCH_UD:
		ch = LSCH_UD
	    case LSCH_UL:
		ch = LSCH_UL
	    case LSCH_ULD:
		ch = LSCH_ULD
	    case LSCH_R:
		ch = LSCH_RU
	    case LSCH_RD:
		ch = LSCH_RUD
	    case LSCH_RL:
		ch = LSCH_RUL
	    case LSCH_RLD:
		ch = LSCH_RULD
	    case LSCH_RU:
		ch = LSCH_RU
	    case LSCH_RUD:
		ch = LSCH_RUD
	    case LSCH_RUL:
		ch = LSCH_RUL
	    case LSCH_RULD:
		ch = LSCH_RULD
	    }

	dimage(col,lin) = ch
	dfont(col,lin) = NORMAL_FONT
	changed(lin) = .true.
	dlinend(lin) = max0 ( dlinend(lin), col )
	firstcol(lin) = min0 ( firstcol(lin), col )
	}

    return

end


### cdlsud - put vertical line character (internal)
#
# synopsis
#
#    integer lin, col
#    call cdlsud ( lin, col )
#
#	lin - line number
#	col - column number
#
# description
#
#   Puts the vertical line character at the specified position.
#
subroutine cdlsud ( lin, col )
NOIMPLICIT
integer lin, col

    character ch
    include "cdcom"

    if ( 1 <= lin  &  lin <= maxlin  &
         1 <= col  &  col <= maxcol )
	{
	switch ( dimage ( col, ( lin ) ) )
	    {
	    default:
		ch = LSCH_UD
	    case LSCH_D:
		ch = LSCH_UD
	    case LSCH_L:
		ch = LSCH_ULD
	    case LSCH_LD:
		ch = LSCH_ULD
	    case LSCH_U:
		ch = LSCH_UD
	    case LSCH_UD:
		ch = LSCH_UD
	    case LSCH_UL:
		ch = LSCH_ULD
	    case LSCH_ULD:
		ch = LSCH_ULD
	    case LSCH_R:
		ch = LSCH_RUD
	    case LSCH_RD:
		ch = LSCH_RUD
	    case LSCH_RL:
		ch = LSCH_RULD
	    case LSCH_RLD:
		ch = LSCH_RULD
	    case LSCH_RU:
		ch = LSCH_RUD
	    case LSCH_RUD:
		ch = LSCH_RUD
	    case LSCH_RUL:
		ch = LSCH_RULD
	    case LSCH_RULD:
		ch = LSCH_RULD
	    }

	dimage(col,lin) = ch
	dfont(col,lin) = NORMAL_FONT
	changed(lin) = .true.
	dlinend(lin) = max0 ( dlinend(lin), col )
	firstcol(lin) = min0 ( firstcol(lin), col )
	}

    return

end


### cdmove - move position of cursor in desired image
#
# synopsis
#
#    integer lin, col
#    call cdmove ( lin, col )
#
#	lin - line number
#	col - column number
#
subroutine cdmove ( lin, col )
NOIMPLICIT
integer lin, col

    include "cdcom"

    if ( lin >= 1 & lin <= maxlin )
	dlin = lin
    if ( col >= 1 & col <= maxcol )
	dcol = col

    return

end


### cdmovtc - move translated characters (portable version)
#
# synopsis
#
#    character ibuf(), obuf(), trntbl(256)
#    integer count
#    call cdmovtc ( ibuf, obuf, trntbl, count )
#
#	ibuf - input buffer
#       obuf - output buffer
#       trntbl - character translation table
#       count - count of characters to translate
#
subroutine cdmovtc ( ibuf, obuf, trntbl, count )
NOIMPLICIT
character ibuf(ARB), obuf(ARB), trntbl(ARB)
integer count

    integer i, ich
    character ch
    equivalence (ich, ch)

    for ( i = 1; i <= count; i = i + 1 )
	{
	ich = 0
	ch = ibuf(i)
	obuf(i) = trntbl(ich+1)
	}
    return

end


### cdnotfont - make font containing all attributes not in given font
#
# synopsis
#
#    character nfont, cdnotfont, font
#    nfont = cdnotfont ( font )
#
#	font - a font
#	nfont - new font
#
character function cdnotfont ( f )
NOIMPLICIT
character f

    return ( ! f )

end


### cdnulfont - make font with no attributes
#
# synopsis
#
#    character font, cdnulfont
#    font = cdnulfont ( 0 )
#
#	font - font with no attributes
#
character function cdnulfont ( dummy )
NOIMPLICIT
integer dummy

    return ( NORMAL_FONT )

end


### cdoffatt - turn attribute off
#
# synopsis
#
#    integer n
#    call cdoffatt ( n )
#
#	n - the attribute to turn off
#
# description
#
#   ``n'' may be one of ATTRIBUTE_HIGHLIGHT, ATTRIBUTE_UNDERLINE,
#   ATTRIBUTE_REVERSE, ATTRIBUTE_BLINK, ATTRIBUTE_GRAPHICS,
#   ATTRIBUTE_ALTERNATE
#
subroutine cdoffatt ( n )
NOIMPLICIT
integer n

    character f, cdandfont, cdnotfont, cdsngfont
    include "cdcom"

    f = dfontc
    dfontc = cdandfont ( f, cdnotfont ( cdsngfont ( n ) ) )

    return

end


### cdonatt - turn attribute on
#
# synopsis
#
#    integer n
#    call cdonatt ( n )
#
#	n - the attribute to turn on
#
# description
#
#   ``n'' may be one of ATTRIBUTE_HIGHLIGHT, ATTRIBUTE_UNDERLINE,
#   ATTRIBUTE_REVERSE, ATTRIBUTE_BLINK, ATTRIBUTE_GRAPHICS,
#   ATTRIBUTE_ALTERNATE
#
subroutine cdonatt ( n )
NOIMPLICIT
integer n

    character f, cdsngfont, cdorfont
    include "cdcom"

    f = dfontc
    dfontc = cdorfont ( f, cdsngfont ( n ) )

    return

end


### cdorfont - make font containing anything in either of two fonts
#
# synopsis
#
#
#    character font, cdorfont, font1, font2
#    font = cdorfont ( font1, font2 )
#
#	font1 - a font
#	font2 - a font
#	font - a font containing any attribute in either font1 or font2
#
character function cdorfont ( font1, font2 )
NOIMPLICIT
character font1, font2

    return ( font1 | font2 )

end


### cdplay - make screen image look like desired image
#
# synopsis
#
#     logical stoponinput
#     call cdplay ( stoponinput )
#
# description
#
#   This routine makes the screen image look like the desired image.
#
#   If stoponinput is set, cdplay checks periodically to see if there
#   is a character of input ready; if so, cdplay() returns when called
#   again, it will resume where it left off.
#
subroutine cdplay ( stoponinput )
NOIMPLICIT
logical stoponinput

    integer i
    logical iochav
    include "cdcom"
    include "sclb.c"

    if ( stoponinput )
	if ( iochav ( 0 ) )
	    return

    # Be careful about last column of last line.
    if ( scautonl )
	{
	dimage(maxcol,maxlin) = ' '
	dfont(maxcol,maxlin) = NORMAL_FONT
	dlinend(maxlin) = min0 ( dlinend(maxlin), maxcol - 1 )
	}

    i = upline
    repeat
	{
	if ( changed(i) )
	    {
	    if ( stoponinput )
		if ( iochav ( 0 ) )
		    {
		    upline = i			# remember for next time
		    return
		    }
	    call cdupl ( i )
	    changed(i) = .false.
	    }
	i = i + 1
	if ( i > maxlin )
	    i = 1
	}
    until ( i == upline )

    call cdgoto ( dlin, dcol )
    call ioflush

    # Since we finished a complete update pass if we got here,
    #  start with line one next time through.
    upline = 1

    return

end


### cdput - put character in desired image
#
# synopsis
#
#    character ch
#    integer lin, col
#    call cdput ( ch, lin, col )
#
#	ch - the character to put in the desired image
#	lin - line number
#	col - column number
#
subroutine cdput ( ch, lin, col )
NOIMPLICIT
character ch
integer lin, col

    include "cdcom"

    # Make sure the character is within the screen.
    if ( col < 1 | col > maxcol | lin < 1 | lin > maxlin )
	return

    # Change desired image.
    call cdmovtc ( ch, dimage(col,lin), trntbl, 1 )
    dfont(col,lin) = dfontc
    changed(lin) = .true.

    # Update last non-blank.
    dlinend(lin) = max0 ( dlinend(lin), col )

    # Update first modified character.
    firstcol(lin) = min0 ( firstcol(lin), col )

    return

end


### cdputc - put a string into the display buffer, centered
#
# synopsis
#
#    character str()
#    integer lin
#    call cdputc ( str, lin )
#
#	str - string to output
#	lin - line number
#
subroutine cdputc ( str, lin )
NOIMPLICIT
character str(ARB)
integer lin

    integer length
    include "cdcom"

    call cdputs ( str, lin, (maxcol-length(str))/2 )

    return
end


### cdputn - put a number into the display buffer
#
# synopsis
#
#    integer int, wid, lin, col
#    call cdputn ( int, wid, lin, col )
#
#	int - integer to display
#	wid - minimum width
#	lin - line number
#	col - column number
#
subroutine cdputn ( int, wid, lin, col )
NOIMPLICIT
integer int, wid, lin, col

    character fmt(20), buf(MSGMAXLINE)

    call prints ( fmt, "%%%dd", wid )
    call prints ( buf, fmt, int )
    call cdputs ( buf, lin, col )

    return
end


### cdputr - put a real number into the display buffer
#
# synopsis
#
#    real x
#    integer wid, lin, col
#    call cdputr ( x, wid, lin, col )
#
#	x - real number to display
#	wid - minimum width
#	lin - line number
#	col - column number
#
subroutine cdputr ( x, wid, lin, col )
NOIMPLICIT
real x
integer wid, lin, col

    character fmt(20), buf(MSGMAXLINE)

    call prints ( fmt, "%%%dg", wid )
    call prints ( buf, fmt, x)
    call cdputs ( buf, lin, col )

    return

end


### cdputs - put a string into the display buffer
#
# synopsis
#
#    character str()
#    integer lin, col
#    call cdputs ( str, lin, col )
#
#	str - string to output
#	lin - line number
#	col - column number
#
# description
#
#    This routine puts the string into the display buffer starting at
#    the specified position and proceeding to the right. If the string
#    would go past the edge of the screen, it is truncated.
#
subroutine cdputs ( str, lin, col )
NOIMPLICIT
character str(ARB)
integer lin, col

    integer fcol, lcol, coff, len, length
    integer max0, min0

    include "cdcom"


    # Protect against fantastic vertical deflection.
    if ( lin < 1 | lin > maxlin )
	return

    # Calculate first and last columns to display, the
    # length of displayable part of the string, and
    # the offset into the string (first character to display).
    fcol = max0 ( 1, col )
    lcol = min0 ( maxcol, length ( str ) + col - 1 )
    len = lcol - fcol + 1
    if ( len <= 0 )
	return
    coff = fcol - col + 1

    # Now update the desired image and font.
    call cdmovtc ( str(coff), dimage(fcol,lin), trntbl, len )
    call cdfill ( dfontc, dfont(fcol,lin), len )
    changed(lin) = .true.

    # Update last non-blank character pointer.
    dlinend(lin) = max0 ( dlinend(lin), lcol )

    # Update first modified character pointer.
    firstcol(lin) = min0 ( firstcol(lin), fcol )

    return
end


### cdredo - force a complete refresh on next display
#
# synopsis
#
#    call cdredo
#
# description
#
#   Clears the screen image, so that there will be a complete
#   refresh next time cdplay() is called.
#
subroutine cdredo
NOIMPLICIT

    include "cdcom"

    # Clear screen.
    call scclear

    # Update internal image.
    call cdicll ( 1, maxlin )

    alin = 1
    acol = 1

    # Force cdplay() to start with line one next time it's called.
    upline = 1
    return

end


### cdsetfont - change the actual font
#
# synopsis
#
#    character font
#    call cdsetfont ( newfont )
#
#	newfont - the desired new font
#
subroutine cdsetfont ( newfont )
NOIMPLICIT
character newfont

    integer i
    logical atts(MAX_ATTRIBUTES)
    include "cdcom"
    include "sclb.c"
    include "sclb.ic"

    # We have to change fonts (i.e. character attributes).
    call cddefont ( newfont, atts )

    for ( i = 1 ; i <= MAX_ATTRIBUTES ; i = i + 1 )
	if ( atts ( i )  &  schasatt ( i ) )
	    call sconatt ( i )
	else
	    call scoffatt ( i )

    call scattset ( desatt )
    afontc = newfont

end


### cdsngfont - make font with single attribute
#
# synopsis
#
#    character font, cdsngfont
#    integer att
#    font = cdsngfont ( att )
#
#	att - desired attribute
#	font - returned font
#
character function cdsngfont ( att )
NOIMPLICIT
integer att

    character f

    switch ( att )
	{
	case ATTRIBUTE_HIGHLIGHT:
	    f = 1
	case ATTRIBUTE_UNDERLINE:
	    f = 2
	case ATTRIBUTE_REVERSE:
	    f = 4
	case ATTRIBUTE_BLINK:
	    f = 8
	case ATTRIBUTE_GRAPHICS:
	    f = 16
	case ATTRIBUTE_ALTERNATE:
	    f = 32
	default:
	    f = 0
	}

    return ( f )

end


### cdupl - update one line (internal)
#
# synopsis
#
#    call cdupl ( lin )
#
#	lin - line number
#
# description
#
#    This routine updates the line of the actual image to look like
#    the line of the desired image.
#
subroutine cdupl ( lin )
NOIMPLICIT
integer lin

    integer j
    include "cdcom"
    include "sclb.c"

    # If we can't clear to end of line, the desired end of
    # line may include blanks.
    if ( scccel == CANT )
	dlinend(lin) = max0 ( dlinend(lin), alinend(lin) )

    # Now make the actual image look like the desired image. We
    # take advantage of the fact that usually only one font is
    # used and look for a difference in the characters first.
    for ( j = firstcol(lin) ; j <= dlinend(lin) ; j = j + 1 )
	{
	if ( dimage(j,lin) != aimage(j,lin) )
	    goto 1
	if ( dfont(j,lin) == afont(j,lin) )
	    next
	1 continue
	call cdaput ( dimage(j,lin), dfont(j,lin), lin, j )
	}

    # Clear to end of line if necessary.
    if ( dlinend(lin) < alinend(lin) )
	call cdacel ( lin, dlinend(lin) + 1 )

    # Update actual line end.
    alinend(lin) = dlinend(lin)

    # Fix up first modified column.
    firstcol(lin) = maxcol + 1

 ifdef (DEBUG_CDLB)
	# An expensive check to make sure the lines are correct.
	for ( i = 1 ; i <= maxlin ; i = i + 1 )
	    {
	    if ( dimage ( i, lin ) != aimage ( i, lin ) |
	         dfont ( i, lin ) != afont ( i, lin ) )
		{
		# Doesn't really match at character i.
		call scbeep
		}
	    }
 enddef

    return

end
