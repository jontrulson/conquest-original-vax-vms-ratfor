###############################################################################
#
#                                 E N L B
#
###############################################################################
#
# version date initials remarks
# ------- ---- -------- -------------------------------------------------------
#
#   01h  17Sep83  cal   Removed NOIMPLICIT definition.
#   01g  04Nov83  cal   Changed usage of pathopen() to open().
#   01f  10Sep83  cal   Ratfixed. Added NOIMPLICITs. Modified to not include
#                       "ctrlsyms".
#   01E  13sep82  TH    Decreased size of data structures from about 15000
#                       to about 1500
#   01D  28sep82  TH    Increased maximum number of fields
#                       Shortened field lengths
#   01C  24jul82  TH    Fixed bug not allowing spaces after @<NEWLINE>
#                       (the condition used was (ch != BLANK) | (ch != TAB)
#                       instead of (ch != BLANK) & (ch != TAB))
#   01B  16jul82  TH    Modified to have 80 possible fields (as there were
#                       66 possible fields today, not counting attribute
#                       fields terminal type, and terminal names)
#   01A  14apr82  JP    First written.
#
###############################################################################


# there are over 80 possible fields last count
# not counting attribute fields, terminal type, and name
define(MAXFIELDS,100)
define(MAXENTRY,1500)


###############################################################################
#
###  enopen - open the entry file
#
#  SYNOPSIS
#    integer status, enopen
#    character filename()
#    status = enopen ( filename )
#
#  DESCRIPTION
#
#
integer function enopen ( filename )
NOIMPLICIT
character filename(ARB)

    integer open
    include "enlb.c"

    file = open ( filename, READ )
    if ( file == ERR )
	enopen = ERR
    else
	enopen = OK

    numfields = 0

    return
end


###############################################################################
#
###  enclose - close the entry file
#
#  SYNOPSIS
#    call enclose
#
#  DESCRIPTION
#
#
subroutine enclose
NOIMPLICIT

    include "enlb.c"

    call close ( file )

    return
end


###############################################################################
#
###  engetnext - read in the next entry from the entry file
#
#  SYNOPSIS
#    integer status, engetnext
#    status = engetnext(0)
#
#  DESCRIPTION
#
#
integer function engetnext ( dummy )
NOIMPLICIT
integer dummy

    character ch, getch, cupper, chint
    integer int
    include "enlb.c"

    numfields = 0

    # Skip comment lines, blank lines, and leading blanks and tabs.
    repeat
	{
	ch = getch ( ch, file )
	if ( ch == '#' )
	    {
	    repeat
		{
		ch = getch ( ch, file )
		}
	    until ( (ch == '@n') | (ch == EOF) )
	    }
	}
    until ( (ch != '@n') & (ch != ' ') & (ch != '@t') )
    if ( ch == EOF )
	return ( EOF )

    # Now read in the fields.
    numfields = 1
    fieldlen(numfields) = 0
    fieldstart(numfields) = 1
    repeat
	{
	if ( (ch == '@n') | (ch == EOF) )
	    # end of entry
	    break
	else if ( ch == ':' )
	    { # end of field
	    if ( fieldlen(numfields) > 0 )		# ignore empty fields
		{
		if ( numfields < MAXFIELDS )
		    {
		    fieldstart ( numfields + 1 ) =
		        fieldstart ( numfields ) + fieldlen ( numfields )
		    fieldlen ( numfields + 1 ) = 0
		    }
		numfields = numfields + 1
		}
	    } # end of field
	else if ( ch == '^' )
	    { # controlify
	    ch = cupper ( getch ( ch, file ) )
	    if ( (ch == '@n') | (ch == EOF) )
		break
	    else if ( (ch >= '@@') & (ch <= '_') )
		call enstorchar ( ch - '@@' )
	    else
		call enstorchar ( ch )
	    } # controlify
	else if ( ch == '@@' )
	    { # escapeify
	    ch = getch ( ch, file )
	    if ( ch == EOF )
		break
	    else if ( cupper(ch) == 'E' )
		call enstorchar ( '@^[' )
	    else if ( ch == '@n' )
		{ # escaped NEWLINE
		repeat
		    ch = getch ( ch, file )
		until ( ! ( ch == ' '  |  ch == '@t' ) )
		next
		} # escaped NEWLINE
	    else if ( (ch >= '0') & (ch <= '9') )
		{ # escaped digits
		int = 0
		repeat
		    {
		    int = int * 8
		    int = int + ( ch - '0' )
		    ch = getch ( ch, file )
		    }
		until ( (ch < '0') | (ch > '9') )
		chint = int
		call enstorchar ( chint )
		next
		} # escaped digits
	    else
		call enstorchar ( ch )
	    } # escapeify
	else
	    # normal character
	    call enstorchar ( ch )

	# Get the next character.
	ch = getch ( ch, file )
	}

    numfields = min ( numfields, MAXFIELDS )

    # If the last field is empty, get rid of it.
    if ( fieldlen(numfields) == 0 )
	numfields = numfields - 1

    if ( ch == EOF )
	return ( EOF )
    else
	return ( OK )

    return
end


###############################################################################
#
###  enstorchar - internal routine to store a character in fields
#
#  SYNOPSIS
#    character ch
#    call enstorchar ( ch )
#
#  DESCRIPTION
#
#
subroutine enstorchar ( ch )
NOIMPLICIT
character ch

    include "enlb.c"

    if ( numfields <= MAXFIELDS )
	if ( fieldstart ( numfields ) - 1 + fieldlen ( numfields ) < MAXENTRY )
	    {
	    entry ( fieldstart ( numfields ) + fieldlen ( numfields ) ) = ch
	    fieldlen ( numfields ) = fieldlen ( numfields ) + 1
	    }

    return
end


###############################################################################
#
###  enfldn - get a field according to its number
#
#  SYNOPSIS
#    integer status, enfldn, n
#    character str()
#    status = enfldn ( n, str, MAXSTR )
#
#  DESCRIPTION
#
#
integer function enfldn ( n, str, maxstr )
NOIMPLICIT
integer n, maxstr
character str(ARB)

    integer len, i
    include "enlb.c"

    if ( (n < 1) | (n > numfields) )
	enfldn = ERR
    else
	{
	len = min ( fieldlen(n), maxstr-1 )
	for ( i=1; i <= len; i=i+1 )
	    str(i) = entry ( fieldstart ( n ) + i - 1 )
	str(len+1) = EOS
	enfldn = OK
	}

    return
end


###############################################################################
#
###  enfldl - get a field according to its label
#
#  SYNOPSIS
#    integer status, enfldl
#    character label(), str()
#    status = enfldl ( label, str, MAXSTR )
#
#  DESCRIPTION
#
#
integer function enfldl ( label, str, maxstr )
NOIMPLICIT
integer maxstr
character label(ARB), str(ARB)

    logical eniniseg
    integer length
    integer len
    integer enfldn
    integer n

    include "enlb.c"


    len = length ( label )

    for ( n = 1 ; n <= numfields ; n = n + 1 )
	if ( eniniseg ( label, len,
	                entry ( fieldstart ( n ) ), fieldlen ( n ) ) )
	    break

    return enfldn ( n, str, maxstr )

end


###############################################################################
#
### eniniseg - see if string is initial segment
#
# log = eniniseg ( a, la, b, lb )
#
# description
#   see if string a with length la is an initial segment of string b
#   with length lb
#
# passed
#   a - string to see if is initial segment
#   la - number of characters in a
#   b - string to see if 'a' is an initial segment
#   lb - number of characters in b
#
# returned
#   log - whether a is an initial segment of b

logical function eniniseg ( a, la, b, lb )
NOIMPLICIT
character a ( ARB )
integer la
character b ( ARB )
integer lb

    integer i


    if ( la > lb )
	eniniseg = .false.

    else
	{
	for ( i = 1 ; i <= la ; i = i + 1 )
	    if ( b ( i ) != a ( i ) )
		break
	eniniseg = i > la
	}

    return

end
