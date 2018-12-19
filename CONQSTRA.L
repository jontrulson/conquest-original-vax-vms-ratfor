 # yaccskel - LR parser skeleton

 # ver  date   who remarks
 # --- ------- --- -----------------------------------------------------
 # 05a 17Sep85 cal .Written. Adapted from yyplb.
 #
 # description
 #
 #    A lexical analyzer is required, but is not provided in this set
 #    of routines. A reference is made to the lexical analysis routine,
 #    yylex(), in the routines yyparse() and yyperr(), to indicate where
 #    the call should be made. The scanner routine should read the next
 #    token on the input and return the token number. If there is a value
 #    associated with the token, it should be assigned to the integer
 #    parameter of yylex(). Fancier error correction routines may require
 #    that a queue of tokens be kept. revision of routines yyperr(),
 #    yydotran(), and yyparse() would probably be necessary to handle the
 #    expanded method for handling tokens.
 #

 # XXX REALLY ONLY USE YYENDTOK, YYERROR, AND YYERRTRAN
 include "yypdef"				# various global defines

 # yysstk - state stack
 # yyvstk - value stack
 # yytstk - token stack
 define(YYSCOM,
     integer yysstk(YYMAXSTACK), yyvstk(YYMAXSTACK), yytstk(YYMAXSTACK)
     common /yyscom/ yysstk, yyvstk, yytstk)

 # yyval - token value from yysem
 # yylexval - token value from yylex
 # yytok - token number
 # yyetok - error token
 # yyeval - value of error token
 # yycover - error recovery flag
 # yyerct - recovery error count
 # yysta - current state
 # yystkp - stack pointer
 define(YYPCOM,
     integer yyval, yylexval, yytok, yyetok, yyeval,
	 yycover, yyerct, yysta, yystkp
     common /yypcom/ yyval, yylexval, yytok, yyetok, yyeval,
	 yycover, yyerct, yysta, yystkp)

 define(YYERRACTION,call remark( $1 ))		# default error message code
 define(YYTRACE,)				# optional yylex() trace hook
 define(YYDUMPSTACKS,)				# optional stack dumping hook

 define(TOK_VARIABLE,260)
 define(TOK_OPERATOR,261)
 define(TOK_NUMBER,262)
 define(TOK_TERMINATOR,263)
 define(TOK_ACTION,264)
 define(TOK_AND,265)
 define(TOK_NOT,266)
 define(YYMAXSTACK,25)

%{
### yylex - lexical scanner for conqstrat
#

# Global defines go here.
include "conqdef"

#define(DEBUG,)		# uncomment this to get extra debug code

define(OP_LT,101)
define(OP_LE,102)
define(OP_GT,103)
define(OP_GE,104)
define(OP_EQ,105)
define(OP_NE,106)

define(CONQSTRATCOMMON,
    integer fd, line, rulenum, trstrat(MAX_VAR,10), trvec(32)
    logical goterror, debug, verbose
    character filename(MAXLINE)
    common /conqstratcommon/ fd, line, rulenum, trstrat, trvec,
	goterror, debug, verbose, filename)

define(YYINPUT,
    {line = line + 1
    $3 = getlin( $1($2), fd )})

define(YYDEFAULTACTION,
    {YYGETTEXT( buf )
    call eprintf( "conqstrat: Bad input at line %d, @"%s@"@n", line, buf )
    goterror = .true.})

define(YYERRACTION,call myerrmsg( $1 ))

#define(YYTRACE,call eprintf( " tok %d, (val %d)@n", yytok, yylexval ))

define(RET_VARIABLE,{yyval = $1; return( TOK_VARIABLE )})
define(RET_ACTION,{yyval = $1; return( TOK_ACTION )})
define(RET_OPERATOR,{yyval = $1; return( TOK_OPERATOR )})
%}

WS	[ @t]+
OWS	[ @t]*

%%

    NOIMPLICIT
    character buf(MAXLINE)
    integer i, ctoi
    CONQSTRATCOMMON

    yyval = 0

random			RET_VARIABLE( VAR_RANDOM )
dne			RET_VARIABLE( VAR_DNE )
damage			RET_VARIABLE( VAR_DAMAGE )
incoming		RET_VARIABLE( VAR_INCOMING )
fuel			RET_VARIABLE( VAR_FUEL )
numtorps		RET_VARIABLE( VAR_NUMTORPS )
shields			RET_VARIABLE( VAR_SHIELDS )
etemp			RET_VARIABLE( VAR_ETEMP )
wtemp			RET_VARIABLE( VAR_WTEMP )
phaserdam		RET_VARIABLE( VAR_PHASERDAM )
torpdam			RET_VARIABLE( VAR_TORPDAM )
warp			RET_VARIABLE( VAR_WARP )
shup			RET_VARIABLE( VAR_SHUP )
walloc			RET_VARIABLE( VAR_WALLOC )
orbiting		RET_VARIABLE( VAR_ORBITING )
repairing		RET_VARIABLE( VAR_REPAIRING )
cloaked			RET_VARIABLE( VAR_CLOAKED )
enemycloaked		RET_VARIABLE( VAR_ENEMYCLOAKED )
enemydamage		RET_VARIABLE( VAR_ENEMYDAMAGE )
canread			RET_VARIABLE( VAR_CANREAD )

rob_noop		RET_ACTION( ROB_NOOP )
rob_gohome		RET_ACTION( ROB_GOHOME )
rob_gorepair		RET_ACTION( ROB_GOREPAIR )
rob_allocate		RET_ACTION( ROB_ALLOCATE )
rob_detonate		RET_ACTION( ROB_DETONATE )
rob_mydetonate		RET_ACTION( ROB_MYDETONATE )
rob_phaser		RET_ACTION( ROB_PHASER )
rob_torpedo		RET_ACTION( ROB_TORPEDO )
rob_burst		RET_ACTION( ROB_BURST )
rob_shield		RET_ACTION( ROB_SHIELD )
rob_untractor		RET_ACTION( ROB_UNTRACTOR )
rob_warp_0		RET_ACTION( ROB_WARP_0 )
rob_warp_2		RET_ACTION( ROB_WARP_2 )
rob_warp_5		RET_ACTION( ROB_WARP_5 )
rob_warp_8		RET_ACTION( ROB_WARP_8 )
rob_track		RET_ACTION( ROB_TRACK )
rob_slient		RET_ACTION( ROB_SLIENT )
rob_message		RET_ACTION( ROB_MESSAGE )
rob_takedrugs		RET_ACTION( ROB_TAKEDRUGS )
rob_repair		RET_ACTION( ROB_REPAIR )
rob_readmsg		RET_ACTION( ROB_READMSG )
rob_insult		RET_ACTION( ROB_INSULT )
rob_gofuel		RET_ACTION( ROB_GOFUEL )
rob_runaway		RET_ACTION( ROB_RUNAWAY )

"<"			RET_OPERATOR( OP_LT )
"<="			RET_OPERATOR( OP_LE )
">"			RET_OPERATOR( OP_GT )
">="			RET_OPERATOR( OP_GE )
"="|"=="		RET_OPERATOR( OP_EQ )
"!="|"<>"		RET_OPERATOR( OP_NE )

"!"			return( TOK_NOT )
"&"			return( TOK_AND )
";"			return( TOK_TERMINATOR )

[0-9]			%{
			YYGETTEXT( buf )
			i = 1
			yyval = ctoi( buf, i )
			return( TOK_NUMBER )
			%}

#?*			# eat comments

{WS}			# eat gratuitous white space

@n			# eat newlines

%%

### conqstrat - main program
#
DRIVER(conqstrat)
NOIMPLICIT

    integer i, getarg, open, yyparse
    character buf(MAXLINE)
    CONQSTRATCOMMON
    string usage "usage: conqstrat [-vd] [file]"

    debug = .false.
    verbose = .false.

    while ( getarg( 1, buf, MAXLINE ) != EOF )
	{
	if ( buf(1) != '-' )
	    break
	for ( i = 2; buf(i) != EOS; i = i + 1 )
	    switch ( buf(i) )
		{
		case 'd', 'D':
		    debug = .true.
		case 'v', 'V':
		    verbose = .true.
		default:
		    call error( usage )
		}
	call delarg( 1 )
	}

    if ( getarg( 1, filename, FILENAMESIZE ) == EOF )
	{
	call strcpy( "<STDIN>", filename )
	fd = STDIN
	}
    else
	{
	call delarg( 1 )
	if ( getarg( 1, buf, MAXLINE ) != EOF )
	    call error( usage )
	fd = open( filename, READ )
	if ( fd == ERR )
	    {
	    call eprintf( "conqstrat: %s - can't open", filename )
	    call error( "" )
	    }
	}

    call initrun

    if ( yyparse( i ) == ERR )
	call error( "conqstrat: yyparse() error" )
    if ( goterror )
	call error( "conqstrat: got error" )

    if ( fd != STDIN )
	call close( fd )

    if ( verbose )
	call displayrun

    call dumprun

DRETURN

end


### initrun - initalize for the run
#
# synopsis
#
#    call initrun
#
subroutine initrun
NOIMPLICIT

    integer i, j
    CONQSTRATCOMMON

    # Initialize random stuff.
    rulenum = 1
    line = 0
    goterror = .false.

    # Initialize the strategy table to accept everything.
    for ( i = 1; i <= MAX_VAR; i = i + 1 )
	for ( j = 1; j <= 10; j = j + 1 )
	    trstrat(i,j) = -1		# set all bits

    # Initialize the rule to action vector to nothing.
    for ( i = 1; i <= 32; i = i + 1 )
	trvec(i) = ROB_NOOP

    return

end


### dumprun - dump the run into the common block
#
# synopsis
#
#    call dumprun
#
subroutine dumprun
NOIMPLICIT

    integer i, j
    CONQSTRATCOMMON
    include "conqcom"

    # This is the only place we include/modify the shared common block.
    if ( commonrev != COMMONSTAMP )
	call error ( "conqstrat: Common block ident mismatch." )

    # Copy the strategy table.
    for ( i = 1; i <= MAX_VAR; i = i + 1 )
	for ( j = 1; j <= 10; j = j + 1 )
	    rstrat(i,j) = trstrat(i,j)

    # Copy the action vector.
    for ( i = 1; i <= 32; i = i + 1 )
	rvec(i) = trvec(i)

    return

end


### displayrun - dump the run to STDOUT
#
# synopsis
#
#    call displayrun
#
subroutine displayrun
NOIMPLICIT

    integer i, j
    character buf(MAXLINE)
    CONQSTRATCOMMON
    string strat "trstrat"
    string vec "trvec"

    call getdandt( buf )
    call printf( "# conqdata - robot strategy data generated on %s@n", buf )
    call printf( "    integer %s(%d,10), %s(32)@n", strat, MAX_VAR, vec )

    call printf( "    data %s /", strat )
    for ( j = 1; j <= 10; j = j + 1 )
	for ( i = 1; i <= MAX_VAR; i = i + 1 )
	    {
	    call printf( "%d", trstrat(i,j) )
	    if ( j < 10 | i < MAX_VAR )
		{
		if ( mod( i, 5 ) == 0 )
		    call puts( ",@n@t" )
		else
		    call putc( ',' )
		}
	    }
    call puts( "/@n" )

    call printf( "    data %s /", vec )
    for ( i = 1; i <= 32; i = i + 1 )
	{
	call printf( "%d", trvec(i) )
	if ( i == 16 )
	    call puts( ",@n@t" )
	else if ( i < 32 )
	    call putc( ',' )
	}
    call puts( "/@n" )

    return

end


### dumprule - dump the current rule
#
# synopsis
#
#    integer action
#    call dumprule( action )
#
subroutine dumprule( action )
NOIMPLICIT
integer action

    integer i, j, tbits
    character buf(MAXLINE)
    CONQSTRATCOMMON

    # Store action.
    trvec(rulenum) = action

    # Check for impossible rules.
    for ( i = 1; i <= MAX_VAR; i = i + 1 )
	{
	# There must be at least one value that this variable accepts.
	tbits = 0
	for ( j = 1; j <= 10; j = j + 1 )
	    tbits = ior( tbits, trstrat(i,j) )
	if ( iand( tbits, ibset( 0, rulenum - 1 ) ) == 0 )
	    {
	    call valstr( i, buf )
	    call eprintf(
		"conqstrat: Rule on line %d can't happen, %s is broken@n",
		line - 1, buf)
	    }
	}

    # Update rule number.
    rulenum = rulenum + 1

    return

end


### addrule - add info to the current rule
#
# synopsis
#
#    integer var, op, num
#    call addrule( var, op, num )
#
subroutine addrule( var, op, num )
NOIMPLICIT
integer var, op, num

    integer i, rulebits
    character svar(32), sop(32)
    CONQSTRATCOMMON

    if ( debug )
	{
	call valstr( var, svar )
	call valstr( op, sop )
	call eprintf( "addrule: var %s, op %s, num %d@n", svar, sop, num )
	}

    if ( rulenum > 32 )
	BAIL( "conqstrat: More than 32 rules; line %d@n", line )
    if ( num < 0 | num > 9 )
	BAIL( "addrule: impossible number %d@n", num )
    if ( var < 1 | num > MAX_VAR )
	BAIL( "addrule: impossible variable %d@n", var )

    rulebits = not( ibset( 0, rulenum - 1 ) )
    switch ( op )
	{
	case OP_LT:
	    for ( i = num; i <= 9; i = i + 1 )
		trstrat(var,i+1) = iand( trstrat(var,i+1), rulebits )
	case OP_LE:
	    for ( i = num + 1; i <= 9; i = i + 1 )
		trstrat(var,i+1) = iand( trstrat(var,i+1), rulebits )
	case OP_GT:
	    for ( i = 0; i <= num; i = i + 1 )
		trstrat(var,i+1) = iand( trstrat(var,i+1), rulebits )
	case OP_GE:
	    for ( i = 0; i < num; i = i + 1 )
		trstrat(var,i+1) = iand( trstrat(var,i+1), rulebits )
	case OP_EQ:
	    for ( i = 0; i <= 9; i = i + 1 )
		if ( i != num )
		    trstrat(var,i+1) = iand( trstrat(var,i+1), rulebits )
	case OP_NE:
	    trstrat(var,num+1) = iand( trstrat(var,num+1), rulebits )
	default:
	    BAIL( "addrule: impossible op %d@n", op )
	}

    return

end


### invertop - invert an operator
#
# synopsis
#
#    integer iop, op, invertop
#    iop = invertop( op )
#
integer function invertop( op )
NOIMPLICIT
integer op

    integer iop

    switch ( op )
	{
	case OP_LT:
	    iop = OP_GE
	case OP_LE:
	    iop = OP_GT
	case OP_GT:
	    iop = OP_LE
	case OP_GE:
	    iop = OP_LT
	default:
	    iop = op
	}

    return ( iop )

end


### valstr - convert a var/op value to a string
#
# synopsis
#
#    integer value
#    character buf()
#    call valstr( value, buf )
#
subroutine valstr( value, buf )
NOIMPLICIT
integer value
character buf(ARB)

    switch ( value )
	{
	case VAR_RANDOM:
	    call strcpy( "random", buf )
	case VAR_DNE:
	    call strcpy( "dne", buf )
	case VAR_DAMAGE:
	    call strcpy( "damage", buf )
	case VAR_INCOMING:
	    call strcpy( "incoming", buf )
	case VAR_FUEL:
	    call strcpy( "fuel", buf )
	case VAR_NUMTORPS:
	    call strcpy( "numtorps", buf )
	case VAR_SHIELDS:
	    call strcpy( "shields", buf )
	case VAR_ETEMP:
	    call strcpy( "etemp", buf )
	case VAR_WTEMP:
	    call strcpy( "wtemp", buf )
	case VAR_PHASERDAM:
	    call strcpy( "phaserdam", buf )
	case VAR_TORPDAM:
	    call strcpy( "torpdam", buf )
	case VAR_WARP:
	    call strcpy( "warp", buf )
	case VAR_SHUP:
	    call strcpy( "shup", buf )
	case VAR_WALLOC:
	    call strcpy( "walloc", buf )
	case VAR_ORBITING:
	    call strcpy( "orbiting", buf )
	case VAR_REPAIRING:
	    call strcpy( "repairing", buf )
	case VAR_CLOAKED:
	    call strcpy( "cloaked", buf )
	case VAR_ENEMYCLOAKED:
	    call strcpy( "enemycloaked", buf )
	case VAR_ENEMYDAMAGE:
	    call strcpy( "enemydamage", buf )
	case VAR_CANREAD:
	    call strcpy( "canread", buf )
	case OP_LT:
	    call strcpy( "lt", buf )
	case OP_LE:
	    call strcpy( "le", buf )
	case OP_GT:
	    call strcpy( "gt", buf )
	case OP_GE:
	    call strcpy( "ge", buf )
	case OP_EQ:
	    call strcpy( "eq", buf )
	case OP_NE:
	    call strcpy( "ne", buf )
	default:
	    call prints( buf, "<%d>", value )
	}

    return

end

### tokstr - convert a token to a string
#
# synopsis
#
#    integer token
#    character buf()
#    call tokstr( token, buf )
#
subroutine tokstr( token, buf )
NOIMPLICIT
integer token
character buf(ARB)

    switch ( token )
	{
	case TOK_VARIABLE:
	    call strcpy( "TOK_VARIABLE", buf )
	case TOK_OPERATOR:
	    call strcpy( "TOK_OPERATOR", buf )
	case TOK_NUMBER:
	    call strcpy( "TOK_NUMBER", buf )
	case TOK_TERMINATOR:
	    call strcpy( "TOK_TERMINATOR", buf )
	case TOK_ACTION:
	    call strcpy( "TOK_ACTION", buf )
	case TOK_AND:
	    call strcpy( "TOK_AND", buf )
	case TOK_NOT:
	    call strcpy( "TOK_NOT", buf )
	case YYENDTOK:
	    call strcpy( "YYENDTOK", buf )
	default:
	    call prints( buf, "TOK_<%d>", token )
	}

    return

end

### myerrmsg - yacc error reporting routine
#
# synopsis
#
#    character buf()
#    call myerrmsg( buf )
#
subroutine myerrmsg( buf )
NOIMPLICIT
character buf(ARB)

    CONQSTRATCOMMON

    call eprintf( "conqstrat: error at line %d: %s@n", line, buf )
    goterror = .true.

end

subroutine yysem( yyprod )
integer yyprod
NOIMPLICIT
integer invertop
character buf(MAXLINE)
CONQSTRATCOMMON
    YYPCOM
    YYSCOM
    switch ( yyprod )
	{

         case          4:
            call dumprule( yyvstk(yystkp-0) )
            

         case          5:
            call dumprule( yyvstk(yystkp-0) )
            

         case          9:
            call addrule( yyvstk(yystkp-2), yyvstk(yystkp-1), yyvstk(yystkp-0) )
            

         case         10:
            call addrule( yyvstk(yystkp-0), invertop( yyvstk(yystkp-1) ), yyvstk(yystkp-2) )
            

         case         11:
            call addrule( yyvstk(yystkp-0), OP_NE, 0 )
            

         case         12:
            call addrule( yyvstk(yystkp-0), OP_EQ, 0 )
            

        default:;
	}

return
end

### yyparse - does the actual parsing of the input
#
# SYNOPSIS
# --------
#    sts = yyparse( retvalue )
#         sts   - ERR if couldn't recover from error, OK otherwise
#         retvalue - value of last production reduced
#
# DESCRIPTION
# -----------
#  This is the actual parsing loop. For the current state, yysta,
#  and the current lookahead, yytok, a shift or tranisition is attempted.
#  If that legal, the shift is performed and the next token is retrieved.
#  If no shift is legal, a reduction is attempted. If no reduction is possible
#  an error has occured and the error recovery routines are called.
#  The error recovery routine may either recover sufficiently to
#  resume a parse at the top of the loop, or will fail and die. A call
#  to yylex, which returns the next token on the input stream,
#  is called only after a successful read transition, as no symbols
#  are read when a reduction is performed. The parse stops successfully
#  on two conditions. First, when the reduction performed is number one,
#  you have reduced to the system goal symbol.  This should normally
#  not occur, though, because you should stop successfully when
#  you transfer to the final state, yyfinal.
#
integer function yyparse( retvalue )
NOIMPLICIT
integer retvalue

    integer prodnum				# production num for reduction
    integer status				# status of parse
    integer leptr				# new stack pointer
    integer yyfdrd				# find possible reduction
    integer yyperr				# error recovery routine
    integer yyfdtr				# find possible transition
    integer state				# new state to transfer to
    integer yylex				# lexical analyzer function
    integer dummy
    string illconerr "Illegal Language Construct."
    string nonasserr "Tried To Associate Non-Associating Operator."
    string ovflerr "Syntactic Stack Overflow"
    YYPCOM					# parse global data
    YYSCOM					# parse state stacks

    integer yyfred(24)
    integer yynset(12)
    integer yylset(4)
    integer yyls(9)
    integer yyprod(12)
    integer yylen(12)
    integer yylhs(12)
    integer yyftrn(24)
    integer yyfinal, yytran(32)
    integer yydbg, yyent(24)
    data yyfinal, yydbg/18,0/

    data yytran(1)/2/,yytran(2)/3/,yytran(3)/4/
    data yytran(4)/5/,yytran(5)/6/,yytran(6)/7/
    data yytran(7)/8/,yytran(8)/9/,yytran(9)/10/
    data yytran(10)/11/,yytran(11)/12/,yytran(12)/13/
    data yytran(13)/14/,yytran(14)/15/,yytran(15)/16/
    data yytran(16)/17/,yytran(17)/3/,yytran(18)/4/
    data yytran(19)/5/,yytran(20)/6/,yytran(21)/18/
    data yytran(22)/7/,yytran(23)/8/,yytran(24)/9/
    data yytran(25)/19/,yytran(26)/20/,yytran(27)/21/
    data yytran(28)/3/,yytran(29)/4/,yytran(30)/6/
    data yytran(31)/22/,yytran(32)/23/
    data yyftrn(1)/1/,yyftrn(2)/2/,yyftrn(3)/11/
    data yyftrn(4)/12/,yyftrn(5)/13/,yyftrn(6)/14/
    data yyftrn(7)/15/,yyftrn(8)/15/,yyftrn(9)/15/
    data yyftrn(10)/17/,yyftrn(11)/17/,yyftrn(12)/26/
    data yyftrn(13)/26/,yyftrn(14)/27/,yyftrn(15)/27/
    data yyftrn(16)/28/,yyftrn(17)/32/,yyftrn(18)/33/
    data yyftrn(19)/33/,yyftrn(20)/33/,yyftrn(21)/33/
    data yyftrn(22)/33/,yyftrn(23)/33/
    data yyftrn(24)/33/
    data yyent(1)/266/,yyent(2)/0/,yyent(3)/266/
    data yyent(4)/262/,yyent(5)/263/,yyent(6)/260/
    data yyent(7)/-1/,yyent(8)/271/,yyent(9)/270/
    data yyent(10)/269/,yyent(11)/268/,yyent(12)/260/
    data yyent(13)/261/,yyent(14)/264/,yyent(15)/261/
    data yyent(16)/265/,yyent(17)/263/,yyent(18)/0/
    data yyent(19)/269/,yyent(20)/260/,yyent(21)/262/
    data yyent(22)/271/,yyent(23)/264/
    data yyent(24)/-2/
    data yyfred(1)/1/,yyfred(2)/1/,yyfred(3)/1/
    data yyfred(4)/1/,yyfred(5)/1/,yyfred(6)/1/
    data yyfred(7)/2/,yyfred(8)/3/,yyfred(9)/4/
    data yyfred(10)/4/,yyfred(11)/5/,yyfred(12)/5/
    data yyfred(13)/6/,yyfred(14)/6/,yyfred(15)/7/
    data yyfred(16)/7/,yyfred(17)/7/,yyfred(18)/7/
    data yyfred(19)/8/,yyfred(20)/9/,yyfred(21)/10/
    data yyfred(22)/11/,yyfred(23)/12/
    data yyfred(24)/13/
    data yynset(1)/1/,yynset(2)/2/,yynset(3)/1/
    data yynset(4)/2/,yynset(5)/1/,yynset(6)/2/
    data yynset(7)/3/,yynset(8)/2/,yynset(9)/1/
    data yynset(10)/1/,yynset(11)/1/,yynset(12)/2/
    data yyprod(1)/11/,yyprod(2)/6/,yyprod(3)/7/
    data yyprod(4)/3/,yyprod(5)/12/,yyprod(6)/5/
    data yyprod(7)/1/,yyprod(8)/2/,yyprod(9)/10/
    data yyprod(10)/9/,yyprod(11)/8/,yyprod(12)/4/
    data yylhs(1)/267/,yylhs(2)/268/,yylhs(3)/268/,yylhs(4)/269/
    data yylhs(5)/269/,yylhs(6)/269/,yylhs(7)/270/,yylhs(8)/270/
    data yylhs(9)/271/,yylhs(10)/271/,yylhs(11)/271/,yylhs(12)/271/
    data yylen(1)/3/,yylen(2)/2/,yylen(3)/1/,yylen(4)/3/
    data yylen(5)/2/,yylen(6)/1/,yylen(7)/1/,yylen(8)/3/
    data yylen(9)/3/,yylen(10)/3/,yylen(11)/1/,yylen(12)/2/
    data yylset(1)/1/
    data yylset(2)/3/,yylset(3)/9/,yylset(4)/10/
    data yyls(1)/265/,yyls(2)/263/,yyls(3)/266/
    data yyls(4)/262/,yyls(5)/263/,yyls(6)/260/
    data yyls(7)/0/,yyls(8)/-1/,yyls(9)/0/

    yycover = NO				# reset error recovery flag
    status = OK					# assume success
    yyparse = OK				# assume success
    yycover = NO				# not in error recovery mode
    yysta = 1					# initial state
    yylexval = 0				# initial token value
    yytok = YYENDTOK				# initial token
    yystkp = 1					# initialize state stack pointer
    yysstk(yystkp) = 1				# initial state stack
    yyvstk(yystkp) = 0				# initial value stack

    while ( status != ERR )			# parse loop
	{# parse

	# Test for transition.
	state = yyfdtr( yysta, yytok, yyftrn, yytran, yyent )

	if ( state >= 0 )			# found transition
	    {# dotran
	    # call yydotran( state )		# do the transition
	    # Start of yydotran().
#
# The transition to state 'state' is performed by incrementing
# the stackpointer and stacking the token, it's value, and 'state'.
# Further parallel stacks may be maintained in this routine.
#
	    yystkp = yystkp + 1
	    if ( yystkp > YYMAXSTACK )		# test for stack ovflw
		dummy = yyperr( ovflerr, yyftrn,
		    yytran, yyent )		# call error routine
	    else
		{
		YYDUMPSTACKS			# stack dumping hook
		yytstk(yystkp) = yytok		# stack lookahead token
		yyvstk(yystkp) = yylexval	# stack it's value
		yysstk(yystkp) = state		# stack transition state
		yysta = state			# make it the current state
		}
	    # End of yydotran().

	    if ( state == yyfinal )		# transition to final state
		{
		retvalue = yyval
		return
		}
	    else				# more parsing to be done
		{
		if ( yycover == YES )		# trying to recover from erro
		    call yyrecover
		else
		    {
		    yytok = yylex( yylexval )	# call the lexical analyzer
		    YYTRACE			# yylex() error trace hook
		    }
		}

	    }# dotran
	else if ( state == YYERRTRAN )		# nonassociable association
	    {
	    yyparse = ERR
	    status = yyperr( nonasserr, yyftrn,
		yytran, yyent )			# try to recover from error
	    }
	else
	    {# dored
	    # Find reduction.
	    prodnum = yyfdrd( yysta, yytok, yyfred, yynset,
		yylset, yyprod, yyls )
	    if ( prodnum < 0 )			# no reduction
		{
		yyparse = ERR
		status = yyperr( illconerr, yyftrn,
		    yytran, yyent )		# try to recover from error
		}
	    else				# reduction found
		{
		YYDUMPSTACKS			# stack dumping hook
		# call yydored( prodnum )	# do the reduction
		# Start of yydored().
#
#  The reduction for production prodnum is performed by first finding
#  the new position for the stack pointer, by subtracting the length
#  of the production, found in the yylen vector for the production
#  number, from the stack pointer. The semantic routines are then
#  called into action by calling yysem with the production
#  number. The stacks are then adjusted
#  accordingly, a transition is made to the left hand side of
#  the production, found in vector yylhs, and the state on the top
#  of the stack. The current state is set to the state on the top
#  of the stack, the stack pointer is reset, and the routine returns.
#
		leptr = yystkp - yylen(prodnum) + 1 # get new stack pointer

		if ( leptr > YYMAXSTACK )	# check stack overflow
		    dummy = yyperr( ovflerr, yyftrn,
			yytran, yyent )		# call error routine
		else
		    {
		    yyval = yyvstk( leptr )	# default is '$1'
		    call yysem( prodnum )	# call semantics routine
		    yyvstk(leptr) = yyval	# put token value on stack
		    yytstk(leptr) = yylhs(prodnum) # put LHS token on stack
		    yysta = yyfdtr( yysstk(leptr - 1), yylhs(prodnum),
			yyftrn, yytran, yyent )
		    yysstk(leptr) = yysta	# put goto state on stack
		    yystkp = leptr		# reset stack pointer
		    }
		# End of yydored().

		if ( prodnum == 1 )		# start symbol reduced
		    {
		    retvalue = yyval
		    return
		    }
		}
	    }# dored

	}# parse

    retvalue = yyval
    return
end


### yyfdrd - find reduction corresponding to state stack
#
# SYNOPSIS
# --------
#   call yyfdrd( state, token )
#
#     state - current state
#     token - lookahead token
#
# DESCRIPTION
# -----------
#    If a reduction should be done when in state 'state' looking ahead at
#  symbol 'token', the production number is returned.  Otherwise, a -1 is
#  returned.  No reduction should be made if the token is a
#  non-terminal.  This is true because the parser makes all reductions
#  possible for a given token before it reads the next symbol.  If a
#  reduction were possible looking ahead to a non-terminal, then a
#  symbol must have been read before the reductions were finished on
#  this token, which will not happen.
#  The 'yyfred' vector gives for the state the first and last indices in the
#  'yynset' vector that must be scanned.  The 'yynset' vector gives the
#  lookahead set number.  If the bit for the token is set in the
#  lookahead set bit vector, then the production number found in vector
#  'yyprod' is returned.
#
integer function yyfdrd( state, token, yyfred, yynset, yylset, yyprod, yyls )
NOIMPLICIT
integer state, token, yyfred(ARB), yynset(ARB), yylset(ARB),
    yyprod(ARB), yyls(ARB)

    integer start				# first lookahead scan index
    integer iend				# last lookahead scan index
    integer jstart				# first lookahead bit index
    integer jend				# last lookahead bit index
    integer i, j

    start = yyfred(state)			# get first set index
    iend = yyfred(state + 1) - 1		# get last set index
    yyfdrd = -1

    if ( start <= iend )			# there is a lookahead set

    # Look through lookahead set for the symbol 'token'.

	for ( i = start; i <= iend ; i = i + 1 )
	    {
	    j = yynset(i)			# get lookahead set number
	    jstart = yylset(j)			# 1st lookahead bit index
	    jend = yylset(j + 1) - 1		# last lookahead bit indx

	    # Search for 'token' bit set.

	    for ( j = jstart; j <= jend; j = j + 1 )
		if ( yyls(j) == token )		# token found
		    {
		    yyfdrd = yyprod(i)		# return production number
		    return
		    }
	    }

	return

end


### yyfdtr - find next transition to do
#
# SYNOPSIS
# --------
#
#    newstate = yyfdtr( state, token )
#        state - current state
#        token - lookahead token
#        newstate - goto state of transition, or -1 if no legal transition
#
# DESCRIPTION
# -----------
#     This function decides whether a read transition should be
#  performed when in state 'state' looking at symbol 'token'. For the state
#  'state', you may transfer to the 'newstate' given in the yytran vector
#  if the entrance symbol for that transition is the same as the
#  symbol you want to read; 'token'. The yyftrn vector gives the beginning
#  and ending positions in the yytran vector for legal read
#  transitions in the state 'state'. If 'token' is not found in the
#  entrance symbols for legal read transitions for 'state', a
#  -1 is returned.
#
integer function yyfdtr( state, token, yyftrn, yytran, yyent )
NOIMPLICIT
integer state, token, yyftrn(ARB), yytran(ARB), yyent(ARB)

    integer start				# 1st index into state vector
    integer iend				# last index into state vector
    integer i, j

    yyfdtr = -1
    start = yyftrn(state)			# get first index
    iend = yyftrn(state + 1) - 1		# get last index

    if ( start <= iend )			# got some transitions

	# Read through transition set for lookahead symbol 'token'.

	for ( i = start; i <= iend; i = i + 1 )
	    {
	    j = yytran(i)			# possible trans state
	    if ( token == yyent( iabs(j) ) )	# see if token matches
		{
		if ( j < 0 )			# if non-associative
		    yyfdtr = YYERRTRAN		# return error
		else
		    yyfdtr = j			# else, return state
		return
		}
	    }

    return

end


### yyperr - error recovery routine
#
# SYNOPSIS
# --------
#    status = yyperr( errmsg )
#       errmsg - character array error message
#
# DESCRIPTION
# -----------
#    YYPERR attempts error recovery by popping states off the state stack,
# until a state is found from which it is legal to shift the ERROR token.
# If no such state is found, the parser returns ERR.
# If the state is found; the parser returns OK,
# the state is left on top of the stack, and the ERROR
# token is made the next lookahead token. The error recovery flag, yycover,
# is set and is not reset until 3 tokens have been successfully
# read and shifted. While this flag
# is set, any more errors found will only result in the lookahead
# token being discarded and a new one read in. This is done to hopefully
# not print a zillion error messages for one error.
#
integer function yyperr( msg, yyftrn, yytran, yyent )
NOIMPLICIT
character msg(MAXLINE)
integer yyftrn(ARB), yytran(ARB), yyent(ARB)

    YYSCOM					# parse state stacks
    YYPCOM					# parse global data
    integer state, i
    integer yylex				# returns next token
    integer yyfdtr				# finds transition on token

    yyperr = OK
    if ( yycover == YES )			# still in recovery mode
	{
	if ( yytok == YYENDTOK )		# couldn't recover from error
	    yyperr = ERR
	else
	    {
	    yytok = yylex( yylexval )		# just disregard current token
	    }
	}
    else
	{
	state = -1

	# Save curent token to be restored when error-token is shifted.
	yyetok = yytok
	yyeval = yylexval

	# Test each state to see if it contains a transition on ERROR.
	for ( i = yystkp; i > 0 & state < 0; i = i - 1 )
	    # Test for transition.
	    state = yyfdtr( yysstk(i), YYERROR, yyftrn, yytran, yyent )

	if ( state < 0 )			# didn't find recovery state
	    yyperr = ERR			# couldn't recover from error
	else
	    {
	    yycover = YES			# set error recovery flag
	    yyerct = 0				# initialize recovery count
	    yytok = YYERROR			# set lookahead to err token
	    yystkp = i + 1			# pop all other states off
	    yysta = yysstk(yystkp)		# reset current state
	    }

	YYERRACTION( msg )			# print error message
	}

    return

end


### yyrecover - try to recover from parse error
#
# YYrecover is called after an error has occurred, each time a
# shift is done.  The first shift will be the ERROR token, at
# which time the lookahead token (stored in yyetok) at the time
# of the error must be restored as the next lookahead token.
# After three tokens have been successfully read and shifted,
# the error recovery flag is reset.
#
subroutine yyrecover
NOIMPLICIT

    YYPCOM					# parse global data
    integer yylex


    if ( yyerct == 0 )				# error token just shifted
	{
	# Restore previous lookahead token.
	yytok = yyetok
	yylexval = yyeval
	}
    else
	{
	# Need next lookahead token from input.
	yytok = yylex( yylexval )

	if ( yyerct == 3 )			# 3 tokens succesfully shifted
	    yycover = NO			# reset recovery flag
	}

    yyerct = yyerct + 1

    return

end
