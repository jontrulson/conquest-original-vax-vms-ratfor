###############################################################################
#
#                                I O N L B
#
###############################################################################
#
# Version Date Initials Remarks
# ------- ---- -------- -------------------------------------------------------
#
#   08c  17Sep84  cal   Modified to not include "tslb". Renamed include from
#                       "ionlb.c" to "ioncom".
#
###############################################################################

define(MAXOBUFSIZE,1000)	# maximum size of output buffer (.5sec@19.2Kb)
define(MXSIZE,200)		# size of mailbox
define(MXMUL,5)			# number of message outstanding
define(MXPROT,0)		# protection mask for mailbox

define(MSG_W_TYPE,1)		# message type
define(MSG_W_COUNT,21)		# location of message size in mailbox
define(MSG_S_MSG,23)		# beginning of message in mailbox

define(TT$M_NOBRDCST,16%00020000)
define(TT2$M_BRDCSTMBX,16%10)
define(TT2$M_XON,16%00000020)


###############################################################################
#
###  iogchar - get a character
#
#  SYNOPSIS
#    character ch, iogchar
#    ch = iogchar ( ch )
#
character function iogchar ( ch )
NOIMPLICIT
character ch

    integer status, sys$qiow
    external io$_readlblk, ss$_normal
    include "ioncom"

    # This is a good place to flush the output buffer and to
    #  check for terminal broadcasts.
    if ( autobroad )
	call iomsgflush
    call ioflush

    call sys$setast(%val(1))		# enable asts
    # iogchar() and iogtimed() use a separate event flag...
    status = sys$qiow ( %val(cevfl), %val(chan), %val(%loc(io$_readlblk)),
	, , , ch, %val(1), , %val(0), , )
    call sys$setast(%val(0))		# disable asts
    if ( status != %loc(ss$_normal) )
	call ioerror ( "iolb$iogchar: sys$qiow() - 0x%x", status )

    return ( ch )

end


###############################################################################
#
###  iogtimed - get a character with timeout
#
#  SYNOPSIS
#    logical gotone, iogtimed
#    character ch
#    integer seconds
#    gotone = iogtimed ( ch, seconds )
#
logical function iogtimed ( ch, seconds )
NOIMPLICIT
character ch
integer seconds

    integer status, sys$qiow
    integer*2 iosb(4)
    external io$_readlblk, io$m_timed, ss$_normal, ss$_timeout
    include "ioncom"

    # This is a good place to flush the output buffer.
    if ( autobroad )
	call iomsgflush
    call ioflush

    call sys$setast(%val(1))		# enable asts
    # iogchar() and iogtimed() use a separate event flag...
    status = sys$qiow ( %val(cevfl), %val(chan),
	%val(%loc(io$_readlblk) + %loc(io$m_timed)), iosb, , ,
	ch, %val(1), %val(seconds), %val(0), , )
    call sys$setast(%val(0))		# disable asts

    if ( status == %loc(ss$_normal) )
	status = iosb(1)
    if ( status == %loc(ss$_timeout) )
	return ( .false. )
    if ( status != %loc(ss$_normal) )
	call ioerror ( "iolb$iogtimed: sys$qiow() - 0x%x", status )

    return ( .true. )

end

