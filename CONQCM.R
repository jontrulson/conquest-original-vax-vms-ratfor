###############################################################################
#
#                               C O N Q C M
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


###  block data subprogram for conquest
#
block data conqcm$data
NOIMPLICIT

    integer commonrev
    byte amorphousmassofdata(SIZEOF_COMMONBLOCK-4)
    common /conqcom/ commonrev, amorphousmassofdata

    data commonrev / -1 /

end
