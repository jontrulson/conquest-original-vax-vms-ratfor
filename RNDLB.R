###############################################################################
#
#                               R N D L B
#
###############################################################################
#
# Version Date Initials Remarks
# ------- ---- -------- -------------------------------------------------------
#   02G  26jul84  JP	Changed use of gtime() to getnow().
#
#   02F  12jul83  JIC   Ratfixed.
#                       Added code so that the "include rndlb.c" could be
#                       removed.
#   02E  17apr83  JP    Fixed a bug in rndnor introduced in version 02D.
#
#   02D  06apr83  JP    Fixed a bug in rndexp and sped up rndexp, rndnor,
#                       and rndgeo.
#
#   02C  19mar82  JP    Fixed three bugs in rndnor.
#
#   02B  08mar82  JP    Got rid of rndmod - plain old mod is used now.
#                       Turned integers into integer*4s.
#
#   02A  02mar82  JP    Changed a, c, and m for both sequences.
#                       Should now be "portable".
#
#   01B  02feb82  JP    Changed TWOTOTHE31MINUS1 into HALFMODULUS.
#                       Changed REALMODULUS to realmodulus.
#
#   01A  18nov80  JP    First written.
#
###############################################################################
#
#  All the algorithms in this package are from "The Art of Computer
#  Programming", Vol. 2 (Seminumerical Algorithms), sections 3.2.1 and
#  3.4.1.
#
###############################################################################


define(TABLESIZE,200)
define( rndlb_common, common /rndcom/ value1, value2, mult1, mult2, inc1, inc2,
modu1, modu2, table(TABLESIZE)
integer*4 value1, value2, mult1, mult2, inc1, inc2, modu1, modu2, table ) 


###############################################################################
#
###  rndseq - internal routine to compute the next value in a linear
#             congruential sequence
#
#  SYNOPSIS
#    integer*4 value, multiplier, increment, modulus
#    call rndseq ( value, multiplier, increment, modulus )
#
#  DESCRIPTION
#
subroutine rndseq ( value, multiplier, increment, modulus )
integer*4 value, multiplier, increment, modulus

    value = mod ( value * multiplier + increment, modulus )

    return
end


###############################################################################
#
###  rndini - initialize the random number package
#
#  SYNOPSIS
#    integer seed1, seed2
#    call rndini ( seed1, seed2 )
#
#  DESCRIPTION
#      This routine must be called before any of the other routines in
#    this package can be called.  The seeds are used to initialize the
#    random numbers in a deterministic manner, so that each time you
#    initialize with the same seeds you will get the same sequence.
#    If you want non-deterministic sequences, use 0 as the seeds, and
#    the current time will be used.
#
subroutine rndini ( seed1, seed2 )
integer seed1, seed2

    integer idx
    integer now(7)
    rndlb_common

    # Here are the multipliers, increments, and moduli for the two sequences.
    # Do not change these frivously!  They have been very carefully selected,
    # using the "spectral test" and various other empirical tests of
    # randomness.  Each sequence, by itself, is better than most random
    # sequences currently in use.  Together, they are awesome...
    mult1 = 1541;	inc1 = 3501;	modu1 = 16384
    mult2 = 5146;	inc2 = 4100;	modu2 = 19683

    call getnow ( now )

    if ( seed1 != 0 )
        value1 = seed1
    else
        value1 = ( ( now(3) * 10 + now(4) ) * 10 + now(5) ) * 10 + now(6)
    value1 = mod ( value1, modu1 )

    if ( seed2 != 0 )
        value2 = seed2
    else
        value2 = ( ( now(6) * 10 + now(5) ) * 10 + now(4) ) * 10 + now(3)
    value2 = mod ( value2, modu2 )

    for ( idx=1; idx <= TABLESIZE; idx=idx+1 )
	{
	call rndseq ( value2, mult2, inc2, modu2 )
        table(idx) = value2
	}

    return
end


###############################################################################
#
###  rnd - random real number in the range [0..1)
#
#  SYNOPSIS
#    real r, rnd
#    r = rnd(0)
#
real function rnd ( dummy )
integer dummy

    integer idx
    rndlb_common

    call rndseq ( value1, mult1, inc1, modu1 )
    idx = ifix ( float(value1) / float(modu1) * TABLESIZE ) + 1
    rnd = float(table(idx)) / float(modu2)
    call rndseq ( value2, mult2, inc2, modu2 )
    table(idx) = value2

    return
end


###############################################################################
#
###  rnduni - random real number in the specified range
#
#  SYNOPSIS
#    real rlow, rhigh, r, rnduni
#    r = rnduni ( rlow, rhigh )
#
real function rnduni ( rlow, rhigh )
real rlow, rhigh

    real rnd

    rnduni = rnd(0) * (rhigh-rlow) + rlow

    return
end


###############################################################################
#
###  rndint - random integer in the specified range
#
#  SYNOPSIS
#    integer ilow, ihigh, i, rndint
#    i = rndint ( ilow, ihigh )
#
integer function rndint ( ilow, ihigh )
integer ilow, ihigh

    real rnd

    rndint = ifix ( rnd(0) * float(ihigh-ilow+1) ) + ilow

    return
end


###############################################################################
#
###  rndnor - normally distributed random real number
#
#  SYNOPSIS
#    real mean, stddev, r, rndnor
#    r = rndnor ( mean, stddev )
#
real function rndnor ( mean, stddev )
real mean, stddev

    real rnd, v1, v2, z

    repeat
        {
        v1 = -alog(1.0-rnd(0))
        v2 = -alog(1.0-rnd(0))
        }
    until ( 2.0*v1 >= (v2-1.0)**2 )

    if ( rnd(0) > 0.5 )
        z = 1.0
    else
        z = -1.0
    rndnor = stddev * z * v2 + mean

    return
end


###############################################################################
#
###  rndexp - exponentially distributed random real number
#
#  SYNOPSIS
#    real mean, r, rndexp
#    r = rndexp ( mean )
#
real function rndexp ( mean )
real mean

    real rnd

    rndexp = - alog ( 1.0 - rnd(0) ) * mean

    return
end


###############################################################################
#
###  rndchi - random real number with the chi-square distribution
#
#  SYNOPSIS
#    integer v
#    real r, rndchi
#    r = rndchi ( v )
#
#  DESCRIPTION
#      This distribution is also known as the gamma distribution of order v/2.
#
real function rndchi ( v )
integer v

    integer k, x
    real rndexp, rndnor

    k = v / 2

    rndchi = 0.0
    for ( x=1; x <= k; x=x+1 )
        rndchi = rndchi + rndexp ( 1.0 )
    rndchi = rndchi * 2.0

    if ( k * 2 + 1 == v )
        rndchi = rndchi + rndnor(0.0,1.0)**2

    return
end


###############################################################################
#
###  rndbta - random real number with the beta distribution
#
#  SYNOPSIS
#    integer v1, v2
#    real r, rndbta
#    r = rndbta ( v1, v2 )
#
real function rndbta ( v1, v2 )
integer v1, v2

    real y1, y2, rndchi

    y1 = rndchi ( v1 )
    y2 = rndchi ( v2 )
    rndbta = y1 / (y1+y2)

    return
end


###############################################################################
#
###  rndF - random real number with the F-distribtion
#
#  SYNOPSIS
#    integer v1, v2
#    real r, rndF
#    r= rndF ( v1, v2 )
#
#  DESCRIPTION
#      This distribution is also known as the variance-ratio distribution.
#
real function rndF ( v1, v2 )
integer v1, v2

    real y1, y2, rndchi

    y1 = rndchi ( v1 )
    y2 = rndchi ( v2 )
    rndF = (y1*v2) / (y2*v1)

    return
end


###############################################################################
#
###  rndt - random real number with the t-distribution
#
#  SYNOPSIS
#    integer v
#    real r, rndt
#    r = rndt ( v )
#
real function rndt ( v )
integer v

    real y1, y2, rndnor, rndchi

    y1 = rndnor ( 0.0, 1.0 )
    y2 = rndchi ( v )
    rndt = y1 / sqrt ( y2/v )

    return
end


###############################################################################
#
###  rndgeo - random integer with the geometric distrbution
#
#  SYNOPSIS
#    real prob
#    integer i, rndgeo
#    i = rndgeo ( prob )
#
#  DESCRIPTION
#      If some event occurs with probability p, then the number
#    of independent trials needed until the first event occurs (or
#    between occurrences of the event) has the geometric distribution.
#
integer function rndgeo ( prob )
real prob

    real rnd

    if ( prob < 0.0 | prob > 1.0 )
        rndgeo = 0
    else if ( prob == 1.0 )
        rndgeo = 1
    else
        rndgeo = aint ( alog(1.0-rnd(0)) / alog(1.0-prob) + 0.999999 )

    return
end


###############################################################################
#
###  rndbin - random integer with the binomial distribution
#
#  SYNOPSIS
#    integer trials, i, rndbin
#    real prob
#    i = rndbin ( trials, prob )
#
#  DESCRIPTION
#      If some event occurs with probability p and if we carry out t
#    independent trials, then the total number of occurrences N equals
#    n with probability:
#                             t    n     t-n
#                            ( )  p  (1-p)
#                             n
#
integer function rndbin ( trials, prob )
integer trials
real prob

    integer i
    real rnd

    rndbin = 0
    for ( i=1; i <= trials; i=i+1 )
	if ( rnd(0) <= prob )
	    rndbin = rndbin + 1

    return
end


###############################################################################
#
###  rndpoi - random integer with the Poisson distribution
#
#  SYNOPSIS
#    real mean
#    integer i, rndpoi
#    i = rndpoi ( mean )
#
#  DESCRIPTION
#      The Poisson distribution is related to the exponential distribution
#    as the binomial distribution is related to the geometric: it represents
#    the number of occurrences, per unit time, of an event which can occur
#    at any instant of time; for example, the number of alpha particles
#    emitted by a radioactive substance in a single second has a Poisson
#    distribution.  The probability that N = n (u is the mean) is:
#
#                                -u  n
#                               e   u  /  n !
#
integer function rndpoi ( mean )
real mean

    real p, q, rnd

    p = exp ( -mean )
    rndpoi = 0
    q = 1.0

    repeat # forever
        {
        q = q * rnd(0)

        if ( q < p )
            break

        rndpoi = rndpoi + 1
        }

    return
end
