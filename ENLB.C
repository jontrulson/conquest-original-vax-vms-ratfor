    # common block for enlb

    integer file, numfields, fieldstart(MAXFIELDS), fieldlen(MAXFIELDS)
    character entry(MAXENTRY)

    common /enlbcm/ file, numfields, fieldstart, fieldlen, entry
