    # External include file for the screen package (src/sclb).
    #nolist

    # modification history
    # ------------ -------
    # V03F 03dec82 TH  Added scxonoff
    # V03E 23nov82 TH  Added scpbeep, scpdown, scnbeep, scndown
    # V03D 28sep82 TH  Added sclshigh, sccgraphics
    #                  Removed sccpillow
    # V03C 24jul82 TH  Added sccinit, scpsup, scpsdown, scpcel, scpdlc, scpdll,
    #                  scpinc, scpinl, scptab, scpadc, and corresponding 'scn'
    #                  values (also scnbol, scnclear), added scospeed, scispeed
    #                  Changed scpbol and scpclear to reals
    #                  Removed scblclear, sctabs, pscroll
    # V03B 13jul82 TH  Added sccatt, scccel, sccchscroll, sccdlc, sccdll,
    #                  scciim, sccikey, sccinc, sccinl, scckey, 
    #                  sccoim, sccokey, scpilatt, sccsdown,
    #                  sccsup, scctab, sctabs, scplins, scpcols,
    #                  scpscroll, scpbol, scpclear,
    #                  schardcopy, scblclear, scoverstrike, scwrtop,
    #                  schasatt, sccbeep.
    #                  Removed scenpill, scdepill
    # V03A 14apr82 JP  Removed sctype.
    #                  Changed names of scc variables.
    # V02B 14mar82 JP  Added scautonl and sccplw.
    # V02A 08dec80 JP

    integer sclins			# number of usable lines
    integer scplins			# number of actual lines
    integer sccols			# number of usable columns
    integer scpcols			# number of actual columns

    logical scprch ( 128 )		# printable characters
    # logical sctabs ( MAXSCCOLS )	# tab stops

    # behavior at edges
    logical scautonl			# if autonewlines
    logical scwrtop			# if last char, last line wraps to top

    # if terminal generates xon/xoff if input buffer full (for tset)
    logical scxonoff			# if generates xon/xoff if buffer full

    # how to clear characters, etc
    logical schardcopy			# hardcopy terminal
    # logical scblclear			# blank will clear a character
    logical scoverstrike		# terminal overstrikes

    # character attributes
    integer scnatt			# number of attributes strings
    integer scattact ( MAX_ATTRIBUTES,	# actions of attribute sequences
	    MAX_ATTRIBUTE_SEQUENCES )
    logical schasatt ( MAX_ATTRIBUTES )	# available attributes

    # delays
    # the following are the delays needed in milliseconds
    real scpbeep			# delay for beep
    real scpbol				# delay for bol (e.g. carriage return)
    real scpcel				# delay for clear to end of line
    real scpclear			# delay for clear (e.g. formfeed on
					# hardcopy terminal)
    real scpdlc				# delay to delete character
    real scpdll				# delay to delete line
    real scpdown			# delay for cursor down
    real scpinc				# delay to insert character
    real scpinl				# delay to insert line
    real scpsdown			# delay for scroll down
    real scpsup				# delay for scroll up
    real scptab				# delay for tab
    # the following are the number of characters to use in the delay
    integer scnbeep			# chars for beep
    integer scnbol			# chars for bol (e.g. carriage return)
    integer scncel			# chars for clear to end of line
    integer scnclear			# chars for clear (e.g. formfeed on
					# hardcopy terminal)
    integer scndlc			# chars to delete character
    integer scndown			# chars to do cursor down
    integer scndll			# chars to delete line
    integer scninc			# chars to insert character
    integer scninl			# chars to insert line
    integer scnsdown			# chars for scroll down
    integer scnsup			# chars for scroll up
    integer scntab			# chars for tab
    # the pad character
    character scpadc			# padding character

    # terminal speed (chars/ms)
    real scispeed			# the input speed
    real scospeed			# the output speed

    # control sequences
    integer sccatt ( MAX_ATTRIBUTE_SEQUENCES )
					# sequences to change attributes
    integer sccbeep			# ring bell
    integer sccbol			# go to beginning of line
    integer scccel			# clear to end of line
    integer sccchscroll			# change scrolling region
    integer sccclear			# clear screen and home
    # integer sccdepill			# exit pillow mode (soon obsolete)
    integer sccdlc			# delete character
    integer sccdll			# delete line
    integer sccdown			# cursor down
    # integer sccenpill			# enter pillow mode (soon obsolete)
    integer scchome			# home cursor
    integer scciim			# go in insert mode
    integer sccikey			# go in keypad mode
    integer sccinc			# insert character
    integer sccinit			# initialization sequence
    integer sccinl			# insert line
    integer scckey ( MAX_KEYPAD_KEYS )	# what keypad sends in keypad mode
    integer sccleft			# cursor left
    integer sccmove			# absolute cursor address
    integer sccoim			# go out of insert mode
    integer sccokey			# go out of keypad mode
    integer sccright			# cursor right
    integer sccsdown			# scroll down
    integer sccsup			# scroll up
    integer scctab			# tab
    integer sccup			# cursor up

    # line segments
    logical sclshigh			# if line segments are high resolution
    integer sccgraphics ( NUMBER_GRAPHICS_CHARS )	# graphics characters

    common /sclb/ sclins,
	scplins,
	sccols,
	scpcols,

	scprch,
	# sctabs,

	scautonl,
	scwrtop,

	scxonoff,

	schardcopy,
	# scblclear,
	scoverstrike,

	scnatt,
	scattact,
	schasatt,

	scpbeep,
	scpbol,
	scpcel,
	scpclear,
	scpdlc,
	scpdll,
	scpdown,
	scpinc,
	scpinl,
	scpsdown,
	scpsup,
	scptab,
	scnbeep,
	scnbol,
	scncel,
	scnclear,
	scndlc,
	scndll,
	scndown,
	scninc,
	scninl,
	scnsdown,
	scnsup,
	scntab,
	scpadc,

	scispeed,
	scospeed,

	sccatt,
	sccbeep,
	sccbol,
	scccel,
	sccchscroll,
	sccclear,
	# sccdepill,
	sccdlc,
	sccdll,
	sccdown,
	# sccenpill,
	scchome,
	scciim,
	sccikey,
	sccinc,
	sccinit,
	sccinl,
	scckey,
	sccleft,
	sccmove,
	sccoim,
	sccokey,
	sccright,
	sccsdown,
	sccsup,
	scctab,
	sccup,
	sclshigh,
	sccgraphics

    #list
