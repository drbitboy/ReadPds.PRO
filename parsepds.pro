;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO dropcomment,s,LS
  ;;; Drop trailing regex '/[*].*$'
  ;;; - DO NOT CALL THIS WHILE IN BETWEEN QUOTES (peekstate(stateStack)==wantendq)
  ;;; Ensure it is not preceded by a quote or an earlier / and * pair
  precomment = stregex(s, '^[^/"]*[/][*]', /EXTRACT )  ;;; "] to protect VIM
  IF precomment EQ '' THEN BEGIN
    ;;; No match yet; check the same thing but without a preceding /
    precomment = stregex(s, '^[^"]*[/][*]', /EXTRACT ) ;;; "] to protect VIM
  ENDIF
  IF precomment NE '' THEN BEGIN
    LS = STRLEN(precomment)-2L
    s = STRMID(precomment,0,LS)
  ENDIF
  RETURN
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple stack (LIFO) implementation
;;; N.B. When adding input array of items, first item in array (index 0)
;;;      will be first pushed onto, and last popped off, the stack
;;; N.B. No error checking
;;; N.B. Need a guard element as the first-pushed element on the stack
;;;      as the stack is implemented as a G/IDL array of like objects
;;;      and popping that first-pushed item will generate an error when
;;;      it tries to empty that array, which is not allowed in G/IDL

PRO pushstack, stack, newstack, INIT=init
  IF KEYWORD_SET(init) THEN BEGIN
    stack = [newstack]           ;;; Create new array
  ENDIF ELSE BEGIN
    stack = [newstack, stack]    ;;; Push onto front of array 
  ENDELSE
  RETURN
END
FUNCTION popstack, stack, countArg, PEEK=peek

  ;;; If countArg is not specified, default to taking one element
  count = N_ELEMENTS(countArg) EQ 1L ? countArg[0] : 1L

  ;;; If countArg is not specified, then make popped a scalar,
  ;;; else make popped an array even if countArg is 1
  IF N_ELEMENTS(countArg) EQ 0 THEN popped = stack[0] $
  ELSE popped = stack[0:count-1L]

  ;;; If ,/PEEK is not specified, drop first count items from stack
  IF NOT KEYWORD_SET(peek) THEN stack = stack[count:*]

  RETURN, popped
END

;;; Wrapper function for popstack,...,/PEEK
FUNCTION peekstack, stack, countArg
  RETURN,popstack(stack, countArg, /PEEK)
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parse PDS label opened as LUN=lun
PRO parsepds, lun, debug=debugArg, doubleDebug=doubleDebugArg

  doubleDebug = KEYWORD_SET(doubleDebugArg)
  doDebug = KEYWORD_SET(debugArg) OR doubleDebug

  IF doDebug THEN MESSAGE,/RESET

  amdone = 0L                   ;;; Done, got the END
  wantke = amdone + 1L          ;;; Waiting for KEYWORD =
  wantv = wantke + 1L           ;;; Waiting for value
  wantvsu = wantv + 1L          ;;; Waiting for value or sorted list or unsorted list
  wantends = wantvsu + 1L       ;;; Waiting for end of sorted list, close paren or comma
  wantendu = wantends + 1L      ;;; Waiting for end of unsorted list, close curly brace or comma
  wantendq = wantendu + 1L      ;;; Waiting for end of quoted string

  ;;; UNITS suffix (e.g. <degree> or <BYTES>) may follow any number

  unitsSfx = '( *<[-+/A-Z0-9* ]+>)?'     ;;; : Help VIM syntax highlighting (VSH), fooled by paren-query

  NL = string(10b)    ;;; newline
  CR = string(13b)    ;;; carriage return

  lineno = 0L

  catcherr = 0L
  IF NOT doDebug THEN CATCH,catcherr

  LS = -1L

  currentKeyword = '*'

  ;;; Initial state stack; amdone is first-pushed, never-removed guard
  ;;; item on stack.  Loop will finish when wantke is popped off
  ;;; and only amdone is left; wantke is popped off when END is hit.
  ;;; Other possible ending is EOF e.g. in ^STRUCTURE file

  pushstack, stateStack, [ wantke, amdone ], /INIT

  ;;; Initial file stack; put unusable file state as first-pushed,
  ;;; never-removed guard item on stack.  When ^STRUCTURE keyword is
  ;;; hit, the value(s) as filenames will be designate files to
  ;;; be opened and added to the stack 

  pushstack, fileStack, filestate(), /INIT

  WHILE catcherr EQ 0L AND peekstack(stateStack) NE amdone DO BEGIN

    state0 = peekstack(stateStack)

    ;;; If state is wantke and the current keyword is ^STRUCTURE, then
    ;;; one or more values are on the keyword-value stack (kvStack);
    ;;; valueCount will be the number of values read with ^STRUCTURE;
    ;;; the last value read will be the first one popped off kvStack.
    IF currentKeyword EQ '^STRUCTURE' AND state0 EQ wantke THEN BEGIN
      ;;; While valueCount is not zero:
      ;;; 1) Get the current source lun file state, closing the lun
      ;;;    using the filestate function
      ;;;    N.B filestate closes by default if ,/NOCLOSE not specified
      ;;; 1.1) filestate(lun).NAME is path to source file
      ;;; 2) Push that file state onto the file state stack
      ;;; 3) Pop a value (case-insensitive basename) off kvStack
      ;;; 3.1) Resolve basename as a PDS pointer to filepath
      ;;; 3.1.1) in source file dir or in ((../)+){label,LABEL}
      ;;; 3.2) Open resolved filepath as lun
      ;;; 3) Decrement valueCount
      WHILE valueCount GT 0L DO BEGIN
        oldfst = filestate(lun)
        pushstack, fileStack, oldfst
        newbn = popstack(kvStack)
        newfn = resolvepdspointer(newbn,oldfst.name,currentKeyword)
        if doDebug then print,'Opening file '+newbn+':  '+newfn
        openr, lun, newfn
        valueCount -= 1
      ENDWHILE
    ENDIF

    ;;; Read next line if LS == -1
    IF LS eq -1L THEN BEGIN

      IF EOF(lun) THEN BEGIN

        ;;; If current lun is at End-Of-File, pop file state off of
        ;;; file stack and use it

        filstat = popstack(fileStack)  ;;; pop scalar
        lun = filestate(filstat)       ;;; re-opens & seeks (POINT_LUN)
      ENDIF
      s = ''
      READF,lun,s
      lineno += 1L
    ENDIF

    LS = STRLEN(s)

    ;;; Remove trailing carriage return
    IF LS GT 0L AND STRMID(s,LS-1) EQ CR THEN BEGIN
      LS -= 1
      s = STRMID(s,0,LS)
    ENDIF

    ;;; Trim any trailing space
    s = STRTRIM(s)
    LS = STRLEN(s)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; State is processing quoted string, possibly multi-line
    IF state0 EQ wantendq THEN BEGIN

      ;;; Match leading non-quotes in s as string, add them to current string (currStr)
      sMatch = STREGEX(s,'^[^"]*',/EXTRACT)

      ;;; "] added to help VSH

      currStr = stringLineCount EQ 0L ? '' : (currStr + NL)
      currStr = currStr + sMatch
      stringLineCount += 1

      LsM = STRLEN(sMatch)
      ;;; If we matched the whole string, force a READF on the next pass
      IF LsM EQ LS THEN BEGIN
        LS = -1L
        CONTINUE
      ENDIF

      IF doDebug THEN help,currStr
      ;;; We did not match the whole string, the next char is the closing
      ;;; quote, so cut the matched string plus the quote from s
      s = STRMID(s,LsM+1)
      LS -= LsM + 1

      ;;; Drop wantendq and wantv from state stack
      ignore = popstack(stateStack, 2)

      ;;; Add value to keyword-value stack
      pushstack, kvStack, currStr
      valueCount += 1

      ;;; Drop any trailing comment from the string, and if the new
      ;;; state is wantke, then a trimmed s should  be empty
      dropcomment,s,LS
      IF peekstack(stateStack) EQ wantke AND STRTRIM(s) NE '' THEN MESSAGE,'Bad data <' + s + '> trailing quoted string'

      ;;; Start over; for wantke state or blank s this will be the same
      ;;; as a drop-through from here and a read of the next line
      CONTINUE

    ENDIF

    ;;; Not in a quoted string, truncate any comment, trim leading/trailing space
    dropcomment,s,LS
    s = STRTRIM(s,2)

    IF s EQ '' THEN BEGIN
      ;;; if s is empty, read the next line
      LS = -1L
      CONTINUE
    ENDIF

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; Want KEYWORD =, or END
    IF state0 EQ wantke THEN BEGIN

      ;;; If END is found, then pop wantke off stack and loop back, which
      ;;; will result in exit of loop

      IF STREGEX(s,'^END$') eq 0L THEN BEGIN
        ignore = popstack(stateStack)
        CONTINUE
      ENDIF

      ;;; If END_OBJECT/_GROUP is found without = ... then just take it

      IF STREGEX(s,'^END_(OBJECT|GROUP)$') eq 0L THEN BEGIN
        currentKeyword = s
        missing = 'NO = <objectname> FOLLOWS'
        IF doDebug THEN help,currentKeyword,missing
        s = ''
        CONTINUE
      ENDIF

      match = STREGEX(s, '^([A-Z][A-Z0-9_]*:)?[A-Z][A-Z0-9_]* *= *', /EXTRACT)     ;;; ( VSH

      IF match EQ '' THEN match = STREGEX(s, '^[\^][A-Z][A-Z0-9_]* *= *', /EXTRACT)
      IF match EQ '' THEN MESSAGE,'Bad data <' + s + '> looking for KEYWORD ='

      currentKeyword=STRTRIM(STREGEX(match,'^[^=]*',/EXTRACT),2)
      IF doDebug THEN help,currentKeyword

      ;;; Create a stack of strings, the first-pushed item on the stack
      ;;; is the keyword, which may be peeked but will never be popped;
      ;;; as values are read they will be pushed onto the stack
      pushstack, kvStack, currentKeyword, /INIT
      valueCount = 0L

      s = strmid(s,STRLEN(match))

      pushstack, stateStack, wantvsu

      CONTINUE
      
    ENDIF

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; Transitioned from wantke, now want { or ( or value ;;; } VSH
    IF state0 EQ wantvsu THEN BEGIN
      s0 = STRMID(s,0,1)

      ;;; We must transition to a new state here, pop wantvsu off stateStack
      ignore = popstack(stateStack)

      IF s0 EQ '(' THEN BEGIN
        s = STRMID(s,1)
        pushstack, stateStack, wantends
      ENDIF ELSE IF s0 EQ '{' THEN BEGIN
        s = STRMID(s,1)
        pushstack, stateStack, wantendu
      ENDIF

      ;;; And there must be a value
      pushstack, stateStack, wantv
      CONTINUE
    ENDIF
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; Looking for a value
    IF state0 EQ wantv THEN BEGIN

      s0 = STRMID(s,0,1)
      ;;; Check for quote indicating start of quoted string

      IF s0 EQ '"' THEN BEGIN

        stringLineCount = 0L

        ;;; Set state to 'want end-quote', drop leading quote from s
        pushstack, stateStack, wantendq
        s = STRMID(s,1)

        ;;; loop back to start
        CONTINUE
      ENDIF

      ;;; Check for UTC date with optional time
      sDate = STREGEX( s, '^[0-9][0-9][0-9][0-9]-(0[1-9]|1[012])-([012][0-9]|3[01])(T([01][0-9]|2[0-3])(:[0-5][0-9](:([0-5][0-9]|60)([.][0-9]*)?)?)?)?Z?', /EXTRACT )     ;;; ( VSH

      IF sDate NE '' THEN BEGIN
        IF doDebug THEN help,sDate
        s = STRMID(s,STRLEN(sDate))
        ignore = popstack(stateStack)
        pushstack, kvStack, sDate
        valueCount += 1
        CONTINUE
      ENDIF

      ;;; Preliminary check for floating-point (fp) value
      ;;; N.B. may be an integer; check later
      ;;; N.B. Needs to be terminated by one of: 
      ;;;      end of line; space; comma; close brace; close parentheses
      sDbl = STREGEX( s, '^[-+]? *[0-9]+([.][0-9]*)?([E][-+]?[0-9]+)?'+unitsSfx+'($|[ ,})])', /EXTRACT, /FOLD_CASE)

      ;;; Strip off termination character
      IF sDbl NE '' THEN BEGIN
        iwLastDelimiter = STREGEX(sDbl,'[ ,})]$')
        IF iwLastDelimiter GT 0L THEN sDbl = STRMID(sDbl,0,iwLastDelimiter)
      ENDIF

      ;;; Check for integer
      sInt = STREGEX( s, '2#[-+]?[01]+#'+unitsSfx, /EXTRACT)
      IF sInt EQ '' THEN sInt = STREGEX( s, '8#[-+]?[0-7]+#'+unitsSfx, /EXTRACT)
      IF sInt EQ '' THEN sInt = STREGEX( s, '10#[-+]?[0-9]+#'+unitsSfx, /EXTRACT)
      IF sInt EQ '' THEN sInt = STREGEX( s, '16#[-+]?[0-9A-F]+#'+unitsSfx, /EXTRACT)      ;;; ( VSH

      ;;; Special check of fp value; it may be an integer (no . or e or E)

      IF sInt EQ '' THEN sInt = STREGEX( sDbl, '^[-+]? *[0-9]+'+unitsSfx+'$', /EXTRACT)    ;;; ( VSH

      ;;; If an integer, save it, pop wantv state, loop back to start
      IF sInt NE '' THEN BEGIN
        IF doDebug THEN help,sInt
        s = STRMID(s,STRLEN(sInt))
        ignore = popstack(stateStack)
        pushstack, kvStack, sInt
        valueCount += 1
        CONTINUE
      ENDIF

      ;;; Check for different form of fp value
      IF sDbl EQ '' then sDbl = STREGEX( s, '^[-+]? *[.][0-9]+([eE][-+]?[0-9]+)?)'+unitsSfx, /EXTRACT, /FOLD_CASE)     ;;; ( VSH
      ;;; If an fp, save it, pop wantv state, loop back to start
      IF sDbl NE '' THEN BEGIN
        IF doDebug THEN help,sDbl
        s = STRMID(s,STRLEN(sDbl))
        ignore = popstack(stateStack)
        pushstack, kvStack, sDbl
        valueCount += 1
        CONTINUE
      ENDIF

      ;;; Not a quoted string, date-time, integer or floating point; must be an
      ;;; unquoted string made up of of [A-Z0-9_]

      sString = STREGEX( s, '^[A-Z0-9_]+($|[ ,)}])', /EXTRACT)      ;;; ( VSH
      if sString NE '' THEN sString = STREGEX( s, '^[A-Z0-9_]+', /EXTRACT)
      IF sString NE '' THEN BEGIN
        IF doDebug THEN help,sString
        s = STRMID(s,STRLEN(sString))
        ignore = popstack(stateStack)
        pushstack, kvStack, sString
        valueCount += 1
        CONTINUE
      ENDIF

      MESSAGE,'Bad data <' + s + '> looking for value'

    ENDIF

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; Looking for either a comma or the end of a sorted list 
    IF state0 EQ wantends THEN BEGIN
      IF STREGEX(s,'^[,)]') NE 0L THEN MESSAGE,'Bad data <' + s + '> looking for comma or end of sorted list'
      IF STRMID(s,0,1) EQ ',' THEN BEGIN
        pushstack, stateStack, wantv
      ENDIF ELSE BEGIN
        ignore = popstack(stateStack)
      ENDELSE
      s = STRMID(s,1)
      CONTINUE
    ENDIF

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; Looking for either a comma or the end of an unsorted list
    IF state0 EQ wantendu THEN BEGIN
      IF STREGEX(s,'^[,}]') NE 0L THEN MESSAGE,'Bad data <' + s + '> looking for comma or end of unsorted list' ;;; ( VSH
      IF STRMID(s,0,1) EQ ',' THEN BEGIN
        pushstack, stateStack, wantv
      ENDIF ELSE BEGIN
        ignore = popstack(stateStack)
      ENDELSE
      s = STRMID(s,1)
      CONTINUE
    ENDIF

    MESSAGE,'Bad state <' + strtrim(state0,2) + '>'

  ENDWHILE

  catch,/cancel
  IF doubleDebug OR (doDebug AND catcherr NE 0L) THEN begin
    help,/st,!ERROR_STATE
    help,lineno,stateStack
    print,stateStack
  ENDIF

  RETURN
END
