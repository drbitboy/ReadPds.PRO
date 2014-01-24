
PRO dropcomment,s,LS
  ;;; Drop trailing regex '/[*].*$'
  ;;; - DO NOT CALL THIS WHILE IN BETWEEN QUOTES (stateStack[0]==wantendq)
  ;;; Ensure it is not preceded by a quote or an earlier /*
  precomment = stregex(s, '^[^/"]*/[*]', /EXTRACT )
  IF precomment EQ '' THEN BEGIN
    ;;; No match yet; check the same thing but without a preceding /
    precomment = stregex(s, '^[^"]*/[*]', /EXTRACT )
  ENDIF
  IF precomment NE '' THEN BEGIN
    LS = STRLEN(precomment)-2L
    s = STRMID(precomment,0,LS)
  ENDIF
  RETURN
END

PRO pushstack, stack, newstack
  stack = [newstack, stack]
  RETURN
END

PRO popstack, stack, countArg
  count = N_ELEMENTS(countArg) EQ 1L ? countArg[0] : 1L
  stack = stack[count:*]
  RETURN
END

PRO parsepds, lun, debug=debug

  doDebug = KEYWORD_SET(debug)

  amdone = 0L                   ;;; Done, got the END
  wantke = amdone + 1L         ;;; Waiting for KEYWORD =
  wantv = wantke + 1L           ;;; Waiting for value
  wantvsu = wantv + 1L          ;;; Waiting for value or sorted list or unsorted list
  wantends = wantvsu + 1L       ;;; Waiting for end of sorted list, close paren or comma
  wantendu = wantends + 1L      ;;; Waiting for end of unsorted list, close curly brace or comma
  wantendq = wantendu + 1L      ;;; Waiting for end of quoted string

  unitsSfx = '( *<[-+/*A-Z0-9 ]+>)?'

  NL = string(10b)    ;;; newline
  CR = string(13b)    ;;; carriage return

  catcherr = 0L
  IF NOT doDebug THEN CATCH,catcherr

  LS = -1L
  stateStack = [ wantke, amdone ]

  lineno = 0L

  WHILE catcherr EQ 0L AND stateStack[0] NE amdone DO BEGIN

    state0 = stateStack[0]

    ;;; Read next line if LS == -1
    IF LS eq -1L THEN BEGIN
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
      currStr = stringCount EQ 0L ? '' : (currStr + NL)
      currStr = currStr + sMatch
      stringCount += 1

      LsM = STRLEN(sMatch)
      ;;; If we matched the whole string, force a readf on the next pass
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
      popstack, stateStack, 2

      ;;; Drop any trailing comment from the string, and if the new
      ;;; state is wantke, then a trimmed s should  be empty
      dropcomment,s,LS
      IF stateStack[0] EQ wantke AND STRTRIM(s) NE '' THEN MESSAGE,'Bad data <' + s + '> trailing quoted string'

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
      IF STREGEX(s,'^END$') eq 0L THEN BEGIN
        popstack, stateStack
        CONTINUE
      ENDIF

      match = STREGEX(s, '^([A-Z][A-Z0-9_]*:)?[A-Z][A-Z0-9_]* *= *', /EXTRACT)
      IF match EQ '' THEN match = STREGEX(s, '^[\^][A-Z][A-Z0-9_]* *= *', /EXTRACT)
      IF match EQ '' THEN MESSAGE,'Bad data <' + s + '> looking for KEYWORD ='

      IF doDebug THEN BEGIN 
        keyword=STRTRIM(STREGEX(match,'^[^=]*',/EXTRACT),2)
        help,keyword
      ENDIF

      s = strmid(s,STRLEN(match))

      pushstack, stateStack, wantvsu

      CONTINUE
      
    ENDIF

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; Transitioned from wantke, now want { or ( or value
    IF state0 EQ wantvsu THEN BEGIN
      s0 = STRMID(s,0,1)

      ;;; We must transition to a new state here, drop old stateStack[0]
      popstack, stateStack

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
      ;;; Check for string
      IF s0 EQ '"' THEN BEGIN
        pushstack, stateStack, wantendq
        stringCount = 0L
        s = STRMID(s,1)
        CONTINUE
      ENDIF

      ;;; Check for UTC date with optional time
      sDate = STREGEX( s, '^[0-9][0-9][0-9][0-9]-(0[1-9]|1[012])-([012][0-9]|3[01])(T([01][0-9]|2[0-3])(:[0-5][0-9](:([0-5][0-9]|60)([.][0-9]*)?)?)?)?', /EXTRACT )
      IF sDate NE '' THEN BEGIN
        IF doDebug THEN help,sDate
        s = STRMID(s,STRLEN(sDate))
        popstack, stateStack
        CONTINUE
      ENDIF

      ;;; Preliminary check for floating-point value; may be an integer
      sDbl = STREGEX( s, '^[-+]? *[0-9]+([.][0-9]*)?([E][-+]?[0-9]+)?'+unitsSfx, /EXTRACT, /FOLD_CASE)

      ;;; Check that for integer
      sInt = STREGEX( s, '2#[-+]?[01]+#'+unitsSfx, /EXTRACT)
      IF sInt EQ '' THEN sInt = STREGEX( s, '8#[-+]?[0-7]+#'+unitsSfx, /EXTRACT)
      IF sInt EQ '' THEN sInt = STREGEX( s, '10#[-+]?[0-9]+#'+unitsSfx, /EXTRACT)
      IF sInt EQ '' THEN sInt = STREGEX( s, '16#[-+]?[0-9A-F]+#'+unitsSfx, /EXTRACT)
      IF sInt EQ '' THEN sInt = STREGEX( sDbl, '^[-+]? *[0-9]+'+unitsSfx+'$', /EXTRACT)
      IF sInt NE '' THEN BEGIN
        IF doDebug THEN help,sInt
        s = STRMID(s,STRLEN(sInt))
        popstack, stateStack
        CONTINUE
      ENDIF

      ;;; Check for different form of floating-point value
      IF sDbl EQ '' then sDbl = STREGEX( s, '^[-+]? *[.][0-9]+([eE][-+]?[0-9]+)?)'+unitsSfx, /EXTRACT, /FOLD_CASE)
      IF sDbl NE '' THEN BEGIN
        IF doDebug THEN help,sDbl
        s = STRMID(s,STRLEN(sDbl))
        popstack, stateStack
        CONTINUE
      ENDIF

      ;;; Must be an unquoted string of A-Z, 0-9 and _
      sString = STREGEX( s, '^[A-Z][A-Z0-9_]*($|[ ,)}])', /EXTRACT)
      if sString NE '' THEN sString = STREGEX( s, '^[A-Z][A-Z0-9_]*', /EXTRACT)
      IF sString NE '' THEN BEGIN
        IF doDebug THEN help,sString
        s = STRMID(s,STRLEN(sString))
        popstack, stateStack
        CONTINUE
      ENDIF

      MESSAGE,'Bad data <' + s + '> looking for value'

    ENDIF

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; Looking for either a comma or the end of a sorted list 
    IF state0 EQ wantends THEN BEGIN
      IF STREGEX(s,'^[,)]') NE 0L THEN MESSAGE,'Bad data <' + s + '> looking for comma or end of sorted list'
      IF STRMID(s,0,1) EQ ',' THEN pushstack, stateStack, wantv ELSE popstack, stateStack
      s = STRMID(s,1)
      CONTINUE
    ENDIF

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; Looking for either a comma or the end of an unsorted list 
    IF state0 EQ wantendu THEN BEGIN
      IF STREGEX(s,'^[,}]') NE 0L THEN MESSAGE,'Bad data <' + s + '> looking for comma or end of unsorted list'
      IF STRMID(s,0,1) EQ ',' THEN pushstack, stateStack, wantv ELSE popstack, stateStack
      s = STRMID(s,1)
      CONTINUE
    ENDIF

  ENDWHILE
  catch,/cancel
  IF doDebug THEN help,/st,!ERROR_STATE
  IF doDebug THEN help,stateStack
  print,stateStack

  RETURN
END
