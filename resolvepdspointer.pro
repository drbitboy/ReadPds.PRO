
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Returns path to file if found, else return ''

function rpp_test, testdir, upperAndLowerBasename
  ps = PATH_SEP()
  FOR i=0L,N_ELEMENTS(upperAndLowerBasename)-1L DO BEGIN
    fnpath = testdir + ps + upperAndLowerBasename[i]
    IF NOT FILE_TEST(fnpath,/READ) THEN CONTINUE
    IF FILE_TEST(fnpath,/REGULAR) THEN RETURN,fnpath
  ENDFOR
  RETURN, ''
END

FUNCTION resolvepdspointer,basenameArg,sourcefilepath,pdskeyword $
       , debug=debug

  doDebug = KEYWORD_SET(debug)

  ps = PATH_SEP()

  ;;; Start in directory with source file that has pointer in it

  testdir = FILE_DIRNAME(sourcefilepath)

  ;;; Build uppercase and lowercase names; put them in an array

  upper = STRUPCASE(basenameArg)
  lower = STRLOWCASE(basenameArg)
  sbn = FILE_BASENAME(sourcefilepath)
  ulbn = sbn EQ strupcase(sbn) ? [upper,lower] : [lower,upper]

  ;;; Test if pointer file is in same dir with source file

  rtn = rpp_test(testdir,ulbn)
  IF rtn THEN RETURN, rtn

  ;;; If not, determine name of subdir to look at for pointer file
  ;;; (default=document) in both upper and lower case

  IF KEYWORD_SET(pdskeyword) THEN BEGIN

    CASE pdskeyword[0] OF
    '^STRUCTURE': tmp = 'label'
    '^CATALOG': tmp = 'catalog'
    '^DATA_SET_MAP_PROJECTION': tmp = 'catalog*'
    '^INDEX_TABLE': tmp = 'index'
    ELSE: tmp = 'document'
    ENDCASE

    subdirup = STRUPCASE(tmp)
    subdirlo = STRLOWCASE(tmp)
    IF doDebug THEN help, ulbn , upper , subdirup,subdirlo ,subdirlo,subdirup
    subdirSave = ulbn[0] EQ upper ? [subdirup,subdirlo] : [subdirlo,subdirup]
  ENDIF ELSE BEGIN
    ;;; Empty string means use no subdir
    subdirSave = ''
  ENDELSE

  ;;; Save representation of PWD; will be converted to absolute later

  dot = file_dirname('')   ;;; Convert relative dir . to absolute

  if doDebug then print,'subdirSave:',subdirSave

  ;;; Save directories which were already tried, and recursively
  ;;; ascend parent directories, looking for file

  testeddirs = [testdir]

  WHILE 1b DO BEGIN

    ;;; Go up one directory level from last dir and try again

    testdir = FILE_DIRNAME(testeddirs[0])

    IF testdir EQ dot THEN BEGIN
      ;;; Convert dot to absolute path if and when we get there
      testdir = (file_expand_path(dot))[0]

    ENDIF ELSE BEGIN
      ;;; Exit loop if we have checked this dir before
      iw = where( testeddirs EQ testdir, iwct)
      IF iwct GT 0L THEN BREAK
    ENDELSE

    ;;; Add this testdir to testeddirs so we do not test it again

    testeddirs = [testdir,testeddirs]

    if doDebug then print,'testeddirs:',testeddirs

    ;;; Loop over possible subdirs (e.g. [LABEL, label])

    FOR iDir=0L,N_ELEMENTS(subdirSave)-1L DO BEGIN

      subdir = subdirSave[iDir]

      IF subdir EQ '' THEN BEGIN
        ;;; If subdir is empty string, only check in testdir
        dirsToTest = [testdir]

      ENDIF ELSE BEGIN
        ;;; Find subdirs that match subdir, store in dirsToTest array

        dirsToTest = FILE_SEARCH(testdir + ps + subdir)
        IF dirsToTest[0] EQ '' THEN CONTINUE

        ;;; Filter dirsToTest:

        FOR iTest=0L,N_ELEMENTS(dirsToTest)-1L DO BEGIN

          ;;; Any results that are not directories are excluded by
          ;;; setting their array element to ''

          IF NOT FILE_TEST(dirsToTest[iTest],/DIR) THEN dirsToTest[iTest] = ''

        ENDFOR  ;;; dirsToTest

        ;;; Only keep the non-empty subdirs; CONTINUE to next subdirSave

        iw = WHERE( dirsToTest NE '', iwct)
        IF iwct EQ 0L THEN CONTINUE
        dirsToTest = dirsToTest[iw]

      ENDELSE

      FOR iTest=0L,N_ELEMENTS(dirsToTest)-1L DO BEGIN
        rtn = rpp_test(dirsToTest[iTest],ulbn)
        IF rtn THEN RETURN, rtn
      ENDFOR

    ENDFOR  ;;; subdirSave

  ENDWHILE

  RETURN, ''
END
