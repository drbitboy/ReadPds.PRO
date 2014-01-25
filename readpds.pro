;------------------------------------------------------------------------------
; NAME: READPDS
;
; PURPOSE: To read a PDS file into IDL data and label variables
;
; CALLING SEQUENCE: Result = READPDS (Filename [,/SILENT, /NOSCALE])
;
; INPUTS:
;    Filename: Scalar string containing the name of the PDS file to read
; OUTPUTS:
;    Result: PDS data structure constructed from designated record
;
; OPTIONAL INPUTS:
;    SILENT: suppresses any message from the procedure
;    NOSCALE: does not perform scaling and offset of values in image
;       or qube arrays, default is to scale and offset
;
; EXAMPLES:
;    To read a PDS file TEST.LBL into an IDL image array, img:
;       IDL> img = READPDS ("TEST.LBL",/SILENT)
;       IDL> help, /STRUCTURE, img
;            OBJECTS      INT       1
;            IMAGE        LONG     [200,200]
;    To read a PDS file with multiple objects:
;       IDL> data = READPDS ("MULTIPLE.LBL",/SILENT)
;       IDL> help, /STRUCTURE, data
;            OBJECTS      INT       2
;            TABLE        STRUCT    -> ARRAY[1]
;            IMAGE        STRUCT    -> ARRAY[1]
;
; PROCEDURES USED:
;    Functions: ARRCOLPDS, HEADPDS, OBJPDS, IMAGEPDS, TASCPDS,
;        TBINPDS, QUBEPDS.
;
; MODIFICATION HISTORY:
;    Written by: Puneet Khetarpal [24 Feb, 2003]
;    For a complete list of modifications, see changelog.txt file.
;
;    Modified by : A.Cardesin 27Apr2005: removed filename print in get_all_object
;                  A.Cardesin 04Jan2006: Added readSpreadsheet routine
;                  A.Cardesin 24Feb2006: Change IDL PATH to correct Directory
;                  S.Martinez 05Jul2010: Updated to handle BIT_COLUMN/CONTAINER
;                                        objects
;                  S.Martinez 04Dec2010: Updated to handle ASCII ARRAY and COLLECTION
;                                        objects
;
;
;------------------------------------------------------------------------------

;- level 1 --------------------------------------------------------------------

;------------------------------------------------------------------------------
; precondition: label is a viable PDS label.
; postcondition: all viable PDS object names, label indices, and count
;     are extracted from the label, and if there is an error, the flag
;     field of the returned struct is set to -1.

function get_all_objects, label
    ; initialize variable
    struct = {flag:1}

    ; extract all objects using objpds:
    obj = objpds(label, "ALL")            ; external routine
    if (obj.flag eq -1) then begin
;A.Cardesin 27-04-2005
;Modified: filename printing was incorrect. (unknown for this procedure)
        print, "Error: no viable PDS object found in file" ; + filename
        goto, endfunction
    endif

    ; check if there exist BIT_COLUMN, BIT_ELEMENT, and CONTAINER objects
    ; in the PDS file:
    
    ; 2010Jul, smartinez: Updated to handle BIT_COLUMN/CONTAINER objects
    ;bcol = objpds(label, "BIT_COLUMN")    ; external routine
    ;cont = objpds(label, "CONTAINER")     ; external routine
    belem = objpds(label, "BIT_ELEMENT")  ; external routine
    if (belem.flag eq 1) then begin 
        print, "Error: BIT_ELEMENT object" + $
               " found. Currently not supported by PDSREAD."
        goto, endfunction
    endif

    ; temporary assignment of object structure's fields:
    tmpcount = obj.count
    tmparray = obj.array
    tmpindex = obj.index

    ; go through the object indices, if multiple, and remove sub-objects:
    if (obj.count gt 0) then begin
        flag = 0
        rcount = 0

        while (~flag) do begin
            ; obtain end_index for current objarray object:
            endindex = get_index(label, tmpindex[rcount])
            if (endindex eq -1) then goto, endfunction
            ; obtain all indices where sub objects are not included:
            pos = where (tmpindex le tmpindex[rcount] or tmpindex gt endindex)
            ; set object index and object array to appropriate values:
            tmpindex = tmpindex[pos]
            tmparray = tmparray[pos]
            tmpcount = n_elements(tmparray)
            ; increment count
            rcount += 1
            ; set flag to 1 if all objects have been accounted for:
            if (rcount eq tmpcount) then begin
                flag = 1
            endif
        endwhile
    endif

    struct = create_struct(struct, "array", tmparray, "index", tmpindex, $
         "count", rcount)
    return, struct

    endfunction:
        struct.flag = -1
        return, struct
end

;------------------------------------------------------------------------------
; precondition: fname is a viable PDS file name, label is a viable PDS label,
;     st contains either the value of 0 or 1, and objindex is a valid index
;     for a table, series, spectrum, or palette object.
; postcondition: the tabular data is read from the file specified by fname
;     and returned to the main block after checking for interchange format
;     keyword.

function dotable, fname, label, st, objindex
    ; obtain interchange format keyword from label:
    inform = pdspar (label, "INTERCHANGE_FORMAT", COUNT=cnt, INDEX=index)
    if (cnt eq 0) then begin
        print, "Error: " + fname + " missing required INTERCHANGE_FORMAT " + $
               "keyword."
        return, -1
    endif

    ; determine the index of the interchange format keyword that belongs
    ; to the current tabular object:
    w = where (index gt objindex)

    ; select which subroutine to pass on the tasks:
    if (strpos (inform(w[0]),"ASCII") gt -1) then begin
        data = (~st) ? tascpds (fname, label, objindex) : $
                       tascpds (fname, label, objindex, /silent)
    endif else if (strpos (inform(w[0]),"BINARY") gt -1) then begin
        data = (~st) ? tbinpds (fname, label, objindex) : $
                       tbinpds (fname, label, objindex, /silent)
    endif else begin
        print, "Error: Invalid PDS table interchange format" + inform[0]
        return, -1
    endelse

    return, data
end

;------------------------------------------------------------------------------
; precondition: fname is a viable PDS file name, label is a viable PDS label,
;     st contains either the value of 0 or 1, and objindex is a valid index
;     for an array or collection object.
; postcondition: the array of collection data is read from the file specified 
;     by fname and returned to the main block after checking for interchange 
;     format keyword.
function doarrcol, fname, label, st, objindex
    ; obtain interchange format keyword from label:
    inform = pdspar (label, "INTERCHANGE_FORMAT", COUNT=cnt, INDEX=index)
    if (cnt eq 0) then begin
        print, "Error: " + fname + " missing required INTERCHANGE_FORMAT " + $
               "keyword."
        return, -1
    endif

    ; determine the index of the interchange format keyword that belongs
    ; to the current tabular object:
    w = where (index gt objindex)

    ; select which subroutine to pass on the tasks:
    if (strpos (inform(w[0]),"ASCII") gt -1) then begin
        data = (~st) ? arrcolascpds (fname, label, objindex) : $
                       arrcolascpds (fname, label, objindex, /silent)
    endif else if (strpos (inform(w[0]),"BINARY") gt -1) then begin
        data = (~st) ? arrcolpds (fname, label, objindex) : $
                       arrcolpds (fname, label, objindex, /silent)
    endif else begin
        print, "Error: Invalid PDS table interchange format" + inform[0]
        return, -1
    endelse

    return, data
end


;- level 0 --------------------------------------------------------------------

function readpds, filename, SILENT = silent, NOSCALE = noscale, HISTOGRAM = histogram
    ; error protection:
    on_error, 2

;A.Cardesin 24 February 2006
; FORCE IDL PATH to look first into the correct directory
; this is done to avoid name conflicts with other programs
sSBNIDLpath=FILE_DIRNAME(FILE_WHICH('readpds.pro',/INCLUDE_CURRENT_DIR))
case STRUPCASE(!version.os_name) of
   'MICROSOFT WINDOWS' : sep=';'
   'SOLARIS' : sep = ':' 
   'LINUX' : sep=':+'
   'MAC OS X' : sep=':'
   else :
   endcase
;Save current path to recover it in the end
sSavePath = !PATH
!PATH=sSBNIDLpath+sep+!PATH

    ; check for number of parameters in function call:
    if (n_params() lt 1) then begin
        print, "Syntax Error: result = READPDS (filename [,/SILENT, /NOSCALE])"
        return, -1
    endif

    ; check for silent, noscale, and histogram keyword presence:
    st      = keyword_set(silent)
    noscale = keyword_set(noscale)
    hist    = keyword_set(histogram)

    ; save file name
    savefile = filename

    ; obtain PDS label:
    label = (~st) ? headpds(filename) : headpds(filename, /silent)
    if (label[0] eq "-1") then return, -1

IF getenv("READPDS_WRITELBL") NE '' THEN BEGIN
 lun=-1L
 catcherr=0L
 catch,catcherr
 IF catcherr EQ 0L THEN BEGIN
  openw,lun,'lblsave.txt',/get_lun
  printf,lun,strjoin(label,''),f='(a)'
 ENDIF
 catch,/cancel
 IF lun GT 0L then free_lun,lun
ENDIF

    ; reset file name
    filename = savefile

    ; obtain all viable objects from label array:
    objects = get_all_objects(label)  ; subroutine
    if (objects.flag eq -1) then return, -1
    objarray = objects.array
    objindex = objects.index
    objcount = objects.count

    ; initialize the object structure
    result = create_struct("objects", objcount)

    ; create a flag variable to hold a flag for IMAGE objects whether to
    ; perform multiple object read or not
    flag_image = 1

    ;/******* start loop to populate viable objects ************************/
    for i = 0, objcount - 1 do begin
        obj = objarray[i]

        ; check for each type of OBJECT and read the individual objects:
        ; first check whether multiple object read flag is set to -1
        ; if flag eq 1 then process IMAGE objects, and set flag to 0:

        ; test to process IMAGE:
        pos = strpos(obj, "IMAGE")
        if ((pos[0] gt -1) && flag_image) then begin
            if (~st) then begin
                data = (~noscale) ? imagepds(filename, label) : $
                                    imagepds(filename, label, /noscale)
            endif else begin
                data = (~noscale) ? imagepds(filename, label, /silent) : $
                                  imagepds(filename, label, /silent, /noscale)
            endelse
            flag_image = 0
            result = create_struct (result, objarray[i], data)
        endif

        ; test to process ARRAY and COLLECTION:
        pos_array = strpos(obj, "ARRAY")
        pos_collection = strpos(obj, "COLLECTION")
        if ((pos_array gt -1) || (pos_collection gt -1)) then begin
        
            data = doarrcol (filename, label, st, objindex[i])  ; subroutine
;            data = (~st) ? arrcolpds(filename, label, objindex[i]) : $
;                           arrcolpds(filename, label, objindex[i], /silent)
            result = create_struct (result, objarray[i], data)
        endif

        ; test to process QUBE:
        if (strpos(obj, "QUBE") gt -1) then begin
            if (~st) then begin
                data = (~noscale) ? qubepds(filename, label) : $
                                    qubepds(filename, label, /noscale)
            endif else begin
                data = (~noscale) ? qubepds(filename, label, /silent) : $
                                  qubepds(filename, label, /silent, /noscale)
            endelse
            result = create_struct (result, objarray[i], data)
        endif

        ; test to process TABLE, SERIES, PALETTE, or SPECTRUM:
        if ((strpos(obj, "TABLE") gt -1) || $
            (strpos(obj, "SERIES") gt -1) || $
            (strpos(obj, "PALETTE") gt -1) || $
            (strpos(obj, "SPECTRUM") gt -1)) then begin
             data = dotable (filename, label, st, objindex[i])  ; subroutine
             result = create_struct (result, objarray[i], data)
        endif

    ;Modified A.Cardesin 04Jan2006
    ;Added readSpreadsheet routine
       if (strpos(obj, "SPREADSHEET") gt -1) then begin
             data = (~st) ? readspreadsheet(filename, label, objindex[i], /PRINT_TIME) : $
                            readspreadsheet(filename, label, objindex[i], /silent)
             result = create_struct (result, objarray[i], data)
        endif

    endfor

    ; display the contents of the structure if not in silent mode:
    if (~st) then help, /st, result

;A.Cardesin 24 February 2006
;Recover idl path
!PATH = sSavePath 

    if hist EQ 0 then return, result
    if hist EQ 1 then begin
        numtab = -1.1
        while numtab LT 1 or (numtab-floor(numtab)) NE 0 do begin
            read,"How many tabs do you want to run Histogram with (Integer >= 1): ",numtab
        endwhile
        histogram,result,numtab
    endif
end
