;-----------------------------------------------------------------------------
; NAME: TASCPDS   [Table ASCII PDS]
; 
; PURPOSE: To read a PDS ascii table file into an IDL structure containing 
;     columns of the data table as elements.
;
; CALLING SEQUENCE: Result = TASCPDS (filename, label, objindex, [/SILENT])
; 
; INPUTS:
;     Filename: Scalar string containing the name of the PDS file to read.
;     Label: String array containing the table header information.
;     Objindex: Integer specifying the starting index of teh current table
;         object in the label array to be read.
;
; OUTPUTS:
;     Result: Table structure constructed from designated records with fields
;         for each column in the table, along with a string array field 
;         containing the name of each column.
;
; OPTIONAL INPUT:
;     SILENT: Suppresses any messages from the procedure.
;
; EXAMPLES:
;     To read an ascii table file TABLE.LBL into a structure "table":
;         IDL> label = HEADPDS ('TABLE.LBL', /SILENT)
;         IDL> table = TASCPDS ('TABLE.LBL', label, /SILENT)
;
; PROCEDURES USED:
;     Functions: OBJPDS, GET_INDEX, CLEAN, REMOVE, STR2NUM, PDSPAR, POINTPDS
;
; MODIFICATION HISTORY:
;     Written by: John D. Koch [December 1994] (adapted from READFITS by
;                                               Wayne Landsman)
;     Re-written by: Puneet Khetarpal [08 July, 2004] 
;     
;     Modifications:
;     S. Martinez [11 February, 2009]. ROW_SUFFIX/PREFIX_BYTES data type changed to 
;        unsigned long 64. Range of previous data type exceeded.
;
;     J. Ritchie [10 August, 2011].  Modified the code for tables with RECORD_TYPE = STREAM to not 
;        require the optional keyword RECORD_BYTES in that case.
;
;     J. Ritchie [03 October, 2011].  Updated to handle CONTAINER objects.
;        Functions OBTAIN_TABLE_CONTAINERS, ORGANIZE_TABLE_DATA and FORMAT_TABLE_COLUMN added to this file.
;
;     To view a complete list of modifications made to this routine, 
;     please see changelog.txt file.
;
;-----------------------------------------------------------------------------

;- level 2 -------------------------------------------------------------------

;-----------------------------------------------------------------------------
; precondition: the name variable is a viable required keyword scalar string,
;     label is a viable PDS label string array, and start and end_ind are
;     viable integers pointing at the start and end of the current table
;     object being processed.
; postcondition: extracts the "name" keyword from the label, stores it in a 
;     structure, and returns to the main block.

function obtain_keyword, name, label, start_ind, end_ind
   ; initialize keyword structure:
   struct = create_struct("flag",1)

   ; obtain keyword parameters:
   val = pdspar (label, name, count=count, index=index)    ; external routine

   ; if no viable params found, then issue error:
   if (count eq 0) then begin
       print, "Error: missing required " + name + " keyword(s)."
       goto, endfun
   endif

   ; extract the indices where keyword index is between start_ind and end_ind:
   pos = where (index gt start_ind and index lt end_ind, cnt)

   ; if none found then return flag as -1:
   if (cnt eq 0) then goto, endfun

   ; store the viable values, indices, and count for "name" keyword:
   val = val[pos]
   index = index[pos]
   count = n_elements(val)

   ; place the stored params in the structure:
   struct = create_struct(struct,"val",val,"count",count,"index",index)
   return, struct

   endfun:
      struct.flag = -1
      return, struct
end

;-----------------------------------------------------------------------------
; precondition: name is a structure containing all names param for current
;     table object, label is a viable PDS label string array, and start_ind
;     and end_ind are integer pointers to start and end of current object.
; postcondition: the table name is searched through all the name values and
;     then removed from the structure, the count is decremented by one, and
;     the index value is also removed from the corresponding structure field.
;

function remove_table_name, name, label, start_ind, end_ind
    ; first obtain COLUMN object indices for current table object:
    column = objpds(label, "COLUMN")                      ; external routine
    pos = where (column.index gt start_ind and column.index lt end_ind)
    column.index = column.index[pos]

    ; check to see if the first name index is less than the first column
    ; index value. If found then there exists a table name, and is removed:
    if (name.index[0] lt column.index[0]) then begin
        temp = name.val[1:name.count - 1]
        name.val = ''
        name.val = temp
        
        temp = name.index[1:name.count - 1]
        name.index = ''
        name.index = temp
        
        name.count = name.count - 1
    endif

    ; clean up name array values, and removed unwanted characters:
    param = ['"', "'", "(", ")"]
    for j = 0, name.count-1 do begin
        name.val[j] = clean (name.val[j])                 ; external routine
        name.val[j] = remove (name.val[j], param)         ; external routine
    endfor

    return, name
end

;-----------------------------------------------------------------------------
; precondition: data_type is a structure containing the required keyword 
;     params for DATA_TYPE keyword.
; postcondition: the data type keyword values are cleaned, and the values after
;     the first '_' character are extracted, if present. 
;

function separate_table_data_type, data_type
    ; set the param variable:
    param = ['"', "'", ")", "("]

    ; start the loop to go through each data_type value array:
    for j = 0, data_type.count - 1 do begin
        ; first clean data_type:
        data_type.val[j] = clean (data_type.val[j], /space)  ; external routine
        data_type.val[j] = remove (data_type.val[j], param)  ; external routine

        ; extract the second component of value if '_' present:
        if (!version.release gt 5.2) then begin
            temp = strsplit(data_type.val[j], '_', /extract)
        endif else begin
            temp = str_sep (data_type.val[j], '_')   ; obsolete in IDL v. > 5.2
        endelse
        if (n_elements(temp) eq 3) then begin
            data_type.val[j] = temp[1] + '_' + temp[2] 
        endif else if (n_elements(temp) gt 1) then begin
            data_type.val[j] = temp[1]
        endif
    endfor

    return, data_type
end

;-----------------------------------------------------------------------------
; precondition: keywds contains all required keyword values in a structure,
;     items contains all item keyword values, curr_ind and next_ind are 
;     integer pointers to current and next column object being processed, and
;     curpos is an integer containing the current cursor position being
;     processed in the record.
; postcondition: the routine tests whether there are any table items for
;     current column object, and if found, then populates the necessary
;     item structure for current column object and returns to main block.
;     If no table items are found, then simply extracts the number of
;     bytes for current object and returns it as a bytarr.

function process_table_items, keywds, items, curpos, curr_ind, next_ind
    ; first determine whether there exist ITEMS for current COLUMN:
        
    if (items.flag eq -1) then begin
        ipos = -1
    endif else begin
        ipos = where (items.items.index gt curr_ind and items.items.index lt $
                  next_ind)
    endelse

    ; if items not present then extract the number of bytes for current object:
    if (ipos[0] eq -1) then begin
        ; determine the index for current bytes values to be assigned:
        pos = where (keywds.bytes.index gt curr_ind and $
                     keywds.bytes.index lt next_ind)
        element = bytarr(long(keywds.bytes.val[pos[0]]))

        ; increment the cursor position by current bytes:
        curpos = curpos + long(keywds.bytes.val[pos[0]])
    endif else begin
        ; if items are present, then create a secondary element bytarr 
        ; for each item, and start a temp cursor position for this column:
        item_bytes = long(items.bytes.val[ipos[0]])
        element = bytarr(item_bytes)
        temppos = item_bytes

        ; check for offset keyword values. if offset is 0 then simply
        ; set the item element structure to be element, else construct
        ; a bytarr for offset buffer to be read, increment temppos, and
        ; add the offset buffer to item element structure along with element:
        if (items.offset.count eq 0) then begin
            item_offset = 0
        endif else begin
            item_offset = long(items.offset.val[ipos[0]])
        endelse
        if (item_offset eq 0) then begin
            item_elem = create_struct("element", element)
        endif else begin
            offtemp = bytarr(item_offset)
            temppos = temppos + item_offset
            item_elem = create_struct("element", element, "offtemp", offtemp)
        endelse

        ; now replicate the item element structure number of items - 1 times.
        ; it is being replicated items - 1 times because of the offset buffer.
        ; when there is an offset, the offset bytes do not carry after the
        ; last item, so the item_elem structure constructed earlier would be
        ; incorrect:
        ;
        ; 1/13/2010 : Some datasets use ITEMS = 1. Add logical branch to
        ; handle this special case. If ITEMS = 1, we simply store item_elem
        ; to element, and avoid the creation of special temporary elements.
        num_items = long(items.items.val[ipos[0]])

        if num_items gt 1 then begin
            item_elem = replicate(item_elem, num_items - 1)

            ; multiply temppos by the number of items - 1:
            temppos = temppos * (num_items - 1)

            ; create a temporary structure to hold last array element, and
            ; increment temppos by item_bytes:
            temp_struct = create_struct("element",element)
            temppos = temppos + item_bytes

            ; populate the column structure to act as the element to be read:
            element = create_struct("item_elem",item_elem,"last",temp_struct)
        endif else begin
            element = create_struct("item_elem", item_elem)
        endelse

        ; finally increment main cursor position by temppos value:
        curpos = curpos + temppos
    endelse

    ; add cursor position and element into column object structure:
    object_struct = create_struct("curpos",curpos,"data",element)

    return, object_struct
end

;-----------------------------------------------------------------------------
; precondition: element is an n dimensional bytarr data read from the file,
;     type contains the data type of the element being processed, and rows
;     is the number of rows in the table.
; postcondition: element is converted into the n dimensional array of type 
;     "type", and returned to main block.
;

function convert_table_element, element, type, rows, repetitions
    ; error protection:
    on_ioerror, error_case

    ; depending on the case of the type assign each element to its own
    ; element conversion type:
    data = create_struct("flag",1)
    stat = size(element)

    ; IDL has issues with converting from bytarr into string for n rows by 1 
    ; column bytarr for n > 1, as a string conversion reduces the dimension of
    ; the array by one. Therefore, when there are n rows GT 1, the number of
    ; dimensions is 1, and the size of that one dimension is 1, then perform
    ; a conversion into string, element by element. e.g., array is in format:
    ; array = [[1], [2], [3], [4], [5]]:
    if (type ne "N/A") then begin
        if (stat[0] eq 1 && stat[1] gt 1 && rows gt 1) then begin
            temp = strarr(stat[1])
            for i = 0, stat[1] - 1 do begin
                temp[i] = string(element[i])
            endfor
            element = temp
        endif else begin
            element = string(element)
        endelse
    endif

    ; If type is to be converted to a number, change the blank spaces into 0s
    if type NE "CHARACTER" && type NE "DATE" && type NE "TIME" then begin   
      space=' '
      for loop=0, stat[1]-2 do space=space+' '
      space_ind=where( element eq space, cnt)
      if cnt NE 0 then element(space_ind)=0
    endif
    case type of
                 "INTEGER": element = long(element)
        "UNSIGNED_INTEGER": element = long(element)
             "MSB_INTEGER": element = long(element)
                    "REAL": element = double(element)
                   "FLOAT": element = float(element)
               "CHARACTER": element = string(element)
                  "DOUBLE": element = double(element)
                    "BYTE": element = long(element)
                 "BOOLEAN": element = long(element)
                    "TIME": element = string(element)
                    "DATE": element = string(element)
                     "N/A": ; no conversion performed, spare column
                      else: begin
                                print, "Error: " + type + $
                                       " not a recognized data type!"
                                data.flag = -1
                                return, data
                            end                        
    endcase

    data = create_struct(data, "element",element)
    return, data

    error_case:
        on_ioerror, null
        print, !err_string
        ;stop
        print, "WARNING: bad table data - no conversion performed" 
        data = create_struct(data, "element", element)
        return, data
end

;- level 1 -------------------------------------------------------------------

;-----------------------------------------------------------------------------
; precondition: label is a viable PDS label string array, start_ind is the
;     index specifying the start of the current object, and end index its
;     end. Label must contain a valid ASCII table with INTERCHANGE format
;     keyword.
; postcondition: returns a boolean (1 or -1) depending on whether it finds
;     an ASCII interchange format keyword, or not.
;

function tasc_interform, label, start_ind, end_ind
    ; obtain all INTERCHANGE_FORMAT keyword values from label:
    interform = pdspar (label, "INTERCHANGE_FORMAT", count=cnt, index=int_ind)

    ; check whether there exist any interchange format keywords:
    if (cnt eq 0) then begin
        print, "Error: missing required INTERCHANGE_FORMAT keyword"
        return, -1
    endif else begin
        ; obtain the indices of interchange format array object, where
        ; they are between start and end index:
        interpos = where (int_ind gt start_ind and int_ind lt end_ind, cnt2)

        ; extract the interform values:
        interform = interform[interpos[0]]
        interform = interform[0]

        ; remove all white space, and remove the '"' chars if found:
        interform = clean(interform,/space)            ; external routine
        interform = remove(interform, '"')             ; external routine

        ; if interchange format value is binary, then return -1 with error:
        if (interform eq "BINARY") then begin
            print, "Error: This is binary table file; try TBINPDS."
            return, -1
        endif
    endelse
    ; if all goes well, then return 1:
    return, 1
end

;-----------------------------------------------------------------------------
; precondition: label is a viable PDS label string array, and start_ind and
;     end_ind are valid integers specifying the start and end of the 
;     current table object as index pointers in the string array.
; postcondition: extracts required keywords for table object in the label.
;

function obtain_table_req, label, start_ind, end_ind
    ; initialize keyword structure:
    keywds = create_struct("flag",1)

    ; obtain table object definitions for current object:
    ; extract the keyword structure from subroutine, and check whether
    ; there are any params for the keyword, if not then return to main block
    ; else store the value

    ; first obtain number of COLUMNS:
    columns_num = obtain_keyword("COLUMNS", label, start_ind, end_ind)
    if (columns_num.flag eq -1) then goto, endfun
    columns = fix(columns_num.val[0])

    ; obtain ROW_BYTES keyword:
    row_bytes = obtain_keyword("ROW_BYTES",label, start_ind, end_ind)
    if (row_bytes.flag eq -1) then goto, endfun
    row_bytes = long(row_bytes.val[0])

    ; obtain ROWS keyword:
    rows = obtain_keyword("ROWS", label, start_ind, end_ind)
    if (rows.flag eq -1) then goto, endfun
    rows = long(rows.val[0])

    ;Modified J.Ritchie 09 Aug 2011
    ; obtain RECORD_TYPE keyword and RECORD_BYTES keyword if necessary
     record_type=pdspar(label,"RECORD_TYPE", count=count)
     if count eq 0 then begin
       record_bytes = obtain_keyword("RECORD_BYTES", label, 0, end_ind)
       if (record_bytes.flag eq -1) then goto, endfun
       record_bytes = long(record_bytes.val[0])
     endif else begin
       record_type=obtain_keyword("RECORD_TYPE", label, 0, end_ind)
       if (record_type.val[0] eq 'STREAM' OR record_type.val[0] eq '"STREAM"') then begin
         record_bytes = row_bytes
       endif else begin
         record_bytes = obtain_keyword("RECORD_BYTES", label, 0, end_ind)
         if (record_bytes.flag eq -1) then goto, endfun
         record_bytes = long(record_bytes.val[0])
       endelse
     endelse

    ; check whether either columns, rows or row_bytes equal 0:    
    if ((columns le 0) or (rows le 0) or (row_bytes le 0)) then begin
        print, "Error: ROWS OR ROW_BYTES or COLUMNS <= 0. No data read."
        goto, endfun
    endif

    ; obtain information for each column object:

    ; obtain NAME keyword:
    name = obtain_keyword("NAME", label, start_ind, end_ind)
    if (name.flag eq -1) then goto, endfun

    ; remove table name from name structure if present:
    name = remove_table_name(name, label, start_ind, end_ind)

    ; obtain DATA_TYPE keyword, then clean, separate, and extract it:
    data_type = obtain_keyword("DATA_TYPE", label, start_ind, end_ind)
    if (data_type.flag eq -1) then goto, endfun

    ; obtain data architecture and separate data type:
    data_type = separate_table_data_type(data_type)

    ; obtain BYTES keyword:
    bytes = obtain_keyword ("BYTES", label, start_ind, end_ind)
    if (bytes.flag eq -1) then goto, endfun

    ; obtain START_BYTE keyword, and subtract 1 for IDL indexing:
    start_byte = obtain_keyword("START_BYTE", label, start_ind, end_ind)
    if (start_byte.flag eq -1) then goto, endfun
    start_byte.val = long(start_byte.val) - 1

    ; store values into the keyword structure:
    keywds = create_struct(keywds, "columns",columns,"row_bytes",row_bytes, $
                           "rows",rows,"name",name,"data_type",data_type, $
                           "bytes",bytes,"start_byte",start_byte, $
                           "record_bytes",record_bytes)

    return, keywds

    endfun:
       keywds.flag = -1
       return, keywds
end

;-----------------------------------------------------------------------------
; precondition: label is a viable PDS label string array, start_ind and end_ind
;     are integer pointers to start and end of current table in label.
; postcondition: obtains the optional table keywords from the label, and 
;     returns them in a structure.

function obtain_tasc_opt, label, start_ind, end_ind
    ; initialize structure:
    keywds = create_struct("flag", 1, "prefix", 0ULL, "suffix", 0ULL)

    ; obtain ROW_PREFIX_BYTES keyword:
    rowprefix = pdspar (label, "ROW_PREFIX_BYTES", count=pcount, index=pindex)
    if (pcount gt 0) then begin
        pos = where (pindex gt start_ind and pindex lt end_ind, cnt)
        if (cnt gt 0) then begin
            keywds.prefix = ulong64(rowprefix[pos[0]])
            if (keywds.prefix lt 0) then begin
                print, "Error: invalid ROW_PREFIX_BYTES (" + $
                       clean(string(keywds.prefix), /space) + ")."
                goto, endfun
            endif
        endif
    endif

    ; obtain ROW_SUFFIX_BYTES keyword:
    rowsuffix = pdspar (label, "ROW_SUFFIX_BYTES", count=scount, index=sindex)
    if (scount gt 0) then begin
        pos = where (sindex gt start_ind and sindex lt end_ind, cnt)
        if (cnt gt 0) then begin
            keywds.suffix = ulong64(rowsuffix[pos[0]])
            if (keywds.suffix lt 0) then begin
                print, "Error: invalid ROW_SUFFIX_BYTES (" + $
                       clean(string(keywds.suffix), /space) + ")."
                goto, endfun
            endif
        endif
    endif

    return, keywds

    endfun:
        keywds.flag = -1
        return, keywds
end

;-----------------------------------------------------------------------------
; precondition: label is viable PDS label string array, req_keywds is a 
;     structure containing required table object keywords, and start and 
;     end_ind are viable integer pointers to start and end of the current
;     table object.
; postcondition: Extracts table items keyword params from label and returns
;     to main block as a structure.

function obtain_table_items, label, req_keywds, start_ind, end_ind
    ; initialize items keyword structure:
    keywds = create_struct("flag", 1, "flag2", 1)

    ; obtain necessary ITEM keyword values:
    ; first obtain ITEMS keyword values:
    items = pdspar (label, "ITEMS", count=items_count, index=items_index)
    if (items_count eq 0) then begin
        goto, endfun
    endif

    ; extract values of items between start_ind and end_ind, and store:
    pos = where (items_index gt start_ind and items_index lt end_ind, cnt)
    if (cnt eq 0) then goto, endfun
    items = create_struct("val",items[pos],"count",n_elements(items[pos]), $
                          "index",items_index[pos])

    ; now we know that there are columns with ITEMS keyword in current table
    ; object, so extract the other item keyword values:

    ; obtain ITEM_BYTES keyword values:
    bytes = pdspar (label, "ITEM_BYTES", count=byte_count, index=byte_index)
    if (byte_count eq 0) then begin
        print, "Error: missing required ITEM_BYTES keyword for items column."
        keywds.flag2 = -1
        goto, endfun
    endif
    pos = where (byte_index gt start_ind and byte_index lt end_ind, cnt)
    if (cnt eq 0) then begin
        print, "Error: missing required ITEM_BYTES keyword for current table."
        keywds.flag2 = -1
        goto, endfun
    endif
    bytes = create_struct("val", bytes[pos], "count", n_elements(bytes[pos]), $
                          "index", byte_index[pos])

    ; initialize a temp structure to hold default values:
    temp = create_struct("val",0,"count",0,"index",0)

    ; obtain ITEM_OFFSET keyword values, if present:
    offset = pdspar (label, "ITEM_OFFSET", count=off_count, index=off_index)

    ; if there exist ITEM_OFFSET keywords, then obtain ones between
    ; start and end_ind else set it to temp structure:
    if (off_count gt 0) then begin
        pos = where (off_index GT start_ind AND off_index LT end_ind, cnt)
        if (cnt eq 0) then begin
            offset = temp
        endif else begin
            offset = create_struct("val", long(offset[pos]), "count", $
                                   n_elements(offset[pos]),"index",off_index[pos])
            ; since the offset values in the PDS label are given as the 
            ; number of bytes from the beginning of a one item to the
            ; beginning of next, therefore, the offset that we are concerned
            ; with is the difference between the end of one item and the
            ; beginning of next:
            offset.val = long(offset.val) - long(bytes.val)
            lt_zero = where(offset.val lt 0, lt_cnt)
            if (lt_cnt gt 0) then begin
                print, "Error: invalid ITEM_OFFSET value, must be >=ITEM_BYTES"
                keywds.flag2 = -1
                goto, endfun
            endif
        endelse
    endif else offset = temp

    ; store info into a structure:
    keywds = create_struct(keywds, "items",items, "bytes",bytes,"offset", $
                           offset)
    return, keywds 

    endfun:
        keywds.flag = -1
        return, keywds
end

;ADDED 8-12-11 J. Ritchie
;-----------------------------------------------------------------------------
; precondition: label is viable PDS label string array, req_keywds is a 
;     structure containing required table object keywords, and start and 
;     end_ind are viable integer pointers to start and end of the current
;     table object.
; postcondition: Extracts table container keyword params from label and returns
;     to main block as a structure.

function OBTAIN_TABLE_CONTAINERS, label, req_keywds, start_ind, end_ind
  ; initialize items keyword structure:
  keywds = create_struct("flag", 1, "flag2",1)
  
  container = objpds(label, "CONTAINER")
  if (container.flag eq -1) then goto,endfun
  
  container_info = replicate(create_struct('name','','start_byte',0ULL,'bytes',0ULL,'repetitions',0ULL,'description','',$
                                           'start_index',0ULL,'end_index',0ULL,'columns',0ULL),n_elements(container.index))
  
  for i=0,n_elements(container.index)-1 do begin
    container_info[i].start_index = container.index[i]
    container_info[i].end_index = get_index(label,container.index[i])
    
    column = objpds(label[container_info[i].start_index:container_info[i].end_index],"COLUMN")
    container_info[i].columns = column.count
    end_header = container_info[i].start_index + column.index[0]
    
    name = obtain_keyword("NAME",label, container_info[i].start_index, end_header)    
    repetitions = obtain_keyword("REPETITIONS",label, container_info[i].start_index, end_header)
    bytes = obtain_keyword("BYTES",label, container_info[i].start_index, end_header)
    start_byte = obtain_keyword("START_BYTE",label, container_info[i].start_index, end_header)
    if (name.flag eq 0 || repetitions.flag eq 0 || bytes.flag eq 0 || start_byte.flag eq 0) then begin
      print, "Error: missing required keyword in CONTAINER object: NAME, REPETITIONS, BYTES or START_BYTE"
      keywds.flag2 = -1
      goto, endfun
    endif
    
    container_info[i].name = name.val[0]
    container_info[i].repetitions = repetitions.val[0]
    container_info[i].bytes = bytes.val[0]
    container_info[i].start_byte = start_byte.val[0]
  endfor

  ; store info into a structure:
  keywds = create_struct(keywds, "name", container_info.name, "bytes",container_info.bytes,"repetitions",container_info.repetitions,"start_byte",container_info.start_byte,$
                         "start_index",container_info.start_index,"end_index",container_info.end_index,"columns",container_info.columns,"count",n_elements(container_info.name))
  return, keywds 

  endfun:
    keywds.flag = -1
    return, keywds
end


;-----------------------------------------------------------------------------
; precondition: keywds contains required keywords for current table object,
;     items contains items keywords for current table object, label is a 
;     viable PDS label string array, start_ind and end_ind are integer
;     pointers to start and end of current table object
; postcondition: creates a structure to be read from the data file and returns
;     to the main block.

function create_table_struct, keywds, opt, items, containers, label, start_ind, end_ind
    ; initialize variables:
    curpos = 0            ; cursor position
    complete_set = create_struct("flag", 1)      ; structure to be returned
    data_set = 0          ; data structure set

    ; create structure for row prefix bytes if any:
    if (opt.prefix gt 0) then begin
        data_set = create_struct("prefix", bytarr(opt.prefix))
    endif

    ;; If no CONTAINER objects in label
    if (containers.flag eq -1) then begin

    ; start the loop:
    for j = 0, keywds.columns - 1 do begin
        
        ; set current and next COLUMN object index pointers:
        curr_ind = keywds.name.index[j]
        if (j eq keywds.columns - 1) then begin
            next_ind = end_ind
        endif else begin
            next_ind = keywds.name.index[j+1]
        endelse

        ; name of current column:
        name = "COLUMN" + clean(string(j+1),/space)     ; external routine
        ; extract start byte value for current column:
        start_byte = long(keywds.start_byte.val[j])

        ; the temp buffer to be read is: difference between start_byte of
        ; current column and last curpos:
        buffer_length = start_byte - curpos

        if (buffer_length lt 0) then begin
            print, "Error: inconsistent START_BYTE and BYTES specification" + $
                   " in COLUMNS " + clean(string(j),/space) + " and " + $
                   clean(string(j+1), /space) + "."
            goto, endfun
        endif else if (buffer_length eq 0) then begin
            temp = -1            
        endif else begin
            temp = bytarr (buffer_length)
            curpos = start_byte
        endelse

        ; process item objects for current COLUMN object:
        element = process_table_items (keywds, items, curpos,curr_ind,next_ind)

        ; create column structure:
        if (temp[0] eq -1) then begin
            col_struct = create_struct ("element", element.data)
        endif else begin
            col_struct = create_struct ("temp", temp, "element", element.data)
        endelse

        ; construct structure to be read:
        if (size(data_set,/type) eq 8) then begin
            data_set = create_struct(data_set, name, col_struct)
        endif else begin
            data_set = create_struct(name, col_struct)
        endelse
    endfor

    ; accomodate for row bytes
      bytescheck = (opt.suffix gt 0) ? keywds.row_bytes : keywds.row_bytes - 2
    ; *** added for testing*** bytescheck = keywds.row_bytes
    ; set additional buffer array at end of record:
    ; if cursor position < bc then take the difference between bc and curpos
    ; and create a temporary buffer to be read at the end of each record.
    ; if cursor position > bc then issue error, and exit routine:
    ; Following if statements were restructured to remove BYTE specification problem - Parin K
    if (bytescheck gt curpos) then begin
        diff = bytescheck - curpos
        data_set = create_struct(data_set, "temp", bytarr(diff))
        if (opt.suffix gt 0) then begin
            data_set = create_struct(data_set, "suffix", bytarr(opt.suffix))
        endif
    endif 
    if (bytescheck eq curpos) then begin
        ; check for row suffix bytes:
        if (opt.suffix gt 0) then begin
            data_set = create_struct(data_set,"suffix", bytarr(opt.suffix))
        endif
    endif
    if (bytescheck lt curpos) then begin
            print, "Error: Improper START_BYTE or BYTES specification"
            print, "in PDS label, or data file missing carriage return"
            print, "and/or line feed characters."
            goto, endfun
    endif

    ;; CONTAINER objects in label
    endif else begin 

      container_index = 0
      
      columns = objpds(label, "COLUMN")
      pos = where(columns.index gt start_ind and columns.index lt end_ind)
      columns.count = n_elements(pos)
      columns.array[0:columns.count-1] = columns.array(pos)
      columns.index[0:columns.count-1] = columns.index(pos)
      columns_end_index = uintarr(columns.count)   
      for i=0,columns.count-1 do columns_end_index[i] = get_index(label,columns.index[i])
      curpos2=0     

      ;; start the loop for columns
      for j = 0, columns.count - 1 do begin 

        ; set current and next COLUMN object index pointers:
        curr_ind = columns.index[j]
        next_ind = columns_end_index[j]
        
        ;; Check if column belongs to container object
        container_flag = 0
        if (container_index lt containers.count) then begin
          if (curr_ind gt containers.start_index[container_index] and next_ind lt containers.end_index[container_index]) then container_flag = 1 $
          else container_flag = 0
        endif
        
        if (container_flag eq 0) then begin
        
          ; name of current column:
          name = "COLUMN" + CLEAN(string(j+1),/SPACE)     ; external routine
        
          ; extract start byte value for current column:
          ; determine the index for current start_byte values to be assigned:
          pos = where (long(keywds.start_byte.index) GT curr_ind AND long(keywds.start_byte.index) LT next_ind)
          start_byte = long(keywds.start_byte.val[pos(0)])
        
          ; the temp buffer to be read is: difference between start_byte of
          ; current column and last curpos:
          buffer_length = start_byte - curpos

          if (buffer_length LT 0) then begin
              print, "Error: inconsistent START_BYTE and BYTES specification" + $
                   " in COLUMNS " + CLEAN(string(j),/SPACE) + " and " + $
                   CLEAN(string(j+1), /SPACE) + "."
              GOTO, ENDFUN
          endif else if (buffer_length EQ 0) then begin
            temp = -1            
          endif else begin
            temp = bytarr (buffer_length)
            curpos = start_byte
          endelse

          ; process item objects for current COLUMN object:
          element = PROCESS_TABLE_ITEMS(keywds, items, curpos,curr_ind,next_ind)

          ; create column structure:
          if (temp[0] EQ -1) then begin
            col_struct = create_struct ("element", element.data)
          endif else begin
            col_struct = create_struct ("temp", temp, "element", element.data)
          endelse

          ; construct structure to be read:
          if (size(data_set, /TYPE) NE 8) then begin
            data_set = create_struct(name, col_struct)
          endif else begin
            data_set = create_struct(data_set, name, col_struct)
          endelse
          
        endif else begin
        
          ; name of current column:
          container_name = "CONTAINER" + CLEAN(string(container_index+1),/SPACE)     ; external routine
          container_struct = 0
          container_curpos = 0  

          for k=0,containers.columns[container_index]-1 do begin

            ; set current and next COLUMN object index pointers:
            curr_ind = columns.index[j]
            next_ind = columns_end_index[j]
        
          ; name of current column:
          name = "COLUMN" + CLEAN(string(j+1),/SPACE)     ; external routine
        
          ; extract start byte value for current column:
          ; determine the index for current start_byte values to be assigned:
          pos = where (long(keywds.start_byte.index) GT curr_ind AND long(keywds.start_byte.index) LT next_ind)
          start_byte = long(keywds.start_byte.val[pos(0)])
          bytes=long(keywds.bytes.val[pos(0)])   
     
          ; the temp buffer to be read is: difference between start_byte of
          ; current column and last curpos:
          buffer_length = start_byte - container_curpos 

          if (buffer_length LT 0) then begin
              print, "Error: inconsistent START_BYTE and BYTES specification" + $
                   " in COLUMNS " + CLEAN(string(j),/SPACE) + " and " + $
                   CLEAN(string(j+1), /SPACE) + "."
              GOTO, ENDFUN
          endif else if (buffer_length EQ 0) then begin
            temp = -1            
          endif else begin
            temp = bytarr (buffer_length)
            ;curpos = start_byte
             container_curpos=start_byte
          endelse

          ;process item objects for current COLUMN object:
          element = PROCESS_TABLE_ITEMS(keywds, items, container_curpos,curr_ind,next_ind)

          ; create column structure:
          if (temp[0] EQ -1) then begin
            col_struct = create_struct ("element", element.data)
          endif else begin
            col_struct = create_struct ("temp", temp, "element", element.data)
          endelse

          ; construct structure to be read:
          if (size(container_struct, /TYPE) NE 8) then container_struct = create_struct(name, col_struct) $
          else container_struct = create_struct(container_struct, name, col_struct)
        
          if (k ne containers.columns[container_index]-1) then j++
          if (k eq containers.columns[container_index]-1) then curpos2=curpos2+(k+1) ;curpos2 == number of columns in containers
        endfor

      
        container_struct_aux = replicate(container_struct,containers.repetitions[container_index]) 

        curpos = container_curpos*containers.repetitions[container_index] + curpos

        container_index++
        
        ; construct structure to be read:
        if (size(data_set, /TYPE) NE 8) then begin
          data_set = create_struct(container_name, container_struct_aux)
        endif else begin
          data_set = create_struct(data_set, container_name, container_struct_aux)
        endelse
       endelse   
    endfor

    ; accomodate for row bytes
      bytescheck = (opt.suffix gt 0) ? keywds.row_bytes : keywds.row_bytes - 2

    ; *** added for testing*** bytescheck = keywds.row_bytes
    ; set additional buffer array at end of record:
    ; if cursor position < bc then take the difference between bc and curpos
    ; and create a temporary buffer to be read at the end of each record.
    ; if cursor position > bc then issue error, and exit routine:
    ; Following if statements were restructured to remove BYTE specification problem - Parin K
    if (bytescheck gt curpos) then begin
        diff = bytescheck - curpos
        data_set = create_struct(data_set, "temp", bytarr(diff))
        if (opt.suffix gt 0) then begin
            data_set = create_struct(data_set, "suffix", bytarr(opt.suffix))
        endif
    endif 
    if (bytescheck eq curpos) then begin
        ; check for row suffix bytes:
        if (opt.suffix gt 0) then begin
            data_set = create_struct(data_set,"suffix", bytarr(opt.suffix))
        endif
    endif
    if (bytescheck lt curpos) then begin
            print, "Error: Improper START_BYTE or BYTES specification"
            print, "in PDS label, or data file missing carriage return"
            print, "and/or line feed characters."
            goto, endfun
    endif

    endelse

    ; set the CR and LF structure elements if needed:
    if (opt.suffix eq 0) then begin
        data_set = create_struct(data_set, "crlf", bytarr(2))
    endif 

    ; replicate data structure ROWS times and construct the complete data structure:
    complete_set = create_struct (complete_set, "data_set", replicate (data_set, keywds.rows))

    return, complete_set

    endfun:
        complete_set.flag = -1
        return, complete_set
end

;-----------------------------------------------------------------------------
; precondition: pointer is a structure containing viable pointer information
;     for current table object, data_struct contains the data structure to
;     be read from file, keywds contains all the required keyword info, and 
;     silent is set to either 0 or 1 depending on the function specification.
; postcondition: this subroutine reads the data from the pointer datafile
;     and returns the data structure.
;

function read_table_data, pointer, data_struct, keywds, silent
    ; error protection:
    on_ioerror, signal

    bigflag = 1
    ; construct data_set structure:
    data_set = create_struct("flag", 1)

    ; first inform user of status:
    if (silent eq 0) then begin
        text = clean(string(keywds.columns), /space) + " Columns and " + $
               clean(string(keywds.rows), /space) + " Rows" ; external routine
        print, "Now reading table with " + text
    endif

    ; now open file and read data after setting the file pointer to skip:
    openr, unit, pointer.datafile, /get_lun
    point_lun, unit, pointer.skip
    readu, unit, data_struct
    close, unit
    free_lun, unit

    data_set = create_struct(data_set, "data", temporary(data_struct))
    return, data_set

    signal:
        on_ioerror, null
        print, "Error: File either corrupted or bad PDS label."
        data_set.flag = -1
        close, unit
        free_lun, unit
        return, data_set
end


function FORMAT_TABLE_COLUMN, element, keywds, items, repetitions, rows, type, index1, index2

  stat = size(element, /TYPE)
  
  ; check to see if current column is a structure:
  if (stat EQ 8) then begin

    ; determine item bytes for current column:
    if (index1 EQ keywds.columns - 1) then begin
      items_count = items.items.val[items.bytes.count - 1]
    endif else begin
      pos = where (items.bytes.index GT keywds.bytes.index[index1])
      items_count = items.items.val[pos[0]]
    endelse

    ; perform conversion of the element into arrays using routine:
    stat = size(element.item_elem.element)
   
    if (repetitions eq 0) then begin
      if (items_count eq 2) then begin
        combo = bytarr(stat[1], 2, stat[2])
        combo[*, 0, *] = element.item_elem.element
        combo[*, 1, *] = element.last.element
        temp = convert_table_element(combo, type, rows)
      endif else begin
        
        if float(strtrim(items_count,1)) ne 1 then begin
            combo = bytarr(stat[1], stat[2] + 1, stat[3])
            combo[*, 0:stat[2] - 1, *] = element.item_elem.element
            combo[*, stat[2], *] = element.last.element
        endif else begin
            combo = bytarr(stat[1], stat[2], stat[3])
            combo = element.item_elem.element
        endelse
        temp = convert_table_element(combo, type, rows)
      endelse

    endif else begin

      combo=bytarr(stat[1],stat[2]+1,stat[3],repetitions)
      combo[*,0:stat[2]-1,*,*] = element.item_elem.element
      combo[*,stat[2],*,*] = element.last.element
      
      temp = convert_table_element(combo, type, rows, repetitions)
    endelse
  endif else begin
    temp = (repetitions eq 0) ? convert_table_element(element, type, rows) : convert_table_element(element, type, rows,  repetitions)  ; external routine
  endelse

  return, temp
end

;-----------------------------------------------------------------------------
; precondition: keywds contains the required keyword params, data contains
;     the data as read from the file in a structure format.
; poscondition: the data structure is parsed and all the column data is 
;     extracted, organized into arrays, and returned as a structure.
function ORGANIZE_TABLE_DATA, keywds, items, data, containers
    ; initialize data structures:
    data_set = create_struct("flag", 1)
    name_val = keywds.name.val[0:keywds.name.count-1]
    data_struct = create_struct("names", name_val)
    rows = keywds.rows

    ;; Check if container object
    names = tag_names(data)

    pos = where(strmid(names,0,9) eq "CONTAINER",count)
    if (count eq 0) then begin

    ; go through each column and convert data structure into array:
    for i = 0, keywds.columns - 1 do begin
        ; get the column title and data type:
        title = "column" + CLEAN(i + 1,/SPACE)
        type = keywds.data_type.val[i]

        ; obtain the ith tag's element and store it as temp element, and 
        ; determine the type of object element is, i.e., a structure, array:
        if (size(data.(0), /TYPE) NE 8) then element = data.(i+1).element $
        else element = data.(i).element
        stat = size(element, /TYPE)

        temp = FORMAT_TABLE_COLUMN(element, keywds, items, 0, rows, type, i, i)
         
        ; if conversion not successful then return to main block with flag:
        if (temp.flag EQ -1) then begin
            GOTO, ENDFUN 
        endif

        ; add to data structure:
        data_struct = create_struct(data_struct, title, temp.element)
    endfor
    
    endif else begin
    
      column_ind = 0ULL
      total_ind = 0ULL
      
      ; go through each column and convert data structure into array:
      for i = 0, n_elements(names)-1 do begin
        if (strmid(names[i],0,9) eq "CONTAINER") then begin
          container_content = tag_names(data.(i))
          stat = size(data.(i))

          ;repetitions = stat[1] ;; repetitions? If not how do I get value for repetitions?
          repetitions=containers.repetitions[i]

          total_ind++
          
          container_struct = 0
          for j=0,n_elements(container_content)-1 do begin
         
            title = "column" + CLEAN(column_ind + 1,/SPACE)
            type = keywds.data_type.val[column_ind]
            element = data.(i).(j).element
            temp = FORMAT_TABLE_COLUMN(element, keywds, items, repetitions, rows, type, i, total_ind)

            if (temp.flag EQ -1) then goto, endfun ; if conversion not successful then return to main block with flag
            
            ; add to data structure:
            container_struct =  (size(container_struct, /TYPE) NE 8) ? create_struct(title, temp.element) : create_struct(container_struct, title, temp.element)
          
            column_ind++
            total_ind++
          
          endfor
          data_struct = create_struct(data_struct,names[i],container_struct)
        
        endif else if (strmid(names[i],0,4) eq "CRLF") then begin  
        endif else begin
          title = "column" + CLEAN(column_ind + 1,/SPACE)
          type = keywds.data_type.val[column_ind]
          
          ; obtain the ith tag's element and store it as temp element, and 
          ; determine the type of object element is, i.e., a structure, array:
          if (size(data.(0), /TYPE) NE 8) then element = data.(i+1).element else element = data.(i).element
          
          temp = FORMAT_TABLE_COLUMN(element, keywds, items, 0, rows, type, i, total_ind)
          if (temp.flag EQ -1) then goto, endfun ; if conversion not successful then return to main block with flag

          ; add to data structure:
          data_struct = create_struct(data_struct, title, temp.element)
          
          column_ind++
          total_ind++
        endelse
    
      endfor
    
    endelse

    data_set = create_struct(data_set, "data", data_struct)
    return, data_set

    ENDFUN:
       data_set.flag = -1
       return, data_set
end

;- level 0 -------------------------------------------------------------------

function tascpds, fname, label, objindex, SILENT=silent
    ; error protection:
    on_error, 2

    ; check for number of parameters in function call:
    if (n_params() lt 3) then begin
        print, "Syntax: result = tascpds (filename,label,objindex[,/SILENT])"
        return, -1
    endif

    ; check for silent keyword:
    silent = keyword_set(SILENT)

    ; obtain viable objects for label:
    objects = objpds(label, "ALL")                         ; external routine
    if (objects.flag eq -1) then begin
        print, "Error: No viable TABLE objects found in label."
        return, -1
    endif

    ; match the indices of all viable objects against objindex:
    objpos = where (objindex eq objects.index)

    ; if a match is found, then store the name of that object, else 
    ; indicate the specification of invalid objindex:
    if (objpos[0] ne -1) then begin
        objects.array = objects.array[objpos[0]]
        objectname = objects.array[0]
    endif else begin
        print, "Error: Invalid objindex specified: " + $
             clean(string(objects.index[objpos[0]]), /space)
        return, -1
    endelse

    ; set start and end object pointers for current table object:
    start_ind = objindex
    end_ind = get_index(label, start_ind)                  ; external routine
    if (end_ind eq -1) then return, -1

    ; check for valid interchange format using subroutine:
    if (tasc_interform(label, start_ind, end_ind) eq -1) then return, -1

    ; obtain required keywords for the current object:
    req_keywds = obtain_table_req(label, start_ind, end_ind)   ; subroutine
    if (req_keywds.flag eq -1) then return, -1

    ; obtain optional keywords for current object:
    opt_keywds = obtain_tasc_opt(label, start_ind, end_ind)    ; subroutine
    if (opt_keywds.flag eq -1) then return, -1

    ; obtain items keywords for the column objects using subroutine:
    items = obtain_table_items(label, req_keywds, start_ind, end_ind)
    if (items.flag2 eq -1) then return, -1

    ;ADDED 8-12-11 J. Ritchie
    ; obtain CONTAINER objects for the column objects using subroutine:
    containers = OBTAIN_TABLE_CONTAINERS(label, req_keywds, start_ind, end_ind)
    if (containers.flag2 EQ -1) then begin
        return, -1
    endif

    ; obtain pointer information for table object:
    pointer = pointpds (label, fname, objectname)          ; external routine
    if (pointer.flag EQ -1) then return, -1

    ; create data structure to be read:
    complete_set = create_table_struct (req_keywds, opt_keywds, items, containers, label, $
                                        start_ind, end_ind)    ; subroutine
    if (complete_set.flag eq -1) then begin
        return, -1
    endif else begin
        data_struct = complete_set.data_set
    endelse

    ; read data structure from file using subroutine:
    data_set = read_table_data (pointer, data_struct, req_keywds, silent)
    if (data_set.flag eq -1) then return, -1

    ; check for CR and LF chars
    if (opt_keywds.suffix gt 0) then begin
        cr_arr = data_set.data.suffix
    endif else begin
        cr_arr = data_set.data.crlf
    endelse

    ; find the positions where the cr and lf chars are present in struct:
    crpos = where (cr_arr eq 13B, crcount)
    lfpos = where (cr_arr eq 10B, lfcount)

    ; since data is an array of structures for each record, therefore, the 
    ; cr and lf chars must equal the number of elements in the structure array:
    if ((crcount ne n_elements(data_set.data)) && $
        (lfcount ne n_elements(data_set.data))) then begin

        ;Modified A.Cardesin 02 Jun 2005
        ;print ROW number, to be able to verify the file easily

        print, "Error in ROW: ",lfcount+1
        print, "Error in table: Insufficient number of carriage return "+ $
               "and line feed characters found. Carriage return and " + $
               "line feed characters should terminate each line."
        CRLFflag = -1
    endif else begin
        CRLFflag = 1
    endelse

    if (CRLFflag eq -1) then begin
        return, -1
    endif

    data = data_set.data

    ; separate data into columns and convert into appropriate type:
    data_set = ORGANIZE_TABLE_DATA(req_keywds, items, data, containers) ; subroutine
    if (data_set.flag EQ 1) then begin
        data_struct = data_set.data
    endif else begin
        return, -1
    endelse

    if (data_set.flag eq 1) then begin
        return, data_struct
    endif else begin
        return, -1
    endelse
end
