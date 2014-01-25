;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Save and restore file state
;;; - no error handling

function filestate,lunORfst,noClose=noClose

  ;;; Special handling:  if not input, return unusable FSTAT structure
  ;;; with .UNIT = -999L
  if n_elements(lunORfst) eq 0L then begin
    rtn = {FSTAT}
    rtn.unit = -999L
    return, rtn
  endif

  ;;; lunORfst is not a structure; assume it is a LUN

  if size(lunORfst,/type) NE size({guard:0b},/type) then begin
    ;;; Save file state, optionally close file, and
    ;;; return fstat structure
    fst = fstat(lunORfst)
    if NOT keyword_set(noClose) then close,lunORfst
    return, fst
  endif

  ;;; Assume lunORfst is a FSTAT structure
  ;;; - Force LUN close; re-open if it was open before

  close,lunORfst.unit

  if lunORfst.open then begin

    ;;; - Assume read-only, assume PWD has not changed
    openr,lunORfst.unit,lunORfst.name

    point_lun, lunORfst.unit, lunORfst.cur_ptr
  endif

  return,lunORfst.unit

end
  
