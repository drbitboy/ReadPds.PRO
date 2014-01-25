#!/bin/bash

### Usage:  $0 testparsepds.bash testdata/xyz.lbl[ | ./gdl > x.x]

echo '!QUIET=1'
for i in $* ; do

  echo "print,f='(/a)','### Opening $i"
  echo "openr,lun,'$i',/get_lun"
  echo "parsepds,lun,/doubleDebug"
  echo "free_lun,lun"
  echo "print,f='(a)','### Done with $i"

done
