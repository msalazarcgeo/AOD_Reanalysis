#!/bin/bash

for DIR in ./*;
do for FILE in $DIR/*;
   do readarray -d "_" -t strarr <<< "$FILE"
      if [ ${strarr[5]} != ${strarr[6]} ]; then
      	  echo "${strarr[5]}"
	  echo "${strarr[6]}"
	  readarray -d "/" -t arrmove <<< "$FILE"
	  endarr=${#arrmove[@]}
	  echo mv $FILE ./${strarr[6]}/${arrmove[@]:2:$endarr}
	  echo $FILE ./${strarr[6]}/${arrmove[@]:2:$endarr}
      fi
   done
done


