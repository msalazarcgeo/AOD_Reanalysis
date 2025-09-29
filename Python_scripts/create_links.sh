#!/bin/bash
DIR_PATH="path_topredictions/AOD_Reanalysis/data/raster/ZMVM_predictions"
DIR_ALL="path_to_all/data/raster/all_predictions_symlinks_AMM/"
for VAR in $(ls $DIR_PATH)
do
    echo $DIR_PATH$VAR
    for file in $(ls $DIR_PATH$VAR)
    do
	##echo ${file##*\.}
	extent="${file##*\.}"
        if [[ $extent == tif ]]; then
	    echo ln -s $DIR_PATH$VAR/$file $DIR_ALL$file
	    ln -sf $DIR_PATH$VAR/$file $DIR_ALL$file
	fi
	
    done
done
	  
