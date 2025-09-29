#!/bin/bash

PATH_ALL="../data/raster/ZMVM_predictions"
for YEAR in {2022}
do
    echo $YEAR
    echo python rename_bands_predictions.py --prediction_path "$PATH_ALL/$YEAR/"
    python rename_bands_predictions.py --prediction_path "$PATH_ALL/$YEAR/"

done
