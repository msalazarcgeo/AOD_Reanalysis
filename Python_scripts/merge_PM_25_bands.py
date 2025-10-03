import just_todo_2_py
import os
import argparse
## python merge_PM_25_bands.py --path_glue_rasters "../datos/raster/ZMVM_9PBHL_prediction_glue/" --save_path_merge "../datos/raster/ZMVM_9PBHL_merge_final/" --upper_band "PM25_predict" --under_band "PM25_predict_spline" --merge_band "PM25_merge" --save_prefix "PM25_merge_" --start_year 2006 --end_year 2022 --verbose TRUE




if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Rename the bands of all the file in directory')
    parser.add_argument('--path_glue_rasters',
                         help= "Path to the day spline predictions folder")
    parser.add_argument('--save_path_merge',
                         help= "Path to save the merge predictions")
    parser.add_argument('--upper_band',
                         help= "String with the upper band ", default="PM25_predict")
    parser.add_argument('--under_band',
                         help= "String with the upper band", default= "PM25_predict_spline")
    parser.add_argument('--merge_band',
                         help= "String with the band_merge", default= "PM25_merge")
    parser.add_argument('--save_prefix',
                         help= "String with the prefix to save", default= "PM25_merge_")
    parser.add_argument("--start_year" ,
                        help= "start year")
    parser.add_argument("--end_year" ,
                        help= "start year")
    parser.add_argument("--verbose" ,
                        help= "Verbose" , default=False)
    args = parser.parse_args()
    
    
    for year in range(int(args.start_year),int(args.end_year) +1 ):
        
        just_todo_2_py.merge_pm25_bands_folders(
            path_predic_glue  = args.path_glue_rasters+str(year)+"/",
            path_predic_exit = args.save_path_merge+str(year)+"/",
            band_under= args.under_band,
            band_upper= args.upper_band,
            band_merge = args.merge_band,
            prefix_save = args.save_prefix,
            verbose =args.verbose
        )
        
     