import just_todo_2_py
import os
import argparse
## python glue_bands_folder_all.py  --day_season_spline_path "../datos/raster/ZMVM_9PBLH_seasons_days/" --day_predictions  "../datos/raster/ZMVM_predictions_9PBLH/" --save_path_glue "../datos/raster/ZMVM_9PBHL_prediction_glue/" --start_year "2006" --end_year "2022"


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Rename the bands of all the file in directory')
    parser.add_argument('--day_season_spline_path',
                         help= "Path to the day spline predictions folder")
    parser.add_argument('--day_predictions',
                         help= "Path predictions")
    parser.add_argument('--save_path_glue',
                         help= "Path predictions")
    parser.add_argument('--prefic_save',
                         help= "Prefix to save", default= "PM25_glue_")
    parser.add_argument("--start_year" ,
                        help= "start year")
    parser.add_argument("--end_year" ,
                        help= "start year")
    args = parser.parse_args()
    
    
    for year in range(int(args.start_year),int(args.end_year) +1 ):
        
        print(args.day_season_spline_path + str(year)+"/")
        print(args.day_predictions + str(year)+"/")
        print(args.save_path_glue + str(year)+"/")

        just_todo_2_py.glue_prediction_season_m3(
            path_predic_spline = args.day_season_spline_path + str(year)+"/",
            path_predic_multiple =args.day_predictions + str(year)+"/",
            path_glue  = args.save_path_glue + str(year)+"/",
            prefix_save = args.prefic_save
        )
        
  
  
  
  
  
  
 
