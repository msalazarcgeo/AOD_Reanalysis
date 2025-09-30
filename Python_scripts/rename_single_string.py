import just_todo_2_py
import os
import argparse
##python rename_single_string.py --season_spline_path "../datos/raster/Season_tensor_9PBLH/2017/cold_2017.tif" 




if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Rename the bands of all the file in directory')
    parser.add_argument('--season_spline_path',
                         help= "Path to the predictions folder")
    parser.add_argument('--name_band',
                         help= "String  to rename the band", default="Spline")
    args = parser.parse_args()

    path_estimates_spline = args.season_spline_path

    lis_file = os.listdir(path_estimates_spline)
    lis_file =  [file for file in lis_file if file[-4:] == ".tif" ]
    for file in lis_file:
        just_todo_2_py.set_single_band_name(path_estimates_spline + file, args.name_band)
     