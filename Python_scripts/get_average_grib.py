# El programa genera los dias promedio para las fechas determinadas 
### python get_average_grib.py  --grib_file "../datos/ECMWF/2015_2022_PBLH.grib" --path_dir_day_average ../datos/raster/ECMWF_rasters/2019/ --initial_day "2019-02-01" --final_day "2019-12-31"



import conta_PBLH
import argparse



if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Generate the average day of the PBLH')
    parser.add_argument('--grib_file',
                         help= "Path to the GRIB file")
    parser.add_argument('--path_dir_day_average',
                         help= "Path to directory to store the average days")
    parser.add_argument('--initial_day',
                         help= "Year-Month-Day of the staring period")
    parser.add_argument('--final_day',
                         help= "Year-Month-Day of the end period")
    
    args = parser.parse_args()
    conta_PBLH.get_mean_day(
        args.initial_day,
        args.final_day,
        args.grib_file,
        save_path= args.path_dir_day_average
    )