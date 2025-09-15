## Program to obtain the merge rasters (resampling, cutting and merge)
#### python get_res_cut_merg.py --grib_file ../datos/ECMWF/2015_2022_PBLH.grib --path_dir_day_average ../datos/raster/ECMWF_rasters/2018/ --path_dir_multy_band ../datos/raster/ZMVM_8_bands/2018/ --path_dir_resample ../datos/raster/middle_PBLH/resample/2018/ --path_dir_cut ../datos/raster/middle_PBLH/cut/2018/ --path_dir_merge ../datos/raster/middle_PBLH/all_bands/2018/ --file_metropolitan_polygon ../datos/Zonas_metro/ZMVM.shp

import conta_PBLH
import argparse
import os 
import pandas as pd
import pygrib

def get_files_average(
        dir_average,
        dir_multi_bands,
        ):
    """
    Obtain all the file paths to create the corresponding files
    """
    files_aver = os.listdir(dir_average)
    files_aver = [dir_average+i for i in files_aver ]
    #### Encontrar el archivo respectivo 
    file_dec = [i[i.rfind("/")+1:i.rfind(".tif")].split("_") for i in files_aver ]
    file_dec = ["_".join(i[1:]) for i in file_dec ] 
    df_files = pd.DataFrame({"PBLH_file": files_aver, "Date":file_dec})
    files_multy= os.listdir(dir_multi_bands)
    #print(files_multy)
    df_files["Multi_file"]= df_files["Date"].apply(lambda l: [s for s in files_multy if s.find(l)!= -1 ] )
    df_files["Multi_file"]= df_files["Multi_file"].apply(lambda l : dir_multi_bands+l[0])
    return df_files

def get_dic_grib(file_grib):
    """
    Get the dicctionary from the grib file 
    """
    grbs = pygrib.open(file_grib)
    grb= grbs.select()[0]
    #### Se obtienen las coordenadas de las esquinas y el tamaño del raster original. 
    grbs.close()
    min_lat_r = grb['latitudeOfLastGridPointInDegrees']
    max_lat_r = grb['latitudeOfFirstGridPointInDegrees']
    min_lon_r = grb['longitudeOfFirstGridPointInDegrees']
    max_lon_r = grb['longitudeOfLastGridPointInDegrees']

    lat_size_r= grb.data()[1][0][0]- grb.data()[1][1][0] 
    lon_size_r= grb.data()[2][0][0]- grb.data()[2][0][1]
    dic_grib= {
        'min_lat_r':min_lat_r,
        'max_lat_r':max_lat_r,
        'min_lon_r':min_lon_r,
        'max_lon_r':max_lon_r,
        'lat_size_r':lat_size_r,
        'lon_size_r':lon_size_r

    }
    return dic_grib


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Generate the average day of the PBLH')
    parser.add_argument('--grib_file',
                         help= "Path to the GRIB file")
    parser.add_argument('--path_dir_day_average',
                         help= "Path to directory where the the average days are store")
    parser.add_argument('--path_dir_multy_band',
                         help= "Path to directory where the the multy bands raster day are store")
    parser.add_argument('--path_dir_resample',
                         help= "Path to directory to store the resample days")
    parser.add_argument('--path_dir_cut',
                         help= "Path to directory to store the cut resample days")
    parser.add_argument('--path_dir_merge',
                         help= "Path to directory to store the merge rasters")
    parser.add_argument('--file_metropolitan_polygon',
                         help= "Path to the metropolytan polygon")
    
    args = parser.parse_args()
    df_files = get_files_average(
        args.path_dir_day_average, 
        args.path_dir_multy_band, 
        )
    dic_grib = get_dic_grib(args.grib_file)
    for num_in, file_var in df_files.iterrows():
        print("obtaining PBHL file:",  file_var["PBLH_file"])
        print("obtaining Multy file:",  file_var["Multi_file"])
        conta_PBLH.res_bound_merge(
            file_var["PBLH_file"],
            file_var["Multi_file"],
            args.path_dir_merge, 
            args.path_dir_resample,
            args.file_metropolitan_polygon,
            args.path_dir_cut,
            dic_grib_info=dic_grib,
        )
    
