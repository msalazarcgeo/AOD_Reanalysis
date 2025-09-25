#!/usr/bin/env Rscript
rm(list = ls())
library(argparser, quietly=TRUE)

# Rscript year_prediction_day_just_script.R --path_estations_cont "./datos/datos_limpios_03_2023/BDA_dia_sr.Rda" --path_estations_location "./datos/Estaciones/Coord_estaciones_SINAICA.xlsx" --path_save_predictions "./datos/raster/ZMVM_predictions_9PBLH/2017_bis/" --prefix_save "predi_25PM_" --path_save_coef "./datos/coeff/" --path_data_raster "./datos/raster/middle_PBLH/all_bands/2017/" --path_vialidades "./datos/raster/Vialidades_Miguel/ZMVM/all_vialidades_2.tif" -s "2017-01-01" -e "2017-12-31" --year 2017 --file_name_datos_pm "./datos/datos_pm_2017.rda"


all_function <- function(
    rda_path,
    est_loc_path,
    start_date,
    end_date,
    zona_metro,
    year,
    path_data_rast_year,
    path_vialidades,
    path_save_coeff,
    path_save_rasters,
    file_name_datos_pm,
    #prefix_save=FALSE,
    ...
) {
    if (!require("pacman")) install.packages("pacman");
    source("./year_prediction_day_just.R")
    print("Data to get")
    print("R data path:")
    print(rda_path)
    print("Estation data path:")
    print(est_loc_path)
    df_all_data  <- all_var_dataframe(
        start_date,
        end_date,
        zona_metro,
        year,
        rda_path,
        est_loc_path,
        path_data_rast_year,
        path_vialidades,
        #prefix_file
        ...
    )
    df_all_data_scale <- zscore_values_pm(df_all_data)    %>% drop_na()
    if(TRUE){
        print(head(df_all_data_scale, n= 15))
    }
    
    model_just <- lmer(
        "PM25_z~ Optical_Depth_047_z + temperature_2m_z  + relative_humidity_z +  total_precipitation_sqrt_z+PBLH_z+ vialidades_z+(1 + Optical_Depth_047_z | dia )",
        data = df_all_data_scale
    )
    df_all_data_scale $predict_just <- predict(
        model_just,
        newdata = df_all_data_scale, 
        allow.new.levels = TRUE
    )
    save(df_all_data_scale, file = file_name_datos_pm)
    df_scalars_values <- get_df_scalars(df_all_data_scale)
    coef_table <- coef(model_just)
    coef_table <- data.frame(coef_table$dia)
    name_coef <- paste0(
        path_save_coeff,
        "coefficientes_",
        year,
        "_modelos.rda"
    )
    print('Saving_file of coefficients:')
    print(name_coef)
    save(coef_table, file = name_coef)

    get_raster_predictions(
        start_date,
        end_date,
        path_data_rast_year,
        path_vialidades,
        coef_table,
        df_scalars_values,
        path_save_rasters,
        zona_metro = zona_metro,
        prefix_save = prefix_save
    )
}


main <- function() {
    
    p <- arg_parser("A program to create the predictions")
    p <- add_argument(
        p,
        "--path_estations_cont",
        help = "path to Rda with the stations contaminants"
    )
    p <- add_argument(
        p,
        "--path_estations_location",
        help = "path to xls with the estations",
        default = "./data/Estaciones/Coord_estaciones_SINAICA.xlsx"
    )
    p <- add_argument(
        p,
        "--path_save_predictions",
        help = "path to save the predictions"
    )
    p <- add_argument(
        p,
        "--prefix_save",
        help = "prefix to save the files"
    )
    p <- add_argument(
        p,
        "--path_save_coef",
        help = "path to save the coefficients of the model"
    )
    p <- add_argument(
        p,
        "--path_data_raster",
        help = "path to the directory where the rasters with all bands are stored"
    )
    p <- add_argument(
        p,
        "--path_vialidades",
        help = "path to the file with the raster with streets"
    )
    p <- add_argument(
        p,
        "--start_date",
        help = "String with the initial date 'YYYY-MM-DD'"
    )
    p <- add_argument(
        p,
        "--end_date",
        help = "String with the final date 'YYYY-MM-DD'"
    )
    p <- add_argument(
        p,
        "--zona_metro",
        help = "String with the metropolitan zone 'ZMVM' , 'AMM' , 'ZMG', 'ZMVT'"
    )
    p <- add_argument(
        p,
        "--year",
        help = "Year"
    )
    p <- add_argument(
        p,
        "--loc_est",
        help = "path to the file with the localization of the stations"
    )
    p <- add_argument(
        p,
        "--file_name_datos_pm",
        help = "path to the save the model"
    )
    #print(p)
    argv <- parse_args(p)
    cosa_rda <- argv$path_estations_cont[[1]]
    all_function(
        cosa_rda,
        est_loc_path =argv$path_estations_location,
        start_date = argv$start_date,
        end_date = argv$end_date,
        zona_metro = argv$zona_metro,
        year = argv$year,
        path_data_rast_year = argv$path_data_raster,
        path_vialidades = argv$path_vialidades,
        path_save_coeff = argv$path_save_coef,
        path_save_rasters = argv$path_save_predictions,
        prefix_save = argv$prefix_save,
        file_name_datos_pm = argv$file_name_datos_pm,
        verbose=TRUE
    )
}
main()
