## -----------------------------------------------------------------------------

df_te_se <- get_dataframe_season_spline(
    path_spline = './datos/datos_model3/cell_vals_ses_on_splinesestation.csv',
    season_str = 'warm',
    year_string = '2011',
    path_prediction = "./datos/raster/ZMVM_All/all_predictions_symlinks_ZMVM/",
    predi_prefix = 'predi_25PM_' ,
    str_band= 'PM25_predict',
    string_zm = ''
)
head(df_te_se)




## -----------------------------------------------------------------------------

df_pred_warm_2011 <- model3_prediction_daily_season_df( 
    start_date = '2011-03-01',
    end_date   = '2011-04-30',
    path_predictions= "./datos/raster/ZMVM_All/all_predictions_symlinks_ZMVM/",
    season_str = 'warm',
    year_string = '2011',
    predi_prefix =  'predi_25PM_' ,
    str_band  ='PM25_predict',
    string_zm   = 'ZMVM',
    est_ubi_loc =  estaciones_ubi ,
    season_path_spline_df ='./datos/datos_model3/cell_vals_ses_on_splinesestation.csv',
    season_path_spline = './datos/raster/ZMVM_All/seasons_total_ZMVM/',
    df_est_data  = BDA_dia
)



## -----------------------------------------------------------------------------

#' The function saves the corresponding day of the dataframe as rassters 
#' @param df_predictions dataframe with the daily data and predictions 
#' @param path_save_predic where to store the resulting rasters 
#' @param prefix_predic prefix to save 
#' @param csr_obj_save crs of the rasters to save 
#' @param extent_obj the extent of the raster (The coord of the bb)
#' 
save_days_df_2_raster <- function(
    df_predictions, # dataframe with the predictions for each day 
    path_save_predic,
    prefix_predic,
    csr_obj_save,
    extent_obj
){
    print(head(df_predictions$dia))
    dias_posible <- unique( df_predictions$dia)
    for(dia_pos in dias_posible){
        dia_date = format(dia_pos , format= "%Y_%m_%d")
        print(dia_date)
        dia_cel_posible <- df_predictions[df_predictions$dia == dia_date, ]
        array_pred_posible  <- array(,
            c(max(dia_cel_posible$y ), max(dia_cel_posible$x))
        )
        for(i in 1:nrow(dia_cel_posible)){
            array_pred_posible[dia_cel_posible[i,"y"],  dia_cel_posible[i,"x"]] <- dia_cel_posible[i,"val_pred_model"]
        }
        date_dia_str_posible <- format(dia_date , format= "%Y_%m_%d")
        
        ras_pred_com_posible <- raster(array_pred_posible) 
        crs(ras_pred_com_posible) <- csr_obj_save
        extent(ras_pred_com_posible)<- extent_obj
        names(ras_pred_com_posible) <- "Spline_day"
        #update(ras_pred_com_posible, names=TRUE)
        print("saving file ")
        
        
        file_save_name<- paste0(
            path_save_predic,
            prefix_predic,
            "_",
            date_dia_str_posible,
            "_",
            "Spline_day",
            ".tif"
        )

        print(file_save_name)

        raster::writeRaster(
            ras_pred_com_posible, 
            filename=file_save_name,
            format = "GTiff",
            overwrite=TRUE
        )
    
    }
    return()
}



## -----------------------------------------------------------------------------
df_pred_warm_2011_f<- bind_rows( df_pred_warm_2011)
raster_complete <- raster::brick("./datos/raster/ZMVM_All/all_predictions_symlinks_ZMVM/predi_25PM_2004_01_01.tif")
csr_obj_to_<- crs(raster_complete)
extent_obj_to_ <- extent(raster_complete)  
save_days_df_2_raster(
    df_pred_warm_2011_f,
    "./datos/raster/ZMVM_All/model_3_test/",
    "pred_mod3_",
    csr_obj_to_,
    extent_obj_to_ 
)


## -----------------------------------------------------------------------------
predic_season_all_m3 <- function(
    season_str, 
    season_year, 
    start_date, 
    end_date, 
    path_predictions,
    str_zm,
    predi_prefix, #=  'predi_25PM_'
    str_band,  #='PM25_predict',
    est_ubi_loc, #=  estaciones_ubi ,
    path_good_raster, ### path to the good raster to extract csr and
    season_path_spline_df,# ='./datos/datos_model3/cell_vals_ses_on_splinesestation.csv',
    season_path_spline,# = './datos/raster/ZMVM_All/seasons_total_ZMVM/',
    df_est_data,  #= BDA_dia3 
    path_save_pred_m3, 
    prefix_pred_m3 ="pred_mod3_",
    
    ...
){
    df_pred_season_li  <- model3_prediction_daily_season_df( 
        start_date = start_date,
        end_date   = end_date,
        path_predictions=path_predictions,
        season_str =  season_str,
        year_string = season_year,
        predi_prefix = predi_prefix,
        str_band  =str_band,
        string_zm   = str_zm,
        est_ubi_loc =  estaciones_ubi ,
        season_path_spline_df =season_path_spline_df,
        season_path_spline = season_path_spline,
        df_est_data  = df_est_data
    )

    df_pred_season_f<- bind_rows( df_pred_season_li)
    raster_complete <- raster::brick(path_good_raster)
    csr_obj_to_<- crs(raster_complete)
    extent_obj_to_ <- extent(raster_complete)  
    save_days_df_2_raster(
        df_pred_season_f,
        path_save_pred_m3,
        prefix_pred_m3,
        csr_obj_to_,
        extent_obj_to_ 
    )
}







## -----------------------------------------------------------------------------
predic_season_all_m3(
    season_str = "warm",
    season_year = "2011",
    start_date = '2011-03-01',
    end_date = '2011-04-30',
    path_predictions = "./datos/raster/ZMVM_All/all_predictions_symlinks_ZMVM/",
    str_zm = 'ZMVM',
    predi_prefix = 'predi_25PM_' ,
    str_band = 'PM25_predict',
    est_ubi_loc = estaciones_ubi,
    path_good_raster = "./datos/raster/ZMVM_All/all_predictions_symlinks_ZMVM/predi_25PM_2004_01_01.tif",
    season_path_spline_df = './datos/datos_model3/cell_vals_ses_on_splinesestation.csv',
    season_path_spline = './datos/raster/ZMVM_All/seasons_total_ZMVM/',
    df_est_data = BDA_dia, 
    path_save_pred_m3 = "./datos/raster/ZMVM_All/model_3_test/warm_2011/",
    prefix_pred_m3 = paste0("pred_m3_","warm", "_", "2011")
)










## -----------------------------------------------------------------------------


get_dates_season("warm_2012")







## -----------------------------------------------------------------------------
te <- read.csv( './datos/datos_model3/cell_vals_ses_on_splinesestation.csv', # Read only header of example data
           head = TRUE,
           nrows = 1
           )
seasons_avilable<- names(te)[4:ncol(te)]

for (season_st in seasons_avilable){
    print(season_st)
    vec_dates <- get_dates_season(season_st)
    vec_se_year <- str_split( season_st, "_")  
    season_str_s <- vec_se_year[[1]][1]
    season_year_s<- vec_se_year[[1]][2]
    predic_season_all_m3(
        season_str = season_str_s,
        season_year = season_year_s,
        start_date = vec_dates[[1]],
        end_date = vec_dates[[2]],
        path_predictions = "./datos/raster/ZMVM_All/all_predictions_symlinks_ZMVM/",
        str_zm = 'ZMVM',
        predi_prefix = 'predi_25PM_' ,
        str_band = 'PM25_predict',
        est_ubi_loc = estaciones_ubi,
        path_good_raster = "./datos/raster/ZMVM_All/all_predictions_symlinks_ZMVM/predi_25PM_2004_01_01.tif",
        season_path_spline_df = './datos/datos_model3/cell_vals_ses_on_splinesestation.csv',
        season_path_spline = './datos/raster/ZMVM_All/seasons_total_ZMVM/',
        df_est_data = BDA_dia, 
        path_save_pred_m3 = paste0("./datos/raster/ZMVM_All/model_3_predict/",
            season_year_s, "/"
            ),
        prefix_pred_m3 = paste0("pred_m3_",season_str_s, "_", season_year_s)
    )
}

