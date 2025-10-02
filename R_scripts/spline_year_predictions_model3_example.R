## -----------------------------------------------------------------------------
#| purl: true
#| echo: false
rm(list = ls())
if (!require("pacman")) install.packages("pacman");

pacman::p_load(tidyverse, sf, lubridate, ggplot2, ggpmisc, lme4, gridExtra, magrittr, dplyr, geosphere, Metrics,MLmetrics, ggridges, reshape2, gstat, raster,tibble, spacetime, rlang,  multilevelTools, readxl, mgcv, knitr)

source("./Just_todo_process.R")
source("./Just_todo_2.R")
source("./spline_year_predictions.R")
source("./year_prediction_day_just.R")


## -----------------------------------------------------------------------------
#| purl: true
#| echo: false

#' Get the dataframe for the season  
#' 
#' returns a DataFrame with the coords, the prediction 
#'  and the splin value for all ther avilable days in the interval 
#' 
#' 
#' 
#' @param season_str
#' @param year_string
#' @param path_spline
#' @param path_prediction
#' @param predi_prefix
#' @param string_zm
#' @param str_band

get_dataframe_season_spline <- function( 
    season_str, # season string
    year_string, # year of the season 
    path_spline, #path where the df with the splines are located 
    path_prediction, ##path where the predicions are store 
    predi_prefix, ##prefix files with the predictions `
    string_zm , ## zona metro str"_ZMVM_"
    str_band #### String metropolitan zone  
){
    #### Como ya salve todos los dados en un rds lo traigo y me quedo con el que quiero 
    
    df_splines <- read.csv(path_spline, header=TRUE)
    print(head(df_splines))
    seson_col <- paste0(season_str, '_',year_string)
    df_splines <-     df_splines[, c('x','y', seson_col   )] ## para obtener el modelo 
    #####@# get all the prediction values for the season interval 
    print(seson_col)
    if(season_str== 'warm'){
        start_date <- as.Date(paste0(year_string, '-','03-01' ))
        end_date <- as.Date(paste0(year_string, '-','04-30' ))
    }
    else if (season_str== 'rain') {
       start_date <- as.Date(paste0(year_string, '-','05-01' ))
       end_date <- as.Date(paste0(year_string, '-','10-31' ))
    }
    else if (season_str== 'cold') {
        start_date <- as.Date(paste0(year_string, '-','11-01' ))
        end_date <- as.Date(paste0(as.numeric(year_string)+1, '-','02-28' ))    
    }
    day_season_in = seq( from= start_date , to =end_date, by= 1)
    ests_day_pred<-list()
    counter <- 1
    for (day in day_season_in){
        print(as.Date(day))
        year_str = toString(year(as.Date(day)))
        day_str =  format(as.Date(day) , format= "%Y_%m_%d")
        #datos/raster/Cross_val/ZMVM/2010/predi_25PM_ACO_ZMVM_2010_10_30.tif
        file_day <- paste0(
            path_prediction,
            predi_prefix,
            #'_',
            #string_zm,
            #'_',
            day_str,
            ".tif"
        )
        print("getting file:")
        print(file_day)
        if(file.exists(file_day)){
            rast_day <- raster::brick(file_day)
            if(str_band %in% names(rast_day)){
            rast_day_val <- as.data.frame(rast_day, xy=TRUE)
            rast_day_val <-  rast_day_val[,c("x","y", str_band)]
            rast_day_val$y <- rowFromY(rast_day, rast_day_val$y)
            rast_day_val$x <- colFromX(rast_day, rast_day_val$x)
            rast_day_val$dia <- as.Date(day)
            ests_day_pred[[counter]] <-  rast_day_val
            counter <- counter+1
            }
            else{
                print("No spline for day")
            }
        }
        else{
           print("No file for day")
        }
        
        
    }
    print(head(ests_day_pred))
    df_estas_day_pred<- bind_rows(ests_day_pred)
    df_estas_day_pred_te <-  df_estas_day_pred %>% left_join(df_splines, by = c("x"="x", "y"="y"))
    
    return(df_estas_day_pred_te)
}


## -----------------------------------------------------------------------------
#| purl: true
#| echo: false

#' Get a data frame with x y coordinates and all the values
#'  start_date, # initial date season
#' @param end_date, # final date season
#' @param path_predictions, # Path to the predictions 
#' @param season_str, # string season 
#' @param year_string, # string year
#' @param predi_prefix, #  'predi_25PM_' 
#' @param str_band, # 'PM25_predict'
#' @param string_zm, #'ZMVM'
#' @param est_ubi_loc, # Location station df 
#' @param season_path_spline_df, # path of spline dataframe sstore 
#' @param season_path_spline, #path to the splines 
#' @param df_est_data # station measure 
model3_prediction_daily_season_df <- function(
    start_date, # initial date season
    end_date, # final date season
    path_predictions, # Path to the predictions 
    season_str, # string season 
    year_string, # string year
    predi_prefix, #  'predi_25PM_' ,
    str_band, # 'PM25_predict',
    string_zm, #'ZMVM'
    est_ubi_loc, # Location station df 
    season_path_spline_df, # path of spline dataframe sstore 
    season_path_spline, #path to the splines 
    df_est_data # station measure 
){
    ###
    print("getting the mesure data")
    
    tempo_est_df <- df_est_data %>% 
        filter(  grepl(string_zm , df_est_data$CVE_EST )  ) %>% 
        filter(dia >= as.Date(start_date) & dia <= as.Date(end_date))

    mean_day_season <- tempo_est_df %>% group_by(dia) %>% summarize(mean_day = mean(PM2.5_S50))
    string_season  <- paste0(season_str,'_',year_string) 
    ###### get the 
    df_splin_se <- get_dataframe_season_spline(
        path_spline =  season_path_spline_df,
        season_str = season_str,
        year_string = year_string,
        path_prediction = path_predictions,
        predi_prefix = predi_prefix,
        str_band = str_band,
        string_zm = string_zm
    )
    df_splin_se <- df_splin_se %>% left_join( mean_day_season, by=  c("dia"= "dia"))
    print(head(df_splin_se))
    ####
    df_splin_se<- df_splin_se%>%
        mutate(
            sqr_pred = sqrt(PM25_predict),
            sqr_mean_day = sqrt(mean_day),
            
        ) 
    df_splin_se$sqr_sspline = sqrt(df_splin_se[,c(string_season)])
    
    print("getting the model")
    #########Aqui se tendria que escalar pero ha funcionado el la validacion cruzada
    ##### asi que lo vamos a dejar 
    print(head(df_splin_se))
    model_season_spline  <- lm(sqr_pred ~  sqr_mean_day +sqr_sspline  , data = df_splin_se)
    


    #########Ya con el modelo vamos a obtener la prediccion 
    

    
    
    print("Calculate the estimation:" )
    df_rast_spline <-  read.csv(season_path_spline_df, header=TRUE) 
    df_rast_spline <- df_rast_spline[, c("x","y", string_season)]
    print(head(mean_day_season) )
    days_season <- seq( from= as.Date(start_date) , to =as.Date(end_date), by= 1)
    

    list_dias_df_li <- list() 
    counter <- 1
    for(dia_no_aod in days_season ){
        dia_str <- format(as.Date(dia_no_aod) , format= "%Y_%m_%d")
        if (as.Date(dia_no_aod)%in% mean_day_season$dia){
            mean_dia <- mean_day_season[mean_day_season$dia == dia_no_aod,]$mean_day[[1]]
            df_day_temp <- df_rast_spline 
            df_day_temp$dia <- dia_str
            df_day_temp$mean_day <- mean_dia
            df_day_temp$Spline <- df_day_temp[,c(string_season)]
            df_day_temp <- df_day_temp%>% 
                mutate(
                    sqr_mean_day = sqrt(mean_day),
                    sqr_sspline = sqrt(Spline)
                )
    
            #print(head(df_day_temp))
            df_day_temp$pred_model  <-  predict(model_season_spline, newdata = df_day_temp)
            df_day_temp <- df_day_temp%>%
            mutate (val_pred_model = pred_model^2)       
            list_dias_df_li[[counter]] <- df_day_temp

            counter <- counter +1
        }
        else{
            print("No data for the estations on this day: ")
            print(dia_str)
        }   
    }
    return(list_dias_df_li)
}




## -----------------------------------------------------------------------------
#| purl: true
#| echo: false

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
#| purl: true
#| echo: false
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
#| purl: true
#| echo: false
get_dates_season <- function(season_str){
    split_ <-str_split(season_str, '_')[[1]]
    season_str <- split_[1]
    year_str <- split_[2]
    station_str <- split_[3]
    #print(season_str)
    #print(year_str)
    #print(station_str)
    if(season_str == 'warm'){
        start_date_str = paste0(year_str, '-03-01')
        end_date_str = paste0(year_str, '-04-30')
    }
    else if (season_str =='rain') {
        start_date_str = paste0(year_str, '-05-01')
        end_date_str = paste0(year_str, '-10-30')
    }
    else if (season_str =='cold'){
        start_date_str = paste0(year_str, '-11-01')
        end_date_str = paste0(as.numeric(year_str)+1, '-02-28')
    }
    dates_ret <- c(start_date_str,end_date_str )
    names(dates_ret)<- c("start_date","end_date" )
    return(dates_ret)
}


