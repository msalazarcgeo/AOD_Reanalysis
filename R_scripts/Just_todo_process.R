
pacman::p_load(tidyverse, sf, lubridate, ggplot2, ggpmisc, lme4, gridExtra,
 magrittr, dplyr, geosphere, Metrics, ggridges, reshape2, gstat, raster,
 spacetime, rlang, geoR, multilevelTools, readxl, mgcv, knitr
)









#rcrs <- crs(raster_mue_ado)
#ptrans <- spTransform(est_coords, rcrs)




#' Get the day values for the raster 
#' 
#' @description 
#' The function will search for the day estation values in the rasters and 
#' exctract the coresponding data.
#' 
#' @param dia  date 
#' @param path_rasters string 
#' @param prefix  string 
#' @param est_df data.frame  The dataframe with th day values
get_day_values_raster <- function(
        dia,
        path_rasters,
        prefix =  "aod_cli_PBLH_ZMVM_",
        estation_loc ,
        est_df
    ){
    
    file_name_raster <- paste0(path_rasters,prefix, dia,".tif")
    print(file_name_raster)
    if(file.exists(file_name_raster)){
        raster_dia_ado   <-  raster(file_name_raster, 7)
        raster_dia_temp  <-  raster(file_name_raster, 3)
        raster_dia_PBLH  <-  raster(file_name_raster, 9)
        raster_dia_rh    <-  raster(file_name_raster, 6)
        raster_dia_pre_h <-  raster(file_name_raster, 4)
        raster_dia_pre_t <-  raster(file_name_raster, 5)
        
        est_df$Optical_Depth_047 <- raster::extract(raster_dia_ado, estation_loc)
        est_df$temperature_2m <- raster::extract(raster_dia_temp, estation_loc)
        est_df$PBLH <-raster::extract(raster_dia_PBLH, estation_loc)
        est_df$relative_humidity <- raster::extract(raster_dia_rh, estation_loc)
        est_df$total_precipitation_hourly <- raster::extract(raster_dia_pre_h, estation_loc)
        est_df$total_precipitation <- raster::extract(raster_dia_pre_t, estation_loc)

    }
    else{

        est_df$Optical_Depth_047 <- NA
        est_df$temperature_2m <- NA
        est_df$PBLH <- NA
        est_df$relative_humidity <- NA
        est_df$total_precipitation_hourly <- NA
        est_df$total_precipitation <-  NA 

    }
    
    return(est_df)
    
}


























#' Obtain the predictions of the PM 2.5  
#' 
#' @description 
#' The function wil get a raster with the PM25 prediction using the 
#'  bands 
#' 
#' @details 
#' 
#' @param dia The string of the day to get YYYY_MM_DD
#' @param path_rasters String of the path where the rasther with the 
#' bands are stored 
#' @param prefix String with the prefix of the files 
#' @param coeff_df_model a dataframe with the coeficients of the day
#'  for the bands
#' @param scale_values_df a dataframe with the scale values of the z-scores
#' @param vialidades_r a raster with the streets. 
#' @return 
pre_ras_pm25_dia <- function(
        dia,
        path_rasters,
        prefix =  "aod_cli_PBLH_ZMVM_",
        coeff_df_model, 
        scale_values_df,
        vialidades_r,
        ret_all= FALSE
){
    file_name_raster <- paste0(path_rasters,prefix, dia,".tif")
    print(file_name_raster)
    if(file.exists(file_name_raster)){
        raster_dia_ado   <-  raster::raster(file_name_raster, 7)
        raster_dia_temp  <-  raster::raster(file_name_raster, 3)
        raster_dia_PBLH  <-  raster::raster(file_name_raster, 9)
        raster_dia_rh    <-  raster::raster(file_name_raster, 6)
        raster_dia_pre_t <-  raster::raster(file_name_raster, 5)
        raster_dia_unce_ado <- raster::raster(file_name_raster, 8)    
        print("todo bien")
    }
    else{
        print ("No hay archivo raster para predicciones")

        return(raster::stack())
    }

    
    
    raster_dia_ado_z <- (raster_dia_ado -as.numeric(scale_values_df[scale_values_df$Variable== "Optical_Depth_047_z",]$center) )/(as.numeric(scale_values_df[scale_values_df$Variable== "Optical_Depth_047_z",]$scale))
    print("AOD")
    raster_dia_temp_z <- (raster_dia_temp -as.numeric(scale_values_df[scale_values_df$Variable== "temperature_2m_z",]$center) )/(as.numeric(scale_values_df[scale_values_df$Variable== "temperature_2m_z",]$scale))
    print("Temperature 2m")
    raster_dia_PBLH_z <- (raster_dia_PBLH -as.numeric(scale_values_df[scale_values_df$Variable== "PBLH_z",]$center) )/(as.numeric(scale_values_df[scale_values_df$Variable== "PBLH_z",]$scale))
    print("PBLH")
    raster_dia_rh_z <- (raster_dia_rh -as.numeric(scale_values_df[scale_values_df$Variable== "relative_humidity_z",]$center) )/(as.numeric(scale_values_df[scale_values_df$Variable== "relative_humidity_z",]$scale))
    print("Relative humidity")
    raster_dia_pre_t_sqr_z <- (sqrt(raster_dia_pre_t) -as.numeric(scale_values_df[scale_values_df$Variable== "total_precipitation_sqrt_z",]$center) )/(as.numeric(scale_values_df[scale_values_df$Variable== "total_precipitation_sqrt_z",]$scale))
    print("Square Precipitation")
    


    vialidades_dia_r_z <- (vialidades_r -as.numeric(scale_values_df[scale_values_df$Variable== "vialidades_z",]$center) )/(as.numeric(scale_values_df[scale_values_df$Variable== "vialidades_z",]$scale))
    print("Vialidades")
    
    dia_date <- as.Date(paste0(str_split(dia, '_')[[1]],collapse="/"))
    print(dia_date)
    if (length(setdiff( 
        c("X.Intercept.","Optical_Depth_047_z" ,"temperature_2m_z" ,
        "relative_humidity_z", "total_precipitation_sqrt_z", "PBLH_z", 
        "vialidades_z"),
         names(coeff_df_model)
    )) != 0){
        val <-setdiff( 
            c("X.Intercept.","Optical_Depth_047_z" ,"temperature_2m_z" ,
             "relative_humidity_z", "total_precipitation_sqrt_z", "PBLH_z", 
             "vialidades_z"
            ),
            names(coeff_df_model)
        )
       coeff_df_model[,eval(val)] <- 0

    }
    if (dia_date %in% row.names(coeff_df_model) ){

        raster_pm25_dia_z<-  coeff_df_model[ row.names(coeff_df_model) == dia_date,]$X.Intercept. +
        (raster_dia_ado_z*coeff_df_model[ row.names(coeff_df_model) == dia_date,]$Optical_Depth_047_z) +
        (raster_dia_temp_z*coeff_df_model[ row.names(coeff_df_model) == dia_date,]$temperature_2m_z) +
        (raster_dia_rh_z*coeff_df_model[ row.names(coeff_df_model) == dia_date,]$relative_humidity_z) +
        (raster_dia_PBLH_z*coeff_df_model[ row.names(coeff_df_model) == dia_date,]$PBLH_z)+
        (vialidades_dia_r_z*coeff_df_model[ row.names(coeff_df_model) == dia_date,]$vialidades_z)+
        (raster_dia_pre_t_sqr_z*coeff_df_model[ row.names(coeff_df_model) == dia_date,]$total_precipitation_sqrt_z)

        print("Predicciones")
        raster_pm25 <- (raster_pm25_dia_z * (as.numeric(scale_values_df[scale_values_df$Variable== "PM25_z",]$scale)) + as.numeric(scale_values_df[scale_values_df$Variable== "PM25_z",]$center) )
        
        print("re-escalando las predicciones")
    } else {
        print (" No hay coeficientes para ese dia ")
        raster_pm25 <- raster::raster(
            ncol = ncol(raster_dia_unce_ado),
            nrow = nrow(raster_dia_unce_ado),
            ext = extent(raster_dia_unce_ado),
            crs= crs(raster_dia_unce_ado),
            
        )
    }
    raster_stack <- raster::stack(
        raster_dia_ado,
        raster_dia_temp,
        raster_dia_PBLH,
        raster_dia_rh,
        raster_dia_pre_t,
        vialidades_r,
        raster_pm25
        
    )
    if(nlayers(raster_stack)==7){
        names(raster_stack) <- c(
         "Optical_Depth_047",
         "temperature_2m",
         "PBLH", 
         "relative_humidity", 
         "total_precipitation",
         "vialidades",
         "PM25_predict"
        )
    }
    if(ret_all){
        ##### return all the layers obtained 
        raster_stack <- raster::stack(
            raster_dia_ado,
            raster_dia_temp,
            raster_dia_PBLH,
            raster_dia_rh,
            raster_dia_pre_t,
            vialidades_r,
            raster_pm25,
            raster_dia_ado_z,
            raster_dia_temp_z,
            raster_dia_PBLH_z,
            raster_dia_rh_z,
            raster_dia_pre_t_sqr_z,
            vialidades_dia_r_z,
            raster_pm25_dia_z
        )
        names(raster_stack) <- c(
         "Optical_Depth_047",
         "temperature_2m",
         "PBLH", 
         "relative_humidity", 
         "total_precipitation",
         "vialidades",
         "PM25_predict",
         "Optical_Depth_047_z",
         "temperature_2m_z",
         "PBLH_z", 
         "relative_humidity_z", 
         "total_precipitation_z",
         "vialidades_z",
         "PM25_predict_z"
        )

    }

    return(raster_stack) 
}

























#' @title 
#' Exctracts the raster data and put it on a dataframe 
#' 
#' @details 
#' The function will search for the area of the raster that is valid and 
#' exctract the corresponding values 
#' 
#' @param file_path string
#' @param ban_complete string the name of the complete band to use
#' @param ban_spline string the name of the band to create the spline
#' @return dataframe with coordinates and data values
raster_dataframe <- function(
        file_pat,
        ban_complete,
        ban_to_spline){
    
    # print(file_pat)
    # print(ban_complete)
    # print(ban_to_spline)
    raster_brick_all <- raster::brick(file_pat)
    if (!(ban_to_spline %in% names(raster_brick_all)) ){
        print("No hay banda para hacer el spline")
        print("Archivo: ")
        print(file_pat)
        val_band <-  data.frame(matrix(ncol = 3, nrow = 0))
        colnames(val_band) <- c("y_coo","x_coo","value")
        return (val_band)
    }
    
    raster_to_spline <-  raster_brick_all[[ban_to_spline]]

    raster_complete <- raster_brick_all[[ban_complete]]
    mat_complete =as.matrix(raster_complete)
    ###Tomar la parte de interes del raster
    row_tu = list()
    counter_fu = 1
    for( row in seq(1,nrow(mat_complete))){
        nan_row_num <- which(!is.na(mat_complete[row,]))    
        max_row <- max(nan_row_num)
        min_row <- min(nan_row_num)
        tuple_row = c(row, min_row, max_row)
        row_tu[[counter_fu]] <- tuple_row
        counter_fu <- counter_fu +1
    }
    dif_li = list()
    counter_fu <-1
    for(tuple_r in row_tu ){
        dif_li[[counter_fu]] <- tuple_r[3]-tuple_r[2]
        counter_fu <- counter_fu +1
    }   
    size_cols = max(unlist(dif_li) )
    row_elem = list()
    counter_fu = 1
    for(tu_row in row_tu){
        row_elem[[counter_fu]]= raster_to_spline[tu_row[1], tu_row[2]:(tu_row[2]+size_cols) ]
        counter_fu <- counter_fu +1
    }

    arr_interes <- do.call("cbind",row_elem)
    arr_interes <- t(arr_interes )

    #### Ponerlo dentro de un dataframe
    x_int <- seq(0, 1, length.out = ncol(arr_interes))
    y_int <- seq(0, 1, length.out = nrow(arr_interes))
    inde_arr <- expand.grid(
            y_coo = 1:nrow(arr_interes),
            x_coo = 1:ncol(arr_interes)
            )
    all_val <- list()
    counter_fu <- 1
    for(i in 1:nrow(inde_arr)){
        all_val[[counter_fu]] <- arr_interes[inde_arr[["y_coo"]][i], inde_arr[["x_coo"]][i]]
        counter_fu  = counter_fu + 1
    }

    val_band <-  data.frame(inde_arr, unlist(all_val))

    names(val_band) <- c("y_coo","x_coo","value")

    return(val_band)

}



#' Create the season spline 
#' 
#' @details 
#' The function create the raster for the spline of the season.
#' 
#' @param file_pat_dir string of the path to the directory where the
#'   PM 2.5 predictions are store
#' @param save_path_raster string Path to the directory where the predictions
#'  will be stored
#' @param ban_complete string of the band to a complete band
#' @param ban_to_spline string of the band to generate the spline
#' @param start_date string starting date "YYYY/MM/DD"
#' @param end_date string end date "YYYY/MM/DD"
#' 
#' 
#'
season_splin <- function(
    file_pat_dir,
    save_path_raster,
    ban_complete,
    ban_to_spline, 
    start_date, 
    end_date,
    ...
    ){

    ###### encontramos los rasters que queremos 
    files_path <- list.files(file_pat_dir, pattern = "\\.tif$")
    files_path <- files_path[grepl(".tif", files_path)]
    file_df <- as.data.frame(files_path)
    names(file_df) <- "Nombres"
    file_df$numbers <- gsub("\\D", "", file_df$Nombres)
    
    file_df <-   file_df %>% 
                mutate( ANO = substr(numbers,3, 6)  ) %>%
                mutate( MES = substr(numbers,7, 8)  ) %>% 
                mutate( DIA = substr(numbers,9, 10)  ) %>%
                mutate( date = paste0(ANO,'/', MES, '/',DIA) ) %>%
                mutate( date = as.Date( date) )
    start_date_date <- as.Date(start_date)
    end_date_date <- as.Date(end_date)
    file_df <- file_df %>% 
            filter( date >= start_date_date & date < end_date_date)
    print(file_df)
    #######
    # Ahora si con los rasters que queremos vamos a ponerlos todos dentro de un dataframe para hacer el modelo

    file_df <- file_df%>%
        mutate(path_total =  paste0(file_pat_dir , Nombres))
    print("files to read:")
    print(file_df$path_total)

    all_val_df <- data.frame()
    for(file_pa  in file_df$path_total ){
        df_rast_ <- raster_dataframe( file_pa, ban_complete, ban_to_spline)
        all_val_df <- rbind(all_val_df, df_rast_)
    }
    print("se termina la carga de los raster al dataframe")
    #### Se obtiene el modelo 
    print(head(all_val_df))
    if(! ("value" %in% names(all_val_df))){
        all_val_df$value <- all_val_df$PM25_predict
    }
    mod_te_val <- gam(
        value~te(y_coo,x_coo,k=c(10,10)),
        bs = "ps" ,
        data = all_val_df,
        method = "NCV"
    )

    ##### Se hace una malla para hacer la prediccion 
    print('Hacer malla')
    inde_arr <- expand.grid(
            y_coo = 1:max(all_val_df$y_coo),
            x_coo = 1:max(all_val_df$x_coo)
            )
    df_predic <- data.frame(inde_arr)

    names(df_predic) <- c("y_coo","x_coo")
    df_predic$predictions <- predict.gam(mod_te_val,
                 newdata= df_predic
    )

    array_pred <- array(,
            c(max(all_val_df$y_coo ), max(all_val_df$x_coo) )
    )
    for(i in 1:nrow(df_predic)){
        array_pred[df_predic[i,"y_coo"],  df_predic[i,"x_coo"]] <- df_predic[i,"predictions"]
    }
    array_pred<- as.matrix(array_pred)
    
    ###### Se necesita el raster completo 
    #### Como el area de estudio es la misma y necesitamos la banda 
    # completa voy a suponer que no importa el archivo que tome
    ## 
    brick_all <- raster::brick(file_df$path_total[2])
    raster_complete <-  brick_all[[ban_complete]]
    ##### Esto es cuando no teniamos el sistema de referencia 
    ###EPSG:4326
    # # brick_all <-raster::brick(file_df$path_total[2])
    # # raster_complete <-  brick_all[[ban_complete]]
    # # mat_complete =as.matrix(raster_complete)
    # # row_tu = list()
    # # counter_fu = 1
    # # for( row in seq(1,nrow(mat_complete))){
    # #     nan_row_num <-which(!is.na(mat_complete[row,]))    
    # #     max_row <- max(nan_row_num)
    # #     min_row <- min(nan_row_num)
    # #     tuple_row = c(row, min_row, max_row)
    # #     row_tu[[counter_fu]] <-tuple_row
    # #     counter_fu <-counter_fu +1
    # # }

    # # array_pred_com <- matrix(nrow =nrow(raster_complete),ncol= ncol(raster_complete) )
    # # for(i in row_tu){
    # #     array_pred_com[i[1],i[2]:(i[2]+119)] <- array_pred[i[1],1:120]
    # # }
    raster_pred <- raster(array_pred)
    crs(raster_pred) <- crs(raster_complete)
    extent(raster_pred) <- extent(raster_complete)
    names(raster_pred) <- "spline"
    #########
    print("Saving file :" )
    print(save_path_raster)

    writeRaster(raster_pred ,
        filename = save_path_raster,
        interpolate = FALSE,
        overwrite=TRUE
    )
}















#knitr::purl("./Just_todo_process.qmd")

