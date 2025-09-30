#| label: load-libraries
#| include: false
pacman::p_load(tidyverse, sf, lubridate, ggplot2, ggpmisc, lme4, gridExtra, magrittr, dplyr, geosphere, Metrics, ggridges, reshape2, gstat, raster, spacetime, rlang, multilevelTools, readxl, mgcv)




#| export: True

#' La funcion extrae los datos del raster y los pone dentro de un dataframe
#' Regresa un dataframe con las coordenadas y los valores 
#' 
#' 
#' @param  file_pat
#' @param  ban_complete
#' @param  ban_to_spline
#' @return dataframe 

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
        val_band <-  data.frame(matrix(ncol = 3, nrow = 0))
        colnames(val_band) <- c("y_coo","x_coo","value")
        return (val_band)
    }
    
    raster_to_spline<-  raster_brick_all[[ban_to_spline]]

    raster_complete <- raster_brick_all[[ban_complete]]
    mat_complete =as.matrix(raster_complete)
    ###Tomar la parte de interes del raster
    row_tu = list()
    counter_fu = 1
    for( row in seq(1,nrow(mat_complete))){
        nan_row_num <-which(!is.na(mat_complete[row,]))    
        max_row <- max(nan_row_num)
        min_row <- min(nan_row_num)
        tuple_row = c(row, min_row, max_row)
        row_tu[[counter_fu]] <-tuple_row
        counter_fu <-counter_fu +1
    }
    dif_li = list()
    counter_fu <-1
    for(tuple_r in row_tu ){
        dif_li[[counter_fu]] <- tuple_r[3]-tuple_r[2]
        counter_fu <-counter_fu +1
    }   
    size_cols = max(unlist(dif_li) )
    row_elem = list()
    counter_fu = 1
    for(tu_row in row_tu){
        row_elem[[counter_fu]]= raster_to_spline[tu_row[1], tu_row[2]:(tu_row[2]+size_cols) ]
        counter_fu <-counter_fu +1
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
        all_val[[counter_fu]]<- arr_interes[inde_arr[["y_coo"]][i], inde_arr[["x_coo"]][i]]
        counter_fu  = counter_fu + 1
    }

    val_band <-  data.frame(inde_arr, unlist(all_val))

    names(val_band) <- c("y_coo","x_coo",ban_to_spline)

    return(val_band)

}




















#' Funcion para obtener el dataframe por como un dataframe para cada dia  
#' dia es un string 
#' 


data_2_part_just <- function(
        dia ,
        file_path_prediction,
        file_path_season, 
        prefix_prediction = 'predi_25PM_',
        ban_exctract_prediction = "PM25_predict",
        est_all_data,
        ...
    ){
    
    date_dia <-  as.Date(dia)
    ### traer las predicciones del dia 
    date_dia_str<- paste0(
            lubridate::year(date_dia),
            '_',
            sprintf('%02d', lubridate::month(date_dia)),
            '_',
            sprintf('%02d', lubridate::day(date_dia))
    )

    file_pred_get <- paste0(file_path_prediction,
        prefix_prediction,
        date_dia_str,
        ".tif"
    )
    print('Archivo del dia')
    print(file_pred_get)

    # df_raster_dia_predict <- raster_dataframe(
    #     file_pred_get, 
    #     ban_complete  = ban_complete_prediction,
    #     ban_to_spline = ban_exctract_prediction
    # )

    if(file.exists(file_pred_get)){
        df_raster_dia_predict <- as.data.frame(raster::brick(file_pred_get), xy=TRUE)
    
        df_raster_dia_predict$x_coo <-raster::colFromX(
                    raster::brick(file_pred_get),
                    df_raster_dia_predict$x
        )
        df_raster_dia_predict$y_coo <- raster::rowFromY(
                    raster::brick(file_pred_get),
                    df_raster_dia_predict$y
        )
    }
    else{
        df_raster_dia_predict <-  data.frame(matrix(ncol = 3, nrow = 0))
        names(df_raster_dia_predict) <- c("y_coo","x_coo",ban_exctract_prediction)
    }
    
                
    ##Cuando no hay banda            
    
    if (!(ban_exctract_prediction %in% names(df_raster_dia_predict)) ){
        print("No hay banda para hacer el spline")
        df_raster_dia_predict <-  data.frame(matrix(ncol = 3, nrow = 0))
        names(df_raster_dia_predict) <- c("y_coo","x_coo",ban_exctract_prediction)
    }
    
    df_raster_dia_predict <- df_raster_dia_predict[
        ,c("y_coo","x_coo",ban_exctract_prediction)
    ]
    month_dia <-lubridate::month(date_dia)
    year_dia <-lubridate::year(date_dia)
    
    if(month_dia<3){
        season= paste0("cold_",year_dia -1)
        file_season_get <- paste0(file_path_season,
            '/',
            year_dia-1,
            '/',
            season,
            ".tif"
        )
    }
    else if (month_dia>=3 & month_dia<5) {
         season= paste0("warm_",year_dia)
         file_season_get <- paste0(file_path_season,
            '/',
            year_dia,
            '/',
            season,
            ".tif"
        )
    }
    else if(month_dia>=5 & month_dia<11){
        season= paste0("rain_", year_dia)
        file_season_get <- paste0(file_path_season,
            '/',
            year_dia,
            '/',
            season,
            ".tif"
        )
    }
    else if(month_dia>=11) {
        season= paste0("cold_", year_dia)
        file_season_get <- paste0(file_path_season,
            '/',
            year_dia,
            '/',
            season,
            ".tif"
        )
    }


    print('Archivo de la temporada')
    print(file_season_get)

    # df_raster_dia_spline <- raster_dataframe(
    #     file_season_get, 
    #     ban_complete  = "Spline",
    #     ban_to_spline = "Spline"
    # )
    df_raster_dia_spline <- as.data.frame(raster(file_season_get), xy=TRUE)
    print(head(df_raster_dia_spline,2))

    df_raster_dia_spline$x_coo <-raster::colFromX(
                    raster(file_season_get),
                    df_raster_dia_spline$x
    )
    df_raster_dia_spline$y_coo <- raster::rowFromY(
                    raster(file_season_get),
                    df_raster_dia_spline$y
    )
    print(head(df_raster_dia_spline,2))

    df_raster_dia_spline <- df_raster_dia_spline [
        ,c("y_coo","x_coo","Spline")
    ]
    print("Aqui")
    df_raster_dia_spline <- df_raster_dia_spline[!is.na(df_raster_dia_spline$Spline), ]
    
    
    #### Hay que checar que el dia tenga la banda de las predicciones
    print(nrow(df_raster_dia_spline))
    
    head(est_all_data) ### Tiene que ser el mismo 
    dia_todo <- est_all_data[est_all_data$dia == dia,]
    dia_todo <- dia_todo[!is.nan(dia_todo$PM2.5_S50), ]
    dia_todo <- dia_todo[!is.na(dia_todo$PM2.5_S50), ]
    dia_mean<- mean(dia_todo$PM2.5_S50 )
    ## pegamos todo
    df_pred_splin_dia <- df_raster_dia_predict %>%
        full_join (
            df_raster_dia_spline,
            by=c("y_coo" = "y_coo", "x_coo"  = "x_coo")
            )%>%
        drop_na(Spline)   #Vamos a elininar aquellos que no se tienen. El problema es el ultimo renglon del spline 
    df_pred_splin_dia$EST_MEAN<-dia_mean
    df_pred_splin_dia$dia <- date_dia
    ### raices
    df_pred_splin_dia$rc_predic_pm25 <- sqrt( 
        df_pred_splin_dia$PM25_predict
    )
    df_pred_splin_dia$rc_est_mean <- sqrt(
        df_pred_splin_dia$EST_MEAN
    )
        
    return(df_pred_splin_dia)
}































#' Esta funcion sirve para hacer la predicción diaria para toda las 
#'  temporadas
#' 
#' 
#' 
#' @param date_start
#' @param date_end
#' @param file_path_prediction
#' @param file_path_season
#' @param prefix_predic
#' @param ban_complete_prediction
#' @param ban_exctract_prediction
#' @param data_pm_reanalisis_est
#' @param path_save_predictions_2
#' @param prefix_save_predic_2
prediction_all_season<- function(
    date_start, 
    date_end, 
    file_path_prediction, 
    file_path_season,
    prefix_predic = "predi_25PM_",
    ban_complete_prediction  = "temperature_2m",
    ban_exctract_prediction = "PM25_predict",
    data_pm_reanalisis_est_path ,
    path_save_predictions_2 ,
    prefix_save_predic_2 ,
    ...

){
    fechas_considerar <- seq( as.Date(date_start), as.Date(date_end), by="+1 day")
    lista_alldf <- list()
    counter =  1
    ### Se obtienen los datos de los raster para cada coordenadas. 
    data_pm_reanalisis_est <- data.frame() 
    for(path in data_pm_reanalisis_est_path){
        print(path)
        load(path, ex <- new.env())
        df_data_scale <- get(ls(ex)[[1]],ex)
        data_pm_reanalisis_est = rbind(data_pm_reanalisis_est, df_data_scale)
    }
    print(head(data_pm_reanalisis_est,2))
    print(tail(data_pm_reanalisis_est,2))
    for(dia_get in fechas_considerar){
        df_por_dia = data_2_part_just (
            dia=as.Date(dia_get),
            file_path_prediction=file_path_prediction,
            file_path_season=file_path_season, 
            prefix = prefix_predic,
            ban_complete_prediction  = ban_complete_prediction,
            ban_exctract_prediction = ban_exctract_prediction,
            est_all_data = data_pm_reanalisis_est
        )
        lista_alldf[[counter]] = df_por_dia 
        counter = counter+ 1
    }
    all_day_df <- data.frame()
    for(df in lista_alldf){
        all_day_df <- rbind(all_day_df, df)
    }
    
    all_day_df<- all_day_df %>% 
        mutate_all(~ifelse(is.nan(.), NA, .))
    all_day_df$to_predict <- all_day_df$rc_predic_pm25 - all_day_df$Spline
    all_day_df$rc_est_mean_z <- scale(all_day_df$rc_est_mean)
    all_day_df$to_predict_z <- scale(all_day_df$to_predict)
    model_season_single <- lm(to_predict_z ~  rc_est_mean_z, data = all_day_df)
    all_day_df$prediction_z <- predict(
                    model_season_single,
                    newdata = all_day_df, 
                    allow.new.levels = TRUE
    )
    all_day_df$dia <- as.Date(all_day_df$dia)
    all_day_df$prediction <- (all_day_df$prediction_z * attr(all_day_df$to_predict_z, "scaled:scale"))+attr(all_day_df$to_predict_z, "scaled:center")
    all_day_df$prediction_rc_pm <- all_day_df$prediction + all_day_df$Spline 
    all_day_df$prediction_pm_full <- all_day_df$prediction_rc_pm^2
    ###### Loop con los dias posibles 



    dias_posible <- unique( all_day_df[!is.na(all_day_df$EST_MEAN), ]$dia)
    for(dia_pos in dias_posible){
        dia_date = as.Date(dia_pos)
        print(dia_date)
        dia_sel_posible <- all_day_df[all_day_df$dia == dia_date, ]
        array_pred_posible  <- array(,
            c(max(dia_sel_posible$y_coo ), max(dia_sel_posible$x_coo))
        )
        for(i in 1:nrow(dia_sel_posible)){
            array_pred_posible[dia_sel_posible[i,"y_coo"],  dia_sel_posible[i,"x_coo"]] <- dia_sel_posible[i,"prediction_pm_full"]
        }
        date_dia_str_posible <- paste0(
                lubridate::year(dia_date),
                '_',
                sprintf('%02d', lubridate::month(dia_date)),
                '_',
                sprintf('%02d', lubridate::day(dia_date))
        )
        file_complete_posible <- paste0( file_path_prediction, prefix_predic,date_dia_str_posible, ".tif")
        print(file_complete_posible)
        raster_brick_posible <- raster::brick(file_complete_posible)
        # # mat_complete_posible =as.matrix(raster_brick_posible[[ban_complete_prediction]])
        # # row_complete <- nrow(mat_complete_posible) 
        # # col_complete <- ncol(mat_complete_posible) 
        # # array_pred_com_posible <- base::matrix(nrow =row_complete,ncol= col_complete)
        # # row_tu_ = list()
        # # counter_fu_ = 1
        # # for( row_ in seq(1, nrow(mat_complete_posible))) {
        # #     nan_row_num_ <-which(!is.na(mat_complete_posible[row,]))    
        # #     max_row_ <- max(nan_row_num_)
        # #     min_row_ <- min(nan_row_num_)
        # #     tuple_row_ = c(row_, min_row_, max_row_)
        # #     row_tu_[[counter_fu_]] <-tuple_row_
        # #     counter_fu_ <-counter_fu_ +1
        # # }
        # # for(i in row_tu){
        # #     array_pred_com_posible[i[1],i[2]:(i[2]+119)] <- array_pred_posible[i[1],1:120]
        # # }
        ras_pred_com_posible <- raster(array_pred_posible) #array_pred_com_posible)
        crs(ras_pred_com_posible) <- crs(raster_brick_posible[[ban_complete_prediction]])
        extent(ras_pred_com_posible) <- extent(raster_brick_posible[[ban_complete_prediction]])
        print("saving file ")
        
        
        file_save_name<- paste0(
            path_save_predictions_2, 
            lubridate::year(dia_date),
            "/",
            prefix_save_predic_2,
            date_dia_str_posible,
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


