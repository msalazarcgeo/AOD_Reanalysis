
if (!require("pacman")) install.packages("pacman");
pacman::p_load(knitr)






source("./Just_todo_process.R")




#' Get the dataframe as wanted
#' 
#' @param pm_data_stations_path
#' @param  year
#' 
#' @return Dataframe with tthe stations data by year 

get_contaminant_df <- function(
    pm_data_stations_path,
    zona_metro,
    year
){
    print("Loading data:")
    print(pm_data_stations_path)
    load(pm_data_stations_path)
    print("data loaded")
    BDA_dia <- BDA_dia_sr  %>% 
    as_tibble() %>% 
    filter(
        grepl(zona_metro , BDA_dia_sr$CVE_EST ) & (year(TIMESTAMP_AOD) == year)
    )%>%
    dplyr::select(
        CVE_EST,
        FECHA,
        PM2.5_S50,
        PM2.5_S60,
        PM2.5_S75)%>%
    mutate(
        CVE_EST= as.character(CVE_EST),
        dia = date(FECHA)
    )
    return(BDA_dia)
}





#' Get ther station localization
#'
#' @param path_est_loc string The location of the coordinates file.
#' @param df_contaminants  The dataframe with the CVE_EST column to get only
#'   the coordinates in the dataframe
#'
#' @return dataframe with the locations
station_loc <- function(
    path_est_loc = "./datos/Estaciones/Coord_estaciones_SINAICA.xlsx",
    df_contaminants
    )
    {
    estaciones_ubi <- read_excel(path_est_loc)%>%
    mutate(   
        Latitud = as.numeric(LAT ),
        Longitud = as.numeric(LONG)
    )
    estaciones_ubi <-estaciones_ubi[estaciones_ubi$CVE_EST %in%  unique(df_contaminants$CVE_EST ), ]
    est_coords <- estaciones_ubi[, c("Longitud", "Latitud")]%>%
    rename(latitude = Latitud, longitude = Longitud)

    est_coords <- SpatialPoints(est_coords)
    raster::crs(est_coords) <- "+proj=longlat +datum=WGS84"
    return(list(estaciones_ubi, est_coords))
}





#' Obtain all the data and variables for a specific period of time.
#' 
#' @description 
#' For a specific interval of time the function will obtain the dataframe with 
#'   all the values from the rasters  
#' 
#' @param start_date string start date YYYY-MM-DD
#' @param end_date string end date YYYY-MM-DD
#' @param zona_metro string of the abreviation of the metropolitan zone in the stations
#' @param year 
#' @param path_raster_year string of the 
#' @param pm_data_stations_path String of the path where the df of the 
#'   stations is store
#' @param est_coord_path 
#' @param path_raster_vialidades String to the path with the raster of streets 
#' @return dataframe
all_var_dataframe <- function(
    start_date,
    end_date,
    zona_metro,
    year,
    pm_data_stations_path,
    est_coord_path,
    path_raster_year,
    path_raster_vialidades,
    ...
){

    bd_dia <- get_contaminant_df(pm_data_stations_path,zona_metro, year)
    print(est_coord_path)
    res_station_loc <- station_loc(est_coord_path, bd_dia)
    estaciones_ubi_df <- res_station_loc[[1]]  
    est_coords_sp <- res_station_loc[[2]]  
    fechas_considerar <- seq( as.Date(start_date), as.Date(end_date), by="+1 day")
    day_df_list = list()
    counter = 1
    if (exists("prefix_file")==FALSE){
        if(zona_metro == "ZMVM"){
            prefix_file <- "aod_cli_PBLH_ZMVM_"
        }
        else if(zona_metro == "AMM"){
            prefix_file <- "aod_cli_PBLH_AMM_"
        }
        else if(zona_metro == "ZMG"){
            prefix_file <- "aod_cli_PBLH_ZMG_"
        }
        else if(zona_metro == "ZMVT"){
            prefix_file <- "aod_cli_PBLH_ZMVT_"
        }

    }
    for(fecha_dia in fechas_considerar){
        fecha_str<-  paste0(
                lubridate::year(as.Date(fecha_dia)),'_',
                sprintf('%02d',  lubridate::month(as.Date(fecha_dia))),'_',
                sprintf('%02d',  lubridate::day(as.Date(fecha_dia)))
        )
        #path_test <- "./datos/raster/middle_PBLH/all_bands/2018/"
        
        df_day_stations <- get_day_values_raster(
            fecha_str,
            path_raster_year, 
            estation_loc = est_coords_sp, 
            prefix = prefix_file,
            est_df = estaciones_ubi_df 
        )
        df_day_stations$DIA <- as.Date(fecha_dia)
        df_day_stations <- df_day_stations[, c("CVE_EST","DIA","Latitud" ,
                                            "Longitud","Optical_Depth_047",
                                            "temperature_2m" ,"PBLH","relative_humidity",
                                            "total_precipitation_hourly","total_precipitation")]
        day_df_list[[counter]]= df_day_stations
        counter = counter+1
    }
    all_day_GEE <- bind_rows(day_df_list)
    
    ##### se tiene el dataframe con todos los valores de los rasters 
    all_day_GEE
    # Se une dataframe con los datos 
    df_rea__pm_all  <- bd_dia  %>% 
    full_join(
        all_day_GEE,
        by=c("CVE_EST" = "CVE_EST",
        "dia"="DIA")
    )%>%
    distinct(.keep_all=TRUE)
    #### Se toman las capas que se necesitan 
    print("Vialidades ")
    vialidades_1 <- raster(path_raster_vialidades,1)
    vialidades_2 <- raster(path_raster_vialidades,2)
    vialidades_3 <- raster(path_raster_vialidades,3)
    vialidades <- vialidades_1 + vialidades_2 + vialidades_3

    estaciones_ubi_via <- estaciones_ubi_df
    estaciones_ubi_via$vialidades <- raster::extract(
        vialidades,
        est_coords_sp
    )

    estaciones_ubi_via <- estaciones_ubi_via[,c("CVE_EST", "vialidades")]
    df_rea__pm_all <- df_rea__pm_all  %>% 
    left_join(
        estaciones_ubi_via,
        by=c("CVE_EST" = "CVE_EST")
    )
    return (df_rea__pm_all)
}





#' Scale the values in the dataframe 
#' 
#' @param df_values A dataframe with the values to get the zscores (scale) 
#' @return a data.frame
zscore_values_pm <-function(df_values){

    df_values$PM25_z <- scale(df_values$PM2.5_S50)
    df_values$Optical_Depth_047_z <- scale(df_values$Optical_Depth_047)
    df_values$temperature_2m_z <- scale(df_values$temperature_2m)
    df_values$relative_humidity_z <- scale(df_values$relative_humidity)
    df_values$vialidades_z <- scale(df_values$vialidades)
    df_values$PBLH_z <- scale( df_values$PBLH)
    df_values$PM25_S60_z <- scale(df_values$PM2.5_S60)
    df_values$PM25_S75_z <- scale(df_values$PM2.5_S75)
    df_values$total_precipitation_sqrt <- sqrt(df_values$total_precipitation)
    df_values$total_precipitation_hourly_sqrt <- sqrt(df_values$total_precipitation_hourly)
    df_values$total_precipitation_sqrt_z <- scale(df_values$total_precipitation_sqrt)
    df_values$total_precipitation_hourly_sqrt_z <- scale(df_values$total_precipitation_hourly_sqrt)
    return(df_values)
}






#' gets the scalars from the dataframe 
#' 
#' @param df_data_scaled A dataframe with the variablas to scale
#' 
#' @return A dataframe with the scale columns and inside the scalars

get_df_scalars <- function(df_data_scaled){
    df_scalars<- data.frame(matrix(ncol = 3, nrow = 0))

    df_scalars <- rbind(df_scalars,
        c("PM25_z",
            attr(df_data_scaled$PM25_z, "scaled:center"),
            attr(df_data_scaled$PM25_z, "scaled:scale")
        )
    )
    df_scalars <- rbind(df_scalars,
        c("Optical_Depth_047_z",
            attr(df_data_scaled$Optical_Depth_047_z, "scaled:center"),
            attr(df_data_scaled$Optical_Depth_047_z, "scaled:scale")
        )
    )
    df_scalars <- rbind(df_scalars,
        c("temperature_2m_z",
            attr(df_data_scaled$temperature_2m_z, "scaled:center"),
            attr(df_data_scaled$temperature_2m_z, "scaled:scale")
        )
    )
    df_scalars <- rbind(df_scalars,
        c("relative_humidity_z",
            attr(df_data_scaled$relative_humidity_z, "scaled:center"),
            attr(df_data_scaled$relative_humidity_z, "scaled:scale")
        )
    )
    df_scalars <- rbind(df_scalars,
        c("total_precipitation_sqrt_z",
            attr(df_data_scaled$total_precipitation_sqrt_z, "scaled:center"),
            attr(df_data_scaled$total_precipitation_sqrt_z, "scaled:scale")
        )
    )
    df_scalars <- rbind(df_scalars,
        c("PBLH_z",
            attr(df_data_scaled$PBLH_z, "scaled:center"),
            attr(df_data_scaled$PBLH_z, "scaled:scale")
        )
    )
    df_scalars <- rbind(df_scalars,
        c("vialidades_z",
            attr(df_data_scaled$vialidades_z, "scaled:center"),
            attr(df_data_scaled$vialidades_z, "scaled:scale")
        )
    )

    names(df_scalars) <- c("Variable", "center", "scale")
    return(df_scalars)
}








#' Get the prediction for an interval of time
#' 
#' @param star_date string of the initial date  YYYY-MM-DD
#' @param end_date string of the final date  YYYY-MM-DD 
#' @param path_rasters string path where the rasters with all 8 bands 
#' @param path_raster_vialidades string path where the rasters with streets
#' @param df_scale
#' @param coef_table
#' @param zona_metro string identifiying the metro area
#' @param path_save string path to store the resulting rasters
get_raster_predictions <- function(
    star_date,
    end_date, 
    path_rasters,
    path_raster_vialidades,
    coef_table, 
    df_scale,
    path_save,
    zona_metro,
    prefix_save = 'predi_25PM_',
    ...
){
    fechas_considerar <- seq( as.Date(star_date), as.Date(end_date), by="+1 day")
    counter = 1
    if (exists("prefix_file")==FALSE){
        if(zona_metro == "ZMVM"){
            prefix_file <- "aod_cli_PBLH_ZMVM_"
        }
        else if(zona_metro == "AMM"){
            prefix_file <- "aod_cli_PBLH_AMM_"
        }
        else if(zona_metro == "ZMG"){
            prefix_file <- "aod_cli_PBLH_ZMG_"
        }
        else if(zona_metro == "ZMVT"){
            prefix_file <- "aod_cli_PBLH_ZMVT_"
        }

    }
    vialidades_1 <- raster(path_raster_vialidades,1)
    vialidades_2 <- raster(path_raster_vialidades,2)
    vialidades_3 <- raster(path_raster_vialidades,3)
    vialidades <- vialidades_1 + vialidades_2 + vialidades_3
    for(fecha_dia in fechas_considerar){
        fecha_str <-  paste0(
                lubridate::year(as.Date(fecha_dia)),'_',
                sprintf('%02d',  lubridate::month(as.Date(fecha_dia))),'_',
                sprintf('%02d',  lubridate::day(as.Date(fecha_dia)))
        )
        dia_rasters <- pre_ras_pm25_dia(
            fecha_str, 
            path_rasters, 
            prefix = prefix_file,
            coeff_df_model = coef_table,
            scale_values_df= df_scale, 
            vialidades_r = vialidades
        )
        file_name_s <- paste0(path_save,prefix_save , fecha_str)
        print(file_name_s)
        print(nlayers(dia_rasters))
        if (nlayers(dia_rasters)>0 ){
            raster::writeRaster(
                dia_rasters, 
                filename=file_name_s,
                format = "GTiff",
                overwrite=TRUE
            )
        }
        else{
            print("No se guarda archivo")
        }
        
    }
}


