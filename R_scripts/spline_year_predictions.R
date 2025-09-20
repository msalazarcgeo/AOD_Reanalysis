
source("./Just_todo_process.R")
source("./Just_todo_2.R")



#' Get all the spline tensor seasons for an specific interval
#' 
#' @param prediction_path Path to the directory wher all the predictions are 
#'   store 
#' @param save_path_season path to the directory where the folders are store
#' @param start_date String with the initial date 'YYYY-MM-DD'
#' @param end_date String with the final date 'YYYY-MM-DD'
#' @param complete_band with the complete band
#' @param splin_band  string with the band to spline
#' 

get_all_seasons_interval <- function(
    prediction_path  ,
    save_path_season ,
    start_date ,
    end_date ,
    complete_band = "temperature_2m",
    splin_band = "PM25_predict",
    ...
    ){
    
    #get seasons 
    all_days <- seq( as.Date(start_date), as.Date(end_date), by="+1 month")
    months_li = list()
    count = 1
    for(day in all_days){
        month = as.numeric(format(as.Date(day), "%m"))
        year = as.numeric(format(as.Date(day), "%Y"))
        
        if (month>2 & month < 5){
            
            months_li[[count]] = list("warm", day,month, year)
        }
        else{
            if(month >= 5 & month <11){    
                months_li[[count]] = list("rain",day, month, year)
            }
            else{
                if(month >=11 ){
                    months_li[[count]] = list("cold", day, month, year)
                }
                else{
                    months_li[[count]] = list("cold", day, month, year-1)
                }
            }
        }
        count <- count +1
    }
    
    df_seasons <- data.frame(matrix(unlist(months_li), ncol= 4, byrow=TRUE))
    names(df_seasons) <- c("temporada","mes_year", "mes", "ano")
    df_seasons$mes_year<-as.Date(as.numeric(df_seasons$mes_year))
    df_seasons_get <- df_seasons%>% 
        group_by( temporada , ano)%>%
        summarize(min_mes = min(mes_year), max_mes = max(mes_year) )
    
    df_seasons_get$max_mes <- df_seasons_get$max_mes + months(1) 
    df_seasons_get$name <- paste0(
        save_path_season,
        df_seasons_get$ano, "/",
        df_seasons_get$temporada, '_',
        df_seasons_get$ano,'.tif')
    by(df_seasons_get,
     seq_len(nrow(df_seasons_get)),

        function(row){
            print(row$min_mes)
            print(splin_band)
            season_splin(
                prediction_path,
                row$name, 
                complete_band,
                splin_band,
                row$min_mes,
                row$max_mes,
            )
      }
    )
    return(df_seasons_get)
}








get_all_days_spline_interval <- function(
    start_date,
    end_date,
    file_path_prediction, #### Vamos a usar los symbolics
    file_path_season_tensor,
    data_pm_reanalisis_est,
    complete_band =  "temperature_2m",
    splin_band = "PM25_predict",
    prefix_predic = "predi_25PM_",
    path_save_predictions_2,
    prefix_save_predic_2 = "predict_just_rain_",
    ...

) {
    ###### Hay que hacerlo por separado y hacerlo cada uno y que solo 
    # tome los cachos bien el intervalo soloi de 1 año maximo
    
     
    all_months <- seq(as.Date(start_date), as.Date(end_date), by = "+1 day") 
    
    count_year_pm <- 1
    datos_pm <- list()
    for(year in unique(format(all_months, "%Y"))){
        datos_pm[[count_year_pm]] <- paste0(data_pm_reanalisis_est,"datos_pm_",year , ".rda" )
        count_year_pm <- count_year_pm+1
    }
    
    # for(year in unique(format(all_months,"%Y" ) ) ) {  
    #     if(format(as.Date(start_date), "%Y")==year) {
    #         start_date_fun = start_date
    #         path_save_predictions_fun = paste0(path_save_predictions_2,year,"/")
    #     }
    #     else{
    #         start_date_year_fun = as.numeric(format(as.Date(start_date), "%Y")) + 1
    #         start_date_fun = paste0(start_date_year_fun, "/01/01")
    #         path_save_predictions_fun = paste0(path_save_predictions_2,year,"/")
    #     }
    #     if(format(as.Date(end_date), "%Y")==year) {
    #         end_date_fun = end_date
    #     }
    #     else{
    #         end_date_year_fun = as.numeric(format(as.Date(end_date), "%Y"))-1
    #         end_date_fun = paste0(end_date_year_fun, "/12/31")
    #     }
        print(datos_pm)
        print("start_date")
        print(start_date)
        print("end_date")
        print(end_date)
        print(path_save_predictions_2)
        
        
        prediction_all_season(
            start_date, 
            end_date, 
            file_path_prediction= file_path_prediction, 
            file_path_season= file_path_season_tensor,
            prefix_predic = prefix_predic ,
            ban_complete_prediction  = complete_band,
            ban_exctract_prediction = splin_band,
            data_pm_reanalisis_est = datos_pm,
            path_save_predictions_2 = path_save_predictions_2,
            prefix_save_predic_2 = prefix_save_predic_2
        )
    # }
}
















knitr::purl("./spline_year_predictions.qmd")


