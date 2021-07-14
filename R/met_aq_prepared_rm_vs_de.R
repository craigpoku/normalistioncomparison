#' Met AQ combined prepared rm vs de function

#' Should you want to run either the deweather or rmweather function, this code will allow you create a dataframe with
#' the required sites and prepare it to be normalised for a single pollutant choice
#' 
#' @param df Output of combined Met and AQ dataset
#' @param pollutant_type Input for pollutant to test code - only been tested for one pollutant (tested for nox)
#' @param rmweather Default at FALSE. If set to TRUE, dataframe will be suitable for rmweather package. If FALSE, 
#' dataframe will be suitable for deweather
#' @keywords met_aq_combined
#' @export
#' @examples 
#' met_aq_prepared_rm_vs_de()
#' 
met_aq_prepared_rm_vs_de = function(df, pollutant_type, rmweather = FALSE){
  
  df_de = df %>%
    select(code, met_code, station, date, all_of(pollutant_type), 
           ws, wd, air_temp, atmos_pres, RH, cl) %>%
    filter(!is.na((get(pollutant_type))))
  
  df_rm = df %>%
    select(code, met_code, station, date, all_of(pollutant_type), 
           ws, wd, air_temp, atmos_pres, RH, cl) %>%
    pivot_longer(-c(code, met_code, station, date, ws, wd, air_temp, atmos_pres, RH, cl),
                 names_to = "pollutant") %>%
    rename(atmospheric_pressure = atmos_pres) %>%
    filter(!is.na(value))
  
  if(rmweather ==TRUE){
    return(tibble::tibble(df_rm))
  } else {
    return(tibble::tibble(df_de))
  }
}
