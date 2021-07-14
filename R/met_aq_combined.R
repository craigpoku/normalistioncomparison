#' Met AQ combined function

#' Should you want to run either the deweather or rmweather function, this code will allow you create a dataframe with
#' the required sites, pollutants and will match the AQ site to the closest met site. Note, the met_directory should be 
#' downloaded on a local system.
#' @param met_directory Input met_directory defined by the user - assumes the data in csv format
#' @param begin_date Beginning date in yyyy-mm-dd string format
#' @param end_date End date in yyyy-mm-dd string format
#' @param aqcode List of metcodes based on AURN, KCL etc. using openair
#' @param metcode List of met sites based on importNOAA
#' @keywords met_aq_combined
#' @export
#' @examples met_uk_df_all = purrr::map2_dfr(.x = aurn_noaa_nearest_urb_sites$code, .y = aurn_noaa_nearest_urb_sites$met_code, .f = ~read_met_sites(directory_met_data, "2006-01-01", "2011-12-31", aqcode = .x, metcode = .y))
#' met_aq_combined()

met_aq_combined = function(met_directory, begin_date, end_date, aqcode, metcode){
  
  met_UK_df = read.csv(paste(met_directory, metcode,".csv")) %>%
    mutate(date = lubridate::ymd_hms(date)) %>% 
    select(-X) %>%
    filter(date >= as.Date(begin_date) & date <= as.Date(end_date)) %>%
    rename(met_code = code)
  
  begin_year = substring(begin_date,1,4)
  end_year = substring(end_date,1,4)
  
  UK_raw_df = openair::importAURN(site = aqcode, year = begin_year:end_year)
  
  if(!is.null(UK_raw_df)){
    met_UK_df = UK_raw_df %>% 
      select(-any_of(c("ws", "wd", "air_temp"))) %>% 
      left_join(met_UK_df, ., by = "date")
    print(paste0(aqcode, "/", metcode, " finished."))
  } else {
    print(paste0(aqcode, "/", metcode, " is missing."))
  }
  
  return(tibble::tibble(met_UK_df))
  
}