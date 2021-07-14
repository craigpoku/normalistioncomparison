#' Pull normalised data comparison function

#' Should you want to run either the deweather or rmweather function, this code will allow you create a dataframe with
#' the required sites and prepare it to be normalised for a single pollutant choice
#' 
#' @param df_de Output of normalised pollutants with deweather module
#' @param df_rm Output of normalised pollutants with rmweather module
#' @param df3_raw Output of raw pollutants
#' @param pollutant_type Input for pollutant to test code - only been tested for one pollutant (tested for nox)
#' @param site List of sites used in normalisation code
#' @keywords pull_normalised_statistics
#' @export
#' @examples 

#' pull_normalised_data_comparison()

pull_normalised_data_comparison = function(df_de, df2_rm, df3_raw, pollutant_type, site){
  
  site_data_de = df[[site]]
  site_data_rm = df2[[site]]
  
  site_data_de_pre = site_data_de %>%
    mutate(site = site)
  
  site_data_rm_pre = site_data_rm$normalised %>% 
    mutate(site = site)
  
  df3 = df3 %>%
    filter(code == site) %>%
    select(code, get(pollutant_type), date) %>%
    rename(site = code)
  
  left_join(site_data_de_pre, site_data_rm_pre, by = c("date", "site")) %>%
    left_join(., df3, by = c("date", "site")) %>%
    rename(rmweather_predict = value_predict)
}