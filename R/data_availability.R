#' Data availability Function
#'
#' This function allows you to check the availability of met stations across a given region and time frame.
#' Useful if you want to combine met data with AQ data.
#' Note, this function is dependent on having the met data downloaded on a local system.
#' @param codes List of metcodes that you want to check the availability for specific variables e.g. wind speed
#' @param met_directory Input met_directory defined by the user - assumes the data in csv format
#' @keywords met_data_availability
#' @export
#' @examples
#' data_availability()

data_availability = function(codes, met_directory) {
  code = as.character(codes)
  site_name = as.character(codes)
  wd_na = as.numeric(codes)
  ws_na = as.numeric(codes)
  air_temp_na = as.numeric(codes)
  atmos_pres_na = as.numeric(codes)

  #"C:/Users/cpdav/Documents/GitHub/UK_met_data/noaa_UK_met_data_"
  
  directory_met_data = met_directory
  
  for (i in 1:length(codes)){
    print(c(i, codes[i]))
    x = paste(directory_met_data,toString
              (codes[i]),".csv")
    if (file.size(x) >= 50) {
      file_i = read.csv(x)
      
      code[i] = codes[i]
      site_name[i] = unique(file_i$station)
      wd_na[i] = 100-(sum(is.na(file_i$wd))/nrow(file_i))*100
      ws_na[i] = 100-(sum(is.na(file_i$ws))/nrow(file_i))*100
      air_temp_na[i] = 100-(sum(is.na(file_i$air_temp))/nrow(file_i))*100
      atmos_pres_na[i] = 100-(sum(is.na(file_i$atmos_pres))/nrow(file_i))*100
      
    } 
  }
  
  data.frame(code, site_name, wd_na, ws_na, air_temp_na, atmos_pres_na, stringsAsFactors=FALSE)
}