#' Pull normalised statistics function
#'
#' Should you want to run either the deweather or rmweather function, this code will allow you create a dataframe with
#' the required sites and prepare it to be normalised for a single pollutant choice
#' @param df_de Output of normalised pollutants with deweather module
#' @param df_rm Output of normalised pollutants with rmweather module
#' @param site List of sites used in normalisation code
#' @keywords pull_normalised_statistics
#' @export
#' @examples 
#' pull_normalised_statistics()

pull_normalised_statistics = function(df_de, df2_rm, site){
  
  site_data_de = df_de[[site]]
  site_data_rm = df2_rm[[site]]
  
  r_2_deweather = format(round(100.*site_data_de$plot$plot_env$stats_train$value[9]^2, 2))
  RMSE_deweather = format(round(site_data_de$plot$plot_env$stats_train$value[8], 2))
  
  r_2_rmweather = format(round(100.*site_data_rm$model$r.squared, 2))
  RMSE_rmweather = format(round(site_data_rm$model$prediction.error^(0.5), 2))
  
  data.frame(site, r_2_deweather, r_2_rmweather, RMSE_deweather, RMSE_rmweather,
             stringsAsFactors=FALSE)  
}