#' @title Lat_long
#' @description This function converts Latitude and Longitude measurements in degrees/minutes/seconds form to decimal degrees form
#' @param input Latitude or Longitude in degrees/minutes form
#' @param split List of delimiters between degrees, minutes, and seconds
#' @param hemisphere Tell lat_long if inputted values use cardinal directions to represent hemisphere
#' @export



lat_long <- function(input, split, hemisphere) {
  
  degrees <- as.numeric(sapply(strsplit(input, split), "[", 1))
  minute_seconds <- (as.numeric(sapply(strsplit(input, split), "[", 2)))/60
  
  decimal_degrees <- degrees + minute_seconds
  
  if(hemisphere == TRUE){
    ifelse(sapply(strsplit(input, split), "[", 3) %in% c("S", "W"), decimal_degrees <- decimal_degrees*-1, decimal_degrees)
  }
  
  return(decimal_degrees)
  
}



##edit to local repository
