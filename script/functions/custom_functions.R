calculate_centroid_longitude <- function(coordlist) {
  
  coord_df <- coordlist %>% 
    data.frame()
  
  mean(coord_df$X1)
  
}

calculate_centroid_latitude <- function(coordlist) {
  
  coord_df <- coordlist %>% 
    data.frame()
  
  mean(coord_df$X2)
  
}