#' extract_metadata_tif
#'
#' @param file_dir 
#' @param meta_data 
#'
#' @return
#' @export
#'
#' @examples
extract_metadata_tif <- function(file_dir, meta_data = "y_resolution"){
  
  if(meta_data == "all"){
    metadata_df <- read_tags(file_dir, frames = "all") %>% 
      pluck(pluck("frame1"))
    
  }else{
    metadata_df <- meta_data %>%
      map_dfc(~read_tags(file_dir, frames = "all") %>% 
                pluck(pluck("frame1"))) %>% 
      select(all_of(meta_data))
  }
  
  return(metadata_df)
  
}