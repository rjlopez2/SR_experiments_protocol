#' make_trace_df
#'
#' @param my_file_dir 
#' @param msg 
#'
#' @return
#' @export
#'
#' @examples
#' # example is missing
#' 
make_trace_df <- function(my_file_dir, msg = F){
  
  # y_px_size = read_tags(my_file_dir) %>% 
  #   pluck("frame1", "y_resolution")
  
  my_trace_df <- my_file_dir %>%
    ijtiff::read_tif(msg = msg) %>%
    `[` ( , , 1, 1) %>%
    rowMeans() %>%
    # dplyr::tibble(Time = 1:length(.) * y_px_size / 1000, # set the time in seconds
    #        Fluorescence = .)
    dplyr::tibble(Fluorescence = .)
  
  return(my_trace_df)
}