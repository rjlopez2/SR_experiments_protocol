#' clean_data_func
#'
#' @param raw_pooled_LS_df 
#'
#' @return
#' @export
#'
#' @examples
#' #example is missing
clean_data_func <- function(raw_pooled_LS_df){
  raw_pooled_LS_df %>%
    mutate(file_id = str_sub(file_id, start = -24)) %>% 
    mutate(Time = 1:length(Fluorescence) * y_px_size / 1000) %>% #set in sec the time
    select(-y_px_size)
}