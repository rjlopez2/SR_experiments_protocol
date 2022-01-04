#' multiple_ls_profile
#'
#' @param my_dir 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' # missing example
multiple_ls_profile <- function(my_dir){
  #list the tif files in the provided directory
  my_ls_file_list <- my_dir %>%
    fs::dir_ls(regexp = "[0-9]{4,4}_BG.tif$")

  # create df of files metadata
  metadata <- my_ls_file_list %>%
    map_dfr(~extract_metadata_tif(.x,
                                  meta_data =  c("y_resolution")),
            .id = "file_id")

  full_ls_df <- my_ls_file_list %>%
    map_dfr(make_trace_df, .id = "file_id") %>%
    left_join(metadata, by = "file_id") %>%
    mutate(file_id = str_sub(file_id, start = -24)) %>%
    mutate(Time = 1:length(Fluorescence) * y_resolution / 1000) %>% #set in sec the time
    select(-y_resolution)

  rm(my_ls_file_list, metadata)

  return(full_ls_df)
}