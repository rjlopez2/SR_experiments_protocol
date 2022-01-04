#' make_plotly_profile
#'
#' @param ls_profile_df 
#'
#' @return
#' @export
#'
#' @examples
#' # example still missing
make_plotly_profile <- function(ls_profile_df){

  {ls_profile_df %>%
      ggplot2::ggplot(aes(x = Time,
                          y = Fluorescence)) +
      geom_line()} %>%
    ggplotly()

}