#' shinycluster
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{shinyepoxy()}
shinycluster <- function () {
  shiny::runApp(system.file("shinycluster",
                            package = "PROJECT2AFORCEMATH5793"),
                launch.browser = TRUE)
}
