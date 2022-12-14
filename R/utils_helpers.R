#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

create_table <- function(df) {
  DT::renderDT(
    df,
    server = FALSE,
    rownames = FALSE,
    extensions = c('Buttons', 'Responsive'),
    options = list(
      title = NULL,
      pageLength = 5,
      # initComplete = DT::JS('function(setting, json) { alert("Bem-vind@ ao app ControleCIs!"); }'),
      scrollX = TRUE,
      autoWidth = TRUE,
      dom = 'frtip',
      class = "display compact nowrap",
      searchHighlight = TRUE,
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')
    )
  )

}
