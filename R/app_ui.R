#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      tags$img(
        src = "https://portalibre.fgv.br/sites/default/themes/custom/portalibre/logo.png",
        align = 'right'),
      h1("ControleCIs"),
      p('O aplicativo permite acrescentar à planilha de controle existente, a nova demanda recebida, sinalizando duplicidades, quando houver.'),
      hr(),
      theme = bslib::bs_theme(
        version = 4,
        bootswatch = "lumen"
        # bootswatch = "pulse"
      ),
      mod_tab_main_ui("tab_main_1")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www",
                    app_sys("app/www"))

  tags$head(favicon(),
            bundle_resources(path = app_sys("app/www"),
                             app_title = "ControleCIs"))
  # Add here other external resources
  # for example, you can add shinyalert::useShinyalert())
}
