#' tab_main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_main_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
    # UPLOAD ARQUIVO...
    sidebarPanel(
      width = 4,
      align = 'left',
      bs4Dash::tooltip(
        fileInput(
          ns("upload_id"),
          "Selecione os arquivos de inputs:",
          width = '100%',
          multiple = TRUE,
          buttonLabel = list(icon("fa-solid fa-upload"), "Carregar"),
          placeholder = "Nenhum arquivo selecionado",
        ),
        title = "Selecionar a planilha de controle e a planilha de CI",
        placement = "top"
        # )
      ),
      bs4Dash::tooltip(
        actionButton(ns("goButton"), "Atualizar", icon = icon('fa-regular fa-pen-to-square')),
        title = "Atualiza a planilha de controle existente com a nova demanda recebida.",
        placement = "top"
      )
    )
  ),
  # TABELA DE CONTROLE CIS
  fluidRow(column(
    width = 12,
    align = 'center',
    div(DT::DTOutput(ns("tab_controle_id")),
        style = "font-size: 90%; width: 80%")
  )))
}

#' tab_main Server Functions
#'
#' @noRd
mod_tab_main_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mydata <-
      reactive({
        req(input$upload_id)

        dplyr::lst(
          # Planilha de controle de solicitações
          df_contr = readxl::read_xlsx(
            (input$upload_id$datapath)[2],
            col_types = c("text", "text", "text",
                          "text", "text", "text", "text", "text",
                          "text", "text", "text", "text", "date",
                          "date", "text", "text", "numeric",
                          "numeric", "text", "date", "date",
                          "text"),
            .name_repair = janitor::make_clean_names
          ),

          # Planilha da demanda (extraída do sistema de Cis)
          df_cis =   readxl::read_xlsx(
            (input$upload_id$datapath)[1],
            col_types = c("text", "text", "text",
                          "text", "text", "text", "text", "text",
                          "text", "text", "text", "text", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "numeric",
                          "numeric", "numeric", "text", "text",
                          "text", "text", "numeric", "text")
          )
        )

      })

    observeEvent(input$goButton, {

      # Obter cada elemento da lista
      controle  <- mydata()$df_contr
      cis <- mydata()$df_cis


      # TRANSFORM AND BIND! -----------------------------------------------------

      controle = controle |> dplyr::mutate(
        data_da_solicitacao = data_da_solicitacao |> as.Date(),
        data_prazo = data_prazo |> as.Date(),
        data_da_ultima_parcial = data_da_ultima_parcial |> as.Date(),
        data_da_finalizacao = data_da_finalizacao |> as.Date()
      )

      col_names = names(controle)

      cis = cis |>
        dplyr::select(1:12, 39, 40) |>
        dplyr::mutate(
          data = data |> stringr::str_sub(1, 10),
          ano = data |> stringr::str_sub(7, 10),
          mes = data |> stringr::str_sub(4, 5),
          dia = data |> stringr::str_sub(1, 1),
          data = paste(ano, mes, dia, sep = "-") |> as.Date(),
          cod_ci = paste0("CI ", cod_ci),
          cod_ci_ext = cod_ci_ext |> stringr::str_replace("-", " "),
          quantidade_de_itens = dplyr::n_distinct(cod_ext_insumo) |> as.double(),
          quantidade_total_de_itens_por_uf = dplyr::n() |> as.double(),
          data_prazo = as.Date(""),
          tipo_do_pedido = " ",
          responsavel_pela_solicitacao = " ",
          data_da_ultima_parcial = as.Date(""),
          data_da_finalizacao = as.Date(""),
          observacoes = " "
        ) |>
        dplyr::select(-ano, -mes, -dia) |>
        dplyr::select(1:12, 14, 17:18, 13, 15:16, 19:22) |>
        purrr::set_names(col_names)

      # ATUALIZAR PLAN CONTROLE - BIND
      novo_controle = dplyr::bind_rows(controle, cis)


      # ALERTA - DUPLICIDADE DE SOLICITAÇÕES

      check_elem = function(x = cis$elementar) {

        list_elementar = unique(x)

        elem_dup = controle |> dplyr::filter(elementar %in% list_elementar) |>
          dplyr::select(elementar) |> dplyr::pull()

        origem_dup = controle |> dplyr::filter(elementar %in% list_elementar) |>
          dplyr::select(origem) |> dplyr::pull()

        check = nrow(controle |> dplyr::filter(elementar %in% list_elementar))

        if(isTruthy(check > 1)) {
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Alerta de duplicidade de solicitações:",
            text = paste0(unique(elem_dup), ' (Origem: ', unique(origem_dup),') ', collapse = "; "),
            type = "error",
            btn_labels = c("Ok")
          )
        } else {
          shinyWidgets::show_alert(
            session = session,
            title = "Concluído!",
            text = 'Não há duplicidade de solicitações.',
            type = "success",
            btn_labels = c("Ok")
          )
        }
      }

      check_ext = function(x = cis$item_externo) {

        list_externo = unique(x)

        item_dup = controle |> dplyr::filter(item_externo %in% list_externo) |>
          dplyr::select(item_externo) |> dplyr::pull()

        origem_dup = controle |> dplyr::filter(item_externo %in% list_externo) |>
          dplyr::select(origem) |> dplyr::pull()

        check = nrow(controle |>
                       dplyr::mutate(temp = stringr::str_extract(item_externo, "([0-9]+).*$")) |>
                       dplyr::filter(temp %in% list_externo)
                     )

        if(isTruthy(check > 1)) {
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Alerta de duplicidade de solicitações:",
            text = paste0(unique(item_dup), ' (Origem: ', unique(origem_dup),') ', collapse = "; "),
            type = "error",
            btn_labels = c("Ok")
          )
        } else {
          shinyWidgets::show_alert(
            session = session,
            title = "Concluído!",
            text = 'Não há duplicidade de solicitações.',
            type = "success",
            btn_labels = c("Ok")
          )
        }
      }

      if (length(cis$elementar) > 0) {
        check_elem()
      } else if (length(cis$item_externo) > 0) {
        check_ext()
      } else {"Não há elementar e item externo informado."}

      output$tab_controle_id <- create_table(df = novo_controle)

    })

  })
}
