library(magrittr)

x <- dir("data-raw/TESTE1")
csv_files  <- x[stringr::str_detect(x, ".csv")]

df_contr = readr::read_delim(
  paste0("data-raw/TESTE1","/",csv_files),
  col_types = c("text", "text", "text",
                "text", "text", "text", "text", "text",
                "text", "text", "text", "text", "date",
                "date", "text", "text", "numeric",
                "numeric", "text", "date", "date",
                "text","text"),
  .name_repair = janitor::make_clean_names()
)

x <- dir("data-raw/TESTE1")
csv_files  <- x[stringr::str_detect(x, ".csv")]

csv2 = read.csv2(paste0("data-raw/TESTE1","/",csv_files),
                 sep = ";", fileEncoding="latin1") |>
  tibble::as_tibble()

csv = vroom::vroom(paste0("data-raw/TESTE1/",csv_files), delim = ";")


csv |> vroom::vroom_write(
  iconv("testando.csv",to = "UTF-8")
  )

csv |> readr::write_csv2("write_csv2.csv")
csv |> readr::write_delim("write__delim.csv")
csv |> write.csv2("write.csv2.csv")
csv |> writexl::write_xls("write_xls.xls")
csv |> write.csv2("wirte.csv2_utf8.csv", fileEncoding = 'UTF-8')
csv |> write.csv2("wirte.csv2_latin1.csv", fileEncoding = "latin1")

# -------------------------------------------------------------------------

# PAREI AQUI! O tipo das colunas está mudando na leitura do arquivo pelo vroom
# entender o pq.

control_file = "data-raw/TESTE2/ControleCIs_15092022.csv"

df_contr = vroom::vroom(control_file, delim = ";")

dplyr::glimpse(df_contr)

# -------------------------------------------------------------------------




x <- dir("data-raw/TESTE1")
xlsx_files  <- x[stringr::str_detect(x, ".xlsx")]

ci_files = paste0("data-raw/TESTE1/",xlsx_files)

cis =
  purrr::map_df(
    1:length(ci_files),
    ~ readxl::read_xlsx(
      ci_files[.x],
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


col_names = names(csv)

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
    observacoes = " ",
    duplicidades = NA
  ) |>
  dplyr::select(-ano, -mes, -dia) |>
  dplyr::select(1:12, 14, 17:18, 13, 15:16, 19:23) |>
  purrr::set_names(col_names)

controle = csv

x = cis$elementar

list_elementar = unique(x)

#
#
# lista = unique(controle$elementar)
#
# teste = cis |> dplyr::mutate(
#   test = dplyr::case_when(
#     elementar %in% lista ~ 'duplicou'
#   )
# )

# deu certo! --------------------------------------------------------------


w = unique(controle[,c(1,5)]) |> dplyr::filter(origem != "CI 657")

cis = cis |>
  dplyr::left_join(w, by = "elementar") |>
  dplyr::mutate(
    duplicidades = ifelse(is.na(origem.y),duplicidades, origem.y)
  ) |> dplyr::select(-origem.y) |> dplyr::rename(origem = origem.x)

novo_controle = dplyr::bind_rows(controle, cis)



# -------------------------------------------------------------------------



