## code to prepare `DATASET` dataset goes here

controle = readxl::read_excel("data-raw/controle.xlsx",
                              .name_repair = janitor::make_clean_names)

controle |> dplyr::glimpse()

usethis::use_data(controle, overwrite = TRUE)
