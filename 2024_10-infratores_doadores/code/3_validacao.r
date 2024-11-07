# df |>
infracoes_tidy |> |>
  dplyr::filter(cpf_cnpj == "28442253904") |>
  dplyr::glimpse()

receita <- readr::read_csv2(
  here::here(data_raw, "receita.csv"),
  locale = readr::locale(encoding = "Latin1")
) |>
  janitor::clean_names()

receita |>
  dplyr::filter(nr_cpf_cnpj_doador == "28442253904") |>
  dplyr::glimpse()


receita <- receita |>
  dplyr::mutate(
    cpf_cnpj_valido_doacao = purrr::map_vec(nr_cpf_cnpj_doador, check_cpf)
  )

check_cpf("28442253904")


cpf <- "28442253904"

check_cpf <- function(cpf) {
  len <- stringr::str_length(cpf)
  if (len == 14) {
    TRUE
  } else if (len != 11) {
    FALSE
  } else {
    dig1 <- cpf |>
      stringr::str_remove(".{2}$") |>
      stringr::str_split_1("") |>
      as.numeric() |>
      purrr::map2_vec(1:9, `*`) |>
      sum() |>
      (\(x) x %% 11)() |>
      (\(x) x %% 10)()
    if (dig1 == stringr::str_sub(cpf, 10, 10)) {
      dig2 <- cpf |>
        stringr::str_remove(".$") |>
        stringr::str_split_1("") |>
        as.numeric() |>
        purrr::map2_vec(0:9, `*`) |>
        sum() |>
        (\(x) x %% 11)() |>
        (\(x) x %% 10)()
      dig2 == stringr::str_sub(cpf, 11, 11)
    } else {
      FALSE
    }
  }
}