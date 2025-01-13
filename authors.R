library(tidyverse)

releases <- nanoparquet::read_parquet("releases.parquet")

people <- releases |>
  filter(row_number() == max(row_number()), .by = Package) |>
  mutate(authors = map(`Authors@R`, \(x) unclass(eval(parse(text = x))))) |>
  select(package = Package, authors)
people |>
  unnest_longer(authors) |>
  unnest_wider(authors) |>
  unnest_longer(role) |>
  mutate(given = map_chr(given, paste, collapse = " ")) |>
  filter(
    role == "aut",
    is.na(email) | !str_detect(email, "posit.co|rstudio.com"),
    !is.na(family)) |>
  mutate(name = paste0(given, " ", family), .after = package, .keep = "unused") |>
  print(n = Inf)

|>
  count(given, family, role, sort = TRUE) |>
  print(n = Inf)

person_to_df <- function(x) {
  x <- unclass(x)
  tibble(
    given = x |> map("given"),
  )
}
