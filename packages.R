library(dplyr)
library(purrr)
library(nanoparquet)

core_pkgs <- tidyverse:::core
tidy_pkgs <- setdiff(tidyverse:::tidyverse_packages(), core_pkgs)
db_pkgs <- c("DBI", "RMySQL", "RPostgres", "RSQLite", "odbc")
pkgs <- c(tidy_pkgs, db_pkgs, core_pkgs)

packages <- bind_rows(
  tibble(package = db_pkgs, type = "db"),
  tibble(package = tidy_pkgs, type = "tidy"),
  tibble(package = core_pkgs, type = "tidy-core")
)
nanoparquet::write_parquet(packages, "packages.parquet")

releases <- map_dfr(packages$package, pkgsearch::cran_package_history, .progress = TRUE)
# Can't currently store list-cols
releases$dependencies <- NULL
nanoparquet::write_parquet(releases, "releases.parquet")
