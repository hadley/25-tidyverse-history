library(dplyr)
library(purrr)
library(nanoparquet)
library(stringr)
library(lubridate)
library(tidyr)

# Define packages --------------------------------------------------------

core_pkgs <- tidyverse:::core
tidy_pkgs <- c(setdiff(tidyverse:::tidyverse_packages(), c(core_pkgs, "rstudioapi")), "httr2")
precusors <- c("plyr", "reshape", "reshape2", "ggplot")
db_pkgs <- c("DBI", "RMySQL", "RPostgres", "RSQLite", "odbc")
pkgs <- c(tidy_pkgs, db_pkgs, core_pkgs)

packages <- bind_rows(
  tibble(package = db_pkgs, type = "db"),
  tibble(package = tidy_pkgs, type = "tidyverse"),
  tibble(package = core_pkgs, type = "core"),
  tibble(package = precusors, type = "precursor")
)
nanoparquet::write_parquet(packages, "packages.parquet")

# Get raw release info ---------------------------------------------------

releases <- map_dfr(packages$package, pkgsearch::cran_package_history, .progress = TRUE)
# Can't currently store list-cols
releases$dependencies <- NULL
nanoparquet::write_parquet(releases, "releases.parquet")


# Process into useful format ---------------------------------------------

package_release <- releases |>
  select(package = Package, version = Version, date = date, maintainer = Maintainer) |>
  separate_wider_delim(
    version,
    delim = ".",
    names = c("major", "minor", "patch"),
    too_few = "align_start",
    too_many = "merge",
    cols_remove = FALSE
  ) |>
  replace_na(list(patch = "0")) |>
  mutate(
    date = as.Date(date),
    year = year(date),
    release = case_when(
      minor == "0" & patch == "0" ~ "major",
      patch == "0" ~ "minor",
      TRUE ~ "patch"
    ),
    maintainer = str_remove(maintainer, " <.*?>"),
  ) |>
  mutate(release = ifelse(row_number() == 1, "first", release), .by = package) |>
  select(-(major:patch)) |>
  left_join(packages) |>
  # Only include releases after I took over
  filter(cumany(maintainer == "Hadley Wickham"), .by = package)


package_release

package_release |>
  filter(release == "first") |>
  arrange(date) |>
  select(package, version, year, maintainer, type) |>
  print(n = Inf)

package_release |> count(maintainer, sort = TRUE)
package_release |> count(package)
package_release |> count(year(date))
package_release |> count(release)
package_release |> filter(release == "major") |> arrange(date)

nanoparquet::write_parquet(package_release, "package_release.parquet")
