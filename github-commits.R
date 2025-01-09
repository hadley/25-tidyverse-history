library(tidyverse)
library(gh)

repos_json <- gh("GET /orgs/:org/repos", org = "tidyverse")
repos <- repos_json %>% map_chr("name")

get_commits <- function(repo) {
  gh(
    "GET /repos/:owner/:repo/commits",
    owner = "tidyverse",
    repo = repo,
    since = "2016-01-01",
    until = "2024-12-31",
    .limit = Inf
  )
}

if (file.exists("commits.rds")) {
  commits_json <- readRDS("commits.rds")
} else {
  commits_json <- repos |> map(get_commits)
  saveRDS(commits_json, "commits.rds")
}

commits <- tibble(repo = repos, json = map(commits_json, c))

team <- c(
  "hadley",
  "romainfrancois",
  "jennybc",
  "lionel-",
  "batpigandme",
  "jimhester",
  "topepo",
  "DavisVaughan",
  "gaborcsardi",
  "thomasp85",
  "mine-cetinkaya-rundel",
  "krlmlr",
  "teunbrand"
)

commits <- commits |>
  unnest_longer(json) |>
  hoist(
    json,
    author = c("author", "login"),
    date = c("commit", "committer", "date")
  ) |>
  mutate(
    internal = author %in% team,
    date = as.Date(parse_datetime(date)),
    year = year(date)
  )
commits <- commits |> select(-json)

# -------------------------------------------------------------------------

commits |>
  count(author, internal, sort = TRUE) |>
  print(n = 20)

commits |>
  filter(!internal, !is.na(author)) |>
  group_by(year) |>
  summarise(contrib = n_distinct(author))

commits |>
  filter(!internal, !is.na(author)) |>
  count(repo) |>
  filter(n > 100) |>
  ggplot(aes(n, fct_reorder(repo, n))) +
  geom_col()

commits |>
  count(repo, internal) |>
  mutate(n_external = sum(n[!internal]), .by = repo) |>
  mutate(internal = ifelse(internal, "Team", "Community")) |>
  filter(n_external > 100) |>
  ggplot(aes(n, fct_reorder(repo, n, sum), fill = internal)) +
  geom_col() +
  labs(x = "Contributions", y = NULL, fill = NULL) +
  theme(legend.position = "bottom")

commits |>
  count(year = year(date), internal) |>
  mutate(internal = ifelse(internal, "Team", "Community")) |>
  ggplot(aes(year, n, colour = internal)) +
  geom_line() +
  labs(y = "Contributions", x = NULL, colour = NULL) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = scales::breaks_width(2))

acumulative <- commits |>
  count(internal, date) |>
  mutate(cum_count = cumsum(n), .by = internal)

cumulative |>
  ggplot(aes(date, cum_count, colour = ifelse(internal, "team", "community"))) +
  geom_line() +
  labs(
    title = "Cumulative GitHub commits",
    x = NULL,
    y = "Commits",
    colour = "Contributor"
  ) +
  scale_y_continuous(labels = scales::label_comma())

first_commit <- commits |>
  filter(!is.na(author)) |>
  slice_min(date, n = 1, with_ties = FALSE, by = author) |>
  arrange(date)

first_commit |> count(date)

first_commit |>
  mutate(n_contrib = row_number()) |>
  ggplot(aes(date, n_contrib)) +
  geom_line() +
  labs(
    title = "Cumulative unique contributors",
    x = NULL,
    y = "Contributors"
  )
