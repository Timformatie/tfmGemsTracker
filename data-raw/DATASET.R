## code to prepare `DATASET` dataset goes here

api_urls <- config::get("gt", file = "inst/extdata/urls.yml")
usethis::use_data(api_urls, internal = TRUE, overwrite = TRUE)
