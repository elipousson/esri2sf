## code to prepare `esri_version_ref` dataset goes here
## Data scraped from website on 2023 January 6

esri_version_ref <-
  readr::read_csv(
    here::here("inst/extdata/esri_version_ref.csv")
  )

usethis::use_data(esri_version_ref, overwrite = TRUE)
