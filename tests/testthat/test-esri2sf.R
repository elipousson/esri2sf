test_that("esri2sf works", {
  sample_url <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/CommercialDamageAssessment/FeatureServer/0"
  sample_df_url <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/NYTimes_Covid19Cases_USCounties/MapServer/2"

  sample_sf <-
    esri2sf(
      url = sample_url
    )

  expect_s3_class(
    esri2sf(
      url = sample_url
    ),
    "sf"
  )

  expect_s3_class(
    esri2sf(
      url = sample_url,
      crs = NULL
    ),
    "sf"
  )

  expect_s3_class(
    esri2sf(
      url = sample_url,
      outFields = "placename"
    ),
    "sf"
  )

  # expect_s3_class(
  #   esri2sf(
  #     url = sample_url,
  #     geometry = sample_sf[1, ]
  #   ),
  #   "sf"
  # )

  expect_s3_class(
    esri2sf(
      url = sample_url,
      geometry = sf::st_bbox(sample_sf)
    ),
    "sf"
  )

  expect_s3_class(
    esri2sf(
      url = sample_url,
      geometry = sf::st_bbox(sample_sf),
      spatialRel = "esriSpatialRelIntersects"
    ),
    "sf"
  )

  expect_s3_class(
    esri2sf(
      url = sample_url,
      progress = TRUE
    ),
    "sf"
  )

  # expect_s3_class(
  #   esri2sf(
  #     url = sample_df_url,
  #     where = "new_deaths > 1000"
  #   ),
  #   "data.frame"
  # )
})
