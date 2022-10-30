test_that("esriIndex works", {
  sample_url <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services"

  expect_s3_class(
    esriIndex(
      url = sample_url
    ),
    "data.frame"
  )

  # NOTE: This will fail if ESRI changes the number of services on the sample
  # server
  expect_equal(
    nrow(esriIndex(
      url = sample_url
    )),
    75
  )

  expect_s3_class(
    esriIndex(
      url = sample_url,
      recurse = TRUE
    ),
    "data.frame"
  )
})
