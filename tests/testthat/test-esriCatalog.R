test_that("esriCatalog works", {
  expect_type(
    esriCatalog("https://sampleserver6.arcgisonline.com/arcgis/rest/services/LocalGovernment/CitizenRequests/FeatureServer/0"),
    "list"
  )
})
