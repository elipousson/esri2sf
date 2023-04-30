test_that("esrigeocode works", {
  url <- "https://geodata.baltimorecity.gov/egis/rest/services/Locator/EGISCompositeLocator/GeocodeServer"

  test_geocode <- esrigeocode(url, address = "100 HOLLIDAY STREET")

  expect_s3_class(
    test_geocode,
    "sf"
  )

  expect_s3_class(
    esrigeocode(url, coords = test_geocode),
    "data.frame"
  )

})
