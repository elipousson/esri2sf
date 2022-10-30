test_that("esriUrl_serviceUrl returns correct substring", {
  # skip_if_offline(host = "arcgisonline.com")
  expect_identical(esriUrl_serviceUrl("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3"), "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer")
  expect_identical(esriUrl_serviceUrl("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3/"), "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer")
  expect_identical(esriUrl_serviceUrl("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer"), "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer")
  expect_identical(esriUrl_serviceUrl("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/"), "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer")
  expect_error(esriUrl_serviceUrl("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/"), "`url` must end in one of the supported service types", fixed = TRUE)
})

# test_that("esriUrl_isValidType checks", {
#   skip_if_offline(host = "arcgisonline.com")
#
#   # General Errors
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/Census/MapServer/3"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3", displayReason = TRUE), "Url is not a valid ESRI Service Url.\n'/rest/services' not found in the url.")
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/ESRI_Census_USA"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/ESRI_Census_USA", displayReason = TRUE), "Url is not a valid ESRI Service Url.\nCould not access url with {httr}.", fixed = TRUE)
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/test"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/test", displayReason = TRUE), "Url is not a valid ESRI Service Url.\nError code: 400\nMessage: Unable to complete  operation")
#
#   # General Successes
#   expect_true(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3"))
#   expect_true(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3/", type = NULL))
#   expect_true(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer"))
#   expect_true(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/"))
#   expect_true(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services"))
#
#   # Valid Feature
#   expect_true(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3", type = "feature"))
#   expect_true(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3/", type = "feature"))
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer", type = "feature"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer", type = "feature", displayReason = TRUE), "`url` must end in a feature ID")
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census", type = "feature"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census", type = "feature", displayReason = TRUE), "`url` must end in a feature ID")
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/ArcGIS/rest/services", type = "feature"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/ArcGIS/rest/services", type = "feature", displayReason = TRUE), "`url` must end in a feature ID")
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3", type = "feature"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3", type = "feature", displayReason = TRUE), "Url is not a valid ESRI Service Url.\n'/rest/services' not found in the url.")
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/ESRI_Census_USA", type = "feature"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/ESRI_Census_USA", type = "feature", displayReason = TRUE), "Url is not a valid ESRI Service Url.\nCould not access url with {httr}.", fixed = TRUE)
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/test", type = "feature"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/test", type = "feature", displayReason = TRUE), "Url is not a valid ESRI Service Url.\nError code: 400\nMessage: Unable to complete  operation")
#
#   # Valid Service
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3", type = "service"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3", type = "service", displayReason = TRUE), "`url` must end in one of the supported service types")
#   expect_true(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer", type = "service"))
#   expect_true(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/", type = "service"))
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census", type = "service"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census", type = "service", displayReason = TRUE), "`url` must end in one of the supported service types")
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/ArcGIS/rest/services", type = "service"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/ArcGIS/rest/services", type = "service", displayReason = TRUE), "`url` must end in one of the supported service types")
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3", type = "service"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3", type = "service", displayReason = TRUE), "Url is not a valid ESRI Service Url.\n'/rest/services' not found in the url.")
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/ESRI_Census_USA", type = "service"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/ESRI_Census_USA", type = "service", displayReason = TRUE), "Url is not a valid ESRI Service Url.\nCould not access url with {httr}.", fixed = TRUE)
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/test", type = "service"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/test", type = "service", displayReason = TRUE), "Url is not a valid ESRI Service Url.\nError code: 400\nMessage: Unable to complete  operation")
#
#   # Valid Folder
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3", type = "folder"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3", type = "folder", displayReason = TRUE), "must be a 'Folder' endpoint")
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer", type = "folder"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer", type = "folder", displayReason = TRUE), "`url` must be a 'Folder' endpoint")
#   expect_true(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census", type = "folder"))
#   expect_true(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/", type = "folder"))
#   expect_true(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/ArcGIS/rest/services", type = "folder"))
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3", type = "folder"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3", type = "folder", displayReason = TRUE), "Url is not a valid ESRI Service Url.\n'/rest/services' not found in the url.")
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/ESRI_Census_USA", type = "folder"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/ESRI_Census_USA", type = "folder", displayReason = TRUE), "Url is not a valid ESRI Service Url.\nCould not access url with {httr}.", fixed = TRUE)
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/test", type = "folder"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/test", type = "folder", displayReason = TRUE), "Url is not a valid ESRI Service Url.\nError code: 400\nMessage: Unable to complete  operation")
#
#   # Valid Root
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3", type = "root"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3", type = "root", displayReason = TRUE), "Url is not a valid ESRI Service Url.\nUrl does not end in '/rest/services'.")
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer", type = "root"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer", type = "root", displayReason = TRUE), "Url is not a valid ESRI Service Url.\nUrl does not end in '/rest/services'.")
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census", type = "root"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census", type = "root", displayReason = TRUE), "Url is not a valid ESRI Service Url.\nUrl does not end in '/rest/services'.")
#   expect_true(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/ArcGIS/rest/services", type = "root"))
#   expect_true(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/ArcGIS/rest/services/", type = "root"))
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3", type = "root"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3", type = "root", displayReason = TRUE), "Url is not a valid ESRI Service Url.\n'/rest/services' not found in the url.")
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/ESRI_Census_USA", type = "root"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/ESRI_Census_USA", type = "root", displayReason = TRUE), "Url is not a valid ESRI Service Url.\nCould not access url with {httr}.", fixed = TRUE)
#   expect_false(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/test", type = "root"))
#   expect_message(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/test", type = "root", displayReason = TRUE), "Url is not a valid ESRI Service Url.\nError code: 400\nMessage: Unable to complete  operation")
#
#   # Test returnType - no type
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3", returnType = TRUE), "feature")
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer", returnType = TRUE), "service")
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/", returnType = TRUE), NA_character_)
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/", returnType = TRUE), "folder")
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/ArcGIS/rest/services", returnType = TRUE), "root")
#
#   # Test returnType - Feature
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3", type = "feature", returnType = TRUE), "feature")
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3", type = "service", returnType = TRUE), "feature")
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3", type = "folder", returnType = TRUE), "feature")
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3", type = "root", returnType = TRUE), "feature")
#
#   # Test returnType - Service
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer", type = "feature", returnType = TRUE), "service")
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer", type = "service", returnType = TRUE), "service")
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer", type = "folder", returnType = TRUE), "service")
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer", type = "root", returnType = TRUE), "service")
#
#   # Test returnType - Service
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census", type = "feature", returnType = TRUE), "folder")
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census", type = "service", returnType = TRUE), "folder")
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census", type = "folder", returnType = TRUE), "folder")
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census", type = "root", returnType = TRUE), "folder")
#
#   # Test returnType - Root
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/ArcGIS/rest/services", type = "feature", returnType = TRUE), "root")
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/ArcGIS/rest/services", type = "service", returnType = TRUE), "root")
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/ArcGIS/rest/services", type = "folder", returnType = TRUE), "root")
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/ArcGIS/rest/services", type = "root", returnType = TRUE), "root")
#
#   # Test returnType - Error
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/", type = "feature", returnType = TRUE), NA_character_)
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/", type = "service", returnType = TRUE), NA_character_)
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/", type = "folder", returnType = TRUE), NA_character_)
#   expect_identical(esriUrl_isValidType("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/", type = "root", returnType = TRUE), NA_character_)
# })

test_that("esriUrl_isValid checks", {
  skip_if_offline("arcgisonline.com")

  expect_true(esriUrl_isValid("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3"))
  expect_true(esriUrl_isValid("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer"))
  expect_true(esriUrl_isValid("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Elevation"))
  expect_true(esriUrl_isValid("https://sampleserver6.arcgisonline.com/ArcGIS/rest/services"))
  expect_true(esriUrl_isValid("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3/"))
  expect_true(esriUrl_isValid("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/"))
  expect_true(esriUrl_isValid("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Elevation/"))
  expect_true(esriUrl_isValid("https://sampleserver6.arcgisonline.com/ArcGIS/rest/services/"))

  expect_message(esriUrl_isValid("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3", displayReason = TRUE), "Url is not a valid ESRI Service Url.\n'/rest/services' not found in the url.")
  expect_false(esriUrl_isValid("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3"))

  expect_message(esriUrl_isValid("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/ESRI_Census_USA", displayReason = TRUE), "Url is not a valid ESRI Service Url.\nCould not access url with {httr}.", fixed = TRUE)
  expect_false(esriUrl_isValid("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/ESRI_Census_USA"))

  expect_message(esriUrl_isValid("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/test", displayReason = TRUE), "Url is not a valid ESRI Service Url.\nError code: 400\nMessage: Unable to complete  operation.")
  expect_false(esriUrl_isValid("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/test"))

  expect_message(esriUrl_isValid("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/ESRI_Census/MapServer/3", displayReason = TRUE), "Url is not a valid ESRI Service Url.\nError code: 400\nMessage: Unable to complete  operation")
  expect_false(esriUrl_isValid("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/ESRI_Census/MapServer/3"))

  # expect_message(esriUrl_isValid("https://sampleserver1.arcgisonline.com/ArcGI/rest/services/Demographics/ESRI_Census_USA/MapServer/3", displayReason = TRUE), "Url is not a valid ESRI Service Url.\nCould not access url with {httr}.", fixed = TRUE)
  # expect_false(esriUrl_isValid("https://sampleserver1.arcgisonline.com/ArcGI/rest/services/Demographics/ESRI_Census_USA/MapServer/3"))
})

test_that("esriUrl_isValid requires token", {
  skip_if_not(keyExists(service = "ArcGISServer", username = "login"), "Secret ArcGISServer key not found. Only works for maintainer and is okay to skip.")
  creds <- jsonlite::fromJSON(keyring::key_get(service = "ArcGISServer", username = "login"))
  token <- generateToken(server = paste0("https://", creds[["servername"]]), uid = creds[["username"]], pwd = creds[["password"]], expiration = 5000)
  tokenUrl <- paste0("https://", creds[["servername"]], "/arcgis/rest/services/PublicSafety/PublicSafetyMapService/MapServer")
  # skip_if_offline_url(url = tokenUrl)

  expect_message(esriUrl_isValid(url = tokenUrl, token = "", displayReason = TRUE), "Url is not a valid ESRI Service Url.\nError code: 499\nMessage: Token Required")
  expect_false(esriUrl_isValid(url = tokenUrl, token = ""))

  expect_true(esriUrl_isValid(url = tokenUrl, token = token))
})

test_that("esriUrl_isValidFeature checks", {
  # skip_if_offline("arcgisonline.com")

  expect_true(esriUrl_isValidFeature("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3"))
  expect_true(esriUrl_isValidFeature("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3/"))

  expect_false(esriUrl_isValidFeature("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/"))
  expect_message(esriUrl_isValidFeature("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/", displayReason = TRUE), "`url` must end in a feature ID")
  expect_message(esriUrl_isValidFeature("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer", displayReason = TRUE), "`url` must end in a feature ID")
})


test_that("esriUrl_isValidService checks", {
  # skip_if_offline_url(url = "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3")

  expect_true(esriUrl_isValidService("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer"))
  expect_true(esriUrl_isValidService("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/"))

  expect_false(esriUrl_isValidService("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/0"))
  expect_message(esriUrl_isValidService("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/0", displayReason = TRUE), "`url` must end in one of the supported service types")
})


test_that("esriUrl_isValidRoot checks", {
  skip_if_offline("arcgisonline.com")

  expect_true(esriUrl_isValidRoot("https://sampleserver6.arcgisonline.com/arcgis/rest/services"))
  expect_true(esriUrl_isValidRoot("https://sampleserver6.arcgisonline.com/arcgis/rest/services/"))

  expect_false(esriUrl_isValidRoot("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census"))
  expect_message(esriUrl_isValidRoot("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census", displayReason = TRUE), "`url` must end in '/rest/services")
})


test_that("esriUrl_isValidFolder checks", {
  # skip_if_offline_url("https://sampleserver6.arcgisonline.com/arcgis/rest/services/StoryMaps")

  expect_true(esriUrl_isValidFolder("https://sampleserver6.arcgisonline.com/ArcGIS/rest/services"))
  expect_true(esriUrl_isValidFolder("https://sampleserver6.arcgisonline.com/ArcGIS/rest/services/"))
  expect_true(esriUrl_isValidFolder("https://sampleserver6.arcgisonline.com/arcgis/rest/services/StoryMaps"))
  expect_true(esriUrl_isValidFolder("https://sampleserver6.arcgisonline.com/arcgis/rest/services/StoryMaps/"))

  expect_false(esriUrl_isValidFolder("https://sampleserver6.arcgisonline.com/arcgis/rest/services/StoryMaps/StoryMapPlaces1/FeatureServer"))
  expect_message(esriUrl_isValidFolder("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer", displayReason = TRUE), "`url` must be a 'Folder' endpoint")
})


test_that("esriUrl_parseUrl", {
  # skip_if_offline_url(url = "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3")
  # skip_if_offline_url(url = "https://services.arcgis.com/V6ZHFr6zdgNZuVG0/arcgis/rest/services/Landscape_Trees/FeatureServer/0")

  expect_snapshot(esriUrl_parseUrl("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3"))
  expect_snapshot(esriUrl_parseUrl("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer"))
  expect_snapshot(esriUrl_parseUrl("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census"))
  expect_snapshot(esriUrl_parseUrl("https://sampleserver6.arcgisonline.com/ArcGIS/rest/services"))
  expect_snapshot(esriUrl_parseUrl("https://services.arcgis.com/V6ZHFr6zdgNZuVG0/arcgis/rest/services/Landscape_Trees/FeatureServer/0/"))

  expect_error(esriUrl_parseUrl("https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/4"), "HTTP 503 Service Unavailable", fixed = TRUE)
})
