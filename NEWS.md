# esri2sf (development version)

* Add `{arcgisutils}` to Suggests. `esri2sf()` also now uses `arcgisutils::arc_token()` (if installed) to set the default value of token. Set token to `""` to override this potentially breaking change.
* Added `esriitem()` and `esiruser()` functions supporting content and community APIs. (2023-10-23)
* Added `glue_ansi_sql()`, `glue_sql_bbox()`, `fmt_epoch_date()`, and `fmt_epoch_dates()` helper functions. (2023-08-05)
* Allow input `url` values that contain a trailing query. This query is stripped from the input URL by default. (2023-08-22) Also add `trimws()` to support URLs with trailing or leading white space. (2023-10-23)

# esri2sf 0.2.0 (2023-07-19)

* Initial version including the migration from `{httr}` to `{httr2}`, extensive refactoring of URL helper functions, and new functions including `esri2rast()` (2022-08-02), `esriIndex()` (2022-05-22), `esriInfo()` (2022-05-22), `esrigeocode()` (2022-07-18), `esrisearch()` (2022-09-06), and `esrihub()` (2023-07-10). [elipousson/esri2sf](https://github.com/elipousson/esri2sf) is expected to continue as a hard fork from the original yonghah/esri2sf repository.

# esri2sf 0.1.0

* The original 0.1.0 version of the esri2sf package is available at [yonghah/esri2sf](https://github.com/yonghah/esri2sf). Changes on this fork were not versioned prior to 2023-07-19. 
