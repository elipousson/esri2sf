#' @title Add domain information to the return (sf) dataframe
#'
#' @description Uses the esrimeta function to get information about the field
#' domains in the returned dataframe and joins the domain information to the
#' dataframe. This is especially important for the codedValue type domains that
#' are essentially factors and you loose information without replacing the data.
#'
#' @param df The returned (sf) df from esri2sf/esri2df.
#' @param url The url for the Map/Feature server layer/table.
#' @param token string for authentication token (if needed).
#' @inheritParams esrimeta
#'
#' @return An {sf} dataframe
addDomainInfo <- function(df, url, token = NULL, call = caller_env()) {
  # Get Field Metadata
  layerTableFields <- esrimeta(url, token = token, fields = TRUE, call = call)

  # Check for domain column
  if (!("domain" %in% names(layerTableFields))) {
    return(df)
  }

  # Check if any values in the domain list column are NULL
  if (any(lapply(layerTableFields[["domain"]], is_null))) {
    cli::cli_alert("This layer has partial domain information but {.fn addDomainInfo} does not support this possibility.")
    return(df)
  }

  # Check that domain column is logical (all NA so domains to handle)
  if ("logical" %in% class(layerTableFields[["domain"]])) {
    return(df)
  }

  # Check that domain column is a dataframe
  if (!("data.frame" %in% class(layerTableFields[["domain"]]))) {
    cli::cli_abort("The domain field in the layerTableFields is not a dataframe. Edits need to be made to the addDomainInfo() function. Please start an issue at 'https://github.com/yonghah/esri2sf/issues/new/choose' so that the issue can be fixed.")
  }

  # Check that domain dataframe is the same length as the layerTableFields dataframe.
  if (nrow(layerTableFields) != nrow(layerTableFields[["domain"]])) {
    cli::cli_abort("The domain dataframe in the layerTableFields is not the same length as the layerTableFields dataframe. Please start an issue at 'https://github.com/yonghah/esri2sf/issues/new/choose' so that the issue can be fixed.")
  }

  # Reformat layerTableFields so that its all one dataframe
  domainDF <- layerTableFields[["domain"]]
  names(domainDF) <- paste0("domain_", names(domainDF))
  layerTableFields <- dplyr::bind_cols(layerTableFields[, -which(names(layerTableFields) == "domain")], domainDF)

  # Check handled domain types
  handledDomainTypes <- c("codedValue", "range")
  domainTypes <- stats::na.omit(layerTableFields[["domain_type"]])
  if (!all(domainTypes %in% handledDomainTypes)) {
    newDomainTypes <- domainTypes[!(domainTypes %in% handledDomainTypes)]
    cli::cli_abort(paste0("Field domain of type(s): ", paste0("'", newDomainTypes, "'", collapse = ", "), " found in the function esri2sf:::getDomainInfo(). Please start an issue at 'https://github.com/yonghah/esri2sf/issues/new/choose' so that the novel domain type can be handled by the package."))
  }


  if (length(stats::na.omit(layerTableFields[["domain_type"]])) == 0) {
    return(df)
  } else {
    # replace values in df with domain information (codedValue domains)
    codedFields <- layerTableFields[!is.na(layerTableFields[["domain_type"]]) & layerTableFields[["domain_type"]] == "codedValue", ]
    if (nrow(codedFields) > 0) {
      # row = split(codedFields,codedFields$name)[[1]]
      for (row in split(codedFields, codedFields$name)) {
        # Get coded values df
        codedValues <- row[["domain_codedValues"]][[1]]
        names(codedValues) <- paste0("domain_codedValues_", names(codedValues))

        # Join dataframes
        df[[row[["name"]]]] <- as.character(df[[row[["name"]]]])
        codedValues[["domain_codedValues_code"]] <- as.character(codedValues[["domain_codedValues_code"]])
        df <- dplyr::left_join(df, codedValues, by = `names<-`("domain_codedValues_code", row[["name"]]))

        # Clean up join an insert updated column to the same place as original
        oldLocation <- which(names(df) == row[["name"]])
        df <- df[, -oldLocation]
        df <- dplyr::rename(df, `names<-`("domain_codedValues_name", row[["name"]]))
        df <- dplyr::relocate(df, row[["name"]], .before = oldLocation)
      }
    }
    # range domains are ignored
  }

  return(df)
}
