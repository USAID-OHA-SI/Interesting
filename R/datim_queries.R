#' @title Query Datim SQLViews
#'
#' @param username
#' @param password
#'
#' @export
#' @return SQLView uid or dataset as data frame
#'
datim_sqlviews <- function(username, password,
                           view_name = NULL,
                           dataset = FALSE,
                           datauid = NULL,
                           query = NULL,
                           base_url = NULL) {

  # Base url
  if (missing(base_url))
    base_url <- "https://final.datim.org"

  # Other Options
  end_point <- "/api/sqlViews/"

  options <- "?format=json&paging=false"

  # API URL
  api_url <- base_url %>% paste0(end_point, options)

  # Query data
  data <- api_url %>%
    grabr::datim_execute_query(username, password, flatten = TRUE) %>%
    purrr::pluck("sqlViews") %>%
    tibble::as_tibble() %>%
    dplyr::rename(uid = id, name = displayName)

  # Filter if needed
  if (!base::is.null(view_name)) {

    print(glue::glue("Searching for SQL View: {view_name} ..."))

    data <- data %>%
      dplyr::filter(stringr::str_to_lower(name) == str_to_lower(view_name))
  }

  # Number of rows
  rows = base::nrow(data)

  # Return only ID when results is just 1 row
  if(rows == 0) {
    base::warning("No match found for the requested SQL View")
    return(NULL)
  }
  # Flag non-unique sqlview names
  else if (rows > 1 && dataset == TRUE) {
    base::warning("There are more than 1 match for the requested SQL View data. Please try to be specific.")
    print(data)
    return(NULL)
  }
  # Return only ID when results is just 1 row
  else if (rows == 1 && dataset == FALSE) {
    return(data$uid)
  }
  # Return SQLVIEW data
  else if(base::nrow(data) == 1 && dataset == TRUE) {

    dta_uid <- data$uid

    dta_url <- base_url %>%
      paste0(end_point, dta_uid, "/data", options, "&fields=*") #:identifiable, :nameable

    # Apply varialbe or field query if needed
    if (!is.null(query)) {

      q <- names(query$params) %>%
        map_chr(~paste0(.x, "=", query$params[.x])) %>%
        paste0(collapse = "&") %>%
        paste0("QUERY PARAMS: type=", query$type, "&", .)

      print(print(glue::glue("SQL View Params: {q}")))

      if (query$type == "variable") {
        vq <- names(query$params) %>%
          map_chr(~paste0(.x, ":", query$params[.x])) %>%
          paste0(collapse = "&") %>%
          paste0("&var=", .)

        print(glue::glue("SQL View variable query: {vq}"))

        dta_url <- dta_url %>% paste0(vq)
      }
      else if (query$type == "field") {
        fq <- names(query$params) %>%
          map_chr(~paste0("filter=", .x, ":eq:", query$params[.x])) %>%
          paste0(collapse = "&") %>%
          paste0("&", .)

        print(glue::glue("SQL View field query: {fq}"))

        dta_url <- dta_url %>% paste0(fq)
      }
      else {
        print(glue::glue("Error - Invalid query type: {query$type}"))
      }
    }


    print(glue::glue("SQL View url: {dta_url}"))

    # Query data
    data <- dta_url %>%
      grabr::datim_execute_query(username, password, flatten = TRUE)

    # Detect Errors
    if (!is.null(data$status)) {
      print(glue::glue("Status: {data$status}"))

      if(!is.null(data$message)) {
        print(glue::glue("Message: {data$message}"))
      }

      return(NULL)
    }

    # Headers
    headers <- data %>%
      purrr::pluck("listGrid") %>%
      purrr::pluck("headers") %>%
      dplyr::pull(column)

    # Data
    data <- data %>%
      purrr::pluck("listGrid") %>%
      purrr::pluck("rows") %>%
      tibble::as_tibble(.name_repair = "unique") %>%
      janitor::clean_names() %>%
      magrittr::set_colnames(headers)
  }

  return(data)
}


#' @title Pull Orgunits
#'
#' @param username
#'
#' @export
#' @return OU Orgunit as data frame
#'
datim_orgunits <- function(username, password, cntry, base_url = NULL) {

  if(!cntry %in% pepfar_countries$country) return(NULL)

  # Get Country ISO Code
  cntry_iso <- pepfar_countries %>%
    dplyr::filter(country == cntry) %>%
    dplyr::pull(country_iso)

  # get distinct levels
  cntry_levels <- pepfar_countries %>%
    dplyr::filter(country == meta$ou) %>%
    dplyr::select(ends_with("_lvl")) %>%
    tidyr::pivot_longer(cols = ends_with("_lvl"),
                        names_to = "orgunit_label",
                        values_to = "orgunit_level") %>%
    dplyr::mutate(orgunit_label = str_remove(orgunit_label, "_lvl$"),
                  orgunit_level = as.character(orgunit_level))

  # Get orgunits
  .orgs <- datim_sqlviews(
    username,
    password,
    view_name = "Data Exchange: Organisation Units",
    dataset = TRUE,
    query = list(
      type = "variable",
      params = list("OU" = cntry_iso)
    ),
    base_url = base_url
  )

  # Clean up orgunit
  .orgs %>%
    dplyr::select(-c(orgunit_code, moh_id)) %>%
    dplyr::rename_with(.cols = ends_with("internal_id"),
                       .fn = ~ str_replace(., "internal_id", "uid")) %>%
    dplyr::rename_with(.cols = starts_with("regionor"),
                       .fn = ~str_remove(., "regionor")) %>%
    dplyr::select(orgunit = orgunit_name, orgunituid = orgunit_uid,
                  orgunit_level, orgunit_parent, orgunit_parent_uid,
                  dplyr::everything()) %>%
    dplyr::left_join(cntry_levels, by = "orgunit_level") %>%
    dplyr::relocate(orgunit_label, .after = orgunit_level) %>%
    dplyr::left_join(pepfar_countries[, c("operatingunit", "country")],
                     by = c("country_name" = "country"))
}

#' @title Pull Orgunits
#'
#' @param username
#'
#' @export
#' @return OU Orgunit as data frame
#'
datim_mechs <- function(username, password, cntry, base_url = NULL) {

  df_mechs <- datim_sqlviews(
    username,
    password,
    view_name = "Mechanisms partners agencies OUS Start End",
    dataset = TRUE,
    query = list(
      type = "field",
      params = list("ou" = cntry, agency = "USAID")
    ),
    base_url = base_url
  )

  # Reshape Results - mech code, award number, and name separations chars
  sep_chrs <- c("[[:space:]]+",
                "[[:space:]]+-",
                "[[:space:]]+-[[:space:]]+",
                "[[:space:]]+-[[:space:]]+-",
                "[[:space:]]+-[[:space:]]+-[[:space:]]+",
                "[[:space:]]+-[[:space:]]+-[[:space:]]+-",
                "-[[:space:]]+",
                "-[[:space:]]+-",
                "-[[:space:]]+-[[:space:]]+",
                "-[[:space:]]+-[[:space:]]+-",
                "-[[:space:]]+-[[:space:]]+-[[:space:]]+",
                "-[[:space:]]+-[[:space:]]+-[[:space:]]+-")

  # Reshape Results - separation
  df_mechs %>%
    dplyr::rename(
      mech_code = code,
      operatingunit = ou,
      prime_partner_name = partner,
      prime_partner_uid = primeid,
      funding_agency = agency,
      operatingunit = ou
    ) %>%
    dplyr::mutate(
      mech_name = stringr::str_remove(mechanism, mech_code),
      mech_name = stringr::str_replace_all(mech_name, "\n", ""),
      award_number = dplyr::case_when(
        stringr::str_detect(prime_partner_name, "^TBD") ~ NA_character_,
        TRUE ~ stringr::str_extract(
          mech_name,
          pattern = "(?<=-[:space:])[A-Z0-9]+(?=[:space:]-[:space:])"
        )
      ),
      mech_name = dplyr::case_when(
        !is.na(award_number) ~ stringr::str_remove(mech_name, award_number),
        TRUE ~ mech_name
      ),
      mech_name = stringr::str_remove(
        mech_name,
        paste0("^", rev(sep_chrs), collapse = "|")
      )
    ) %>%
    dplyr::select(uid, mech_code, mech_name, award_number, mechanism, everything())
}
