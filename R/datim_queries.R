#' @title Country organisation units levels
#'
#' @param cntry Country name
#'
#' @return Country org levels as data frame
#'

datim_orglevels <- function(cntry) {

  if (!cntry %in% pepfar_countries$country) {
    usethis::ui_stop(glue::glue("Invalid country name: {cntry}"))
  }

  pepfar_countries %>%
    dplyr::filter(country == cntry) %>%
    dplyr::select(ends_with("_lvl")) %>%
    tidyr::pivot_longer(cols = ends_with("_lvl"),
                        names_to = "label",
                        values_to = "level") %>%
    dplyr::mutate(label = stringr::str_remove(label, "_lvl$"),
                  level = as.character(level)) %>%
    dplyr::select(level, label) %>%
    dplyr::arrange(desc(level))
}

#' @title Query Datim SQLViews
#'
#' @param username Datim username
#' @param password Datim password
#' @param view_name Datim SQLView name
#' @param dataset   Return SQLView dataset or uid? Default is false
#' @param datauid   Data UID
#' @param query     SQLView Query params, a list containing type and params key value pairs
#' @param base_url  Datim API Base URL
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
#' @param username Datim username
#' @param password Datim password
#' @param cntry    Country name
#' @param base_url Datim API Base URL
#'
#' @export
#' @return OU Orgunit as data frame
#'
datim_orgunits <- function(username, password, cntry,
                           base_url = NULL) {

  if(!cntry %in% pepfar_countries$country) {
    usethis::ui_stop(glue::glue("Invalid country name: {cntry}"))
  }

  # Get Country ISO Code
  cntry_iso <- pepfar_countries %>%
    dplyr::filter(country == cntry) %>%
    dplyr::pull(country_iso)

  # get distinct levels
  cntry_levels <- pepfar_countries %>%
    dplyr::filter(country == cntry) %>%
    dplyr::select(ends_with("_lvl")) %>%
    tidyr::pivot_longer(cols = tidyr::ends_with("_lvl"),
                        names_to = "orgunit_label",
                        values_to = "orgunit_level") %>%
    dplyr::mutate(orgunit_label = stringr::str_remove(orgunit_label, "_lvl$"),
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
    clean_orgview(levels = cntry_levels) #%>%
    #reshape_orgview(levels = cntry_levels) #%>%
    #rename_orgview(levels = cntry_levels)
}


#' @title Clean OrgUnits SQLView
#'
#' @param .data  Data Frame
#' @param levels Org Levels
#'

clean_orgview <- function(.data, levels) {
  # Clean up orgunit
  .data %>%
    dplyr::select(-c(orgunit_code, moh_id)) %>%
    dplyr::rename_with(.cols = ends_with("internal_id"),
                       .fn = ~ str_replace(., "internal_id", "uid")) %>%
    dplyr::rename_with(.cols = starts_with("regionor"),
                       .fn = ~str_remove(., "regionor")) %>%
    dplyr::select(orgunit = orgunit_name, orgunituid = orgunit_uid,
                  orgunit_level, orgunit_parent, orgunit_parent_uid,
                  dplyr::everything()) %>%
    dplyr::left_join(levels, by = "orgunit_level") %>%
    dplyr::relocate(orgunit_label, .after = orgunit_level) %>%
    dplyr::left_join(pepfar_countries[, c("operatingunit", "country")],
                     by = c("country_name" = "country"))
}


#' @title Reshape OrgUnits SQLView
#'
#' @param .data  Data Frame
#' @param levels Org Levels
#'

reshape_orgview <- function(.data, levels) {

  levels <- levels %>%
    dplyr::rename(level = orgunit_level,
                  label = orgunit_label)

  # Add levels
  .data <- .data %>%
    dplyr::filter(orgunit_level >= 3) %>%
    dplyr::mutate(
      orgunit_parent_level = as.character(as.integer(orgunit_level) - 1)) %>%
    dplyr::left_join(levels, by = c("orgunit_parent_level" = "level")) %>%
    dplyr::rename(orgunit_parent_label = label) %>%
    dplyr::select(operatingunit, country = country_name,
                  orgunit_parent_uid, orgunit_parent_name = orgunit_parent,
                  orgunit_parent_level, orgunit_parent_label,
                  orgunituid, orgunit, orgunit_level, orgunit_label)

  # Map 2nd level Child to Parent relationship
  .df_org <- .data %>%
    dplyr::inner_join(.data,
                      by = c("orgunit_parent_uid" = "orgunituid",
                             "orgunit_parent_name" = "orgunit"),
                      suffix = c("", "_top"))

  # Append non-matched Orgs - Optional
  # .data <- .data %>%
  #   dplyr::anti_join(.df_org) %>%
  #   dplyr::bind_rows(.df_org, .)

  # Map 3nd level Child to Parent relationship
  level_psnu <- .data %>%
    dplyr::filter(orgunit_label == "psnu") %>%
    dplyr::distinct(orgunit_level) %>%
    dplyr::pull()

  if (as.integer(level_psnu) - 3 > 1) {

    .df_org <- .data %>%
      dplyr::inner_join(.data,
                        by = c("orgunit_parent_uid" = "orgunituid",
                               "orgunit_parent_name" = "orgunit"),
                        suffix = c("", "_overtop"))

    # Append non-matched Orgs - Optional
    # .data <- .data %>%
    #   dplyr::anti_join(.df_org) %>%
    #   dplyr::bind_rows(.df_org, .) #%>%
      # dplyr::mutate(
      #   orgunit_label = dplyr::case_when(
      #     orgunit_level == as.integer(level_psnu) + 1 ~ "snu1",
      #     TRUE ~ orgunit_label
      #   )
      # )
  }

  if (as.integer(level_psnu) - 3 > 2) {

    .df_org <- .df_org %>%
      dplyr::inner_join(.df_org,
                        by = c("orgunit_parent_uid" = "orgunituid",
                               "orgunit_parent_name" = "orgunit"),
                        suffix = c("", "_supertop"))

    # Append non-matched Orgs - Optional
    # .data <- .data %>%
    #   dplyr::anti_join(.df_org) %>%
    #   dplyr::bind_rows(.df_org, .) #%>%
      # orgunit_label = dplyr::case_when(
      #   orgunit_level == as.integer(level_psnu) + 2 ~ "snu2",
      #   TRUE ~ orgunit_label
      # )
  }

  .data <- .data %>%
    dplyr::anti_join(.df_org) %>%
    dplyr::bind_rows(.df_org, .)

  # Reorder columns
  .data %>%
    dplyr::select(orgunituid, orgunit, orgunit_level, orgunit_label,
                  dplyr::matches(".*_parent_.*"),
                  dplyr::matches(".*_parent_.*_top"),
                  dplyr::matches(".*_parent_.*_overtop"),
                  dplyr::matches(".*_parent_.*_supertop"),
                  dplyr::everything())
}

#' @title Rename OrgUnit SQLView
#'
#' @param .data  Data Frame
#' @param levels Org Levels
#'

rename_orgview <- function(.data, levels) {
  # Levels
  level_fac <- levels$orgunit_level[levels$orgunit_label=="facility"]
  level_com <- levels$orgunit_level[levels$orgunit_label=="community"]
  level_psnu <- levels$orgunit_level[levels$orgunit_label=="psnu"]
  level_snu1 <- level_psnu
  level_snu2 <- NA
  level_cntry <- levels$orgunit_level[levels$orgunit_label=="country"]

  # Add SNU1 Level
  if(as.integer(level_psnu) - as.integer(level_cntry) > 1) {
    level_snu1 <- as.character(as.integer(level_psnu) +1)

    .data <- .data %>%
      dplyr::mutate(
        orgunit_label = dplyr::case_when(
          orgunit_level == level_snu1 & is.na(orgunit_label) ~ "snu1",
          TRUE ~ orgunit_label
        ))
  }
  else if(as.integer(level_psnu) - as.integer(level_cntry) == 1) {
    .data <- data %>%
      dplyr::filter(orgunit_level == level_psnu) %>%
      dplyr::mutate(orgunit_level == level_snu1, orgunit_label == "snu1") %>%
      dplyr::bind_rows(.data, .)
  }

  # Add SNU2 Level
  if(as.integer(level_snu1) - as.integer(level_cntry) > 1) {
    level_snu2 <- as.character(as.integer(level_snu1) +1)

    .data <- .data %>%
      dplyr::mutate(
        orgunit_label = dplyr::case_when(
          orgunit_level == level_snu2 & is.na(orgunit_label) ~ "snu2",
          TRUE ~ orgunit_label
        ))
  }

  # filter and transform
  levels %>%
    dplyr::select(level = orgunit_level, label = orgunit_label) %>%
    dplyr::add_row(level = level_snu1, label = "snu1") %>%
    purrr::pmap_dfr(function(level, label){

      .df_org <- .data %>% dplyr::filter(orgunit_level == level)

      usethis::ui_info(glue("{level} => {label}: {nrow(.df_org)}"))

      .df_org %>%
        dplyr::mutate(
          facilityuid = dplyr::case_when(
            orgunit_level == level_fac ~ orgunituid,
            TRUE ~ "~"
          ),
          facility = dplyr::case_when(
            orgunit_level == level_fac ~ orgunit,
            TRUE ~ "Data reported above facility level"
          ),
          communityuid = dplyr::case_when(
            orgunit_level == level_fac ~ orgunit_parent_uid,
            orgunit_level == level_com ~ orgunituid,
            TRUE ~ "~"
          ),
          community = dplyr::case_when(
            orgunit_level == level_fac ~ orgunit_parent_name,
            orgunit_level == level_com ~ orgunit,
            TRUE ~ "Data reported above community level"
          ),
          psnuuid = dplyr::case_when(
            orgunit_level == level_fac ~ orgunit_parent_uid_top,
            orgunit_level == level_com ~ orgunit_parent_uid,
            orgunit_level == level_psnu ~ orgunituid,
            TRUE ~ "~"
          ),
          psnu = dplyr::case_when(
            orgunit_level == level_fac ~ orgunit_parent_name_top,
            orgunit_level == level_com ~ orgunit_parent_name,
            orgunit_level == level_psnu ~ orgunit,
            TRUE ~ "Data reported above psnu level"
          ),
          # Optional SNU1
          snu1uid = dplyr::case_when(
            orgunit_level == level_fac ~ orgunit_parent_uid_top,
            orgunit_level == level_com ~ orgunit_parent_uid,
            orgunit_level == level_psnu ~ orgunituid,
            orgunit_level == level_snu1 ~ orgunituid,
            TRUE ~ "~"
          ),
          snu1 = dplyr::case_when(
            orgunit_level == level_fac & level_snu1 == level_psnu ~ orgunit_parent_name_top,
            orgunit_level == level_com & level_snu1 == level_psnu ~ orgunit_parent_name,
            orgunit_level == level_psnu & level_snu1 == level_psnu ~ orgunit,
            TRUE ~ "Data reported above psnu level"
          )
        )
    }) %>%
    dplyr::select(orgunit, orgunituid,
                  level = orgunit_level, type = orgunit_label,
                  operatingunit, country, psnu, psnuuid,
                  community, communityuid, facility, facilityuid, everything())
}

#' @title Pull Orgunits
#'
#' @param username Datim username
#' @param password Datim password
#' @param cntry    Country name
#' @param base_url Datim API Base URL
#'
#' @export
#' @return OU Mechanisms as data frame
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
