#' @title Query Datim SQLViews
#'
#' @param username
#' @param password
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
          map_chr(~paste0(.x, ":eq:", query[.x])) %>%
          paste0(collapse = "&") %>%
          paste0("&filter=", .)

        print(glue::glue("SQL View field query: {fq}"))

        dta_url <- dta_url %>% paste0(fq)
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
datim_orgunits <- function(username, password, cntry, base_url = NULL) {

  uid <- grabr::get_ouuid(cntry, username, password)

  datim_sqlviews(
    username,
    password,
    view_name = "Data Exchange: Organisation Units",
    dataset = TRUE,
    query = list(
      type = "variable",
      params = list("OU" = uid)
    ),
    base_url = base_url
  )
}
