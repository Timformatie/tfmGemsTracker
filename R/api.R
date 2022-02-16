# API functions

#' Get api info
#'
#' @param environment One of: ["production", "acceptance", "testing"].
#'
#' @return List with api information.
#'
#' @export
#' @importFrom glue glue
get_api_info <- function(environment) {
  # Assert availability of environment variables
  stopifnot(
    "PULSE_BASE_URL missing" = Sys.getenv("PULSE_BASE_URL") != "",
    "PULSE_API_CLIENT_ID missing" = Sys.getenv("PULSE_API_CLIENT_ID") != "",
    "PULSE_API_CLIENT_SECRET missing" =
      Sys.getenv("PULSE_API_CLIENT_SECRET") != "",
    "PULSE_USERNAME missing" = Sys.getenv("PULSE_USERNAME") != "",
    "PULSE_PASSWORD missing" = Sys.getenv("PULSE_PASSWORD") != "",
    "PULSE_ENCRYPTION_KEY missing" = Sys.getenv("PULSE_ENCRYPTION_KEY") != "",
    "APP_BASE_URL missing" = Sys.getenv("APP_BASE_URL") != ""
  )

  api_info <- list(
    base_url = Sys.getenv("PULSE_BASE_URL"),
    client_id = Sys.getenv("PULSE_API_CLIENT_ID"),
    client_secret = Sys.getenv("PULSE_API_CLIENT_SECRET"),
    username = Sys.getenv("PULSE_USERNAME"),
    password = Sys.getenv("PULSE_PASSWORD"),
    hashkey = Sys.getenv("PULSE_ENCRYPTION_KEY")
  )

  message(glue::glue("De env variable PULSE_BASE_URL is: {api_info$base_url}"))

  dashboard_urls <- list(
    base = Sys.getenv("APP_BASE_URL")
  )

  full_urls <- paste0(api_info$base_url, api_urls)
  names(full_urls) <- names(api_urls)

  # TODO: why vector of these 3, are the last 2 even used?
  api_info <- c(api_info, full_urls, dashboard_urls)

  return(api_info)
}

#' Get access token
#'
#' @param api_info List with api information.
#' @param check_ssl Whether to check the SSL certificate or allow insecure
#'   connections.
#' @param debug Whether to enable debugging messages.
#'
#' @return Token to access the API.
#'
#' @export
#' @importFrom httr authenticate config content POST
#' @importFrom glue glue
#' @importFrom logr log_print
get_access_token <- function(api_info, check_ssl = TRUE, debug = FALSE) {
  # Get access token
  if (debug) {
    logr::log_print(glue::glue(
      "Access token trying to receive from: {api_info$token_url}"
    ), console = FALSE)
    logr::log_print(glue::glue(
      "Access token with user: {api_info$username}"
    ), console = FALSE)
  }

  response <- httr::POST(
    api_info$token_url,
    body = list(grant_type = "password",
                username = api_info$username,
                password = api_info$password),
    httr::authenticate(api_info$client_id,
                 api_info$client_secret),
    config = httr::config(ssl_verifypeer = check_ssl,
                          http_version = 2)
  )

  # check if output is correct
  if (response$status_code != 200) {
    stop(glue::glue("Status code for access token is: {response$status_code}. ",
                    "Something goes wrong with login details"))
  } else {
    message("Access token correctly obtained.")
  }
  access_token <- httr::content(response)$access_token

  return(access_token)
}

#' Get query data
#'
#' @param url The url to make your request to.
#' @param token API access token.
#' @param output_type One of ["parsed", "text"].
#' @param language Language of the response data.
#' @param check_ssl Whether to check the SSL certificate or allow insecure
#'   connections.
#' @param debug Whether to enable debugging messages.
#'
#' @return List including the response data.
#'
#' @importFrom glue glue
#' @importFrom httr add_headers config content GET
#' @importFrom logr log_print
get_query_data <- function(
  url,
  token,
  output_type = "parsed",
  language = "nl",
  check_ssl = TRUE,
  debug = FALSE
) {

  if (debug) {
    logr::log_print(
      glue::glue("Try to retrieve data from {url}"), console = FALSE
    )
  }
  # start.time <- Sys.time()
  req <- httr::GET(
    url,
    httr::add_headers(
      Authorization = glue::glue("Bearer {token}"),
      `Accept-Language` = language
    ),
    config = httr::config(
      ssl_verifypeer = check_ssl,
      http_version = 2
    )
  )

  if (req$status_code != 200) {
    stop(glue::glue(
      "Request failed with status code {req$status_code}. "
      #"{httr::content(req)$error}"
    ))
    if (debug) {
      logr::log_print(glue::glue(
        "Data from url {url} NOT correctly obtained"
      ), console = FALSE)
      logr::log_print(glue::glue(
        "Error in query: {url} with status code: {req$status_code}"
      ), console = FALSE)
    }
    return(NULL)
  }

  if (debug) {
    logr::log_print(
      glue::glue("data from url {url} correctly obtained"), console = FALSE
    )
  }
  json_data <- httr::content(req, as = output_type, encoding = "UTF-8")
  return(json_data)
}

#' Get careplan info
#'
#' @param patient_id String formatted like "patient_id@@organisation_id".
#' @param base_careplan_url API endpoint for careplan data.
#' @param careplan_filter Vector with careplan names to filter on.
#' @inheritParams get_query_data
#'
#' @return Data.table containing careplan data.
#'
#' @examples
#' get_careplan_info("555555@@70", access_token = token,
#'   base_careplan_url = url)
#'
#' @export
#' @import data.table
#' @importFrom jsonlite fromJSON
get_careplan_info <- function(
  patient_id,
  access_token,
  base_careplan_url,
  language = "nl",
  check_ssl = TRUE,
  debug = FALSE,
  careplan_filter = NULL
) {

  careplan_url = paste0(base_careplan_url, patient_id)
  dt_careplan <- get_query_data(
    url = careplan_url,
    token = access_token,
    output_type = "text",
    check_ssl = check_ssl,
    debug = debug
  )

  if (is.null(dt_careplan)) {
    return(NULL)
  }

  dt_careplan <- data.table::as.data.table(jsonlite::fromJSON(dt_careplan))
  if (!is.null(careplan_filter)) {
    dt_careplan = dt_careplan[title %in% careplan_filter]
  }

  return(dt_careplan)

}

#' Get task info
#'
#' @param patient_id String formatted like "patient_id@@organisation_id".
#' @param base_task_url API endpoint for task data.
#' @param tracks Tracks to filter on.
#' @param careplan_ids Careplan id's to filter on.
#' @inheritParams get_query_data
#'
#' @return Data.table containing task info data.
#'
#' @export
#' @import data.table
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
get_task_info <- function(
  patient_id,
  access_token,
  base_task_url,
  language = "nl",
  check_ssl = TRUE,
  debug = FALSE,
  tracks = NULL,
  careplan_ids = NULL
) {

  if (!is.null(careplan_ids)) {
    task_url = glue::glue(
      "{base_task_url}&carePlan=[{paste0(careplan_ids, collapse = ',')}]"
    )
  }

  task_info <- get_query_data(
    url = task_url,
    token = access_token,
    output_type = "text",
    language = language,
    check_ssl = check_ssl,
    debug = debug
  )

  if (is.null(task_info)) {
    return(NULL)
  }

  task_info <- data.table::as.data.table(jsonlite::fromJSON(
    task_info, flatten = TRUE
  ))

  # Unlist nested elements
  nested_elements <-  names(task_info)[sapply(task_info, class) == "list"]
  if (length(nested_elements) > 0) {
    for (element in nested_elements) {
      # Transform nested list elements to whole rows with multiple columns
      nested_data <- data.table::rbindlist(
        task_info[[element]],
        idcol = TRUE,
        use.names = TRUE,
        fill = TRUE
      )
      # Change column type to character
      change_cols <- colnames(nested_data)[
        !colnames(nested_data) %in% c("type", ".id")
      ]
      # nested_data = nested_data[
      #   , (change_cols) := lapply(.SD, na.rm = TRUE, as.character),
      #   .SDcols = change_cols
      # ]
      sapply(change_cols, function(col) {
        data.table::set(
          nested_data, j = col, value = as.character(nested_data[[col]])
        )
      })

      # Transform to long format
      data_long = data.table::melt(
        nested_data, id.vars = c(".id", "type"), measure.vars = change_cols
      )
      # Add count and column name
      data_long = data_long[!is.na(value)][
        ,count := .N, by = c("type", ".id")
      ][
        , col_name := ifelse(count > 1, paste0(type, ".", variable), type)
      ]

      # Only keep relevant columns and transform back to wide format
      data_sub = data_long[, .(col_name, value, `.id`)][order(`.id`)]
      data_sub_wide = data.table::dcast(
        data_sub, `.id` ~col_name, value.var = "value"
      )
      dt_element <- data_sub_wide[, `.id` := NULL]

      task_info <- task_info[, .SD, .SDcols = !names(task_info) == element]
      task_info = cbind(task_info, dt_element)
    }
  }
  task_info <- data.table::as.data.table(task_info)

  if (!is.null(tracks)) {
    task_info = task_info[track %in% tracks]
  }

  return(task_info)
}

#' Get patient info
#'
#' @param patient_id String formatted like "patient_id@@organisation_id".
#' @param base_patient_url API endpoint for patient data.
#' @inheritParams get_query_data
#'
#' @return Data.table containing patient data.
#'
#' @export
#' @import data.table
#' @importFrom jsonlite fromJSON
get_patient_info <- function(
  patient_id,
  access_token,
  base_patient_url,
  language = "nl",
  check_ssl = TRUE,
  debug = FALSE
) {

  patient_url = paste0(base_patient_url, patient_id)
  patient_info <- get_query_data(
    url = patient_url,
    token = access_token,
    output_type = "text",
    language = language,
    check_ssl = check_ssl
  )

  if (is.null(patient_info)) {
    return(NULL)
  }

  patient_info = data.table::as.data.table(t((unlist(jsonlite::fromJSON(
    patient_info
  )))))

  return(patient_info)
}

# TODO: should this return organisations or patient numbers?

#' Get organisation (for given patient)
#'
#' @param patient_number Patient number, without the organisation.
#' @param organisation_id Organisation identifier.
#' @param base_organisation_url API endpoint for organisation data.
#' @inheritParams get_query_data
#'
#' @return Data.table containing organisation data.
#'
#' @export
#' @import data.table
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
get_organisations <- function(
  patient_number,
  organisation_id,
  access_token,
  base_organisation_url,
  check_ssl = TRUE,
  debug = FALSE
) {

  organisation_url <- glue::glue("{base_organisation_url}{patient_number}/{organisation_id}")
  dt_organisations <- get_query_data(
    url = organisation_url,
    token = access_token,
    output_type = "text",
    check_ssl = check_ssl,
    debug = debug
  )

  dt_organisations <- data.table::as.data.table(jsonlite::fromJSON(
    dt_organisations
  ))
  return(dt_organisations)
}

#' Get responses
#'
#' @param task_ids Vector of task id's (same as token id's) to retrieve the
#'   responses from.
#' @param base_response_url API endpoint for response data.
#' @inheritParams get_query_data
#'
#' @return Data.table containing response data.
#'
#' @export
#' @import data.table
#' @importFrom glue glue
get_responses <- function(
  task_ids,
  access_token,
  base_response_url,
  language = "nl",
  check_ssl = TRUE,
  debug = FALSE
) {

  n_tasks <- length(task_ids)
  response_url <- glue::glue(
    "{base_response_url}?id=[{paste0(task_ids, collapse = ',')}]&per_page=",
    "{n_tasks}"
  )

  tasks <- get_query_data(
    url = response_url,
    token = access_token,
    output_type = "parsed",
    language = language,
    check_ssl = check_ssl,
    debug = debug
  )

  responses <- lapply(tasks, function(task) {

    if (task$status != "completed") {
      if (debug) {warning(glue::glue("Task {task$id} has no data"))}
      return(NULL)
    }

    # Flatten nested item data
    item_info <- lapply(task$item, function(item) {
      info <- as.list(unlist(item))
      return(info)
    })
    item_info <- data.table::rbindlist(item_info, use.names = TRUE, fill = TRUE)

    #task = task[-which(names(task) %in% "item")]
    task = task[!grepl("item", names(task))]

    # Create data.table from all elements which are not a list
    index_lists <- unname(which(sapply(task, is.list) == TRUE))
    if (length(index_lists) == 0) {
      dt <- data.table::as.data.table(task)
    } else {
      dt = data.table::as.data.table(task[-index_lists])
    }

    dt = cbind(dt, item_info)

    return(dt)

  })

  responses <- data.table::rbindlist(responses, use.names = TRUE, fill = TRUE)

  return(responses)

}

# TODO: what is the difference between access_token and old_token?

#' Get new token
#'
#' @param access_token API access token.
#' @param old_token Expired API access token.
#' @param api_info List with api information.
#' @param check_ssl Whether to check the SSL certificate or allow insecure
#'   connections.
#' @param debug Whether to enable debugging messages.
#'
#' @return New API access token.
#'
#' @export
#' @importFrom httr add_headers config content PATCH
get_new_token <- function(
  access_token,
  old_token,
  api_info,
  check_ssl = TRUE
) {

  new_token_url <- paste0(api_info$new_token_url, old_token)

  res <- httr::PATCH(
    new_token_url,
    body = list(comment = "Update token"),
    httr::add_headers(Authorization = paste("Bearer", access_token)),
    encode = "json",
    config = httr::config(
      ssl_verifypeer = check_ssl,
      http_version = 2
    )
  )

  if (res$status_code != "201") {
    return(NULL)
  }
  return(httr::content(res)$replacement_token)
}
