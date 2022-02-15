# API functions

# Get api info
get_api_info <- function(environment) {
  # get list with api_info
  #
  # Args:
  #  environment: string with production/acceptance/testing
  #
  # Returns:
  #  list with information for the api

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

  api_urls <- config::get("gt", file = "inst/extdata/urls.yml")
  dashboard_urls <- list(
    base = Sys.getenv("APP_BASE_URL")
  )

  full_urls <- paste0(api_info$base_url, api_urls)
  names(full_urls) <- names(api_urls)

  # TODO: why vector of these 3, are the last 2 even used?
  api_info <- c(api_info, full_urls, dashboard_urls)

  return(api_info)
}

# Get access token
get_access_token <- function(api_info, check_ssl = TRUE, debug = FALSE) {
  # get access token from API which can be used to substract more information from the API
  #
  # Args:
  #  api_info: list containing information for the api
  #  check_ssl: boolean if the certificate should be checked with the call
  #  debug: boolean if the debug messages should be saved
  #
  # Returns:
  #  data.table containing access token

  # Get Access token ------
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

# Get query data
get_query_data <- function(
  url,
  token,
  output_type = "parsed",
  language = "nl",
  check_ssl = TRUE,
  debug = FALSE
) {
  # Gives back data from the api. based on a request
  #
  # Args:
  #   url: a string with the url to get the data from
  #   token: access token which is used for authentication
  #
  # Returns:
  #   list with data from the request

  #TODO: Use trycatch in case something is wrong
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

# Get careplan info
get_careplan_info <- function(
  patient_id,
  access_token,
  base_careplan_url,
  language = "nl",
  check_ssl = TRUE,
  debug = FALSE,
  tracks = NULL
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
  if (!is.null(tracks)) {
    dt_careplan = dt_careplan[title %in% tracks]
  }

  return(dt_careplan)

}

# Get task info
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

  task_info <- jsonlite::fromJSON(task_info, flatten = TRUE)

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
      nested_data = nested_data[
        , (change_cols) := lapply(.SD, na.rm = TRUE, as.character),
        .SDcols = change_cols
      ]
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

      task_info = task_info[-which(names(task_info) == element)]
      task_info = cbind(task_info, dt_element)
    }
  }
  task_info <- data.table::as.data.table(task_info)

  if (!is.null(tracks)) {
    task_info = task_info[track %in% tracks]
  }

  return(task_info)
}

# Get patient info
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

# Get organisation (for given patient)
# TODO: should this return organisations or patient numbers?
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

  dt_organisations <- jsonlite::fromJSON(dt_organisations)
  dt_organisations = data.table::as.data.table(dt_organisations)
  return(dt_organisations)
}

# Get responses
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

# Get new token
# TODO: what is the difference between access_token and old_token?
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
  return(content(res)$replacement_token)
}
