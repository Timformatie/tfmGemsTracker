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

# Decode url key
decode_url_key <- function(hash_key, url_key, debug = FALSE) {
  # decode the key in the url to get a password for the access token
  #
  # Args:
  #  hashKey: key that is given once to decrypt
  #  urlKey: code in the url
  #
  # Returns:
  #  string containing the password which can be used to get an access token

  # urlKey =
  # decode the url key
  data_parameter <- URLdecode(url_key)

  data_decoded <- rawToChar(openssl::base64_decode(data_parameter))
  if (debug) {
    logr::log_print(
      glue::glue("data_decoded is: {data_decoded}"), console = FALSE
    )
  }

  n_last <- 1 # Specify number of characters to extract
  last_char <- substr(data_decoded, nchar(data_decoded) - n_last + 1, nchar(data_decoded)) # Extract last three characters

  if (last_char != "}") {
    if (last_char != '"') {
      data_decoded = paste0(data_decoded, '"}')
    } else {
      data_decoded = paste0(data_decoded, "}")
    }
  }
  data_decoded = paste0(data_decoded, collapse = "")

  if (debug) {
    logr::log_print(
      paste0("data_decoded after paste is: ", data_decoded), console = FALSE
    )
  }

  data_package <- fromJSON(data_decoded);
  iv <- openssl::base64_decode(data_package[["iv"]])
  data <- openssl::base64_decode(data_package[["data"]])
  key <- openssl::base64_decode(hash_key)

  answer <- openssl::aes_cbc_decrypt(
    data,key = key, iv = iv
  )

  password <- rawToChar(answer)

  if ("group" %in% names(data_package)) {
    group <- data_package[["group"]]
  } else {
    group <- ""
  }

  return(list(
    password = paste0("key:", password),
    username = data_package[["user"]],
    group = group
  ))
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
  responseURL <- paste0(basicResponseURL, "?id=[", paste0(task_ids, collapse = ","), "]&per_page=", nTaskIDs)
  res <- getQueryData(url = responseURL, token = access.token, outputType = "parsed", language = language, bool.checkSSLcert = bool.checkSSLcert, debug = debug)

  responses <- lapply(seq_len(n_tasks), function(id){
    tmp <- res[[id]]

    if (tmp$status == "completed"){
      lst.imported <-tmp

      itemInfo <- lapply(seq_len(length(lst.imported$item)), function(x){
        tmp <- as.list(unlist(lst.imported$item[[x]]))
        return(tmp)
      })
      itemInfo <- rbindlist(itemInfo, use.names = T, fill = T)
      lst.imported = lst.imported[-which(names(lst.imported) %in% "item")]

      index.lists <- unname(which(sapply(lst.imported, is.list) == T))

      # create data.table from all elements which are not a list
      if (length(index.lists) == 0) {
        dt <- as.data.table(lst.imported)
      } else {
        dt = as.data.table(lst.imported[-index.lists])
      }

      dt = cbind(dt, itemInfo)

      return(dt)
    } else {
      if (debug) {
        warning(paste0("Task ", task_ids[id], " has no data"))
      }
      return(NULL)
    }
  })
  responses <- rbindlist(responses, use.names = T, fill = T)

  return(responses)
}




#######

getSettings <- function(env.gems, settings) {
  mapping <- settings$`surveys-mapping`
  mapping = mapping[, survey.id := .SD, .SDcols = paste0("survey.id.", env.gems)]
  mapping = mapping[, .(db.survey.id, survey.id)]

  settingNames <- names(settings)
  settings <- lapply(settingNames, function(x) {
    tmp.settings <- settings[[x]]
    if (x == "survey-mapping") {
      tmp.settings = tmp.settings[, survey.id := .SD, .SDcols = paste0("survey.id.", env.gems)]
    } else {

      if ("db.survey.id" %in% names(tmp.settings) ) {
        if ("survey.id" %in% colnames(tmp.settings)){
          tmp.settings = tmp.settings[, survey.id := NULL]
        }

        tmp.settings = merge(tmp.settings,
                             mapping,
                             by.x = "db.survey.id",
                             by.y = "db.survey.id")
      }
    }
    return(tmp.settings)
  })
  names(settings) <- settingNames

  return(settings)
}

getNewToken <- function(access.token, old.token, api_info, bool.checkSSLcert = T) {
  require(httr)

  newTokenURL <- paste0(api_info$new.tokenurl, old.token)

  res <- PATCH(newTokenURL,
               body = list(comment = "oeps, deze was fout"),
               add_headers(Authorization = paste("Bearer", access.token, sep = " ")),
               encode = "json",
               config = httr::config(ssl_verifypeer = bool.checkSSLcert,
                                     http_version = 2)
  )

  if (res$status_code == "201") {
    return(content(res)$replacement_token)
  } else {
    return(NULL)
  }
}

changeExecutionPeriod <- function(access.token, token, api_info, bool.checkSSLcert = T, jsonData) {
  require(httr)

  newTokenURL <- paste0(api_info$taskurl.clean, token)

  res <- PATCH(newTokenURL,
               body = jsonData,
               add_headers(Authorization = paste("Bearer", access.token, sep = " ")),
               encode = "json",
               config = httr::config(ssl_verifypeer = bool.checkSSLcert,
                                     http_version = 2)
  )

  if (res$status_code == "201") {
    return(message("Patch is correct"))
  } else {
    return(message(paste0("Patch is incorrect? status code: ", res$status_code)))
  }
}

getAllData <- function(patientID,
                       access.token,
                       basicPatientURL,
                       basicTaskURL,
                       tracks = NULL,
                       basicResponseURL,
                       basicCarepathURL = NULL,
                       language = "nl",
                       bool.checkSSLcert = T) {
  # get data from one patient
  #
  # Args:
  #   patientID: string with patient id @organisation
  #   access.token: string with the access token
  #   basicPatientURL: string with url to get patient data
  #   basicTaskURL: string with url to get task data
  #   basicReponseURL: string with url to get response data
  #
  # Returns:
  #   list with data in the categories patientInfo, task_info and responses

  require(jsonlite)

  data <- list()

  # patientInfo = fromJSON(patientInfo) %>% as.data.frame
  data[["patientInfo"]] <- getPatientInfo(patientID, access.token, basicPatientURL, language = language, bool.checkSSLcert = bool.checkSSLcert)

  task_info <- gettask_info(patientID, access.token, basicTaskURL = paste0(basicTaskURL, patient), language = language, bool.checkSSLcert = bool.checkSSLcert, tracks = tracks)
  data[["task_info"]] <- task_info

  if ("status" %in% colnames(task_info)) {
    taskIDs <- unique(task_info[status == "completed", id])

    system.time(responses <- getResponses(taskIDs,
                                          access.token,
                                          basicResponseURL, language = "nl", bool.checkSSLcert = T))

    #TODO: if async http2 works, the responses should be tranformed using getResponses
    # #test
    # library(curl)
    # # taskIDs <- rep(taskIDs,1)
    # pool <- new_pool(host_con = 100)
    # cb <- function(req){cat("done:", req$url, ": HTTP:", req$status, "\n")}
    # bodies <- list()
    #
    # done_function <- function(x) {
    #
    #   bodies[[length(bodies) + 1]] <<- fromJSON(rawToChar(x$content))
    # }
    #
    # req <- lapply(taskIDs, function(i){
    #   url = paste0(basicResponseURL, i)
    #   h <- new_handle(url = url) %>%
    #     handle_setopt(http_version = 2, verbose = F) %>%
    #     handle_setheaders("Authorization" = paste("Bearer", access.token, sep = " "),
    #                       "Content-Type" = "application/json")
    #
    #   # bla <- curl_fetch_memory(url, handle = h)
    #   # bla <- fromJSON(rawToChar(bla$content))
    #   # multi_add(handle = h, done = cb, pool = pool)
    #   multi_add(handle = h, done = done_function, pool = pool)
    #
    # })
    #
    # multi_set(pool = pool, multiplex = T, host_con = 6)
    # start.time2 <- Sys.time()
    # out <- multi_run(pool = pool)
    # end.time2 <- Sys.time()
    # print(end.time2-start.time2)

    # library(crul)
    # start.time1 <- Sys.time()
    # cc <- Async$new(
    #   urls = paste0(basicResponseURL, taskIDs),
    #   headers = list("Authorization" = paste("Bearer", access.token, sep = " ")),
    #   opts = list(
    #     verbose = TRUE
    #   )
    # )
    # res <- cc$get()
    # end.time1 <- Sys.time()
    # print(end.time1-start.time1)
    #

  } else {
    responses <- NULL
  }
  data[["responses"]] <- responses

  if (!is.null(basicCarepathURL)) {
    data[["care-plan"]]  <- getCareplanInfo(patientID, access.token, basicCarepathURL, language = "nl", bool.checkSSLcert = bool.checkSSLcert, tracks = tracks)
  }
  return(data)
}

addQuestionnaire <- function(access.token, api_info, bool.checkSSLcert = T,
                             respondenTrackId, surveyId, roundDescription, validFrom, validUntil, roundOrder = NULL){
  require(httr)

  url <- paste0(api_info$insertQuestionnaire)

  jsonData <- list(respondentTrackId = respondenTrackId,
                   surveyId = surveyId,
                   roundDescription = roundDescription,
                   validFrom = validFrom,
                   validUntil = validUntil,
                   roundOrder = roundOrder)

  res <- POST(url,
              body = jsonData,
              add_headers(Authorization = paste("Bearer", access.token, sep = " ")),
              encode = "json",
              config = httr::config(ssl_verifypeer = bool.checkSSLcert,
                                    http_version = 2)
  )

  if (res$status_code == "201") {
    return(message("Questionnaire correctly added"))
  } else {
    return(message(paste0("Not correct? Status code: ", res$status_code)))
  }

}

getReturnURL <- function(baseURL, `patient-nr`, `organisation-id`, env){
  #TODO: session$clientData$url_hostname & session$clientData$url_pathname?

  url.output <- paste0(baseURL, "/respondent/r-dashboard/id1/", `patient-nr`,"/id2/", `organisation-id`, "/redirect/1")

  url.encoded <- base64_encode(url.output)
  return(url.encoded)
}

