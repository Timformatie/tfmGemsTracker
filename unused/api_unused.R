
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

# Get settings
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

# Change execution period
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

# Get all data
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

# Add questionnaire
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

# Get return url
getReturnURL <- function(baseURL, `patient-nr`, `organisation-id`, env){
  #TODO: session$clientData$url_hostname & session$clientData$url_pathname?

  url.output <- paste0(baseURL, "/respondent/r-dashboard/id1/", `patient-nr`,"/id2/", `organisation-id`, "/redirect/1")

  url.encoded <- base64_encode(url.output)
  return(url.encoded)
}

