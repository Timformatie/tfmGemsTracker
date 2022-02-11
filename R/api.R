# API functions

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

  message("De env variable PULSE_BASE_URL is: {api_info$base_url}")

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
    stop(glue::glue("Status code for access token is: {response$status_code}.",
                    "Something goes wrong with login details"))
  } else {
    message("Access token correctly obtained.")
  }
  access_token <- content(response)$access_token

  return(access_token)
}

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

getQueryData <- function(url, token, outputType = "parsed", language = "nl", bool.checkSSLcert = T,
                         debug = 0) {
  # Gives back data from the api. based on a request
  #
  # Args:
  #   url: a string with the url to get the data from
  #   token: access token which is used for authentication
  #
  # Returns:
  #   list with data from the request

  require(httr)

  #TODO: Use trycatch in case something is wrong
  if (debug == 1) {
    log_print(paste0("try to retrieve data from ", url), console = F)
  }
  # start.time <- Sys.time()
  req <- GET(url,
             add_headers(Authorization = paste("Bearer", token, sep = " "), `Accept-Language` = language),
             config = httr::config(ssl_verifypeer = bool.checkSSLcert,
                                   http_version = 2)
  )

  if (req$status_code == 200) {
    if (debug == 1) {
      log_print(paste0("data from url ", url, " correctly obtained"), console = F)
    }
    json.data <- content(req, as = outputType, encoding = "UTF-8")
    return(json.data)
  } else {
    if (debug == 1) {
      log_print(paste0("data from url ", url, " NOT correctly obtained"), console = F)
      log_print(paste0("Fout bij query: ", url, " met status code: ", req$status_code), console = F)
    }
    return("")
  }
  # end.time <- Sys.time()
  # print(paste0("De tijd om data op te halen was: ", end.time-start.time))
  #
}

getPatientInfo <- function(patientID, access.token, basicPatientURL, language = "nl", bool.checkSSLcert = T, debug = 0) {
  require(jsonlite)

  patientURL = paste0(basicPatientURL, patientID)
  patientInfo <- getQueryData(url = patientURL, token = access.token, outputType = "text", language = language, bool.checkSSLcert = bool.checkSSLcert)
  # for empty lists
  if (patientInfo != "") {
    patientInfo = as.data.table(t((unlist(fromJSON(patientInfo)))))

    return(patientInfo)
  } else {
    return(NULL)
  }
}

getOrganisation <- function(patient, organisation, access.token, basicOrganisationURL, bool.checkSSLcert, debug = 0) {
  url <- paste0(basicOrganisationURL, patient,"/", organisation)
  dt.organisations <- getQueryData(url = url, token = access.token, outputType = "text", bool.checkSSLcert = bool.checkSSLcert, debug = debug)

  dt.organisations <- fromJSON(dt.organisations)
  dt.organisations = as.data.table(dt.organisations)
  return(dt.organisations)
}

getCareplanInfo <- function(patientID, access.token, basicCarepathURL, language = "nl", bool.checkSSLcert = T, tracks = NULL, debug = 0) {
  carepathURL = paste0(basicCarepathURL, patientID) #, "&status=active"
  dt.careplan <- getQueryData(url = carepathURL, token = access.token, outputType = "text", bool.checkSSLcert = bool.checkSSLcert, debug = debug)

  if (!is.null(dt.careplan)) {
    if (dt.careplan != "") {
      dt.careplan <- fromJSON(dt.careplan)
      dt.careplan = as.data.table(dt.careplan)

      if (!is.null(tracks)) {
        dt.careplan = dt.careplan[title %in% tracks]
      }
    } else {
      dt.careplan <- NULL
    }
    return(dt.careplan)
  } else {
    return(NULL)
  }

}

getTaskInfo <- function(patientID, access.token, basicTaskURL, language = "nl", bool.checkSSLcert = T, tracks = NULL,
                        carePlan = NULL, debug = 0) {

  require(httr)
  taskURL <- basicTaskURL
  if (!is.null(carePlan)) {
    taskURL = paste0(taskURL, "&carePlan=[", paste0(carePlan, collapse = ","),"]")
  }
  taskInfo <- getQueryData(url = taskURL, token = access.token, outputType = "text", language = language, bool.checkSSLcert = bool.checkSSLcert,
                           debug = debug)

  if (taskInfo != "") {
    taskInfo <- fromJSON(taskInfo, flatten = T)

    lst.elements <-  names(taskInfo)[sapply(taskInfo, class) == "list"]
    if (length(lst.elements) > 0){
      for (element in lst.elements){
        # transform list elements to whole rows with multiple columns
        test <- rbindlist(taskInfo[[element]], idcol = T, use.names = T, fill = T)
        changeCols <- colnames(test)[!colnames(test) %in% c("type", ".id")]
        test = test[, (changeCols) := lapply(.SD,na.rm = T, as.character),.SDcols = changeCols]
        test2 = melt(test, id.vars = c(".id", "type"),
                     measure.vars = changeCols)

        test2 = test2[!is.na(value)][,count := .N, by = c("type", ".id")][, colName := ifelse(count > 1, paste0(type, ".", variable), type)]
        y = test2[,.(colName, value, `.id`)][order(`.id`)]
        z = dcast(y, `.id` ~colName, value.var = "value")
        dt.element <- z[, `.id` := NULL]

        taskInfo = taskInfo[-which(names(taskInfo) == element)]
        taskInfo = cbind(taskInfo, dt.element)
      }
    }
    taskInfo <- as.data.table(taskInfo)

    if (!is.null(tracks)) {
      taskInfo = taskInfo[track %in% tracks]
    }

  } else {
    taskInfo <- NULL
  }

  return(taskInfo)
}

getResponses <- function(taskIDs, access.token, basicResponseURL, language = "nl", bool.checkSSLcert = T,
                         debug = 0) {

  require(httr)

  nTaskIDs <- length(taskIDs)
  responseURL <- paste0(basicResponseURL, "?id=[", paste0(taskIDs, collapse = ","), "]&per_page=", nTaskIDs)
  res <- getQueryData(url = responseURL, token = access.token, outputType = "parsed", language = language, bool.checkSSLcert = bool.checkSSLcert, debug = debug)

  responses <- lapply(seq_len(nTaskIDs), function(id){
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
      if (debug == 1) {
        warning(paste0("Task ", taskIDs[id], " has no data"))
      }
      return(NULL)
    }
  })
  responses <- rbindlist(responses, use.names = T, fill = T)

  return(responses)
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
  #   list with data in the categories patientInfo, taskInfo and responses

  require(jsonlite)

  data <- list()

  # patientInfo = fromJSON(patientInfo) %>% as.data.frame
  data[["patientInfo"]] <- getPatientInfo(patientID, access.token, basicPatientURL, language = language, bool.checkSSLcert = bool.checkSSLcert)

  taskInfo <- getTaskInfo(patientID, access.token, basicTaskURL = paste0(basicTaskURL, patient), language = language, bool.checkSSLcert = bool.checkSSLcert, tracks = tracks)
  data[["taskInfo"]] <- taskInfo

  if ("status" %in% colnames(taskInfo)) {
    taskIDs <- unique(taskInfo[status == "completed", id])

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

decodeURLkey <- function(hashKey, urlKey, debug = 0) {
  # decode the key in the url to get a password for the access token
  #
  # Args:
  #  hashKey: key that is given once to decrypt
  #  urlKey: code in the url
  #
  # Returns:
  #  string containing the password which can be used to get an access token

  require(openssl)
  require(jsonlite)
  require(urltools)
  # urlKey =
  # decode the url key
  dataParameter <- URLdecode(urlKey)

  dataDecoded <- rawToChar(base64_decode(dataParameter))
  if (debug == 1) {
    log_print(paste0("dataDecoded is: ", dataDecoded), console = F)
  }
  n_last <- 1                                # Specify number of characters to extract
  lastChar <- substr(dataDecoded, nchar(dataDecoded) - n_last + 1, nchar(dataDecoded)) # Extract last three characters
  if (lastChar != "}") {
    if (lastChar != '"') {
      dataDecoded = paste0(dataDecoded, '"}')
    } else {
      dataDecoded = paste0(dataDecoded, "}")
    }
  }
  dataDecoded = paste0(dataDecoded, collapse = "")

  if (debug == 1) {
    log_print(paste0("dataDecoded after paste is: ", dataDecoded), console = F)
  }

  dataPackage <- fromJSON(dataDecoded);
  iv <- base64_decode(dataPackage[["iv"]])
  data <- base64_decode(dataPackage[["data"]])
  key <- base64_decode(hashKey)

  answer <- aes_cbc_decrypt(data,
                            key = key,
                            iv = iv)

  password <- rawToChar(answer)

  if ("group" %in% names(dataPackage)) {
    group = dataPackage[["group"]]
  } else {
    group <- ""
  }

  return(list(password = paste0("key:", password),
              username = dataPackage[["user"]],
              group = group))
}
