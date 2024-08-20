#' decode_url_key
#'
#' @description Decode the key in the url to get a password for the access
#'   token.
#'
#' @param hash_key Key that is given to decrypt
#' @param url_key Encrypted key retrieved from the url
#' @param gunzip Whether to gunzip the key after decoding.
#' @param debug Whether to enable debugging messages.
#'
#' @return String containing the encrypted password which can be used to get an
#'   access token
#'
#' @export
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#' @importFrom logr log_print
#' @importFrom openssl aes_cbc_decrypt base64_decode
#' @importFrom utils URLdecode
decode_url_key <- function(hash_key, url_key, gunzip = FALSE, debug = FALSE) {

  # Replace underscores and add padding if key is gzipped
  if (gunzip) {
    url_key <- chartr("-_", "+/", url_key)
    url_key <- paste0(url_key, strrep("=", (4 - nchar(url_key) %% 4) %% 4))
  }

  # Decode key
  key_raw <- openssl::base64_decode(URLdecode(url_key))

  # Unzip string if compressed
  if (gunzip) {
    key_raw <- memDecompress(key_raw, type = "gzip")
  }

  # Decode key
  key_decoded <- rawToChar(key_raw)

  if (debug) {
    logr::log_print(
      glue::glue("Decoded key is: {key_decoded}"), console = FALSE
    )
  }

  # Extract last n characters
  n_last <- 1
  last_char <- substr(
    key_decoded,
    nchar(key_decoded) - n_last + 1,
    nchar(key_decoded)
  )

  if (last_char != "}") {
    if (last_char != '"') {
      key_decoded = paste0(key_decoded, '"}')
    } else {
      key_decoded = paste0(key_decoded, "}")
    }
  }
  key_decoded = paste0(key_decoded, collapse = "")

  if (debug) {
    logr::log_print(
      glue::glue("Decoded key is: {key_decoded}"), console = FALSE
    )
  }

  # Extract and decode elements from key
  key_elements <- jsonlite::fromJSON(key_decoded)

  initialization_vector <- openssl::base64_decode(key_elements[["iv"]])
  data <- openssl::base64_decode(key_elements[["data"]])
  key <- openssl::base64_decode(hash_key)

  # Create password using the encryption key
  password_raw <- openssl::aes_cbc_decrypt(
    data, key = key, iv = initialization_vector
  )
  password <- rawToChar(password_raw)

  if (is.null(key_elements$group)) {
    key_elements[["group"]] <- ""
  }

  return(list(
    password = paste0("key:", password),
    username = key_elements[["user"]],
    group = key_elements[["group"]]
  ))

}
