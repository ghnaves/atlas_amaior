#' Handle API Responses
#'
#' Validates the API response and extracts its content.
#'
#' @param response An `httr` response object.
#'
#' @return The content of the response as a character string.
#' @keywords internal
# Função para lidar com a resposta usando RCurl
handle_response <- function(response) {
  if (response == "") {
    stop("Empty response received.")
  }
  return(response)
}
#' Download Pages from WPP API
#'
#' Downloads paginated data from the WPP API and combines it into a single data frame.
#'
#' @param target A string containing the URL of the API's first page.
#' @param headers A list of HTTP headers, typically including authorization.
#'
#' @return A data frame containing the combined data from all pages.
#' @export
#'
#' @examples
#' headers <- c("Authorization" = Sys.getenv("WPP_ONU_BEARER"))
#' base_url <- "https://population.un.org/dataportalapi/api/v1"
#' target <- paste0(base_url, "/indicators/")
#' indicators <- download_pages_WPP(target, headers)
#' print(indicators$name)
download_pages_WPP <- function(target, headers) {
  ext_json <- list(nextPage = target)
  ext <- NULL
  while (!is.null(ext_json$nextPage)) {
    ext_json$nextPage = gsub("http://10.208.38.29/",
                             "https://population.un.org/dataportalapi/",
                             ext_json$nextPage)
    response <- tryCatch(
      {
        RCurl::getURL(
          ext_json$nextPage,
          .opts = list(httpheader = headers, followlocation = TRUE)
        )
      },
      error = function(e) {
        stop(paste("Request failed:", conditionMessage(e)))
      }
    )
    json_text <- handle_response(response)
    ext_json <- tryCatch(
      {
        jsonlite::fromJSON(json_text)
      },
      error = function(e) {
        stop(paste("Error parsing JSON:", conditionMessage(e)))
      }
    )
    if (is.null(ext)) {
      ext = ext_json$data
    } else {
      ext = rbind(ext, ext_json$data)
    }
  }
  return(ext)
}
