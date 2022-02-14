#' @title Access Aquarius
#' @description Establishes connection to Aquarius server for future functions to search and download data. This function require NPS network access (or VPN).
#'
#' @importFrom httr parse_url GET POST stop_for_status
#' @importFrom utils install.packages
#'
#' @param hostname Defaults to "https://aquarius.nps.gov/aquarius" if the server changes this may leave the option for continued use.
#' @param username Defaults to "aqreadonly", which should work for downloading.  This parameter is retained in the event you need to use your specific log in.
#' @param password Defaults to "aqreadonly", which should work for downloading.  This parameter is retained in the event you need to use your specific log in.
#'
#' @examples
#' AccessAquarius()
#'
#' @export


AccessAquarius <- function(hostname, username, password){

  if(missing(hostname)){
    hostname <- "https://aquarius.nps.gov/aquarius"
  }
  if(missing(username)){
    username <- "aqreadonly"
  }
  if(missing(password)){
    password <- "aqreadonly"
  }

  packagelist <- c("httr", "jsonlite", "utils")

  ## Looping through the name of each package
  for(i in packagelist){
    if(suppressWarnings(!require(i, quietly=TRUE,character.only = TRUE))){
      ## if not installing and loading each package.
      install.packages(i)
      library(i,character.only = TRUE)
    }
  }


  #utils::globalVariables(c("publishUri", "acquisitionUri", "provisioningUri", "publishapiurl"))


  # Support schemeless and schemed hosts for convenience
  prefix <- "http://"
  if (startsWith(hostname, "http://") || startsWith(hostname, "https://")) {
    url <- parse_url(hostname)
    hostname <- paste0(url$scheme, "://", url$hostname)
    prefix <- ""
  }

  ## Checking to see if we can find the server.
  ## Making the msg object start blank
  msg <- ""
  tryCatch({ r <- httr::GET(paste0(prefix, hostname, "/AQUARIUS/apps/v1/version")) },
           error = function(e){msg <<- "Failed to connect"})
  ## Did the connection fail?
  if(msg=="Failed to connect"){
    stop("Unable to connect. Check network connection/VPN.")
  }

  # Grab the version of the AQTS server

  stop_for_status(r, "detecting AQTS version")

  # Compose the base URI for all API endpoints
  publishUri <<- paste0(prefix, hostname, "/AQUARIUS/Publish/v2")
  acquisitionUri <<- paste0(prefix, hostname, "/AQUARIUS/Acquisition/v2")
  provisioningUri <<- paste0(prefix, hostname, "/AQUARIUS/Provisioning/v1")
  publishapiurl <<- "https://aquarius.nps.gov/aquarius/Publish/v2"

  ## Putting the username and password together
  credentials <- list(Username = username, EncryptedPassword = password)


  # Authenticate via the preferred endpoint
  r <- httr::POST(paste0(publishUri, "/session"), body = credentials, encode = "json")

  httr::stop_for_status(r, "authenticate with AQTS")

}


