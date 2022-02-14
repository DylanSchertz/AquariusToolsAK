#' @title Search Dataset IDs
#' @description This function takes searches a specific site for dataset IDs that contain the value provided. It is useful for identifying the correct search terms to find the right datasets names.
#'
#' @importFrom httr content GET
#' @importFrom jsonlite fromJSON
#'
#' @param site Name of the site to search for datasets.
#' @param searchterm (optional) text that will be used to find dataset IDs that contain this value if no search term is provided all dataset IDs will be returned.  Note that this term is case sensitive.
#' @param removeterms (optional) text that will be used to limit the returned dataset IDs. Any IDs that contain this term will not be returned.  Note that this term is case sensitive.
#'
#' @examples
#' Pulling up all the dataset IDs at the site "DENA-023"
#' searchDatasetIDs("DENA-023")
#'
#' Pulling up the dataset IDs that contain "Depth@" at the site "YUCH-005"
#' searchDatasetIDs("YUCH-005", "Depth@")
#'
#' Pulling up the dataset IDs that contain "pH@" but does not contain "synop" at site "BELA-088"
#' searchDatasetIDs("BELA-088",searchterm = "pH@", removeterms = "synop")
#'
#' Pulling up the dataset IDs at the site "KOVA-085" that do not contain "ynop"
#' searchDatasetIDs("KOVA-085",  removeterms = "ynop")
#'
#' @return An object of type character with a value for each of the dataset IDs at that site.
#'
#' @export



########################################################
#### Searching for Dataset IDs - searchLocations() ####
########################################################
## This function takes searches a specific site for dataset IDs that contain the value provided.
## It is useful for identifying the correct search terms to find the right datasets names.
##
## searchDatasetIDs(site, searchterm, removeterms)
##
## - the site argument is the name of the site to search for datasets.
## - the searchterm argument (optional) is the text that will be used to find dataset IDs that contain this value.
##   if no search term is provided all dataset IDs will be returned.  Note that this term is case sensitive.
## - the removeterms argument (optional) is the text that will be used to limit the returned dataset IDs. Any IDs
##   that contain this term will not be returned.  Note that this term is case sensitive.


searchDatasetIDs <- function(site, searchterm, removeterms){

  if(!exists("publishapiurl")){
    publishapiurl <<- "https://aquarius.nps.gov/aquarius/Publish/v2"
  }

  getlocds <- GET(paste0(publishapiurl,'/GetTimeSeriesDescriptionList?LocationIdentifier=',site,sep=''))

  ## Checking to see if we can find the server.
  ## Making the msg object start blank
  msg <- ""
  ## Getting a table of locations in a way that will handle the error if it cannot find the server.
  tryCatch({locsds <- fromJSON(content(getlocds,"text"))$TimeSeriesDescriptions},
           error = function(e){msg <<- "Failed to connect"})
  ## Did the location download fail?
  if(msg=="Failed to connect"){
    stop("Unable to connect. Try AccessAquarius()")
  }




  if(missing(searchterm)){

    if(!missing(removeterms)){
      return(locsds$Identifier[!grepl(removeterms, locsds$Identifier)])
    } else {
      ## Returning all dataset IDs
      return(locsds$Identifier)
    }

  } else {
    matcheddatasets <- locsds$Identifier[grepl(searchterm,locsds$Identifier)]

    if(!missing(removeterms)){
      matcheddatasets <- matcheddatasets[!grepl(removeterms, matcheddatasets)]
    }

    if(length(matcheddatasets)==0){
      ## Returning a message explaining that no dataset IDs contain the search term
      return(paste0("No Dataset IDs contain '",searchterm,"' at ",site))
    } else {
      ## If there are datasets that contain the search term, returning those names.
      return(matcheddatasets)
    }
  }
}
