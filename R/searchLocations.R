#' @title Search Location IDs
#' @description This function takes searches for site IDs that contain the value provided. It is useful for identifying the correct site names. Note that if you use the graph interface you will need to select '[No more]' when you have finished selecting sites.
#'
#' @importFrom httr content GET
#' @importFrom jsonlite fromJSON
#'
#' @param searchterm (optional) text that will be used to find site IDs that contain this value. If no search term is provided all site IDs will be returned.
#' @param searchbyID (optional) boolean (TRUE/FALSE) If FALSE will use the search term on the name.  If this argument is TRUE then the search term will be used on the identifier.  The default is FALSE.
#' @param folder (optional) character (either one or multiple) that will filter the locations to only those that contain the term provided in the folder name.  If multiple are provided the function will return all locations that contain any of the search terms.
#' @param folderOR (optional) boolean (TRUE/FALSE) that by default (FALSE) means that multiple inputs to the argument folder will be all required in any output (AND).  If TRUE this will mean that matching any of the inputs to the argument folder will be enough to be included in the result (OR).
#' @param graphic.interface (optional) boolean (TRUE/FALSE) If TRUE it will use a graphic box to ask the user which datasets to include starting with selecting the primary folder. The default is FALSE which does not impact the function.
#'
#'
#' @examples
#' Returns all locations
#' searchLocations()
#'
#' Returning all locations that contain "AK" in the folder name
#' searchLocations(folder = "AK")
#'
#' Using the graphic interface to select locations
#' searchLocations(graphic.interface = TRUE)
#'
#' Using the graphic interface to select locations but only looking in the folders that contain "Alaska"
#' searchLocations(folder = "Alaska", graphic.interface = TRUE)
#'
#' The same as the previous line of code except it gives the site identifiers instead of the names.
#' searchLocations(folder = "AK", graphic.interface = TRUE, searchbyID = TRUE)
#'
#' Selecting all sites that include "BELA" and using the graphic interface to select more sites
#' searchLocations("BELA", graphic.interface = "TRUE")
#'
#' @return Returns a data frame with two columns, the name and the identifier of each site
#'
#' @export


searchLocations <- function(searchterm, searchbyID=FALSE, folder, folderOR=FALSE, graphic.interface=FALSE){

  if(!exists("publishapiurl")){
    publishapiurl <<- "https://aquarius.nps.gov/aquarius/Publish/v2"
  }

  locdata <- GET(paste0(publishapiurl,'/GetLocationDescriptionList',sep=''))

  ## Checking to see if we can find the server.
  ## Making the msg object start blank
  msg <- ""
  ## Getting a table of locations in a way that will handle the error if it cannot find the server.
  tryCatch({alllocs <- fromJSON(content(locdata,"text",encoding = "UTF-8"))$LocationDescriptions},
           error = function(e){msg <<- "Failed to connect"})
  ## Did the location download fail?
  if(msg=="Failed to connect"){
    stop("Unable to connect. Try AccessAquarius()")
  }

  if(!missing(folder)){
    if(folderOR){
      row <- FALSE
      for(i in folder){
        row <- row|grepl(i,alllocs$PrimaryFolder)
      }
    } else {
      row <- TRUE
      for(i in folder){
        row <- row & grepl(i,alllocs$PrimaryFolder)
      }
    }
    alllocs <- alllocs[row,]
  }
  if(graphic.interface==TRUE){
    ## Asking the user which paths to find their data under
    selecteddata <- menu(paste(unique(alllocs$PrimaryFolder)),"Which data groupings?",graphics = TRUE)
    ## Subsetting the data down to the selected to the user's selection
    scpnlocs <- alllocs[grepl(paste(unique(alllocs$PrimaryFolder)[selecteddata]),alllocs$PrimaryFolder),]

    if(searchbyID==TRUE){
      ## Creating a list of the site names that match the user's selection
      PossibleSites <- paste(scpnlocs$Identifier[order(scpnlocs$Identifier)])
    } else {
      PossibleSites <- paste(scpnlocs$Name[order(scpnlocs$Name)])
    }


    sites <- multipleselectionmenu(unique(PossibleSites),"Which sites? (select one at a time)")

    if(searchbyID==TRUE){
      sites <- alllocs[alllocs$Identifier %in% sites, c("Name", "Identifier", "PrimaryFolder")]
    } else {
      sites <- alllocs[alllocs$Name %in% sites, c("Name", "Identifier", "PrimaryFolder")]
    }
  }


  ## if missing the search term argument
  if(missing(searchterm)){
    if(graphic.interface==TRUE){
      return(sites)
    } else {
      return(alllocs[,c("Name","Identifier","PrimaryFolder")])
    }
    ## return all site names or IDs
  } else {
    if(searchbyID==FALSE){
      matchedlocs <- alllocs[grepl(searchterm,alllocs$Name), c("Name", "Identifier", "PrimaryFolder") ]
    } else {
      matchedlocs <- alllocs[grepl(searchterm,alllocs$Identifier), c("Name", "Identifier", "PrimaryFolder")]
    }

    if(graphic.interface==TRUE){
      matchedlocs <- rbind(matchedlocs,sites)
    }

    if(nrow(matchedlocs)==0){
      if(missing(folder)){
        ## Returning a message explaining that no site IDs have the search term
        return(paste0("No locations contain '", searchterm,"'"))
      } else {
        return(paste0("No locations contain '", searchterm,"' with '",folder,"' in the folder name."))
      }

    } else {
      ## If there are site IDs that contain the search term, returning those names.
      return(matchedlocs)
    }
  }

}
