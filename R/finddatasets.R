#' @title Find Datasets
#' @description This function creates a table of dataset id values using lists of parameters, search terms, and sites.
#'
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#'
#' @param sites (optional) an object of type character with the exact name of each site you want to download in aquarius.  If left blank (and graphic interface is FALSE) 12 ARCN/CAKN shallow lake sites will be used.
#' @param parameters (optional) an object of type character with a clean output name for the given parameter If parameters are not provided, 19 parameters related to the ARCN/CAKN shallow lakes projects will be used.
#' @param searchterms is an object of type character with a short bit of text that is uniquely identifiable to that parameter and will be found in the correct dataset id regardless of site.  The order of these terms must correspond to the parameters. (The first parameter is paired with the first search term) If this argument is left blank then search terms relating to the 19 ARCN/CAKN shallow lakes sites will be used.
#' @param removeterms The removeterms argument (optional) is an object of type character which prevents any datasets from being included if they contain any of these terms.
#' @param graphic.interface (optional) a (boolean) TRUE/FALSE statement that determines whether or not to use the graphic interface to ask for additional sites. The graphic interface only allows selection of sites that contain "AK" or "Alaska" in the folder name.
#' @param duplicate.handling (optional) a boolean (TRUE/FALSE) statement. If FALSE the tool will skip assigning a dataset ID when there are mutliple datasets that match the search terms for a given site.  If TRUE it will use a graphic box to ask the user which dataset to use each time there are multiple options.
#'
#' @return A data frame of dataset IDs by site and parameter. This data frame can be used with the downloadaquariusdata
#'
#' @export
#'
#' @examples
#' # Finding the dataset IDs for the default ARCN/CAKN parameters at the site "BELA-088"
#' ParameterTable <- finddatasets("BELA-088", removeterms = "synoptic")
#'
#' # Using the graphic interface to select more sites
#' ParameterTable <- finddatasets("BELA-088",graphic.interface = TRUE)
#'
#' # Using the default site list and remove term "synoptic"
#' ParameterTable <- finddatasets(removeterms = "synoptic")
#'
#' # Using the duplicate handler to prompt the user when multiple dataset IDs exist matching the search term
#' ParameterTable <- finddatasets("DENA-023", duplicate.handling = TRUE)
#'
#' # Downloading some example weather station data. Specifying sites, parameters, search terms, and remove terms.
#' ParameterTable <- finddatasets(sites=c("BELA_DVLA2", "YUCH_CLCA2","KELA2","KAVA2","LMHA2","CZOA2"),
#'                               parameters = c("WindSpeed","Wind Direction","SolarRadiation","RelativeHumidity", "AirTemp"),
#'                               searchterms = c("WSM","WDD","SRW","RHP","AT"),
#'                               removeterms = c("WSMP","WDDP","Therm","AT1","HMP","109"),
#'                               duplicate.handling = TRUE)
#'


finddatasets <- function(sites, parameters, searchterms, removeterms, graphic.interface=FALSE, duplicate.handling=FALSE){
  ## Add connection checking

  ## Code to run the graphic interface
  if(graphic.interface==TRUE){

    locdata <- GET(paste0(publishapiurl,'/GetLocationDescriptionList',sep=''))
    alllocs <- fromJSON(content(locdata,"text"))$LocationDescriptions

    ## Finding the paths that are from the National Park Services Arctic Network
    datafamily <- alllocs[grepl("AK",alllocs$PrimaryFolder)|grepl("Alaska",alllocs$PrimaryFolder),]
    ## Asking the user which paths to find their data under
    selecteddata <- menu(paste(unique(datafamily$PrimaryFolder)),"Which data groupings?",graphics = TRUE)

    if(selecteddata==0 & missing(sites)){
      stop("No sites selected")
    }
    if(selecteddata!=0){
      ## Subsetting the data down to the selected to the user's selection
      scpnlocs <- alllocs[grepl(paste(unique(datafamily$PrimaryFolder)[selecteddata]),alllocs$PrimaryFolder),]

      ## Creating a list of the site names that match the user's selection
      PossibleSites <- paste(scpnlocs$Identifier[order(scpnlocs$Identifier)])

      if(missing(sites)==TRUE){
        sites <- multipleselectionmenu(PossibleSites,"Which sites? (select one at a time)")
        if(length(sites)==0){
          stop("No sites selected")
        }
      }else {
        sites <- multipleselectionmenu(PossibleSites,"Which sites? (select one at a time)",preselected = sites)
      }
    }
  } else {
    ## If the graphic interface should not be run
    ## Was a list of sites provided?
    if(missing(sites)==TRUE){
      ## if no sites were provided, use the default site list below (the shallow lakes continuous monitoring sites)
      sites <- c("BELA-088","BELA-108","DENA-018","DENA-023","KOVA-085","KOVA-086","NOAT-052","NOAT-107","WRST-005","WRST-016","YUCH-004","YUCH-005")
    }
  }
  ## Was a list of parameters provided?
  if(missing(parameters)){
    ## If no paramters were provided use the default list below.
    parameters <- c("LakeDepth", "SedimentTemperatureC","DissolvedOxygen(ODOconc)","pH","SpecificConductance(SpCond)","Temperature","Turbidity",
                    "WaterTemperature0.25m","WaterTemperature0.50m","WaterTemperature1.0m","WaterTemperature1.5m","WaterTemperature2.0m","WaterTemperature3.0m",
                    "LightIntensity0.25","LightIntensity0.50","LightIntensity1.0","LightIntensity1.5","LightIntensity2.0","LightIntensity3.0"
    )
  }
  ## Was a list of search terms provided?
  if(missing(searchterms)){
    ## if no search terms were given uses the default list below.
    searchterms <- c("Pressure_LakeDepth@", "Pressure_BottomTemp@", "DOconc@", "Sonde_pH@", "Sonde_SpCond@", "Sonde_Temp@", "_Turb@", "_Temp1@",
                     "_Temp2@", "_Temp3@", "_Temp4@", "_Temp5@", "_Temp6@", "_Light1@", "_Light2@", "_Light3@", "_Light4@", "_Light5@","_Light6@")
  }

  ## Making a data frame out of the parameters and search terms
  ParameterMatch <- data.frame(ParName=parameters,SearchTerm=searchterms)

  for(j in sites){
    ## Setting the path to download the dataset table from that site.
    getlocds <- GET(paste0(publishapiurl,'/GetTimeSeriesDescriptionList?LocationIdentifier=',j,sep=''))
    ## Downloading a table of datasets from that site.
    locsds <- fromJSON(content(getlocds,"text"))$TimeSeriesDescriptions
    ## Looping through each parameter.
    for(i in 1:nrow(ParameterMatch)){
      ## Make a list of all of the possible data sets.
      PossibleDatasets <- locsds$Identifier[grepl(ParameterMatch$SearchTerm[i],locsds$Identifier)==TRUE]
      ## If there are terms to remove
      if(missing(removeterms)==FALSE){
        ## Setting the default to FALSE
        Remove <- FALSE
        ## Looping through each of the search terms
        for(k in removeterms){
          ## Checking each dataset for each of the search terms, if it finds a match that position becomes true
          Remove <- grepl(k,PossibleDatasets) | Remove
        }
        ## Subsetting the dataset list to values that do not match any of the remove terms
        PossibleDatasets <- PossibleDatasets[Remove==FALSE]
      }
      ## If the list of dataset IDs has one value
      if(length(PossibleDatasets)==1){
        ## Set that dataset ID in the table for that parameter and site.
        ParameterMatch[i,j] <- paste0(PossibleDatasets)
      } else {
        ## If the list of datasets IDS has no values
        if(length(PossibleDatasets)==0){
          ## Set the table to blank for that parameter and site.
          ParameterMatch[i,j] <- ""
        } else {
          ## We now have multiple possible datasets for a single parameter
          ## Should we prompt the user to handle this issue?
          if(duplicate.handling==TRUE){
            ## Having the user select the correct dataset
            SelectionNum <- menu(paste0(PossibleDatasets), paste0(ParameterMatch$ParName[i], " data at ", j),graphics = TRUE)
            ## Translating the users selection number to the text of the datasetID and assigning it to the table.
            ParameterMatch[i,j] <- paste0(PossibleDatasets[SelectionNum])
          } else {
            ## Reporting a warning because the parameter has too many datasets.
            warning(paste0("The ", ParameterMatch$ParName[i], " parameter at ", j, " has multiple dataset IDs that match the search term.  None were applied.  Add remove terms or set duplicate.handling to TRUE"))
          }
        }
      }
    }
    ## Reporting that the site was downloaded
    print(paste0("Downloaded site ",j))
  }
  ## Returning the parameter table as the output
  return(ParameterMatch)
}
