#' @title Download Aquarius Data
#' @description This function downloads the data from all datasets listed in the parameter table.
#'
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#'
#' @param datasets is a data frame in the format of the output from the finddatasets() function.
#' @param sites (optional) is a list of sites to download data for. This list cannot include any sites not in datasets table but it does not need to include all of those sites. If it is ignored all sites from the parameter table will be included.
#' @param startyear (optional) is a single value of the year you wish to start your time series. Data prior to January 1st of this year will not be downloaded.
#' @param endyear (optional) is a single value of the year you wish to end your time series. Data after December 31st of this year will not be downloaded.
#' @param site.rename (optional) is a vector of names to replace the site names in the output.  This argument allows the site ID in the output to not exactly match the site ID in aquarius.
#'
#' @return A data frame with all the data from the sites and parameters listed in the arguments
#'
#' @export
#'
#' @examples
#' #ParameterTable <- finddatasets(sites=c("BELA-088", "BELA-108", "KOVA-085", "YUCH-005"),
#' #                               parameters = c("LakeDepth", "SedimentTemperatureC","DissolvedOxygen(ODOconc)","pH","SpecificConductance(SpCond)","Temperature"),
#' #                               searchterms = c("Pressure_LakeDepth@", "Pressure_BottomTemp@", "DOconc@", "Sonde_pH@", "Sonde_SpCond@", "Sonde_Temp@"),
#' #                                removeterms = "synoptic")
#'
#' # Downloading the lake data fully based on the defaults
#' DownloadedData <- downloadaquariusdata(ParameterTable)
#'
#' # Downloading the lake data only from the site "BELA-088"
#' DownloadedData <- downloadaquariusdata(ParameterTable, sites = "BELA-088")
#'
#' # Downloading the lake data only from the site "BELA-108" but renaming it "Site1"
#' DownloadedData <- downloadaquariusdata(ParameterTable, sites = "BELA-108", site.rename = "Site1")
#'
#' # Downloading the lake data but only from the sites "KOVA-085" and "YUCH-005" but renaming the KOVA site as "Site1" and the YUCH site as "Site2" and "Site3".
#' DownloadedData <- downloadaquariusdata(ParameterTable, sites = c("KOVA-085","YUCH-005", "YUCH-005") , site.rename = c("Site1","Site2", "Site3"))
#'


downloadaquariusdata <- function(datasets, sites, startyear, endyear, site.rename){
  ## If a list of sites is not given.
  if(missing(sites)==TRUE){
    ## Generating a list of sites from the parameter table.
    sites <- colnames(datasets)[3:ncol(datasets)]
  }
  ## If a list of name replacements for the sites has not been provided.
  if(missing(site.rename)){
    ## Just use the names of the sites.
    sitenamer <- data.frame(sites=sites,sitename=sites)
  } else {
    ## If a list is provided check to see if the sites and site names have the same number of values
    if(length(sites)==length(site.rename)){
      ## If there are the same number of values then make a conversion table.
      sitenamer <- data.frame(sites=sites,sitename=site.rename)
    } else {
      ## If there are not the same number of values then return an error.
      stop(paste0("There are ", length(sites)," sites and ", length(site.rename), " names for sites."))
    }
  }
  ## If startyear was not given
  if(missing(startyear)){
    ## Set starttime to blank which does not limit the dataset start
    starttime <- ''
  } else {
    ## Start dataset at January 1st of the start year.
    starttime <- paste0(startyear,"-01-01T00:00:00.000-08:00")
  }
  ## If endyear was not given
  if(missing(endyear)){
    ## Set endtime to blank which does not limit the dataset end
    endtime <- ''
  } else {
    ## Ends dataset at December 31st of the end year.
    endtime <- paste0(endyear,"-12-31T23:59:59.999-08:00")
  }

  ## Pulling the ParName column to make an object of the parameter names.
  parameters <- datasets$ParName

  ## Making a blank table that data will be added to
  LongTable <- data.frame(matrix(ncol=4,nrow=0))

  ## Looping through each of the sites
  for(i in unique(sites)){
    ## Looping through each of the provided parameter IDs for that site
    for(j in parameters[datasets[,i]!="" & is.na(datasets[,i])==FALSE]){
      ## If there was no site renaming
      if(missing(site.rename)){
        ## Reporting which site and parameter is being downloaded
        print(paste0("Downloading: ", j," from ", i))
      } else {
        ## If the sites will be renamed
        ## Make an object of the new names for the sites
        namesforsites <- sitenamer$sitename[sitenamer$sites==i]
        ## If there is only one site and the name matches the original name
        if(length(namesforsites)==1 && namesforsites==i){
          ## Reporting which site and parameter is being downloaded
          print(paste0("Downloading: ", j," from ", i))
        } else {
          ## If there is only one or two new names for that site
          if(length(namesforsites)<=2){
            ## Reporting which site and parameter is being downloaded (with one or both new names)
            print(paste0("Downloading: ", j," from ", i, " (", paste0(namesforsites,collapse =" & "),")"))
          } else {
            ## Reporting which site and parameter is being downloaded (with the list of new names)
            print(paste0("Downloading: ", j," from ", i, " (", paste0(paste0(namesforsites[1:(length(namesforsites)-1)],collapse =", "),", & ",namesforsites[length(namesforsites)]),")"))

          }
        }
      }

      ## Downloading the dataset unique IDs at that site in JSON format
      getlocds <- GET(paste0(publishapiurl,'/GetTimeSeriesDescriptionList?LocationIdentifier=',i,sep=''))
      ## Converting the result from JSON into a data.frame
      locsds <- fromJSON(content(getlocds,"text"))$TimeSeriesDescriptions
      ## Finding the dataset unique ID for the desired dataset
      datasetID <- locsds$UniqueId[locsds$Identifier==datasets[datasets$ParName==j,i]]
      ## Downloading the data
      gettsd <- fromJSON(content(GET(paste0(publishapiurl, '/GetTimeSeriesData?TimeSeriesUniqueIDs=', datasetID, '&queryFrom=', starttime, '&queryTo=', endtime, sep='')),"text"))
      ## If there are data values
      if(gettsd$NumPoints!=0){
        ## Pulling the data table out
        DownloadTable <- gettsd$Points
        ## Adding the parameter name to the table
        DownloadTable$Parameter <- j
        ## If this dataset only has one replacement name
        if(length(sitenamer$sitename[sitenamer$sites==i])==1){
          ## Adding the new site name to the data table
          DownloadTable$Site <- sitenamer$sitename[sitenamer$sites==i]
          ## Adding the table to the master output table (LongTable)
          LongTable <- tablejoin(LongTable, DownloadTable)
        } else {
          ## If this dataset has multiple new site names
          ## Loop through each of the names
          for(k in sitenamer$sitename[sitenamer$sites==i]){
            ## Replacing the site name column with each of the new names
            DownloadTable$Site <- paste0(k)
            ## Appending the data table with each of the new names
            LongTable <- tablejoin(LongTable, DownloadTable)
          }
        }
      } else {
        ## If there are no data points in the downloaded dataset reporting that it was skipped.
        print(paste0("Skipped ", j, " at site ", i, ". No data found."))
      }
    }
  }
  ## Returning all of the data in a single table
  return(LongTable)
}

