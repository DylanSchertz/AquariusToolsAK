
#' @title Multiple Selection Menu
#' @description This function opens a graphic interface to allow selection of multiple values.
#'
#' @importFrom utils menu
#'
#' @param options Character object of possible values to select.
#' @param Title (optional) Character for title prompt to pop up. Default is "Which options?"
#' @param preselected (optional) character ojbect of sites to be added to the output
#'
#' @return A character object of selected item(s).
#'
#' @export
#'
#' @examples
#' multipleselectionmenu(unique(c("DENA-023", "YUCH-004", "WRST-015", "BELA-088")),"Which sites? (select one at a time)")


multipleselectionmenu <- function(options,Title="Which options?", preselected){
  ## Making sure List and Answer are blank
  List <- character()
  Answer <- character()
  ## Adding preselected values to the output list
  if(missing(preselected)==FALSE){
    List <- c(List,preselected)
  }

  ## Creating a list of the site names that match the user's selection
  pickoptions <- c(options, "[No more]")

  ## Repeating until the user says they have made all of their selections.
  while(length(Answer)==0 || Answer!="[No more]"){
    ## Making a list of the options that have not already been selected
    AvailOptions <- pickoptions[pickoptions%in% List==FALSE]

    ## Asking the user for a new selection
    Answer <- AvailOptions[menu(AvailOptions[AvailOptions %in% List==FALSE],title=Title,graphics=TRUE)]

    if(length(Answer)==0){
      Answer <- "[No more]"
    }

    ## If the user did not select "[No more]" adding their selection to the output list.
    if(Answer!="[No more]"){
      List <- c(List, Answer)
    }
  }
  ## Returning the output list
  return(List)
}
