#' @title Table Join
#' @description This function joins tables using rbind but it will add blank columns to make the columns match if there are discrepancies. Soon to be replaced by dplyr::row_bind.
#'
#' @param table1 First Table
#' @param table2 Second Table
#'
#' @examples
#' tablea <- data.frame(Letter=c("A", "B", "C"), Integer=c(1,2,3))
#' tableb <- data.frame(Letter=c("A", "D", "E"), Animal=c("Cat", "Dog", "Snake"))
#' tablejoin(tablea, tableb)
#'   Letter Integer Animal
#' 1      A       1
#' 2      B       2
#' 3      C       3
#' 4      A            Cat
#' 5      D            Dog
#' 6      E          Snake
#'
#' @export


tablejoin <- function(table1, table2){
  if(nrow(table1)==0|nrow(table2)==0){
    if(nrow(table1)==0){
      return(table2)
    } else {
      return(table1)
    }
  } else {
    table1Names <- colnames(table2)[!colnames(table2) %in% colnames(table1)]
    table2Names <- colnames(table1)[!colnames(table1) %in% colnames(table2)]

    if(length(table1Names)>0){
      for(i in table1Names){
        table1[,i] <- ""
      }
    }

    if(length(table2Names)>0){
      for(i in table2Names){
        table2[,i] <- ""
      }
    }
    outtable <- rbind(table1,table2)
    return(outtable)
  }
}
