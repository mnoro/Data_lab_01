#' Title r_zip_csv: 
#' This function uncompress a number of csv files and loads them
#' into a dataframe
#' 
#'
#' @param ifiles The vector of the files to unzip and load as dataframes
#' @param to_path The path destination 
#' @param unzip To specify whether to unzip or not
#' @param from_path The vector of the origin path of all the files
#' @param add_col_value A column that is added to the end with this value
#' @param remove TRUE to remove unzipped files
#'
#' @return
#' @export
#'
#' @examples
#' \donotrun{
#' ifiles_ <- rep(list.files(file.path(getwd(), "data"), pattern = ".zip"),3)
#' from_path_ <- file.path(getwd(),"data")
#' to_path_ <- file.path(getwd(),"data2")
#' add_col_value_ <- c("Nov", "Dec", "Jan")
#' df <- r_zip_csv(ifiles_, from_path_, to_path_, add_col_value_)}
#' 
r_zip_csv <- function(ifiles,
                      from_path,
                      to_path = getwd(),
                      unzip = TRUE,
                      add_col_value, 
                      remove = FALSE)
{
  result <- data.frame()
  
  for (i in 1:length(ifiles)) {
    # ---------------------------------------------------
    # 1. copy file / Dropped - kept if there are performance issues
    # ---------------------------------------------------
    # file.copy(file.path(from_path,ifiles[i]),to_path, overwrite = TRUE)
    # ---------------------------------------------------
    # 2. unzip file - extracts from original path to destination path
    # ---------------------------------------------------
    if(unzip == TRUE)
    {
      unzip(file.path(from_path[i], ifiles[i]),
            exdir = to_path)
    }
    # ---------------------------------------------------
    # 3. read to df
    # ---------------------------------------------------
    if(unzip == TRUE){
      res <- readr::read_csv(file.path(to_path,
                                       gsub(".zip", "", ignore.case = TRUE, ifiles[i])))      
    } else
    {
      res <- readr::read_csv(file.path(from_path[i], ifiles[i]))
    }
    
    # ---------------------------------------------------
    # Remove file
    # ---------------------------------------------------
    if(remove == TRUE && unzip == TRUE){
      file.remove(file.path(to_path,
                            gsub(".zip", "", ignore.case = TRUE, ifiles[i])))
    }
    # ---------------------------------------------------
    # 4. add column
    # ---------------------------------------------------
    res$new_col <- add_col_value[i]
    
    # ---------------------------------------------------
    # 5. Combine all dataframes and return
    # ---------------------------------------------------
    result <- rbind(result, res)
  }
  return(result)
}
#
# Test Function
#
from_path_ <- rep(file.path(getwd(),"data"),3)
ifiles_ <- list.files(from_path_[1], pattern = ".zip")
to_path_ <- file.path(getwd(),"data2")
add_col_value_ <- c("Nov", "Dec", "Jan")

from_path <- file.path(getwd(),"data")
ifiles <- list.files(from_path, pattern = ".zip")
to_path <- file.path(getwd(),"data2")
add_col_value <- c("Nov", "Dec", "Jan")

# ---------------------------------------------------
# TEST
# ---------------------------------------------------
x <- r_zip_csv(ifiles = ifiles_, 
               to_path = to_path_,
               from_path = from_path_, 
               add_col_value = add_col_value_)

x <- r_zip_csv(ifiles = ifiles_, 
               to_path = to_path_,
               from_path = from_path_, 
               add_col_value = add_col_value_,
               remove = TRUE)
