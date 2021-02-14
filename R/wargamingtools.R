# wargamingtools.R

# This file contains package documentation stuff in addition to mostly
# internal helper functions.


#' @importFrom stringr str_match  str_match_all
#' @importFrom data.table data.table fread setnames setkey setkeyv .N .SD copy := rbindlist dcast.data.table as.data.table set tstrsplit
#' @importFrom ggplot2 ggplot
#' @importFrom jsonlite fromJSON
#' @title wargamingtools: A package for downloading WarGaming data
#' @author Jacob Colvin <jbcolvin@gmail.com>
#' @description  wargamingtools: A package for downloading WarGaming data, specifically for clans in World of Tanks.  See https://github.com/jbcolvin0/wargamingtools for more details.
#' @seealso \code{\link{get_application_id}}
#' @name wargamingtools
NULL


#' @title get_application_id
#' @description Get users default application_id, stored in ".WarGaming_id.txt",
#' which should contain a single line with an id of 32 lower case letters a-f or
#' numbers 0-9.  This removes the user from needed to store application_id in
#' source code.
#' @param path path to users default directory containing ".WarGaming_id.txt"
#' @return Users default application_id.
#' @export
get_application_id <-function(path="~/.WarGaming_id.txt")
{
  if( file.exists(path)){
    application_id=readLines("~/.WarGaming_id.txt",warn=FALSE)[1L]
  }else{
    warning(paste0("Please add your wargaming id to ",path))
    application_id = "None"
  }
  application_id
}


# See BBmisc::chunk for a more elaborate version.
#' @title chunk_vector
#' @description Split a vector into a list of vectors of length no greater than size.
#' @param x A vector
#' @param size Maximum size of a chunk.
#' @return A list of vectors
#' @export
chunk_vector <-function (x, size)
{
  nx = length(x)
  split(x, 0L:(nx-1L) %% min(nx%/%size + (nx%%size > 0L), nx))
}
