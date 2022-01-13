# wargamingtools.R

# This file contains package documentation stuff in addition to mostly
# internal helper functions.


#' @importFrom stringr str_detect str_match str_match_all str_replace_all
#' @importFrom data.table data.table fread fwrite setnames setkey setkeyv .N .SD copy := rbindlist dcast.data.table as.data.table set tstrsplit setattr
#' @importFrom ggplot2 ggplot
#' @importFrom jsonlite fromJSON
#' @importFrom utils write.csv menu
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
#' @param filename filename to users default directory containing ".WarGaming_id.txt"
#' @return Users default application_id.
#' @export
get_application_id <-function(filename="~/.WarGaming_id.txt")
{
  if( file.exists(filename)){
    application_id=readLines(filename,warn=FALSE)[1L]
  }else{
    warning(paste0("Please add your wargaming id to ",filename,
                   ", or explicitly specify application_id."))
    application_id = "None"
  }
  application_id
}

#' @title set_application_id
#' @description Set users default application_id, stored in "~/.WarGaming_id.txt",
#' which should contain a single line with an id of 32 lower case letters a-f or
#' numbers 0-9.  This removes the user from needed to store application_id in
#' source code.
#' @param application_id Your application_id from \url{https://developers.wargaming.net/applications/}.
#' @param filename filename to users default directory writing "~/.WarGaming_id.txt"
#' @return Users default application_id.
#' @export
set_application_id <- function(application_id,filename="~/.WarGaming_id.txt"){
  write(application_id,filename)
  print(paste("application_id writen to",path.expand(filename)))
  application_id
}


#' @title get_path
#' @description Get users default path for storing csv data.
#' Default is "~/wargamingtools_data/", which on windows will be in the Documents folder,
#' unless an override exists in "~/.WarGaming_path.txt".
#' @param path path to users default directory containing csv files.
#' @return Users default path
#' @export
get_path <-function(path)
{
  if( missing(path)){
    if( file.exists("~/.WarGaming_path.txt"))
      path=readLines("~/.WarGaming_path.txt",warn=FALSE)[1L]
    else
      path = "~/wargamingtools_data"
  }

  path = path.expand(path)
  if( !dir.exists(path))
    dir.create(path)
  path
}


#' @title as.clan_id
#' @description coerce to a clan id using numeric clan_id or searching based on clan tag using get_clans_list().
#' @param x clan_id or clan tag.
#' @return integer clan_id
#' @export
as.clan_id <- function( x )
{
  tag=clan_id=NULL  # because of data.table

  if( length(x)>1)
    return(sapply(x,as.clan_id))

  i = strtoi(x)
  if( !is.na(i)){
    return(i)
  } else {
    dt = get_clans_list(x)

    if( nrow(dt) == 0)
      stop("Returned zero search results using get_clans_list(x).")

    if( nrow( dt[tag==x]) == 1 )
      return( dt[tag==x,clan_id])

    if( nrow(dt) == 1)
      return( dt[,clan_id])

    cat( "Warning: please select a tag or clan_id from...\n")
    print( dt[,c("members_count","name","tag","clan_id")])
    stop("bad clan_id")
  }
}

#' @title as.account_id
#' @description coerce to an account id using numeric account_id or searching based on username using get_account....().
#' @param x account_id or username.
#' @return integer account_id
#' @export
as.account_id <- function(x)
{
  nickname=account_id=NULL  # because of data.table

  if( length(x)>1)
    return(sapply(x,as.account_id))

  i = strtoi(x)
  if( !is.na(i)){
    return(i)
  } else {
    dt = get_account_list(x)

    if( nrow(dt) == 0)
      stop("Returned zero search results using get_account_list(x).")

    if( nrow( dt[nickname==x]) == 1 )
      return( dt[nickname==x,account_id])

    if( nrow(dt) == 1)
      return( dt[,account_id])

    cat( "Warning: please select a nickname/username or account_id from...\n")
    print( dt )
    stop("bad account_id")
  }

  x
}

#' @title as.tank_id
#' @description coerce to a tank_id using numeric tank_id or searching based on tank name using get_tank....().
#' @param x tank_id or tank name.
#' @return integer tank_id
#' @export
as.tank_id = function(x){
  x
}


#' @title get_xvm_expectedvalues
#' @description Get WN8 expected values from \url{https://modxvm.com/en/wn8-expected-values/}.
#' @param filename Default is "wn8exp.csv"
#' @param path Path to users default directory containing csv files.
#' @export
get_xvm_expectedvalues = function(filename="wn8exp.csv",path = get_path())
{
  # if less than 28 days, download xvm data, else if file exists, just use that....

  filepath = file.path(path,filename)

  if( file.exists( filepath ) && difftime( Sys.time(), file.info(filepath)$mtime,units="days") < 28)
    return( fread( filepath))

  url = "https://static.modxvm.com/wn8-data-exp/json/wn8exp.json"
  json_xvm = fromJSON(url,flatten = TRUE)
  dt_xvm = as.data.table(json_xvm$data)
  fwrite(dt_xvm,filepath)
  dt_xvm
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
