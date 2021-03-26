
# code for saving a local csv database in a users home directory.
# by default, save to "~/wargamingtools_data"

#' @title save_clan_data
#' @description Get clan members data and save to specified local csv database.
#' @param clan_id clan_id
#' @param tier tier
#' @param clanfile Optional: used to overwrite clanfile name and location.
#' @param path path to users default directory containing csv files.
#' @param application_id application_id
#' @return Users default path
#' @export
save_clan_data = function( clan_id, tier=10, clanfile = "clan_", path = get_path(), application_id = get_application_id() )
{
  clan_id = as.clan_id(clan_id)

  clantime = Sys.time()

  dt = get_clanmember_data( clan_id, tier = tier, application_id = application_id )

  clanfile = paste0(clanfile,clan_id,"_",str_replace_all(clantime,":","-"),".csv")

  file = file.path(path,clanfile)
  print(file)

  fwrite(dt,file)

  dt[]
}


#' @title save_account_data
#' @description Get account data and save to specified local csv database.
#' @param account_id account_id
#' @param tier tier
#' @param accountfile Optional: used to overwrite accountfile name and location.
#' @param path path to users default directory containing csv files.
#' @param application_id application_id
#' @return Users default path
#' @export
save_account_data = function( account_id, tier=10, accountfile = "account_",
                              path = get_path(), application_id = get_application_id() )
{
  account_id = as.account_id(account_id)

  accounttime = Sys.time()

  dt = get_account_tank_data( account_id, tier = tier, application_id = application_id )

  for( id in unique(dt$account_id)){
    file = paste0(accountfile,id,"_",str_replace_all(accounttime,":","-"),".csv")
    file = file.path(path,file)
    print(file)
    fwrite(dt[account_id==id],file)
  }

  dt
}


#' @title load_clan_data
#' @description Load clan data stored at given or default path.
#' @param clan_id Integer vector of clan_ids used to filter data.
#' If missing, all clans are loaded.
#' @param daily Load only the latest data for each day.
#' @param latest Load only the latest data for each clan_id
#' @param path Path to load previously saved data.
#' @return A data.table.
#' @export
load_clan_data <- function(clan_id, daily=TRUE, latest=FALSE, path = get_path())
{
  N=joined_at=DateTime=marksOnGun=NULL

  dt_files = data.table(path=dir(path,pattern = "clan_",full.names=TRUE))

  if(daily){
    dt_files = cbind(dt_files,str_match(dt_files$path,"([0-9]{4}-[0-9]{2}-[0-9]{2}) ([0-9]{2}-[0-9]{2}-[0-9]{2})"))
    setnames(dt_files,c("path","datetime","date","time"))
    setkey(dt_files,"date","time")
    dt_files = dt_files[,.SD[nrow(.SD)],,keyby=date]
  }

  dt <- lapply( dt_files$path, function(x0){

    x=as.data.table(fread(x0))


    if( nrow( x[,.N,keyby=c("account_id","tank_id")][N>1] ) > 0){

  #    browser()

      #warning
      print(paste("duplicates",x0))
      print(x[,.N,keyby=c("account_id","tank_id")][N>1])
      print("end dups")
      x = x[,.SD[1],keyby=c("account_id","tank_id")]
      #browser()
    }
    x
  })

  setattr(dt, 'names', dt_files$path)
  dt = rbindlist(dt, use.names=TRUE, fill=TRUE, idcol="file")
  filedate = str_match(dt$file,"([0-9]{4}-[0-9]{2}-[0-9]{2}) ([0-9]{2}-[0-9]{2}-[0-9]{2})")
  #dt[,file:=NULL]
  dt[,joined_at:=NULL]
  dt[,DateTime:=as.POSIXct( paste( filedate[,2], str_replace_all(filedate[,3],"-",":")) )]
  setnames(dt,"achievements.marksOnGun","marksOnGun")
  dt[is.na(marksOnGun),marksOnGun:=0]

  dt[]
}



#' @title load_account_data
#' @description Load account data stored at given or default path.
#' @param account_id Integer vector of account_ids used to filter data.
#' If missing, all accountss are loaded.
#' @param daily Load only the latest data for each day.
#' @param latest Load only the latest data for each account_id
#' @param path Path to load previously saved data.
#' @return A data.table.
#' @export
load_account_data <- function(account_id, daily=TRUE, latest=FALSE, path = get_path())
{
  N=joined_at=DateTime=marksOnGun=NULL

  dt_files = data.table(path=dir(path,pattern = "account_",full.names=TRUE))

  if(daily){
    dt_files = cbind(dt_files,str_match(dt_files$path,"([0-9]{4}-[0-9]{2}-[0-9]{2}) ([0-9]{2}-[0-9]{2}-[0-9]{2})"))
    setnames(dt_files,c("path","datetime","date","time"))
    setkey(dt_files,"date","time")
    dt_files = dt_files[,.SD[nrow(.SD)],,keyby=date]
  }

  dt <- lapply( dt_files$path, function(x0){

    x=as.data.table(fread(x0))


    if( nrow( x[,.N,keyby=c("account_id","tank_id")][N>1] ) > 0){

  #    browser()

      #warning
      print(paste("duplicates",x0))
      print(x[,.N,keyby=c("account_id","tank_id")][N>1])
      print("end dups")
      x = x[,.SD[1],keyby=c("account_id","tank_id")]
      #browser()
    }
    x
  })

  setattr(dt, 'names', dt_files$path)
  dt = rbindlist(dt, use.names=TRUE, fill=TRUE, idcol="file")
  filedate = str_match(dt$file,"([0-9]{4}-[0-9]{2}-[0-9]{2}) ([0-9]{2}-[0-9]{2}-[0-9]{2})")
  #dt[,file:=NULL]
  dt[,joined_at:=NULL]
  dt[,DateTime:=as.POSIXct( paste( filedate[,2], str_replace_all(filedate[,3],"-",":")) )]
  setnames(dt,"achievements.marksOnGun","marksOnGun")
  dt[is.na(marksOnGun),marksOnGun:=0]

  dt[]
}


