
# code for saving a local csv database in a users home directory.
# by default, save to "~/wargamingtools_data"


#' @title save_wot_data
#' @description Get account data and save to specified local csv database.
#' @param account_id account_id
#' @param clan_id clan_id
#' @param tier tier
# @param accountfile Optional: used to overwrite accountfile name and location.
#' @param path path to users default directory containing csv files.
#' @param application_id application_id
#' @return Users default path
#' @export
save_wot_data = function( account_id, clan_id, tier=10, #accountfile = "account_",
                              path = get_path(), application_id = get_application_id() )
{
  accounttime = Sys.time()
  accountfile = "_account_"
  clanfile = "clan_"
  dt = get_wot_data( account_id=account_id, clan_id = clan_id, tier = tier, application_id = application_id )

  for( aid in unique(dt$account_id)){
    for( cid in unique(dt[account_id==aid]$clan_id)){
      file = file.path(path,paste0(clanfile,cid,accountfile,aid,"_",str_replace_all(accounttime,":","-"),".csv"))
      print(file)
      fwrite(dt[account_id==aid & clan_id==cid],file)
    }
  }

  dt
}


#' @title load_wot_data
#' @description Load World Of Tanks data stored at given or default path.
#' @param account_id Integer vector of account_ids used to filter data.
#' If missing, all accounts are loaded.
#' @param clan_id Integer vector of clan_ids used to filter data.
#' If missing, all clans are loaded.
#' @param select Columns names to retain, default is all columns.
#' @param exclude Column names to exclude, default is none.
#' @param tier Filter based on tank tier
#' @param tank_id Filter based on tank_id
#' @param daily Load only the latest data for each day.
#' @param latest Load only the latest data for each clan_id
#' @param path Path to load previously saved data.
#' @return A data.table.
#' @export
load_wot_data <- function( account_id, clan_id, tank_id, select, exclude,
                           tier = 1:10, daily=TRUE, latest=FALSE, path = get_path())
{
  N=joined_at=DateTime=marksOnGun=NULL

  dt_files = data.table(path=dir(path,pattern = "clan_",full.names=TRUE))
  dt_files[,basename:=basename(path)]
  dt_files = cbind(dt_files,as.data.table(str_match(dt_files$basename,"clan_(NA|[0-9]*)_account_([0-9]*)_(([0-9]{4}-[0-9]{2}-[0-9]{2}) ([0-9]{2}-[0-9]{2}-[0-9]{2}))")[,-1]))
  setnames(dt_files,c("V1","V2","V3","V4","V5"),c("clan_id","account_id","datetime","date","time"))

  if( !missing(account_id)){
    account_id0 = as.account_id(account_id)
    dt_files = dt_files[account_id %in% account_id0]
  }

  if( !missing(clan_id)){
    clan_id0 = as.clan_id(clan_id)
    dt_files = dt_files[clan_id %in% clan_id0]
  }

  if(daily){
    setkey(dt_files,"clan_id","account_id","date","time")
    dt_files = dt_files[,.SD[nrow(.SD)],,keyby=c("clan_id","account_id","date")]
  }

  dt <- lapply( dt_files$path, function(x0){

    x=as.data.table(fread(x0))


    if( nrow( x[,.N,keyby=c("account_id","tank_id")][N>1] ) > 0){

      #warning
      print(paste("duplicates",x0))
      print(x[,.N,keyby=c("account_id","tank_id")][N>1])
      print("end dups")

      browser()

      x = x[,.SD[1],keyby=c("account_id","tank_id")]
    }
    x
  })

  print("load complete")

  setattr(dt, 'names', dt_files$path)
  dt = rbindlist(dt, use.names=TRUE, fill=TRUE, idcol="file")
  filedate = str_match(dt$file,"([0-9]{4}-[0-9]{2}-[0-9]{2}) ([0-9]{2}-[0-9]{2}-[0-9]{2})")
  #dt[,file:=NULL]
  #dt[,joined_at:=NULL]
  dt[,DateTime:=as.POSIXct( paste( filedate[,2], str_replace_all(filedate[,3],"-",":")) )]
  #setnames(dt,"achievements.marksOnGun","marksOnGun")
  #dt[is.na(marksOnGun),marksOnGun:=0]

  dt[]
}

