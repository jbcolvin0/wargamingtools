
# code for saving a local csv database in a users home directory.
# by default, save to "~/wargamingtools_data"






#' @title get_account_tank_data
#' @param account_id A possible vector of account_ids.
#' @param clan_id A single clan_id.
# @param tank_id A possible vector of tank_ids.
#' @param tier Tank tier.
#' @param application_id Your application_id from \url{https://developers.wargaming.net/applications/},
#' retrieved by default using \code{\link{get_application_id}}.
#' @export
get_wot_data = function( account_id=NULL, clan_id=NULL, # tank_id,
                                  tier=10, application_id = get_application_id())
{
  mark_of_mastery=NULL

  if( !is.null(clan_id)){
    clan_id = as.clan_id(clan_id)
    dt_clans = get_clans_info(clan_id, application_id = application_id)
    account_id = unique( c( account_id,dt_clans$account_id) )
  }

  account_id = unique(as.account_id(account_id))

  if( length(account_id)>100){
    return( rbindlist(lapply( chunk_vector(account_id,100L), function(account_id){
      get_wot_data( account_id, tier=tier, application_id = application_id)
    }),fill=TRUE))
  }

  dt1 = get_account_info(account_id, application_id = application_id)
  dt1c = get_clans_accountinfo(account_id, application_id = application_id)

  dt2 = get_tank_ids(tier,application_id = application_id)

  # not full list of tanks???  use tank list from dt2
  dt3 = get_account_tanks( account_id, dt2$tank_id, application_id = application_id)
  dt3[,mark_of_mastery:=NULL]

  dt4 = rbindlist(lapply( account_id,function(account_id) get_tanks_stats( account_id, dt2$tank_id, application_id = application_id )))

  dt5 = rbindlist( lapply( account_id,function(account_id) get_tanks_achievements( account_id, dt2$tank_id, application_id = application_id )),fill=TRUE)
  #dt5[,achievements.markOfMastery:=NULL]

  dt_account = merge(dt1,dt1c,by=c("account_id","account_name","clan_id"),all=TRUE)

  dt_account_tank = merge(dt4,dt5,by=c("account_id","tank_id"),all=TRUE)
  dt_account_tank = merge(dt_account_tank,dt3,all=TRUE)
  dt_account_tank = merge(dt2,dt_account_tank,by=c("tank_id"),all.y=TRUE)

  dt = merge( dt_account,dt_account_tank,by="account_id")

  setkey(dt,"account_id","tank_id")

  dt
}



#' @title save_wot_data
#' @description Get account data and save to specified local csv database.
#' @param account_id account_id
#' @param clan_id clan_id
#' @param tier tier
#' @param path path to users default directory containing csv files.
#' @param application_id application_id
#' @return Users default path
#' @export
save_wot_data = function( account_id=NULL, clan_id=NULL, tier=10,
                          path = get_path(), application_id = get_application_id() )
{
  accounttime = Sys.time()

  dt = get_wot_data( account_id=account_id, clan_id = clan_id, tier = tier, application_id = application_id )

  save_wot_table( dt, accounttime, path=path )

  dt
}


#' @title save_wot_table
#' @description Save an existing table to specified csv wot database.
#' @param x data.frame or data.table
#' @param accounttime Used to specify csv file names.
#' @param accountfile Used to specify csv file names.
#' @param clanfile Used to specify csv file names.
#' @param path path to users default directory containing csv files.
#' @export
save_wot_table = function( x, accounttime = Sys.time(), accountfile = "_account_", clanfile = "clan_", path = get_path())
{
  dt = as.data.table(x)

  for( aid in unique(dt$account_id)){
    for( cid in unique(dt[account_id==aid]$clan_id)){
      file = file.path(path,paste0(clanfile,cid,accountfile,aid,"_",str_replace_all(accounttime,":","-"),".csv"))
      #print(file)
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
load_wot_data <- function( account_id=NULL, clan_id=NULL, tank_id, select, exclude,
                           tier = 1:10, daily=TRUE, latest=FALSE, path = get_path())
{
  N=joined_at=DateTime=marksOnGun=datetime=time=NULL

  dt_files = data.table(path=dir(path,pattern = "clan_",full.names=TRUE))
  dt_files[,basename:=basename(path)]
  dt_files = cbind(dt_files,as.data.table(str_match(dt_files$basename,"clan_(NA|[0-9]*)_account_([0-9]*)_(([0-9]{4}-[0-9]{2}-[0-9]{2}) ([0-9]{2}-[0-9]{2}-[0-9]{2}))")[,-1]))
  setnames(dt_files,c("V1","V2","V3","V4","V5"),c("clan_id","account_id","datetime","date","time"))

  #backward compat with old file format, will be depreciated
  if(TRUE){
    dt_files0 = data.table(path=dir(path,pattern = "clan_stats_",full.names=TRUE))
    dt_files0[,basename:=basename(path)]
    dt_files0 = cbind(dt_files0,as.data.table(str_match(dt_files0$basename,"clan_stats_(NA|[0-9]*)_([0-9]{4}-[0-9]{2}-[0-9]{2})")[,-1]))
    setnames(dt_files0,c("V1","V2"),c("clan_id","date"))

    dt_files0[,time:="00-00-00"]
    dt_files0[,datetime:=paste(date,time)]
    dt_files0[,account_id:=NA]

    dt_files = rbind(dt_files, dt_files0)[!str_detect(basename,"^clan_fame_")]

    #dir(path,pattern="ClanData")
  }

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

    x=as.data.table(fread(x0,select=select))

    if( nrow(x) == 0)
      return( NULL)

    if( !"account_id" %in% names(x))
      browser()

    if( nrow( x[,.N,keyby=c("account_id","tank_id")][N>1] ) > 0){

      #warning
      print(paste("duplicates",x0))
      #print(x[,.N,keyby=c("account_id","tank_id")][N>1])
      #print("end dups")

      #browser()

      x = x[,.SD[1],keyby=c("account_id","tank_id")]
    }
    x
  })

  print("load complete")

  setattr(dt, 'names', dt_files$path)
  dt = rbindlist(dt, use.names=TRUE, fill=TRUE, idcol="file")

  #browser()

  #filedate = str_match(dt$file,"([0-9]{4}-[0-9]{2}-[0-9]{2}) ([0-9]{2}-[0-9]{2}-[0-9]{2})")
  #dt[,file:=NULL]
  #dt[,joined_at:=NULL]
  #dt[,DateTime:=as.POSIXct( paste( filedate[,2], str_replace_all(filedate[,3],"-",":")) )]
  #setnames(dt,"achievements.marksOnGun","marksOnGun")
  #dt[is.na(marksOnGun),marksOnGun:=0]

  dt[]
}




#' @title remove_wot_data
#' @description Remove wot data for GDPR compliance reasons.
#' @param account_id account_ids to search for and remove files.
#' @param accountfile prefix for account_ids.
#' @param test test
#' @param verbose verbose
#' @param path path to users default directory containing csv files.
#' @export
remove_wot_data = function( account_id, accountfile = "_account_", test = TRUE, verbose = TRUE, path = get_path())
{
  files = dir(path, full.names = TRUE, pattern = paste0(account_file,account_id,collapse = "|"))

  cat("Delete the following files?")
  print(files)

  if( !test){

    result = menu(c("Yes", "No"), title="Do you want this?")
    if(result == 1){
      print("removing files...")
      file.remove(files)
    } else {
       print("files not removed, exiting...")
    }
  }

  files
}


