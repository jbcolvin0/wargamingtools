# query.R

# The functions in this file are focused on querying data from api.worldoftanks.com,
# or getting WN8 expected values from modxvm.com
# naming convention is that
#   https://api.worldoftanks.com/wot/foo/goo/?...
# is referenced as
#   get_foo_goo()



#' @title get_clans_info
#' @description Get clan info given clan_id from \url{https://api.worldoftanks.com/wot/clans/info/}.
#'
#' More details at \url{https://developers.wargaming.net/reference/all/wot/clans/info/}.
#' @param clan_id The number representing the \code{clan_id}.
# @param extra Include the extra clan information not account specific.
#' @param application_id Your application_id from \url{https://developers.wargaming.net/applications/},
#' retrieved by default using \code{\link{get_application_id}}.
#' @export
get_clans_info = function( clan_id, application_id = get_application_id())
{
  url = paste0("https://api.worldoftanks.com/wot/clans/info/?application_id=",
               application_id,"&clan_id=",paste0(clan_id,collapse = ","))
  json = fromJSON(url)
  as.data.table(json$data[[1L]]$members)
}


#' @title get_encyclopedia_vehicles
#' @description Get a list of vehicles for a specific tier from \url{https://api.worldoftanks.com/wot/encyclopedia/vehicles/}.
#'
#' More details at \url{https://developers.wargaming.net/reference/all/wot/encyclopedia/vehicles/}.
#' @param tier Integer from 1 to 10, can be a vector.
#' @param application_id Your application_id from \url{https://developers.wargaming.net/applications/},
#' retrieved by default using \code{\link{get_application_id}}.
#' @export
get_encyclopedia_vehicles = function( tier = 10L, application_id = get_application_id())
{
  # Get list of tier 10 tanks.
  url = paste0("https://api.worldoftanks.com/wot/encyclopedia/vehicles/?application_id=",
                               application_id,"&tier=",paste0(tier,collapse=","),"&language=en")
  json_tanks = fromJSON(url)
  dt_tanks = rbindlist(lapply(json_tanks$data,function(x)
    data.table(tank_id = x["tank_id"][[1L]],
               type=x["type"][[1L]],
               short_name=x["short_name"][[1L]],
               is_premium=x["is_premium"][[1L]],
               tier=x["tier"][[1L]]
               )))
  dt_tanks
}


#' @title get_account_info
#' @description Get account information for account_ids from \url{https://api.worldoftanks.com/wot/account/info/}.
#'
#' More details at \url{https://developers.wargaming.net/reference/all/wot/account/info/}.
#' @param account_id A vector of account_ids.
#' @param statistics If TRUE, add additional account overall statistics, FALSE by default.
#' @param application_id Your application_id from \url{https://developers.wargaming.net/applications/},
#' retrieved by default using \code{\link{get_application_id}}.
#' @export
get_account_info = function( account_id, statistics=FALSE, application_id = get_application_id())
{
  account_id = as.integer(account_id)
  rbindlist(lapply( chunk_vector(account_id,100L), function(account_id){
    url = paste0("https://api.worldoftanks.com/wot/account/info/?application_id=",application_id,
                 "&account_id=",paste0(account_id,collapse = ","),
                 "&fields=-statistics.regular_team,-statistics.company,-statistics.historical,-statistics.team,-statistics.frags")
    if(!statistics)
      url = paste(url,",-statistics.clan,-statistics.clan,-statistics.all,-statistics.stronghold_defense,-statistics.stronghold_skirmish,-statistics.trees_cut")
    x=fromJSON(url, flatten=TRUE)
    ldt = lapply(x$data,function(y){
      dt = as.data.table(y)
      dt[,statistics:=NULL]
      z=lapply( names(y$statistics),function(prefix){
        dt0 = as.data.table(y$statistics[[prefix]])
        if( ncol(dt0) > 1 ) names(dt0)<-paste0(prefix,".",names(dt0))
        else names(dt0)<-prefix
        dt0
      })
      cbind(dt,as.data.table(z))
    })
    rbindlist(ldt,fill=TRUE)
  }))
}


#' @title get_account_tanks
#' @description Get general account/tank information from \url{https://api.worldoftanks.com/wot/account/tanks/}.
#' Will repeatedly query as needed when data exceeds 100 rows.
#'
#' More details at \url{https://developers.wargaming.net/reference/all/wot/account/tanks/}.
#' @param account_id account_id Integer vector, max length 100.
#' @param tank_id tank_id Integer vector
#' @param application_id Your application_id from \url{https://developers.wargaming.net/applications/},
#' retrieved by default using \code{\link{get_application_id}}.
#' @export
get_account_tanks = function( account_id, tank_id, application_id = get_application_id())
{
  dt_account <- rbindlist(
  lapply( chunk_vector(tank_id,100L), function(tank_id){
    url = paste0("https://api.worldoftanks.com/wot/account/tanks/?application_id=",application_id,
                 "&account_id=",paste0(account_id,collapse=","),"&tank_id=",paste0(tank_id,collapse=","))
    json_account = fromJSON(url,flatten=TRUE)
    dt_account = as.data.table(rbindlist(json_account$data,idcol = TRUE))
    dt_account
  }))

  setnames(dt_account,".id","account_id",skip_absent=TRUE)
  dt_account[,account_id:=as.integer(account_id)]
  dt_account[]
}


#' @title get_tanks_stats
#' @description Get account/tank statistics from \url{https://api.worldoftanks.com/wot/tanks/stats/}.
#'
#' More details at \url{https://developers.wargaming.net/reference/all/wot/tanks/stats/}.
#' @param account_id A single account_id.
#' @param tank_id A possible vector of tank_ids.
#' @param application_id Your application_id from \url{https://developers.wargaming.net/applications/},
#' retrieved by default using \code{\link{get_application_id}}.
#' @export
get_tanks_stats = function(account_id, tank_id, application_id = get_application_id())
{
  dt_stats = rbindlist(
    lapply( chunk_vector(tank_id,100L), function(tank_id){
      url = paste0("https://api.worldoftanks.com/wot/tanks/stats/?application_id=",application_id,
                    "&account_id=",account_id,
                    "&tank_id=",paste0(tank_id,collapse=","),
                    "&language=en",
                    #"&extra=epic,fallout,random,ranked,ranked_battles"
                    "&extra=random,ranked,ranked_battles",
                   "&fields=-team,-company"

      )
      as.data.table(fromJSON(url)$data[[1L]])
    }), fill=TRUE)


  if( nrow( dt_stats[,.N,keyby=c("account_id","tank_id")][N>1] ) > 0){
    warning("Duplicate dt_stat rows!!!!!!!!!!!!!!!!!!")
    N=NULL
    dt_stats = dt_stats[,.SD[1],keyby=c("account_id","tank_id")]

#    all(dt_stats[account_id==1043990178   & tank_id==59681][1] == dt_stats[account_id==1043990178   & tank_id==59681][2],na.rm = TRUE)
  }

  dt_stats
}


#' @title get_tanks_achievements
#' @description Get account/tank achievements from \url{https://api.worldoftanks.com/wot/tanks/achievements/}.
#'
#' More details at \url{https://developers.wargaming.net/reference/all/wot/tanks/achievements/}.
#' @param account_id A single account_id.
#' @param tank_id A possible vector of tank_ids.
# @param MOEonly Query only the Mark Of Excellence.
#' @param application_id Your application_id from \url{https://developers.wargaming.net/applications/},
#' retrieved by default using \code{\link{get_application_id}}.
#' @export
get_tanks_achievements = function(account_id, tank_id, application_id = get_application_id())
{
  rbindlist(
    lapply( chunk_vector(tank_id,100L), function(tank_id){
      url = paste0("https://api.worldoftanks.com/wot/tanks/achievements/?application_id=",application_id,
                   "&account_id=",account_id,"&tank_id=",paste0(tank_id,collapse=","),"&language=en")
      as.data.table(fromJSON(url, flatten=TRUE)$data[[1L]])
    }), fill=TRUE)
}


#' @title get_globalmap_eventaccountinfo
#' @description Get globalmap account information (like fame points) from
#' \url{https://api.worldoftanks.com/wot/globalmap/eventaccountinfo/}.
#'
#' More details at \url{https://developers.wargaming.net/reference/all/wot/globalmap/eventaccountinfo/}.
#' @param account_id A vector of account_ids.
#' @param front_id Default is "renaissance_bg", previous front_id is "metal_wars_bg".
#' @param event_id Default is "renaissance", previous event_id is "metal_wars".
#' @param application_id Your application_id from \url{https://developers.wargaming.net/applications/},
#' retrieved by default using \code{\link{get_application_id}}.
#' @export
get_globalmap_eventaccountinfo = function(account_id,
                                          front_id="renaissance_bg",
                                          event_id="renaissance",
                                          application_id = get_application_id())
{
  dt_fame = rbindlist( lapply(account_id, function( account_id){
    url = paste0("https://api.worldoftanks.com/wot/globalmap/eventaccountinfo/?application_id=",
                 application_id,"&account_id=",account_id,"&front_id=",front_id,"&event_id=",event_id)
    fromJSON(url)$data[[1L]][[1L]][[1L]]
  }))
  dt_fame = dt_fame[,c("account_id","battles","rank","fame_points")]
  setnames(dt_fame,c("battles","rank"),c("fame_battles","fame_rank"))
  dt_fame[,event_id:=event_id]
  dt_fame[]
}


#' @title get_clans_list
#' @description Get clan info given clan_id from \url{https://api.worldoftanks.com/wot/clans/list/}.
#'
#' More details at \url{https://developers.wargaming.net/reference/all/wot/clans/list/}.
#' @param search Text used to search for clans.
#' @param application_id Your application_id from \url{https://developers.wargaming.net/applications/},
#' retrieved by default using \code{\link{get_application_id}}.
#' @export
get_clans_list = function( search, application_id = get_application_id())
{
  url = paste0("https://api.worldoftanks.com/wot/clans/list/?application_id=",
               application_id,"&language=en&search=",search)
  json_clan = fromJSON(url,flatten = TRUE)
  dt_clan = as.data.table(json_clan$data)
  dt_clan
}


#' @title get_xvm_expectedvalues
#' @description Get WN8 expected values from \url{https://modxvm.com/en/wn8-expected-values/}.
#' @export
get_xvm_expectedvalues = function()
{
  url = "https://static.modxvm.com/wn8-data-exp/json/wn8exp.json"
  json_xvm = fromJSON(url,flatten = TRUE)
  dt_xvm = as.data.table(json_xvm$data)
  dt_xvm
}


#' @title get_tank_ids
#' @param tier Vehicle tier.
#' @param application_id Your application_id from \url{https://developers.wargaming.net/applications/},
#' retrieved by default using \code{\link{get_application_id}}.
#' @export
get_tank_ids = function( tier=6:10, application_id = get_application_id())
{
  url = paste0("https://api.worldoftanks.com/wot/encyclopedia/vehicles/?application_id=",application_id,
               "&tier=",paste0(tier,collapse=","),"&language=en")
  json_tanks = fromJSON(url)
  dt_tanks = rbindlist(lapply(json_tanks$data,function(x){
    data.table(tank_id = x["tank_id"][[1]],
               type=x["type"][[1]],
               short_name=x["short_name"][[1]],
               is_premium=x["is_premium"][[1]],
               tier=x["tier"][[1]])
  }))
  dt_tanks
}


#' @title get_clans_info
#' @param clan_id A vector of clan_ids
#' @param application_id Your application_id from \url{https://developers.wargaming.net/applications/},
#' retrieved by default using \code{\link{get_application_id}}.
#' @export
get_clans_info = function( clan_id, application_id = get_application_id())
{
  clan_id = as.integer(clan_id)
  url = paste0("https://api.worldoftanks.com/wot/clans/info/?application_id=",
               application_id,"&clan_id=",paste0(clan_id,collapse = ","))
  json_members = fromJSON(url)
  dt_members = as.data.table(json_members$data[[1]]$members)
  dt_members[,clan_id:=clan_id]
  dt_members
}


#' @title get_clanmember_data
#' @param clan_id clan_id
# @param tank_id A possible vector of tank_ids.
#' @param tier Tank tier.
#' @param application_id Your application_id from \url{https://developers.wargaming.net/applications/},
#' retrieved by default using \code{\link{get_application_id}}.
#' @export
get_clanmember_data = function( clan_id, tier = 10, application_id = get_application_id())
{
  mark_of_mastery=role_i18n=NULL

  dt1 = get_clans_info(clan_id)

  dt2 = get_tank_ids(tier)

  dt3 = get_account_tanks( dt1$account_id, dt2$tank_id, application_id = get_application_id())
  dt3[,mark_of_mastery:=NULL]

  dt4 = rbindlist(lapply( dt1$account_id,function(account_id) get_tanks_stats( account_id, dt2$tank_id, application_id )))

  dt5 = rbindlist( lapply( dt1$account_id,function(account_id) get_tanks_achievements( account_id, dt2$tank_id, application_id )),fill=TRUE)

  dt1[,role_i18n:=NULL]

  browser()
  dt = merge(dt1,dt3,by="account_id",all=TRUE)
  dt = merge(dt,dt2,by=c("tank_id"),all.x=TRUE)  # don't need tanks what no account has.
  dt = merge(dt,dt4,by=c("account_id","tank_id"),all=TRUE)
  dt = merge(dt,dt5,by=c("account_id","tank_id"),all=TRUE)

  dt
}


#' @title get_account_tank_data
#' @param account_id A possible vector of account_ids.
# @param tank_id A possible vector of tank_ids.
#' @param tier Tank tier.
#' @param application_id Your application_id from \url{https://developers.wargaming.net/applications/},
#' retrieved by default using \code{\link{get_application_id}}.
#' @export
get_account_tank_data = function( account_id,# tank_id,
                                  tier=10, application_id = get_application_id())
{
  mark_of_mastery=NULL

  if( length(account_id)>100)
    warning("too many account_id, limit 100")

  # TODO: find alternative to dt1 = get_clans_info(clan_id) to get account_name
  dt1 = get_account_info(account_id)

  dt2 = get_tank_ids(tier)

  dt3 = get_account_tanks( account_id, dt2$tank_id, application_id = get_application_id())
  dt3[,mark_of_mastery:=NULL]

  dt4 = rbindlist(lapply( account_id,function(account_id) get_tanks_stats( account_id, dt2$tank_id, application_id )))

  dt5 = rbindlist( lapply( account_id,function(account_id) get_tanks_achievements( account_id, dt2$tank_id, application_id )),fill=TRUE)

  dt = merge(dt3,dt2,by=c("tank_id"),all.x=TRUE)  # don't need tanks what no account has.
  dt = merge(dt,dt4,by=c("account_id","tank_id"),all=TRUE)
  dt = merge(dt,dt5,by=c("account_id","tank_id"),all=TRUE)

  dt
}

