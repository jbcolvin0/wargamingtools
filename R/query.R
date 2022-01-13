# query.R

# The functions in this file are focused on querying data from api.worldoftanks.com,
# or getting WN8 expected values from modxvm.com
# naming convention is that
#   https://api.worldoftanks.com/wot/foo/goo/?...
# is referenced as
#   get_foo_goo()



################################################################################
# clans
################################################################################

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
  clan_id = as.clan_id(clan_id)

  dt = rbindlist( lapply( chunk_vector(clan_id, 100L), function(x){
    url = paste0("https://api.worldoftanks.com/wot/clans/info/?application_id=",
                 application_id,"&clan_id=",paste0(x,collapse = ","))
    json = fromJSON(url)

    as.data.table(json$data[[1L]]$members)
  }))

  dt
}
if(FALSE){
  clan_ids = c( "1000012171", "1000021070", "1000011108", "1000016749", "1000021801",
                "1000049558", "1000007818", "1000015461", "1000016521", "1000005754")
}

#  lapply( chunk_vector(account_id, 100L), function( account_id) {
#    url = paste0("https://api.worldoftanks.com/wot/clans/accountinfo/?application_id=",
#               application_id,"&account_id=",paste0(account_id,collapse = ","))
#
#    json = fromJSON(url, flatten=TRUE)
#
#    ldt = lapply(json$data,function(y){
#      data.table( clan_name = y$clan$name,
#                  clan_id = y$clan$clan_id,
#                  clan_tag = y$clan$tag,
#                  account_id = y$account_id,
#                  role = y$role,
#                  joined_at = y$joined_at,
#                  account_name = y$account_name)
#    })
#    rbindlist(ldt,fill=TRUE)
#  })



#' @title get_clans_accountinfo
#' @description Get clan info given account_id from \url{https://api.worldoftanks.com/wot/clans/accountinfo/}.
#'
#' More details at \url{https://developers.wargaming.net/reference/all/wot/clans/accountinfo/}.
#' @param account_id The number representing the \code{clan_id}.
#' @param application_id Your application_id from \url{https://developers.wargaming.net/applications/},
#' retrieved by default using \code{\link{get_application_id}}.
#' @export
get_clans_accountinfo = function( account_id, application_id = get_application_id())
{
  account_id = as.account_id(account_id)

  rbindlist(
  lapply( chunk_vector(account_id, 100L), function( account_id) {
    url = paste0("https://api.worldoftanks.com/wot/clans/accountinfo/?application_id=",
               application_id,"&account_id=",paste0(account_id,collapse = ","))

    json = fromJSON(url, flatten=TRUE)

    ldt = lapply(json$data,function(y){
      data.table( clan_name = y$clan$name,
                  clan_id = y$clan$clan_id,
                  clan_tag = y$clan$tag,
                  account_id = y$account_id,
                  role = y$role,
                  joined_at = y$joined_at,
                  account_name = y$account_name)
    })
    rbindlist(ldt,fill=TRUE)
  }),fill=TRUE)
}


#' @title get_clans_list
#' @description Get clan info given clan tag from \url{https://api.worldoftanks.com/wot/clans/list/}.
#'
#' More details at \url{https://developers.wargaming.net/reference/all/wot/clans/list/}.
#' @param search Text used to search for clans.
#' @param application_id Your application_id from \url{https://developers.wargaming.net/applications/},
#' retrieved by default using \code{\link{get_application_id}}.
#' @export
get_clans_list = function( search, #page_no, all=FALSE,  also check with the meta data, to know if there are more search results.
                           application_id = get_application_id())
{
  url = paste0("https://api.worldoftanks.com/wot/clans/list/?application_id=",
               application_id,"&language=en&search=",search)
  json_clan = fromJSON(url,flatten = TRUE)
  dt_clan = as.data.table(json_clan$data)
  dt_clan
}

################################################################################
# encyclopedia
################################################################################


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


################################################################################
# account
################################################################################

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
  dt = rbindlist(lapply( chunk_vector(account_id,100L), function(account_id){
    url = paste0("https://api.worldoftanks.com/wot/account/info/?application_id=",application_id,
                 "&account_id=",paste0(account_id,collapse = ","),
                 "&fields=-statistics.regular_team,-statistics.company,-statistics.historical,-statistics.team,-statistics.frags")
    if(!statistics)
      url = paste(url,",-statistics.clan,-statistics.clan,-statistics.all,-statistics.stronghold_defense,-statistics.stronghold_skirmish,-statistics.trees_cut,-client_language ")
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
  setnames(dt,"nickname","account_name")
  dt
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

#' @title get_account_list
#' @description Get account info given username from \url{https://api.worldoftanks.com/wot/account/list/}.
#'
#' More details at \url{https://developers.wargaming.net/reference/all/wot/account/list/}.
#' @param search Text used to search for account.
#' @param application_id Your application_id from \url{https://developers.wargaming.net/applications/},
#' retrieved by default using \code{\link{get_application_id}}.
#' @export
get_account_list = function( search, #page_no, all=FALSE,  also check with the meta data, to know if there are more search results.
                           application_id = get_application_id())
{
  url = paste0("https://api.worldoftanks.com/wot/account/list/?application_id=",
               application_id,"&language=en&search=",search)
  json_account = fromJSON(url,flatten = TRUE)
  dt_account = as.data.table(json_account$data)
  dt_account
}


################################################################################
# tanks
################################################################################


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

  if( FALSE ) { #nrow( dt_stats[,.N,keyby=c("account_id","tank_id")][N>1] ) > 0){

    warning("Duplicate dt_stat rows!!!!!!!!!!!!!!!!!!")
    browser()

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

################################################################################
# globalmap
################################################################################

#' @title get_globalmap_fronts
#' @export
get_globalmap_fronts = function(application_id = get_application_id())
{
  url = paste0("https://api.worldoftanks.com/wot/globalmap/fronts/?application_id=",application_id)
  fromJSON(url)
}

#' @title get_globalmap_provinces
#' @export
get_globalmap_provinces = function(
  front_id = "thunderstorm_us_league1",
  page_no = 1,
  application_id = get_application_id())
{
  url = paste0("https://api.worldoftanks.com/wot/globalmap/provinces/?application_id=",application_id,"&front_id=",front_id,"&page_no=",page_no)
  fromJSON(url)
}

#' @title get_globalmap_claninfo
#' @export
get_globalmap_claninfo = function(clan_id,  application_id = get_application_id() )
{
  url = paste0("https://api.worldoftanks.com/wot/globalmap/claninfo/?application_id=",application_id,"&clan_id=",clan_id)
  fromJSON(url)
}

#' @title get_globalmap_clanprovinces
#' @export
get_globalmap_clanprovinces = function(clan_id,  application_id = get_application_id() )
{
  url = paste0("https://api.worldoftanks.com/wot/globalmap/clanprovinces/?application_id=",application_id,"&clan_id=",clan_id)
  fromJSON(url)
}

#' @title get_globalmap_clanbattles
#' @export
get_globalmap_clanbattles = function(clan_id,  application_id = get_application_id() )
{
  url = paste0("https://api.worldoftanks.com/wot/globalmap/clanbattles/?application_id=",application_id,"&clan_id=",clan_id)
  fromJSON(url)
}


#' @title get_globalmap_seasons
#' @description Get globalmap account information (like fame points) from
#' \url{https://api.worldoftanks.com/wot/globalmap/seasons/}.
#'
#' More details at \url{https://developers.wargaming.net/reference/all/wot/globalmap/seasons/}.
#' @param application_id Your application_id from \url{https://developers.wargaming.net/applications/},
#' retrieved by default using \code{\link{get_application_id}}.
#' @export
get_globalmap_seasons = function(application_id = get_application_id())
{
  url = paste0("https://api.worldoftanks.com/wot/globalmap/seasons/?application_id=",
                 application_id)
  print(url)
  fromJSON(url,flatten = TRUE)
}

#' @title get_globalmap_seasonclaninfo
#' @export
get_globalmap_seasonclaninfo = function(clan_id, season_id, vehicle_level=10,  application_id = get_application_id() )
{
  url = paste0("https://api.worldoftanks.com/wot/globalmap/seasonclaninfo/?application_id=",application_id,"&clan_id=",clan_id,"&season_id=",season_id,"&vehicle_level=",vehicle_level)
  fromJSON(url)
}

#' @title get_globalmap_seasonaccountinfo
#' @export
get_globalmap_seasonaccountinfo = function(account_id, season_id, vehicle_level=10,  application_id = get_application_id() )
{
  url = paste0("https://api.worldoftanks.com/wot/globalmap/seasonaccountinfo/?application_id=",application_id,"&account_id=",account_id,"&season_id=",season_id,"&vehicle_level=",vehicle_level)
  fromJSON(url)
}

#' @title get_globalmap_seasonrating
#' @export
get_globalmap_seasonrating = function( season_id, vehicle_level=10,  application_id = get_application_id() )
{
  url = paste0("https://api.worldoftanks.com/wot/globalmap/seasonrating/?application_id=",application_id,"&season_id=",season_id,"&vehicle_level=",vehicle_level)
  fromJSON(url)
}

#' @title get_globalmap_seasonratingsneighbors
#' @export
get_globalmap_seasonratingneighbors = function(clan_id, season_id, vehicle_level=10,  application_id = get_application_id() )
{
  url = paste0("https://api.worldoftanks.com/wot/globalmap/seasonratingneighbors/?application_id=",application_id,"&clan_id=",clan_id,"&season_id=",season_id,"&vehicle_level=",vehicle_level)
  fromJSON(url)
}

#' @title get_globalmap_events
#' @export
get_globalmap_events = function( application_id = get_application_id() )
{
  url = paste0("https://api.worldoftanks.com/wot/globalmap/events/?application_id=",application_id)
  fromJSON(url)
}

#' @title get_globalmap_eventclaninfo
#' @export
get_globalmap_eventclaninfo = function(clan_id, event_id, front_id,  application_id = get_application_id() )
{
  url = paste0("https://api.worldoftanks.com/wot/globalmap/eventclaninfo/?application_id=",application_id,"&clan_id=",clan_id,"&event_id=",event_id,"&front_id=",front_id)
  fromJSON(url)
}

#' @title get_globalmap_eventaccountinfo
#' @export
get_globalmap_eventaccountinfo = function(account_id, front_id,  application_id = get_application_id() )
{
  url = paste0("https://api.worldoftanks.com/wot/globalmap/eventaccountinfo/?application_id=",application_id,"&account_id=",account_id,"&event_id=",event_id,"&front_id=",front_id)
  fromJSON(url)
}

#' @title get_globalmap_eventaccountratings
#' @export
get_globalmap_eventaccountratings = function(event_id="thunderstorm",front_id ="thunderstorm_bg",  page_no=1, limit=100, application_id = get_application_id() )
{
  dt = rbindlist( lapply( page_no, function(x){
    url = paste0("https://api.worldoftanks.com/wot/globalmap/eventaccountratings/?application_id=",application_id,
                 "&event_id=",event_id,"&front_id=",front_id,"&limit=",limit,"&page_no=",x)
    fromJSON(url,flatten = TRUE)$data

  }))

  dt
}


#' @title get_globalmap_eventaccountratingneighbors
#' @export
get_globalmap_eventaccountratingneighbors = function(account_id, event_id, front_id,  application_id = get_application_id() )
{
  url = paste0("https://api.worldoftanks.com/wot/globalmap/eventaccountratingneighbors/?application_id=",application_id,"&account_id=",account_id,"&event_id=",event_id,"&front_id=",front_id)
  fromJSON(url)
}

#' @title get_globalmap_eventrating
#' @export
get_globalmap_eventrating = function(event_id="thunderstorm",front_id ="thunderstorm_bg",  page_no=1, limit=100,  application_id = get_application_id() )
{
#  url = paste0("https://api.worldoftanks.com/wot/globalmap/eventrating/?application_id=",application_id,"&event_id=",event_id,"&front_id=",front_id)
#  fromJSON(url)

  dt = rbindlist( lapply( page_no, function(x){
    url = paste0("https://api.worldoftanks.com/wot/globalmap/eventrating/?application_id=",application_id,
                 "&event_id=",event_id,"&front_id=",front_id,"&limit=",limit,"&page_no=",x)
    fromJSON(url,flatten = TRUE)$data

  }))

  dt
}

#' @title get_globalmap_eventratingneighbors
#' @export
get_globalmap_eventratingneighbors = function(clan_id, event_id, front_id,  application_id = get_application_id() )
{
  url = paste0("https://api.worldoftanks.com/wot/globalmap/eventratingneighbors/?application_id=",application_id,"&clan_id=",clan_id,"&event_id=",event_id,"&front_id=",front_id)
  fromJSON(url)
}

#' @title get_globalmap_info
#' @export
get_globalmap_info = function( application_id = get_application_id() )
{
  url = paste0("https://api.worldoftanks.com/wot/globalmap/info/?application_id=",application_id)
  fromJSON(url)
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



#' @title get_globalmap_seasonrating
#' @description Get globalmap account information (like fame points) from
#' \url{https://api.worldoftanks.com/wot/globalmap/seasonrating/}.
#'
#' More details at \url{https://developers.wargaming.net/reference/all/wot/globalmap/seasonrating/}.
#' @param application_id Your application_id from \url{https://developers.wargaming.net/applications/},
#' retrieved by default using \code{\link{get_application_id}}.
#' @export
get_globalmap_seasonrating = function(
  season_id="thunderstorm_season",
  vehicle_level = 10,
  limit = 100,
  page_no = 1,
  application_id = get_application_id())
{
  url = paste0("https://api.worldoftanks.com/wot/globalmap/seasonrating/?application_id=",
                 application_id,"&vehicle_level=",vehicle_level,"&season_id=",season_id,"&limit=",limit,"&page_no=",page_no)
  x= fromJSON(url,flatten = TRUE)
  as.data.table(cbind(x$meta,x$data))
}




################################################################################
################################################################################
