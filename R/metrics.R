# code for stuff like WN8

#' @title Calculating WN8
#' @description See \url{https://wiki.wargaming.net/en/Player_Ratings_(WoT)#The_Steps_of_WN8_-_The_Formula} for information about calculating WN8.
#' @param dt A data.table with appropriate columns.
#' @param group Prefix name of the columns used for WN8 calculations.
#' @param stats Postfix names of the columns used for WN8 calculations.
#' @param exp_values Table with WN8 expected values, uses \code{\link{get_xvm_expectedvalues}()} by default.
#' @export
calc_WN8 = function( dt, group="random",
                     stats = c("damage_dealt", "spotted", "frags", "dropped_capture_points", "wins", "battles"),
                     exp_values = get_xvm_expectedvalues())
{

  # include calculations for rWIN, rDAMAGE, rSPOT, rFRAG, rDEF,
  # step 1
#rDAMAGE = avgDmg     / expDmg
#rSPOT   = avgSpot    / expSpot
#rFRAG   = avgFrag    / expFrag
#rDEF    = avgDef     / expDef
#rWIN    = avgWinRate / expWinRate


  #step 2
#  rWINc    = max(0,                     (rWIN    - 0.71) / (1 - 0.71) )
#rDAMAGEc = max(0,                     (rDAMAGE - 0.22) / (1 - 0.22) )
#rFRAGc   = max(0, min(rDAMAGEc + 0.2, (rFRAG   - 0.12) / (1 - 0.12)))
#rSPOTc   = max(0, min(rDAMAGEc + 0.1, (rSPOT   - 0.38) / (1 - 0.38)))
#rDEFc    = max(0, min(rDAMAGEc + 0.1, (rDEF    - 0.10) / (1 - 0.10)))

  # step 3
  # WN8 = 980*rDAMAGEc + 210*rDAMAGEc*rFRAGc + 155*rFRAGc*rSPOTc + 75*rDEFc*rFRAGc + 145*MIN(1.8,rWINc)


  dt_xvm = NULL

  dt[,paste(group, stats, sep="."),with=FALSE]

  setnames(dt_wn8,c("account_id","tank_id","DateTime",stats))
 dt_wn8 = merge(dt_wn8,dt_xvm,by.x="tank_id",by.y="IDNum",all.x=TRUE)


  if(!missing(dt_xvm)){
    dt_wn8 = dt[,c("account_id","tank_id","DateTime",paste(group, stats, sep=".")),with=FALSE]
    setnames(dt_wn8,c("account_id","tank_id","DateTime",stats))
    dt_wn8 = merge(dt_wn8,dt_xvm,by.x="tank_id",by.y="IDNum",all.x=TRUE)
  } else {
    dt_wn8 = dt[,c("account_id","tank_id","DateTime","expDef","expFrag","expSpot","expDamage","expWinRate",paste(group, stats, sep=".")),with=FALSE]
    setnames(dt_wn8,c("account_id","tank_id","DateTime","expDef","expFrag","expSpot","expDamage","expWinRate",stats))
  }





  #  if(is.null(names(player_stats)))

  if(length(player_stats)==6){
    names(player_stats) <- c("damage_dealt", "spotted", "frags", "dropped_capture_points", "wins", "battles")
  } else if(length(player_stats)==5){
    names(player_stats) <- c( "dropped_capture_points", "frags", "spotted", "damage_dealt", "wins" )
    player_stats$wins = player_stats$wins/100
    player_stats$battles=1
  }

#  if(is.null(names(exp_values)))
    names(exp_values) <- c( "expDef", "expFrag", "expSpot", "expDamage", "expWinRate" )

  rDAMAGEc = pmax(0, (player_stats$damage_dealt / (player_stats$battles * exp_values$expDamage) - 0.22) / (1 - 0.22) )
  rFRAGc   = pmax(0, pmin(rDAMAGEc + 0.2, (player_stats$frags / (player_stats$battles * exp_values$expFrag) - 0.12) / (1 - 0.12)))

  WN8 = 980*rDAMAGEc +
        210*rDAMAGEc*rFRAGc +
        155*rFRAGc*pmax(0, pmin(rDAMAGEc + 0.1, (player_stats$spotted / (player_stats$battles * exp_values$expSpot) - 0.38) / (1 - 0.38))) +
        75*pmax(0, pmin(rDAMAGEc + 0.1, (player_stats$dropped_capture_points / (player_stats$battles * exp_values$expDef ) - 0.10) / (1 - 0.10)))*rFRAGc +
        145*pmin(1.8,pmax(0,(player_stats$wins / (player_stats$battles * exp_values$expWinRate/100) - 0.71) / (1 - 0.71) ))

  WN8
}
