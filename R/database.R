
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
save_clan_data = function( clan_id, tier=10, clanfile, path = get_path(), application_id = get_application_id() )
{
  clantime = Sys.time()

  dt = get_clanmember_data( clan_id, tier = tier, application_id = application_id )

  if( missing(clanfile))
    clanfile = paste0("clan_",clan_id,"_",str_replace_all(clantime,":","-"),".csv")

  print(file.path(path,clanfile))

  write.csv(dt,file=file.path(path,clanfile),row.names = FALSE)

  dt[]
}
