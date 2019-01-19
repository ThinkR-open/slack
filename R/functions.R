#' @param name
#'
#' @param channel
#'
#' @import slackr
get_channel_id <- function(name,channel = slackr::slackr_channels()){
  get_id(name = name,from=channel)
}
#' Title
#'
#' @param name
#' @param users
#'
#' @return
#' @export
#'
#' @examples
get_user_id <- function(name,users = slackr::slackr_users()){
  get_id(name = name,from=users)
}
#' Title
#'
#' @param name
#' @param from
#' @importFrom dplyr select filter pull
#'
get_id <- function(name,from){
  from %>% select(name,id) %>% filter(name == !!name) %>% pull(id)
}
