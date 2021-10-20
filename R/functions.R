#' doest this channel exist ?
#'
#' @param name
#' @param channel
#'
#' @return
#' @export
#'
channel_exist <- function(name,channel = slackr::slackr_channels()){
  length(get_channel_id(name = name,channel =channel))>0
}

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
  from %>% select(name,id) %>% filter(name %in% !!name) %>% pull(id)
}
