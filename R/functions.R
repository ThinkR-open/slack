#' doest this channel exist ?
#'
#' @param name name
#' @param channel channel
#'
#' @export
#'
channel_exist <- function(name,channel = slackr::slackr_channels()){
  length(get_channel_id(name = name,channel =channel))>0
}

#' get_channel_id
#'
#' @param name name
#'
#' @param  channel channel
#'
#' @import slackr
get_channel_id <- function(name,channel = slackr::slackr_channels()){
  get_id(name = name,from=channel)
}
#' Title
#'
#' @param name name
#' @param users users
#'
#' @export
#'
get_user_id <- function(name,users = slackr::slackr_users()){
  get_id(name = name,from=users)
}
#' Title
#'
#' @param name name
#' @param from from
#' @importFrom dplyr select filter pull
#'
get_id <- function(name,from){
  from %>% select(name,id) %>% filter(name %in% !!name) %>% pull(id)
}
