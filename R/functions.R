#' doest this channel exist ?
#'
#' @param name name
#' @param channel channel
#'
#' @export
#'
channel_exist <- function(name,all_channel = slackr::slackr_channels()){
  length(get_channel_id(name = name,all_channel =all_channel))>0
}

#' get_channel_id
#'
#' @param name name
#'
#' @param  all_channel all_channel
#'
#' @import slackr
get_channel_id <- function(name,all_channel = slackr::slackr_channels()){
  get_id(name = name,from=all_channel)
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

#' List the user ids already in a Slack channel
#'
#' Wraps the `conversations.members` Slack API endpoint. Used by
#' [invite_users_to_channel()] to skip users who are already in the
#' target channel.
#'
#' @param channel channel name (case-insensitive).
#' @param token slack api token.
#' @param all_channel cached `slackr::slackr_channels()` result, passed
#'   through to [get_channel_id()].
#'
#' @return character vector of Slack user ids. Empty when the API call
#'   fails or the channel has no members.
#' @export
#'
#' @importFrom httr POST content
get_users_in_channel <- function(channel,
                                 token = Sys.getenv("SLACK_API_TOKEN"),
                                 all_channel = slackr::slackr_channels()) {
  channel_id <- get_channel_id(name = tolower(channel), all_channel = all_channel)
  if (length(channel_id) == 0L) return(character())

  resp <- tryCatch(
    httr::POST(
      url = "https://slack.com/api/conversations.members",
      body = list(token = token, channel = channel_id)
    ),
    error = function(e) NULL
  )
  if (is.null(resp)) return(character())

  body <- httr::content(resp)
  if (!isTRUE(body$ok)) return(character())
  members <- unlist(body$members, use.names = FALSE)
  if (is.null(members)) character() else as.character(members)
}
