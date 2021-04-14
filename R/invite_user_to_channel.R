#' Add a single user to a channel
#'
#' @param user user name
#' @param channel channel name
#' @param token slack api token
#'
#' @importFrom httr POST content
#'
invite_single_user_to_channel <- function(channel,user,token=Sys.getenv("SLACK_BOT_USER_OAUTH_TOKEN")){

  res <- httr::POST(url="https://slack.com/api/conversations.invite",
                    body=list( token=token,
                               channel=get_channel_id(tolower(channel),channel = slackr::slackr_channels( bot_user_oauth_token = token)),
                               user=get_user_id(user)))
  print(httr::content(res))
  invisible(channel)
}

#' Add user to a channel
#'
#' @param users vector of user's name
#' @param channel channel name
#' @param token slack api token
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @export
#'
invite_user_to_channel <- function(channel,users,token=Sys.getenv("SLACK_BOT_USER_OAUTH_TOKEN")){

  users %>%
    map(invite_single_user_to_channel,channel=channel,token=token) %>%
    print()
    invisible(channel)
}
