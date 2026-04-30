#' Add user(s) to a channel
#'
#' @param users vector of user's name
#' @param channel channel name
#' @param create_channel boolean create chanel if needed
#' @param token slack api token
#' @param current_user current user id
#'
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @export
#'
invite_users_to_channel <- function(channel, users, token=Sys.getenv("SLACK_API_TOKEN"), create_channel = TRUE,
                                    all_channel = slackr::slackr_channels(),current_user = slackr::auth_test()$user_id){

  if (create_channel &
      length(get_channel_id(tolower(channel),all_channel = all_channel)) == 0) {
    create_slack_channel(channel  = channel, token = token)
    # Sys.sleep(1)
  }

  # Skip the current user (would be a self-invite error) AND anyone
  # already in the channel (would otherwise produce a noisy
  # 'already_in_channel' error and re-send a notification).
  already_in <- get_users_in_channel(
    channel = channel, token = token, all_channel = all_channel
  )
  to_invite <- setdiff(get_user_id(users), c(current_user, already_in))

  if (length(to_invite) == 0L) {
    message(
      "Nothing to do: all requested users are already in #", channel, "."
    )
    return(invisible(channel))
  }

  httr::POST(url="https://slack.com/api/conversations.invite",
             body=list( token=token,
                        channel= get_channel_id(name = tolower(channel),all_channel = all_channel),
                        users=paste(to_invite, collapse=",")))
  # Sys.sleep(1)
  invisible(channel)
}



