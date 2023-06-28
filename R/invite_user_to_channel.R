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

  httr::POST(url="https://slack.com/api/conversations.invite",
             body=list( token=token,
                        channel= get_channel_id(name = tolower(channel),all_channel = all_channel),
                        users=paste(     setdiff(  get_user_id(users),current_user )  ,collapse=",")))
  # Sys.sleep(1)
  invisible(channel)
}



