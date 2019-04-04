#' Title
#'
#' @param user
#' @param channel
#' @param token
#'
#' @importFrom httr POST
#' @export
#'
invite_single_user_to_channel <- function(user,channel,token=Sys.getenv("SLACK_API_TOKEN")){

  res <- httr::POST(url="https://slack.com/api/channels.invite",
                    body=list( token=token,
                               channel=get_channel_id(tolower(channel)),
                               user=get_user_id(user)))
  res
}

#' Title
#'
#' @param users
#' @param channel
#' @param token
#'
#' @return
#' @export
#'
#' @examples
invite_user_to_channel <- function(channel,users,token=Sys.getenv("SLACK_API_TOKEN")){

  users %>%
    map(invite_single_user_to_channel,channel=channel,token=token) %>%
    print()
    channel
}
