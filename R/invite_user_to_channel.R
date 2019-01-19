#' Title
#'
#' @param user
#' @param channel
#' @param token
#'
#' @importFrom httr POST
#' @export
#'
invite_user_to_channel <- function(user,channel,token=Sys.getenv("SLACK_API_TOKEN")){

  res <- httr::POST(url="https://slack.com/api/channels.invite",
                    body=list( token=token,
                               channel=get_channel_id(channel),
                               user=get_user_id(user)))
  res
}
