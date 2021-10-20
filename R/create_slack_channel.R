#' Create a new slack channel
#'
#' @param channel channel name
#' @param token slack API token
#'
#' @return channel name as character
#' @export
#'
create_slack_channel <- function(channel,token=Sys.getenv("SLACK_API_TOKEN")){

  res <- httr::POST(url="https://slack.com/api/conversations.create",
                    body=list( token=token,
                               name=tolower(channel)))
  print(httr::content(res))
  invisible(channel)
}
