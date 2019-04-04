#' Title
#'
#' @param channel
#' @param token
#'
#' @return
#' @export
#'
#' @examples
create_slack_channel <- function(channel,token=Sys.getenv("SLACK_API_TOKEN")){

  res <- httr::POST(url="https://slack.com/api/channels.create",
                    body=list( token=token,
                               name=tolower(channel)))
  print(res)
  invisible(channel)
}
