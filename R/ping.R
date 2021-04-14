#' Title
#'
#' @param token
#'
#' @return
#' @export
#'get_channel_id
ping <- function(token=Sys.getenv("SLACK_BOT_USER_OAUTH_TOKEN")){
  res <- httr::POST(url="https://slack.com/api/auth.test",
                    body=list( token=token
                    ))
  print(httr::content(res))
  # invisible(channel)
  res$ok
}
