#' Title
#'
#' @param token token
#'
#' @export
#'get_channel_id
ping <- function(token=Sys.getenv("SLACK_API_TOKEN")){
  res <- httr::POST(url="https://slack.com/api/auth.test",
                    body=list( token=token
                    ))
  # print(httr::content(res))
  # invisible(channel)
  res$ok
}
