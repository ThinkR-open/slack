#' Title
#'
#' @param name
#' @param token
#'
#' @return
#' @export
#'
#' @examples
create_slack_channel <- function(name,token=Sys.getenv("SLACK_API_TOKEN")){

  res <- httr::POST(url="https://slack.com/api/channels.create",
                    body=list( token=token,
                               name=tolower(name)))
  res
}
