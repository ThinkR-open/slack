#' Title
#'
#' @param topic
#' @param channel
#' @param token
#' @importFrom stringr str_sub
#' @export
#'
set_channel_topic <- function(topic,channel,token=Sys.getenv("SLACK_API_TOKEN")){
  res <- httr::POST(url="https://slack.com/api/channels.setTopic",
                    body=list( token=token,
                               channel=get_channel_id(tolower(channel)),
                               topic=topic%>% str_sub(0,250)))
  res
}
