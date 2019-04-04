#' Set channel topic
#'
#' @param channel channel name
#' @param topic topic to set
#' @param token slack api token
#'
#' @importFrom stringr str_sub
#' @export
#'
set_channel_topic <- function(channel,topic,token=Sys.getenv("SLACK_API_TOKEN")){
  res <- httr::POST(url="https://slack.com/api/channels.setTopic",
                    body=list( token=token,
                               channel=get_channel_id(tolower(channel)),
                               topic=topic%>% str_sub(0,250)))
  print(res)
  invisible(channel)
}
