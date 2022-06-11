#' add_bookmark_to_channel
#'
#' @param channel channel
#' @param bookmark as list
#' @param token slack api token
#' @importFrom httr POST
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#'
#' add_bookmark_to_channel(
#' channel = "bac_a_sable_2010bis",
#' bookmark = list(
#'   title = "hophophop4",
#'   link = "https://www.google.fr",
#'   emoji = ":kekette:"
#' )
#' )
#'
#'
#'
#'
#'
#'
#' }
#'
#'
#'
#'
#'
#'
add_bookmark_to_channel <- function(channel,bookmark, token=Sys.getenv("SLACK_API_TOKEN")){

  # bookmark <- list(title="hophophop3",link = "https://www.google.fr",emoji = NULL)
  # channel <- "random"
  res <- httr::POST(url="https://slack.com/api/bookmarks.add",
                    body=list( token= Sys.getenv("SLACK_TOKEN"),
                               title=bookmark$title,
                               emoji = bookmark$emoji,
                               # emoji = ":kekette:",
                               type = "link",
                               link=bookmark$link,

                               channel_id =
                                 slack::get_channel_id(name = channel)))
  print(httr::content(res))
  Sys.sleep(1)
  invisible(channel)
}



#' Title
#'
#' @param channel
#' @param bookmarks
#' @param token
#'
#' @return
#' @export
#'
#' @examples
add_bookmarks_to_channel <- function(channel,bookmarks, token=Sys.getenv("SLACK_API_TOKEN")){


  for (bookmark in bookmarks){

  add_bookmark_to_channel(channel = channel,token = token,bookmark = bookmark)

  }
}
