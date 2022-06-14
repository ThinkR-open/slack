#' Title
#'
#' @param channel
#' @param bookmark
#' @param token
#' @importFrom httr POST content
#' @importFrom purrr map set_names
#' @export
#'
remove_all_bookmarks_from_channel <- function(channel,bookmark, token=Sys.getenv("SLACK_API_TOKEN")){

  all_id <- get_all_bookmarks_id_from_channel(channel = channel,token = token)
  res <-   all_id %>%
    map(
      ~httr::POST(url="https://slack.com/api/bookmarks.remove",
                  body=list( token= token,
                             # title=bookmark$title,
                             # emoji = bookmark$emoji,
                             # emoji = ":kekette:",
                             # type = "link",
                             # link=bookmark$link,
                             bookmark_id =.x,
                             channel_id =
                               slack::get_channel_id(name = channel)))


    ) %>% map(httr::content) %>% set_names(all_id)
  Sys.sleep(1)
  invisible(res)
}


#' Title
#'
#' @param channel
#' @param token
#' @importFrom httr POST content
#' @importFrom purrr map set_names
#' @export
#'
get_all_bookmarks_from_channel <- function(channel, token=Sys.getenv("SLACK_API_TOKEN")){

    res <- httr::POST(url="https://slack.com/api/bookmarks.list",
                    body=list( token= token,
                               # title=bookmark$title,
                               # emoji = bookmark$emoji,
                               # emoji = ":kekette:",
                               # type = "link",
                               # link=bookmark$link,
                               # bookmark_id =,
                               channel_id =
                                 slack::get_channel_id(name = channel)))
  print(out <- httr::content(res))
  Sys.sleep(1)
  invisible(out)
}



#' Title
#'
#' @param channel
#' @param token
#' @importFrom purrr map_chr
#' @export
#'
get_all_bookmarks_id_from_channel<- function(channel, token=Sys.getenv("SLACK_API_TOKEN")){


  out <- get_all_bookmarks_from_channel(channel = channel, token = token)
  out$bookmarks %>% map_chr("id")
}



edit_bookmark <- function(channel,bookmark, token=Sys.getenv("SLACK_API_TOKEN")){

  bookmark[is.na(bookmark)]<-NULL


  res <- httr::POST(url="https://slack.com/api/bookmarks.edit",
                    body=list( token= token,
                               title=bookmark$title,
                               emoji = bookmark$emoji,
                               # emoji = ":kekette:",
                               # type = "link",
                               link=  bookmark$link,
                               bookmark_id = bookmark$id,
                               channel_id =
                                 slack::get_channel_id(name = channel)))


  invisible(httr::content(res))


}

#' @importFrom dplyr bind_rows rows_update
#' @importFrom dplyr anti_join
transfo_list <- function(old,new){
  avant <-  old$bookmarks %>% bind_rows()

  nouveau_titre <-  apres <- new %>% bind_rows()

  if (nrow(avant) > 0) {
    nouveau_titre <- anti_join(apres, avant, by = "title")
  }
  apres_ok <- anti_join(apres, nouveau_titre, by = "title")

  socle <- NULL
  if (nrow(avant) > 0) {
    socle <- rows_update(avant , apres_ok, by = "title")
  }

  #  %>%
  socle %>%
    bind_rows(nouveau_titre)%>%
    asplit(1) %>%
    map(as.list)

}
