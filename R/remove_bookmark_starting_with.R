# channel = "presta_spf_audit"
# pattern = "sablier"
#' Title
#'
#' @param pattern
#' @param channel
#' @param token
#'
#' @return
#' @export
#' @importFrom stringr str_detect
#' @importFrom purrr discard
#' @examples
remove_bookmark_starting_with <- function(pattern, channel,
                                          all = slack::get_all_bookmarks_from_channel(channel = channel,token = token, all_channel = all_channel),
                                          all_channel = slackr::slackr_channels(),
                                          token=Sys.getenv("SLACK_API_TOKEN")){
  bb <- all$bookmarks %>% discard(~is.null(.x$title))
  id <- bb %>% map_chr("id")
  cond <- bb %>% map_chr("title")  %>% str_detect(pattern = pattern)

  id[cond]


  slack::remove_bookmarks_from_channel(ids = id[cond],channel = channel,token = token,all_channel = all_channel)

}
