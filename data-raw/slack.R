library(slackr)
slackr_setup()
packageVersion("slackr")
slackr_channels() %>% View
slackr("coucou")
slackr("je sui un message envoyé par R")

get_channel_id <- function(name,channel = slackr::slackr_channels()){
  get_id(name = name,from=channel)
}
get_user_id <- function(name,users = slackr::slackr_users()){
  get_id(name = name,from=users)
}
get_id <- function(name,from){
  from %>% select(name,id) %>% filter(name == !!name) %>% pull(id)
}
get_channel_id("banana")
get_user_id("colin_")

invite_user_to_channel <- function(user,channel,token=Sys.getenv("SLACK_API_TOKEN")){

res <- httr::POST(url="https://slack.com/api/channels.invite",
                  body=list( token=token,
                             channel=get_channel_id(channel),
                             user=get_user_id(user)))
res
}
# https://slack.com/api/channels.invite


return(invisible(res))
