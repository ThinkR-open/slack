# Mock-based tests for invite_users_to_channel + get_users_in_channel.
# We don't talk to the real Slack API; we stub httr::POST and the slackr
# helpers and inspect what would have been sent.

fake_response <- function(content, ok = TRUE) {
  body <- c(list(ok = ok), content)
  enc  <- charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
  structure(
    list(
      status_code = 200L,
      headers = structure(list(`content-type` = "application/json"),
                          class = "insensitive"),
      content = enc
    ),
    class = "response"
  )
}

# A tibble mimicking slackr::slackr_users() / slackr_channels() shape.
fake_users <- function() {
  tibble::tibble(name = c("alice", "bob", "carol"),
                 id   = c("U_ALICE", "U_BOB", "U_CAROL"))
}
fake_channels <- function() {
  tibble::tibble(name = c("general", "team-r"),
                 id   = c("C_GEN", "C_R"))
}

# Whatever httr::POST stub the test installs returns this — recorded so
# the test can inspect both endpoints (.members, .invite).
post_log <- new.env(parent = emptyenv())

with_slack_mocks <- function(members_in_channel, code) {
  post_log$calls <- list()
  fake_post <- function(url, body = NULL, ...) {
    post_log$calls <- c(post_log$calls, list(list(url = url, body = body)))
    if (grepl("conversations.members$", url)) {
      return(fake_response(list(members = as.list(members_in_channel))))
    }
    if (grepl("conversations.invite$", url)) {
      return(fake_response(list(channel = list(id = body$channel))))
    }
    fake_response(list())
  }
  testthat::with_mocked_bindings(
    POST = fake_post,
    .package = "httr",
    {
      testthat::with_mocked_bindings(
        slackr_users    = function(...) fake_users(),
        slackr_channels = function(...) fake_channels(),
        auth_test       = function(...) list(user_id = "U_ALICE"),
        .package = "slackr",
        code
      )
    }
  )
}

test_that("get_users_in_channel returns the parsed members list", {
  with_slack_mocks(
    members_in_channel = c("U_ALICE", "U_BOB"),
    code = {
      ids <- get_users_in_channel("general", token = "fake")
      expect_equal(sort(ids), c("U_ALICE", "U_BOB"))
    }
  )
})

test_that("get_users_in_channel returns character() when the channel is unknown", {
  with_slack_mocks(
    members_in_channel = character(),
    code = {
      ids <- get_users_in_channel("does-not-exist", token = "fake")
      expect_equal(ids, character())
    }
  )
})

test_that("invite_users_to_channel skips users already in the channel", {
  with_slack_mocks(
    members_in_channel = c("U_ALICE", "U_BOB"),
    code = {
      invite_users_to_channel(
        channel = "general",
        users   = c("alice", "bob", "carol"),
        token   = "fake",
        create_channel = FALSE
      )
    }
  )
  invite_call <- Filter(
    function(x) grepl("invite$", x$url),
    post_log$calls
  )
  expect_length(invite_call, 1L)
  expect_equal(invite_call[[1]]$body$users, "U_CAROL")
})

test_that("invite_users_to_channel does nothing when nobody is to add", {
  with_slack_mocks(
    members_in_channel = c("U_ALICE", "U_BOB", "U_CAROL"),
    code = {
      msg <- capture_messages(invite_users_to_channel(
        channel = "general",
        users   = c("alice", "bob", "carol"),
        token   = "fake",
        create_channel = FALSE
      ))
      expect_match(paste(msg, collapse = " "), "Nothing to do", fixed = TRUE)
    }
  )
  invite_call <- Filter(
    function(x) grepl("invite$", x$url),
    post_log$calls
  )
  expect_length(invite_call, 0L)
})
