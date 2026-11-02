test_that("tracking is off locally and without a key", {
  withr::local_envvar(POSTHOG_PROJECT_KEY = "phc_test", RSTUDIO_PRODUCT = "")
  expect_false(posthog_enabled())
  expect_true(posthog_enabled(force = TRUE))

  withr::local_envvar(RSTUDIO_PRODUCT = "CONNECT")
  expect_true(posthog_enabled())

  withr::local_envvar(POSTHOG_PROJECT_KEY = "")
  expect_false(posthog_enabled())
  expect_false(posthog_enabled(force = TRUE))
})

test_that("posthog_capture sends nothing when disabled or with no person", {
  sent_requests <- 0
  local_mocked_bindings(req_perform = \(...) sent_requests <<- sent_requests + 1, .package = "httr2")

  withr::local_envvar(POSTHOG_PROJECT_KEY = "phc_test", RSTUDIO_PRODUCT = "")
  posthog_capture("dashboard_opened", "a@teachinglab.org", "test_app")
  posthog_capture("dashboard_opened", NULL, "test_app", force = TRUE)
  posthog_capture("dashboard_opened", "", "test_app", force = TRUE)
  later::run_now(1)
  expect_equal(sent_requests, 0)
})

test_that("posthog_capture posts the event body with the app property", {
  captured_body <- NULL
  local_mocked_bindings(
    req_perform = \(req) captured_body <<- req$body$data,
    .package = "httr2"
  )
  withr::local_envvar(POSTHOG_PROJECT_KEY = "phc_test", RSTUDIO_PRODUCT = "CONNECT")

  posthog_capture(
    "dashboard_closed", "a@teachinglab.org", "test_app",
    properties = list(minutes = 2.5),
    timestamp = as.POSIXct("2026-04-03 15:00:00", tz = "UTC")
  )
  later::run_now(1)

  expect_equal(captured_body$event, "dashboard_closed")
  expect_equal(captured_body$distinct_id, "a@teachinglab.org")
  expect_equal(captured_body$api_key, "phc_test")
  expect_equal(captured_body$timestamp, "2026-04-03T15:00:00.000Z")
  expect_equal(captured_body$properties$app, "test_app")
  expect_equal(captured_body$properties$minutes, 2.5)
})

test_that("a failed request warns instead of erroring", {
  local_mocked_bindings(req_perform = \(...) stop("timeout"), .package = "httr2")
  withr::local_envvar(POSTHOG_PROJECT_KEY = "phc_test", RSTUDIO_PRODUCT = "CONNECT")

  posthog_capture("dashboard_opened", "a@teachinglab.org", "test_app")
  expect_warning(later::run_now(1), "dashboard_opened failed: timeout")
})

test_that("the browser script carries the config as escaped JSON", {
  withr::local_envvar(POSTHOG_PROJECT_KEY = "phc_test")
  script <- as.character(posthog_browser_script("test_app", "o'brien\"@x.org", "siteSelect", NULL))

  expect_match(script, "posthog.init(cfg.key", fixed = TRUE)
  # A single tracked input is still a JSON array, so indexOf() works on it
  expect_match(script, "\"inputs\":[\"siteSelect\"]", fixed = TRUE)
  expect_match(script, "\"tab_input\":\"\"", fixed = TRUE)
  expect_match(script, "o'brien\\\"@x.org", fixed = TRUE)
})

test_that("track_usage logs open once and close with minutes", {
  withr::local_envvar(POSTHOG_PROJECT_KEY = "phc_test", RSTUDIO_PRODUCT = "CONNECT")
  captured_events <- list()
  local_mocked_bindings(posthog_capture = \(event, distinct_id, app, properties = list(), ...) {
    captured_events[[length(captured_events) + 1]] <<- list(event = event, distinct_id = distinct_id, app = app, properties = properties)
  })

  test_server <- function(input, output, session) {
    user_email <- shiny::reactiveVal(NULL)
    session$userData$set_email <- user_email
    track_usage(session, app = "test_app", user_email = user_email, tracked_inputs = "siteSelect")
  }

  shiny::testServer(test_server, {
    session$flushReact()
    expect_length(captured_events, 0)

    session$userData$set_email("Duncan.Gates@TeachingLab.org")
    session$flushReact()
    session$userData$set_email("someone.else@teachinglab.org")
    session$flushReact()
  })

  events <- purrr::map_chr(captured_events, "event")
  expect_equal(events, c("dashboard_opened", "dashboard_closed"))
  expect_equal(captured_events[[1]]$distinct_id, "duncan.gates@teachinglab.org")
  expect_equal(captured_events[[2]]$app, "test_app")
  expect_type(captured_events[[2]]$properties$minutes, "double")
})

test_that("track_usage does nothing off Connect", {
  withr::local_envvar(POSTHOG_PROJECT_KEY = "phc_test", RSTUDIO_PRODUCT = "")
  captured_events <- list()
  local_mocked_bindings(posthog_capture = \(event, ...) captured_events[[length(captured_events) + 1]] <<- event)

  shiny::testServer(\(input, output, session) track_usage(session, app = "test_app", user_email = \() "a@teachinglab.org"), {
    session$flushReact()
  })
  expect_length(captured_events, 0)
})
