# PostHog usage tracking for Shiny dashboards. One call in the server,
# tlShiny::track_usage(), sends:
#   - dashboard_opened / dashboard_closed from R (ad blockers can't stop
#     these, and dashboard_closed carries the session length)
#   - tab_viewed / input_changed from the browser, only for the inputs named
#     in `tracked_inputs`, plus PostHog's own pageviews and click autocapture
# Every event carries an `app` property so one PostHog project holds every
# dashboard. Nothing is sent unless the app runs on Posit Connect and
# POSTHOG_PROJECT_KEY is set, so local runs stay out of the data.

posthog_host <- "https://us.i.posthog.com"

#' Is PostHog tracking switched on for this process?
#'
#' TRUE on Posit Connect (`RSTUDIO_PRODUCT` is `CONNECT`) when
#' `POSTHOG_PROJECT_KEY` is set. `force = TRUE` skips the Connect check, for
#' testing a local run against PostHog.
#'
#' @param force Track outside Connect too
#' @return Logical
#' @export
posthog_enabled <- function(force = FALSE) {
  has_key <- nzchar(Sys.getenv("POSTHOG_PROJECT_KEY"))
  on_connect <- identical(Sys.getenv("RSTUDIO_PRODUCT"), "CONNECT")
  has_key && (on_connect || isTRUE(force))
}

#' Send one event to PostHog from R
#'
#' Posts to the capture endpoint later, off the current reactive flush, so a
#' slow or failing request never holds up the session. Failures are warnings.
#'
#' @param event Event name, e.g. "dashboard_opened"
#' @param distinct_id The person, normally their email
#' @param app App name stored in the `app` property
#' @param properties Named list of extra properties
#' @param timestamp POSIXct time of the event, defaults to now
#' @param force Send even outside Connect (see [posthog_enabled()])
#' @return NULL, invisibly
#' @export
posthog_capture <- function(event, distinct_id, app, properties = list(), timestamp = Sys.time(), force = FALSE) {
  if (!posthog_enabled(force = force) || is.null(distinct_id) || !nzchar(distinct_id)) {
    return(invisible(NULL))
  }

  body <- list(
    api_key = Sys.getenv("POSTHOG_PROJECT_KEY"),
    event = event,
    distinct_id = distinct_id,
    timestamp = format(timestamp, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    properties = c(list(app = app, source = "shiny_server"), properties)
  )

  later::later(\() {
    tryCatch(
      httr2::request(posthog_host) |>
        httr2::req_url_path("/i/v0/e/") |>
        httr2::req_body_json(body, auto_unbox = TRUE) |>
        httr2::req_timeout(5) |>
        httr2::req_perform(),
      error = \(e) warning("posthog_capture: ", event, " failed: ", conditionMessage(e), call. = FALSE)
    )
  })
  invisible(NULL)
}

#' Track a Shiny session in PostHog
#'
#' Call once at the top of the server function. Once `user_email()` returns
#' an email it logs `dashboard_opened`, loads PostHog in the browser already
#' identified as that email, and on session end logs `dashboard_closed` with
#' `minutes`. Changes to `tracked_inputs` are sent from the browser as
#' `input_changed` (`tab_input` changes as `tab_viewed`), debounced so a
#' multi-select that changes several times in a second sends one event.
#'
#' @param session The Shiny session
#' @param app App name, e.g. "ongoing_report"
#' @param user_email A reactive or function returning the viewer's email.
#'   Defaults to `session$user` (the Connect login)
#' @param tracked_inputs Input ids whose changes are sent
#' @param tab_input The navbar/tabset input id, if any
#' @param force Track outside Connect too (see [posthog_enabled()])
#' @return NULL, invisibly
#' @export
track_usage <- function(session, app, user_email = \() session$user, tracked_inputs = character(0), tab_input = NULL, force = FALSE) {
  if (!posthog_enabled(force = force)) {
    return(invisible(NULL))
  }

  session_started <- Sys.time()
  tracked_email <- NULL

  shiny::observeEvent(user_email(), {
    tracked_email <<- tolower(user_email())
    posthog_capture("dashboard_opened", tracked_email, app, force = force)
    shiny::insertUI(
      selector = "head",
      where = "beforeEnd",
      ui = posthog_browser_script(app, tracked_email, tracked_inputs, tab_input),
      immediate = TRUE,
      session = session
    )
  }, once = TRUE, ignoreNULL = TRUE)

  session$onSessionEnded(\() {
    minutes <- round(as.numeric(difftime(Sys.time(), session_started, units = "mins")), 1)
    posthog_capture("dashboard_closed", tracked_email, app, properties = list(minutes = minutes), force = force)
  })

  invisible(NULL)
}

# The PostHog loader snippet, initialised as the known user, with the Shiny
# input listener. Values are passed through jsonlite so quotes in an email or
# input id can't break the script
posthog_browser_script <- function(app, email, tracked_inputs, tab_input) {
  config <- jsonlite::toJSON(
    list(
      key = Sys.getenv("POSTHOG_PROJECT_KEY"),
      host = posthog_host,
      app = app,
      email = email,
      inputs = as.list(tracked_inputs),
      tab_input = if (is.null(tab_input)) { "" } else { tab_input }
    ),
    auto_unbox = TRUE
  )

  shiny::tags$script(shiny::HTML(paste0(
    posthog_loader_js,
    "\n(function(cfg) {
  posthog.init(cfg.key, {
    api_host: cfg.host,
    person_profiles: 'identified_only',
    bootstrap: {distinctID: cfg.email, isIdentifiedID: true},
    loaded: function(ph) { ph.register({app: cfg.app, source: 'shiny_browser'}); }
  });
  var timers = {};
  $(document).on('shiny:inputchanged', function(event) {
    var is_tab = cfg.tab_input !== '' && event.name === cfg.tab_input;
    if (!is_tab && cfg.inputs.indexOf(event.name) === -1) { return; }
    var value = event.value;
    // A filter emptied by the server (dependent pickers reset when the site
    // changes) is not a choice the viewer made
    if (value === null || value === undefined || value === '' || (Array.isArray(value) && value.length === 0)) { return; }
    clearTimeout(timers[event.name]);
    timers[event.name] = setTimeout(function() {
      if (is_tab) {
        posthog.capture('tab_viewed', {tab: value});
      } else {
        posthog.capture('input_changed', {input: event.name, value: value});
      }
    }, 1000);
  });
})(", config, ");"
  )))
}

# PostHog's standard array.js loader (from their web install docs)
posthog_loader_js <- "!function(t,e){var o,n,p,r;e.__SV||(window.posthog=e,e._i=[],e.init=function(i,s,a){function g(t,e){var o=e.split(\".\");2==o.length&&(t=t[o[0]],e=o[1]),t[e]=function(){t.push([e].concat(Array.prototype.slice.call(arguments,0)))}}(p=t.createElement(\"script\")).type=\"text/javascript\",p.crossOrigin=\"anonymous\",p.async=!0,p.src=s.api_host.replace(\".i.posthog.com\",\"-assets.i.posthog.com\")+\"/static/array.js\",(r=t.getElementsByTagName(\"script\")[0]).parentNode.insertBefore(p,r);var u=e;for(void 0!==a?u=e[a]=[]:a=\"posthog\",u.people=u.people||[],u.toString=function(t){var e=\"posthog\";return\"posthog\"!==a&&(e+=\".\"+a),t||(e+=\" (stub)\"),e},u.people.toString=function(){return u.toString(1)+\".people (stub)\"},o=\"init capture register register_once register_for_session unregister unregister_for_session getFeatureFlag getFeatureFlagPayload isFeatureEnabled reloadFeatureFlags updateEarlyAccessFeatureEnrollment getEarlyAccessFeatures on onFeatureFlags onSessionId getSurveys getActiveMatchingSurveys renderSurvey canRenderSurvey getNextSurveyStep identify setPersonProperties group resetGroups setPersonPropertiesForFlags resetPersonPropertiesForFlags setGroupPropertiesForFlags resetGroupPropertiesForFlags reset get_distinct_id getGroups get_session_id get_session_replay_url alias set_config startSessionRecording stopSessionRecording sessionRecordingStarted captureException loadToolbar get_property getSessionProperty createPersonProfile opt_in_capturing opt_out_capturing has_opted_in_capturing has_opted_out_capturing clear_opt_in_out_capturing debug\".split(\" \"),n=0;n<o.length;n++)g(u,o[n]);e._i.push([i,s,a])},e.__SV=1)}(document,window.posthog||[]);"
