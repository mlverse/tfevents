#' Logs hyperparameters configuration
#'
#' Logs the hyperaparameter configuration for a HyperParameter tuning experiment.
#' It allows you to define the domain for each hyperparameters and what are the
#' metrics that should be shown in the TensorBoard UI, along with configuring
#' their display name and descriptions.
#'
#' @section Recommendations:
#'
#' When loging hyperparameter tuning experiments, the log directory organization
#' is:
#'
#' ```
#' - root:
#'  - log_hparams_config(...)
#'  - run1:
#'    - log_hparams(...)
#'    - log_event(...)
#'  - run2:
#'    - log_hparams(...)
#'    - log_event(...)
#' ```
#' Ie you should have a root logdir that will only contain the hyperaparameter
#' config log, as created with [log_hparams_config()]. Then each run in the
#' experiment will have it's own logdir as a child directory of the root logdir.
#'
#' @param hparams A list of `hparams` objects as created by [hparams_hparam()].
#' @param metrics A list of `metrics` objects as created by [hparams_metric()].
#'   These metrics will be tracked by TensorBoard UI when displaying the
#'   hyperparameter tuning table.
#' @inheritParams log_hparams
#'
#' @seealso [log_hparams()]
#' @returns Invisibly returns the HParam conffuration data as a `summary` object.
#'
#' @export
log_hparams_config <- function(hparams, metrics, time_created_secs = get_wall_time()) {
  log_event(
    summary_hparams_config(hparams, metrics, time_created_secs),
    step = 0
  )
}

#' @describeIn log_hparams_config For advanced users only. Creates a hyperaparameter
#'   configuration summary that can be logged with [log_event()].
#' @export
summary_hparams_config <- function(hparams, metrics, time_created_secs = get_wall_time()) {
  summary_values(
    summary_metadata(
      plugin_name = "hparams",
      plugin_content = new_hparams_hparams_plugin_data(
        version = 0,
        experiment = as_hparams_experiment(hparams, metrics, time_created_secs),
        session_start_info = NA
      )
    ),
    tag = "_hparams_/experiment", # this tag is recognized by tensorboard, can't change
    class = "summary_hparams_config"
  )
}

as_hparams_experiment <- function(hparams, metrics, time_created_secs) {
  hparams <- lapply(hparams, function(hparam) {
    info <- list(
      name = hparam$name,
      display_name = hparam$display_name,
      description = hparam$description
    )

    if (!rlang::is_named(hparam$domain)) {
      info$domain_discrete <- hparam$domain
    } else {
      info$domain_interval <- new_hparams_interval(
        min_value = hparam$domain["min_value"],
        max_value = hparam$domain["max_value"]
      )
    }

    do.call(hparams_hparam_info, info)
  })

  metrics <- lapply(metrics, function(metric) {
    new_hparams_metric_info(
      name = new_hparams_metric_name(group = metric$group, tag = metric$tag),
      display_name = metric$display_name,
      description = metric$description,
      dataset_type = metric$dataset_type
    )
  })

  new_hparams_experiment(
    name = NA,
    description = NA,
    user = NA,
    time_created_secs = time_created_secs,
    hparam_infos = list(vec_c(!!!hparams)),
    metric_infos = list(vec_c(!!!metrics))
  )
}

#' Defines a HParam
#'
#' Hparam object are used to describe names and domains of hyperparameters so
#' TensorBoard UI can show additional information about them.
#'
#' @param name Name of the hyperparameter.
#' @param domain A list of values that can be assumed by the hyperparameter.
#'   It can be `character()`, `numeric()` or `logical()` vector. You can also
#'   pass a named numeric vector with eg `c(min_value = 0, max_value = 10)` in
#'   this case, any value in this range is accepted.
#' @param display_name Display name of the hparameter for the TensorBoard UI.
#'   By default it's identical to the name.
#' @param description Parameter description. Shown in tooltips around the
#'   TensorBoard UI.
#'
#' @note A list of `hparam` values can be passed to [log_hparams_config()] so
#'   you define the hyperparameters that are tracked by the experiment.
#'
#' @returns A `hparams_hparam` object.
#' @examples
#' hparams_hparam("optimizer", domain = c("adam", "sgd"))
#' hparams_hparam("num_units", domain = c(128, 512, 1024))
#' hparams_hparam("use_cnn", domain = c(TRUE, FALSE))
#' hparams_hparam("dropout", domain = c(min_value = 0, max_value = 0.5))
#' @export
hparams_hparam <- function (name, domain = NA, display_name = name, description = name) {
  structure(list(
    name = name,
    domain = domain,
    display_name = display_name,
    description = description
  ), class = "hparams_hparam")
}

#' Defines a Metric
#'
#' Metric objects are passed to [log_hparams_config()] in order to define the
#' collection of scalars that will be displayed in the HParams tab in TensorBoard.
#'
#' @param tag The tag name of the scalar summary that corresponds to this
#'  metric.
#' @param group An optional string listing the subdirectory under the
#'   session's log directory containing summaries for this metric.
#'   For instance, if summaries for training runs are written to
#'   events files in `ROOT_LOGDIR/SESSION_ID/train`, then `group`
#'   should be `"train"`. Defaults to the empty string: i.e.,
#'   summaries are expected to be written to the session logdir.
#' @param display_name An optional human-readable display name.
#' @param description An optional Markdown string with a human-readable
#'   description of this metric, to appear in TensorBoard.
#' @param dataset_type dataset_type: Either `"training"` or `"validation`, or
#'   `NA`.
#'
#' @returns A `hparams_metric` object.
#' @examples
#' hparams_metric("loss", group = "train")
#' hparams_metric("acc")
#' @export
hparams_metric <- function(tag, group = NA,
                           display_name = tag,
                           description = tag,
                           dataset_type = NA) {

  if (is.na(dataset_type)) {
    dataset_type <- "unknown"
  }

  structure(list(
    tag = tag, group = group,
    display_name = display_name,
    description = description,
    dataset_type = dataset_type
  ), class = "hparams_metric")
}

#' Log hyperaparameters
#'
#' @param ... Named values of hyperparameters.
#' @param trial_id A name for the current trail. by default it's the hash of the
#'   hparams names and values.
#' @param time_created_secs The time the experiment is created in seconds
#'   since the UNIX epoch.
#'
#' @details This function should only be called once in a logdir and it will
#' record the set of hyperparameters used in that run. Undefined behavior can
#' happen if it's called more than once in a logdir - specially how TensorBoard
#' behaves during visualization.
#'
#' @returns A hyperparameter summary. USed for the side effects of logging the
#'   hyperparameter to the logdir.
#'
#' @seealso [log_hparams_config()]
#'
#' @examples
#' temp <- tempfile()
#' with_logdir(temp, {
#'   log_hparams(optimizer = "adam", num_units = 16)
#' })
#' @export
log_hparams <- function(..., trial_id = NA, time_created_secs = get_wall_time()) {
  log_event(summary_hparams(..., trial_id = trial_id, time_created_secs = time_created_secs), step = 0)
}

#' @describeIn log_hparams For advanced users only. It's recommended to use the `log_hparams()`
#' function instead. Creates a hyperparameter summary that can be written with `log_event()`.
#'
#' @export
summary_hparams <- function(..., trial_id = NA, time_created_secs = get_wall_time()) {
  hparams <- rlang::dots_list(..., .homonyms = "error")
  stopifnot(rlang::is_named(hparams))
  if (is.na(trial_id)) {
    trial_id <- digest::digest(hparams, algo = "sha256")
  }
  summary_values(
    metadata = summary_metadata(
      plugin_name = "hparams",
      plugin_content = new_hparams_hparams_plugin_data(
        version = 0,
        session_start_info = new_hparams_session_start_info(
          group_name = trial_id,
          start_time_secs = time_created_secs,
          hparams = list(hparams),
          model_uri = NA,
          monitor_url = NA
        ),
        experiment = NA
      )
    ),
    tag = "_hparams_/session_start_info", # this tag is read by TensorBoard, don't change.
    class = "summary_hparams_config"
  )
}

new_hparams_hparams_plugin_data <- function(version = integer(),
                                            experiment = new_hparams_experiment(),
                                            session_start_info = new_hparams_session_start_info()) {
  new_rcrd(
    fields = list(
      version = version,
      experiment = vec_cast(experiment, new_hparams_experiment()),
      session_start_info = vec_cast(session_start_info, new_hparams_session_start_info())
    ),
    class = "hparams_hparams_plugin_data"
  )
}

new_hparams_session_start_info <- function(group_name = character(),
                                           start_time_secs = integer(),
                                           hparams = list(),
                                           model_uri = character(),
                                           monitor_url = character()) {
  new_rcrd(
    fields = list(
      group_name = vec_cast(group_name, character()),
      start_time_secs = vec_cast(start_time_secs, integer()),
      hparams = vec_cast(hparams, list()),
      model_uri = vec_cast(model_uri, character()),
      monitor_url = vec_cast(monitor_url, character())
    ),
    class = "hparams_session_start_info"
  )
}

new_hparams_experiment <- function(name = character(), description = character(),
                           user = character(), time_created_secs = integer(),
                           hparam_infos = new_list_of(ptype=new_hparams_hparam_info()),
                           metric_infos = new_list_of(ptype=new_hparams_metric_info())) {
  new_rcrd(
    fields = list(
      name = vec_cast(name, character()),
      description = vec_cast(description, character()),
      user = vec_cast(user, character()),
      time_created_secs = vec_cast(time_created_secs, integer()),
      hparam_infos = hparam_infos,
      metric_infos = metric_infos
    ),
    class = "hparams_experiment"
  )
}

new_hparams_metric_info <- function(name = new_hparams_metric_name(),
                                    display_name = character(),
                                    description = character(),
                                    dataset_type = character()) {
  new_rcrd(
    fields = list(
      name = name,
      display_name = vec_cast(display_name, character()),
      description = vec_cast(description, character()),
      dataset_type = vec_cast(dataset_type, character())
    ),
    class = "hparams_metric_info"
  )
}

new_hparams_metric_name <- function(group = character(), tag = character()) {
  new_rcrd(
    fields = list(
      group = vec_cast(group, character()),
      tag = vec_cast(tag, character())
    ),
    class = "hparams_metric_name"
  )
}

hparams_hparam_info <- function(name, display_name = rep("", length(name)),
                                description = rep("", length(name)),
                                type = NA, ..., domain_discrete = NA,
                                domain_interval = NA) {

  stopifnot(!is.na(name))
  if (is.na(type)) {
    if (!is.na(domain_interval)) {
      type <- "float64"
    } else if (!rlang::is_na(domain_discrete)) {
      if (is.character(domain_discrete)) {
        type <- "string"
      } else if (is.numeric(domain_discrete)) {
        type <- "float64"
      } else if (is.logical(domain_discrete)) {
        type <- "bool"
      }
    }
  }

  if (!rlang::is_na(domain_discrete)) {
    domain_discrete <- list(domain_discrete)
  }

  new_hparams_hparam_info(
    name = vec_cast(name, character()),
    display_name = vec_cast(display_name, character()),
    description = vec_cast(description, character()),
    type = vec_cast(type, character()),
    domain_discrete = vec_cast(domain_discrete, list()),
    domain_interval = vec_cast(domain_interval, new_hparams_interval())
  )
}

new_hparams_hparam_info <- function(name = character(), display_name = character(),
                                    description = character(), type = character(),
                                    ...,
                                    domain_discrete = list(),
                                    domain_interval = new_hparams_interval()) {
  new_rcrd(
    fields = list(
      name = name,
      display_name = display_name,
      description = description,
      type = type,
      domain_discrete = domain_discrete,
      domain_interval = domain_interval
    ),
    class = "hparams_hparam_info"
  )
}

new_hparams_interval <- function(min_value = numeric(), max_value = numeric()) {
  new_rcrd(
    fields = list(
      min_value = min_value,
      max_value = max_value
    ),
    class = "hparams_interval"
  )
}

#' @export
vec_ptype2.hparams_session_start_info.hparams_session_start_info <- function(x, y, ...) {
  new_hparams_session_start_info()
}
#' @export
vec_cast.hparams_session_start_info.hparams_session_start_info <- function(x, to, ...) {
  x
}

#' @export
vec_ptype2.hparams_hparams_plugin_data.list <- function(x, y, ...) {
  list()
}
#' @export
vec_cast.list.hparams_hparams_plugin_data <- function(x, to, ...) {
  if (is.na(x))
    return(NA)

  if (vec_size(x) == 1)
    return(list(x))
}

#' @export
vec_ptype2.hparams_experiment.hparams_experiment <- function(x, y, ...) {
  new_hparams_session_start_info()
}
#' @export
vec_cast.hparams_experiment.hparams_experiment <- function(x, to, ...) {
  x
}

#' @export
vec_ptype2.tfevents_summary_values.summary_hparams_config <- function(x, y, ...) {
  new_summary_values()
}
#' @export
vec_ptype2.summary_hparams_config.tfevents_summary_values <- function(x, y, ...) {
  new_summary_values()
}

#' @export
vec_ptype2.hparams_interval.hparams_interval <- function(x, y, ...) {
  new_hparams_interval()
}
#' @export
vec_cast.hparams_interval.hparams_interval <- function(x, to, ...) {
  x
}

#' @export
vec_ptype2.hparams_hparam_info.hparams_hparam_info <- function(x, y, ...) {
  new_hparams_hparam_info()
}
#' @export
vec_cast.hparams_hparam_info.hparams_hparam_info <- function(x, to, ...) {
  x
}

