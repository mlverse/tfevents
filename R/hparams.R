hparams_config <- function(hparams, metrics, time_created_secs = get_wall_time()) {
  log_event(
    "_hparams_/experiment" =
      summary_hparams_config(hparams, metrics, time_created_secs),
    step = 0
  )
}

summary_hparams_config <- function(hparams, metrics) {
  new_summary_values(
    summary_metadata(
      plugin_name = "hparams",
      plugin_content = new_hparams_hparams_plugin_data(
        version = 1,
        experiment = as_hparams_experiment(hparams, metrics, time_created_secs)
      )
    )
  )
}

as_hparams_experiment <- function(hparams, metrics, time_created_secs) {
  hparams <- lapply(x$hparams, function(hparam) {
    info <- list(
      name = hparam$name,
      display_name = hparam$display_name,
      description = hparam$description
    )

    if (is.character(hparam$domain)) {
      info$domain_discrete <- hparam$domain
    } else {
      info$domain_interval <- new_hparams_interval(
        min_value = hparam$domain[1],
        max_value = hparam$domain[2]
      )
    }

    do.call(new_hparams_hparam_info, info)
  })

  metrics <- lapply(x$metrics, function(metric) {
    new_hparams_metric_info(
      name = new_hparams_metric_name(group = metric$group, tag = metric$tag),
      display_name = metric$name,
      description = metric$description,
      dataset_type = metric$dataset_type
    )
  })

  new_hparams_experiment(
    name = NA,
    description = NA,
    user = NA,
    time_created_secs = wall_time,
    hparam_infos = vec_c(!!!hparams),
    metric_infos = vec_c(!!!metrics)
  )
}

hparams_hparam <- function (name, domain = NA, display_name = NA, description = NA) {
  structure(data = list(
    name = name,
    domain = domain,
    display_name = display_name,
    description = description
  ), class = "hparams_hparam")
}

hparams_metric <- function(tag, group = NA,
                           display_name = NA,
                           description = NA,
                           dataset_type = NA) {
  structure(list(
    tag = tag, group = group,
    display_name = display_name,
    description = description,
    dataset_type = dataset_type
  ), class = "hparams_metric")
}

hparams_hparams <- function() {
}

new_hparams_hparams_plugin_data <- function(version = integer(),
                                            experiment = new_hparams_experiment()) {
  new_rcrd(
    fields = list(
      version = version,
      experiment = experiment
    ),
    class = "hparams_hparams_plugin_data"
  )
}

new_hparams_experiment <- function(name = character(), description = character(),
                           user = character(), time_created_secs = integer(),
                           hparam_infos = new_list_of(ptype=new_hparams_hparam_info()),
                           metric_infos = new_list_of(ptype=new_hparams_metric_info())) {
  new_rcrd(
    fields = list(
      name = name,
      description = description,
      user = user,
      time_created_secs = time_created_secs,
      hparam_infos = hparam_infos,
      metric_infos = metric_infos
    ),
    class = "hparams_experiment"
  )
}

new_hparams_metric_info <- function(name = new_hparams_matric_name(),
                                    display_name = character(),
                                    description = character(),
                                    dataset_type = character()) {
  new_rcrd(
    fields = list(
      name = name,
      display_name = display_name,
      description = decription,
      dataset_type = dataset_type
    ),
    class = "hparams_metric_info"
  )
}

new_hparams_metric_name <- function(group = character(), tag = character()) {
  new_rcrd(
    fields = list(
      group = group,
      tag = tag
    ),
    class = "hparams_metric_name"
  )
}

hparams_hparam_info <- function(name, display_name = rep("", length(name)),
                                description = rep("", length(name)),
                                type = NA, ..., domain_discrete = NA,
                                domain_interval = NA) {
  new_hparams_hparam_info(
    name = vec_cast(name, character()),
    display_name = vec_cast(display_name, character()),
    description = vec_cast(description, character()),
    type = vec_cast(type, character()),
    domain_discrete = vec_cast(domain_discrete, new_list_of(ptype = character())),
    domain_interval = vec_cast(domain_interval, new_hparams_interval())
  )
}

new_hparams_hparam_info <- function(name = character(), display_name = character(),
                                    description = character(), type = character(),
                                    ...,
                                    domain_discrete = new_list_of(ptype = character()),
                                    domain_interval = new_hparams_interval()) {
  new_rcrd(
    fields = list(
      name = name,
      display_name = displpay_name,
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
    class = "hparams_internal"
  )
}

