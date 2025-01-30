################################################################################
#                                FUNCTIONS
################################################################################

#' Summarise task results
#'
#' Generate a summary of results from a single task
#'
#' @param task_name Name of the task to summarise
#' @param foundation_model_ids Vector of foundation model IDs
#' @param excluded_dataset_ids Vector of excluded dataset IDs
#'
#' @returns A list of summarised task information
summarise_task <- function(task_name, foundation_model_ids,
                           excluded_dataset_ids) {
  # Set the data directory for this task
  base_dir <- here::here("resources_test", "task_foundation_models")
  processed_dir <- fs::path(base_dir, task_name, "processed")

  # Read the dataset information for this task
  datasets <- jsonlite::fromJSON(
    fs::path(processed_dir, "dataset_info.json")
  ) |>
    # Remove excluded datasets
    dplyr::filter(
      !(dataset_id %in% excluded_dataset_ids),
      !(is.na(dataset_id))
    )
  # Read the methods information for this task
  methods <- jsonlite::fromJSON(fs::path(processed_dir, "method_info.json"))
  # Read the results for this task
  results <- jsonlite::fromJSON(fs::path(processed_dir, "results.json")) |>
    # Remove excluded datasets
    dplyr::filter(
      !(dataset_id %in% excluded_dataset_ids),
      !(is.na(dataset_id))
    )
  # Read the task information for this task
  task <- jsonlite::fromJSON(fs::path(processed_dir, "task_info.json"))

  # Get IDs for control method for this task
  control_method_ids <- methods |>
    dplyr::filter(task_id == "control_methods") |>
    dplyr::pull(method_id)

  # Get IDs for standard methods for this task
  standard_method_ids <- methods |>
    dplyr::filter(
      !(method_id %in% c(control_method_ids, foundation_model_ids))
    ) |>
    dplyr::pull(method_id)

  # Get results for control methods
  control_results <- results |>
    dplyr::select(dataset_id, method_id, mean_score) |>
    dplyr::filter(method_id %in% control_method_ids) |>
    # For each dataset, get the highest and lowest mean scores
    # for control methods
    dplyr::group_by(dataset_id) |>
    dplyr::summarise(
      positive_control = max(mean_score),
      negative_control = min(mean_score)
    ) |>
    tidyr::pivot_longer(
      c(positive_control, negative_control),
      names_to = "method_id",
      values_to = "mean_score"
    )

  # Get results for standard methods
  standard_overall <- results |>
    dplyr::select(dataset_id, method_id, mean_score) |>
    dplyr::filter(method_id %in% standard_method_ids) |>
    dplyr::group_by(method_id) |>
    # Calculate an average mean score across datasets
    dplyr::summarise(overall = mean(mean_score))

  # Get the standard method with the highest overall score
  best_standard_id <- standard_overall |>
    dplyr::filter(overall == max(overall)) |>
    dplyr::pull(method_id)

  # Get the standard method with the median overall score
  median_standard_id <- standard_overall |>
    dplyr::filter(overall == quantile(overall, p = 0.5, type = 1)) |>
    dplyr::pull(method_id)

  # Reformat results for the selected standard methods
  standard_results <- results |>
    dplyr::select(dataset_id, method_id, mean_score) |>
    dplyr::filter(method_id %in% c(best_standard_id, median_standard_id)) |>
    dplyr::mutate(
      method_id = dplyr::if_else(
        method_id == best_standard_id,
        "best_standard",
        "median_standard"
      )
    )

  # Get results for foundation models
  foundation_model_results <- results |>
    dplyr::filter(method_id %in% foundation_model_ids) |>
    # Add exit codes
    dplyr::mutate(exit_code = resources$exit_code) |>
    # If an exit code is missing set it to -1
    # This usually indicates a stopped AWS spot instance
    dplyr::mutate(
      exit_code = dplyr::if_else(
        is.na(exit_code),
        -1,
        exit_code
      )
    ) |>
    dplyr::select(dataset_id, method_id, mean_score, exit_code)

  # Combine and reformat the results
  task_results <- dplyr::bind_rows(
    control_results, standard_results, foundation_model_results
  ) |>
    # Add the task name to score and exit code columns
    dplyr::mutate(
      "metric_values.{task$task_id}" := mean_score,
      "scaled_scores.{task$task_id}" := mean_score,
      "exit_codes.{task$task_id}" := exit_code
    ) |>
    dplyr::select(-mean_score, -exit_code)

  # Create new metric information from the task information
  task_metrics <- data.frame(
    task_id = "metrics",
    component_name = task$task_id,
    metric_id = task$task_id,
    metric_name = task$task_name,
    metric_summary = task$task_summary,
    metric_description = task$task_description,
    implementation_url = task$repo,
    code_version = task$version,
    maximize = TRUE
  )
  if (!is.null(task$commit_sha)) {
    task_metrics$commit_sha <- task$commit_sha
  }

  # Return a list of the processed task information
  list(
    task = list(
      id = task$task_id,
      name = task$task_name
    ),
    results = task_results,
    metrics = task_metrics,
    datasets = datasets,
    standard_methods = list(
      best = list(
        id = best_standard_id,
        name = methods$method_name[methods$method_id == best_standard_id]
      ),
      median = list(
        id = median_standard_id,
        name = methods$method_name[methods$method_id == median_standard_id]
      )
    )
  )
}

#' Get standard methods list
#'
#' Generate a text string with a formatted list of standard methods selected for
#' each task
#'
#' @param task_summaries A list of task summaries
#' @param type The type of standard methods to list
#'
#' @returns A formatted character string
get_standard_methods_list <- function(task_summaries,
                                      type = c("best", "median")) {
  type <- match.arg(type)

  # Generate the list item for each task
  purrr::map_chr(task_summaries, \(.task) {
    task_id <- .task$task$id
    task_name <- .task$task$name
    # URLs for task pages don't include the "task_" prefix
    task_page <- paste0("../", stringr::str_remove(task_id, "^task_"))

    method_name <- .task$standard_methods[[type]]$name
    # The handle for the method on the task page is lower case method name
    # with spaces replaced by hyphens
    method_handle <- stringr::str_replace_all(tolower(method_name), " ", "-")

    # Create the list item for this task
    # - [TaskName](TaskPage): [MethodName](TaskPage#MethodHandle)
    paste0(
      "- ",
      paste0("[", task_name, "](", task_page, ")"),
      ": ",
      paste0("[", method_name, "](", task_page, "#", method_handle, ")")
    )
  }) |>
    # Merge list items into a single string
    paste(collapse = "\n")
}

################################################################################
#                             MAIN SCRIPT
################################################################################

# Read the base foundation models methods info file
# TODO: This should be generated by a workflow
base_methods <- jsonlite::fromJSON("base_method_info.json")

# Get IDs for foundation models from the method info
foundation_model_ids <- base_methods |>
  dplyr::filter(task_id == "methods") |>
  dplyr::pull(method_id)

# Datasets to be excluded from the combined results
# Currently, most foundation models can only work with human data
excluded_dataset_ids <- c(
  "cellxgene_census/hypomap",
  "cellxgene_census/mouse_pancreas_atlas"
)

# Define the tasks to combine
task_names <- c("label_projection", "batch_integration")

# Generate summaries for each task
task_summaries <- purrr::map(task_names, \(.task) summarise_task(
  .task, foundation_model_ids, excluded_dataset_ids
)) |>
  setNames(task_names)

# Create the combined method information
combined_methods <- base_methods |>
  dplyr::mutate(
    # Add a list of the selected methods to the "best_standard" method
    method_description = dplyr::case_when(
      method_id == "best_standard" ~ paste0(
        method_description,
        "\n\n",
        "The selected methods are:",
        "\n\n",
        get_standard_methods_list(task_summaries, type = "best")
      ),
      # Add a list of the selected methods to the "median_standard" method
      method_id == "median_standard" ~ paste0(
        method_description,
        "\n\n",
        "The selected methods are:",
        "\n\n",
        get_standard_methods_list(task_summaries, type = "median")
      ),
      TRUE ~ method_description
    )
  )

# Create the combined metric information where each task is a metric
combined_metrics <- purrr::map_dfr(task_summaries, "metrics")

# Create a text list of the metrics (tasks) to add to the task description
# - [TaskName](TaskPage)
metrics_list <- purrr::map2_chr(
  combined_metrics$metric_id, combined_metrics$metric_name, \(.id, .name) {
    task_page <- paste0("../", stringr::str_remove(.id, "^task_"))
    paste0("- [", .name, "](", task_page, ")")
  }
) |>
  paste(collapse = "\n")

# Read the base task info file
# TODO: This should be generated by a workflow
new_task_info <- jsonlite::fromJSON("base_task_info.json")
# Add the combined metrics list to the task description
# This is a list so can't use dplyr
new_task_info$task_description <- paste0(
  new_task_info$task_description,
  "\n\n",
  "This overview combines results from the following benchmarks for individual tasks:",
  "\n\n",
  metrics_list
)

# Create the combined dataset information
combined_datasets <- task_summaries |>
  # Select the datasets from the task summaries
  purrr::map("datasets") |>
  # Merge the datasets from each task
  purrr::reduce(\(.merged, .datasets) {
    # Only add datasets that are not already in the merged dataset list
    new_datasets <- !(.datasets$dataset_id %in% .merged$dataset_id)
    dplyr::bind_rows(.merged, .datasets[new_datasets, ])
  })

# Create the combined results information
combined_results <- task_summaries |>
  # Select the results from the task summaries
  purrr::map("results") |>
  # Merge the results from each task
  purrr::reduce(\(.merged, .results) {
    dplyr::full_join(
      .merged,
      .results,
      by = c("dataset_id", "method_id")
    )
  }) |>
  # Nest the scores and exit codes into list columns
  tidyr::nest(
    metric_values = tidyselect::starts_with("metric_values."),
    scaled_scores = tidyselect::starts_with("scaled_scores."),
    exit_codes = tidyselect::starts_with("exit_codes."),
    .names_sep = "."
  ) |>
  # Convert items in list columns to data frames and unbox single values
  dplyr::mutate(
    metric_values = lapply(metric_values, \(.x) {
      jsonlite::unbox(as.data.frame(.x))
    }),
    scaled_scores = lapply(scaled_scores, \(.x) {
      jsonlite::unbox(as.data.frame(.x))
    }),
    exit_codes = lapply(exit_codes, \(.x) {
      jsonlite::unbox(as.data.frame(.x))
    })
  ) |>
  # Calculate the overall mean score for each method/dataset
  dplyr::mutate(
    mean_score = purrr::map_dbl(scaled_scores, \(.x) {
      mean(as.matrix(.x)[1, ])
    })
  )

# Output JSON files
jsonlite::write_json(
  new_task_info, "task_info.json",
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)
jsonlite::write_json(
  combined_methods, "method_info.json",
  pretty = TRUE, na = NULL
)
jsonlite::write_json(combined_results, "results.json", pretty = TRUE)
jsonlite::write_json(combined_metrics, "metric_info.json", pretty = TRUE)
jsonlite::write_json(combined_datasets, "dataset_info.json", pretty = TRUE)
