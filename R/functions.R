keep_columns <- function(df) {
  # inconsistent column names in the MPI files
  df %>%
    select_if(names(.) %in% c(
      "project_id", "proj_id",
      "project_name", "proj_nm",
      "telephone", "fin_by",
      "estimated_cost", "est_cost",
      "construction_type", "proj_cons_typ",
      "construction_subtype", "proj_con_subtyp",
      "project_type", "proj_typ",
      "region",
      "project_status", "status",
      "project_stage", "stage",
      "project_category_name", "proj_cat",
      "first_entry_date", "entry_dt",
      "last_update", "last_up_dt"
    ))
}
Mode <- function(x) {
  # calculates the most common value of a vector
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_fill <- function(tbbl) {
  # over-writes variables that SHOULD be constant with the project's modal (most common) value.
  tbbl %>%
    mutate(
      project_name = Mode(project_name),
      construction_type = Mode(construction_type),
      construction_subtype = Mode(construction_subtype),
      project_type = Mode(project_type),
      region = Mode(region),
      project_category_name = Mode(project_category_name)
    )
}

mode_fill_long <- function(tbbl) {
  # over-writes variables that SHOULD be constant with the project's modal (most common) value.
  tbbl %>%
    mutate(
      project_name = Mode(project_name),
      project_type = Mode(project_type),
      region = Mode(region),
      project_category_name = Mode(project_category_name)
    )
}

updown_fill <- function(tbbl) {
  # creates a regular quarterly grid that extends from the first_entry_date to the maximum value of last_update, then
  # fills first in the up direction (backwards in time) and then down (forward in time)
  assertthat::assert_that(all(month(tbbl$first_entry_date) %in% c(3, 6, 9, 12)))
  assertthat::assert_that(all(day(tbbl$first_entry_date) == 1))
  assertthat::assert_that(all(month(tbbl$last_update) %in% c(3, 6, 9, 12)))
  assertthat::assert_that(all(day(tbbl$last_update) == 1))
  all_quarters <- tibble(
    last_update = seq(min(tbbl$first_entry_date, na.rm = TRUE),
      max(tbbl$last_update, na.rm = TRUE),
      by = "quarter"
    )
  )
  tbbl <- left_join(all_quarters, tbbl, by = c("last_update" = "last_update")) %>%
    distinct(last_update, .keep_all = TRUE) %>% # some last_updates incorrect: same for 4 quarters
    fill(estimated_cost, .direction = "updown") %>%
    fill(telephone, .direction = "updown") %>%
    fill(project_status, .direction = "updown") %>%
    fill(first_entry_date, .direction = "updown")
}

updown_fill_long <- function(tbbl) {
  # creates a regular quarterly grid that extends from the min to max of published_dates , then
  # fills first in the up direction (backwards in time) and then down (forward in time)
  assertthat::assert_that(all(month(tbbl$published_dates) %in% c(3, 6, 9, 12)))
  assertthat::assert_that(all(day(tbbl$published_dates) == 1))
  all_quarters <- tibble(
    published_dates = seq(min(tbbl$published_dates, na.rm = TRUE),
      max(tbbl$published_dates, na.rm = TRUE),
      by = "quarter"
    )
  )
  tbbl <- left_join(all_quarters, tbbl, by = c("published_dates" = "published_dates")) %>%
    #  distinct(published_dates, .keep_all = TRUE) %>%
    fill(estimated_cost, .direction = "updown") %>%
    fill(project_status, .direction = "updown")
}

possibly_abandoned <- function(tbbl) {
  # find projects where they were previously present in the MPI, are now missing AND were NOT reported completed.
  last_reported_stage <- tbbl %>%
    filter(last_update == max(last_update)) %>%
    pull(project_stage)
  value <- max(tbbl$last_update) < global_last_date & !last_reported_stage == "Completed"
}
last_var <- function(tbbl, var) {
  # get the last value of a variable.
  tbbl %>%
    filter(last_update == max(last_update)) %>%
    pull({{ var }}) %>%
    as.character()
}

add_weight <- function(tbbl) {
  # without weights group averages would be biased towards long lived projects i.e. with weights each project gets equal weight regardless of how long lived.
  nobs <- dim(tbbl)[1]
  tbbl <- tbbl %>%
    mutate(weight = 1 / nobs)
}


plot_diff <- function(var) {
  plt <- all_data %>%
    group_by(get(var), last_update, data_type) %>%
    summarize(total_cost = sum(estimated_cost, na.rm = TRUE)) %>%
    ggplot(aes(last_update, total_cost, colour = data_type)) +
    geom_line() +
    facet_wrap(~`get(var)`, scales = "free_y") +
    scale_y_continuous(labels = scales::comma) +
    scale_colour_brewer(palette = "Dark2")
  plt <- aest::aest_fix_labs(plt)
  #  plotly::ggplotly(plt)
}

plot_diff_long <- function(var) {
  grp_mn <- all_data_long %>%
    group_by(get(var), published_dates, data_type) %>%
    summarize(total_cost = sum(estimated_cost, na.rm = TRUE))
  # browser()
  plt <- grp_mn %>%
    ggplot(aes(published_dates, total_cost, colour = data_type)) +
    geom_line() +
    facet_wrap(~`get(var)`, scales = "free_y") +
    scale_y_continuous(labels = scales::comma) +
    scale_colour_brewer(palette = "Dark2")
  plt <- aest::aest_fix_labs(plt)
  #  plotly::ggplotly(plt)
}

add_flags <- function(modified_df, raw_df) {
  left_join(modified_df,
    raw_df,
    by = c(
      "project_id" = "project_id",
      "published_dates" = "published_dates"
    ),
    suffix = c("_modified", "_raw")
  ) %>%
    mutate(
      project_name_change = if_else(project_name_modified == project_name_raw,
        FALSE,
        TRUE,
        TRUE
      ),
      project_type_change = if_else(project_type_modified == project_type_raw,
        FALSE,
        TRUE,
        TRUE
      ),
      region_change = if_else(region_modified == region_raw,
        FALSE,
        TRUE,
        TRUE
      ),
      project_category_name_change = if_else(project_category_name_modified == project_category_name_raw,
        FALSE,
        TRUE,
        TRUE
      ),
      estimated_cost_change = if_else(near(
        estimated_cost_modified,
        estimated_cost_raw
      ),
      FALSE,
      TRUE,
      TRUE
      ),
      project_status_change = if_else(project_status_modified == project_status_raw,
        FALSE,
        TRUE,
        TRUE
      ),
      any_change = project_name_change == TRUE |
        project_type_change == TRUE |
        region_change == TRUE |
        project_category_name_change == TRUE |
        estimated_cost_change == TRUE |
        project_status_change == TRUE
    )
}
