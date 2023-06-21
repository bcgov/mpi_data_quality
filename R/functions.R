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
      project_description= Mode(project_description),
      construction_type = Mode(construction_type),
      construction_subtype = Mode(construction_subtype),
      project_type = Mode(project_type),
      region = Mode(region),
      municipality = Mode(municipality),
      project_category_name = Mode(project_category_name),
      standardized_start_date = Mode(standardized_start_date),
      latitude = Mode(latitude),
      longitude =  Mode(longitude),
      latitude_dms = Mode(latitude_dms),
      longitude_dms =  Mode(longitude_dms),
      first_entry_date = Mode(first_entry_date)
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
  #browser()
  tbbl <- left_join(all_quarters, tbbl, by = c("last_update" = "last_update"))%>%
    distinct(last_update, .keep_all = TRUE) %>% # some last_updates incorrect: same for 4 quarters
    fill(estimated_cost, .direction = "updown") %>%
    fill(environmental_assessment_stage, .direction='updown')%>%
    fill(developer, .direction='updown')%>%
    fill(architect, .direction='updown')%>%
    fill(project_status, .direction='updown')%>%
    fill(project_stage, .direction='updown')%>%
    fill(public_funding_ind, .direction='updown')%>%
    fill(provinvial_funding, .direction='updown')%>%
    fill(federal_funding, .direction='updown')%>%
    fill(municipal_funding, .direction='updown')%>%
    fill(other_public_funding, .direction='updown')%>%
    fill(green_building_ind, .direction='updown')%>%
    fill(green_building_desc, .direction='updown')%>%
    fill(clean_energy_ind, .direction='updown')%>%
    fill(indigenous_ind, .direction='updown')%>%
    fill(indigenous_names, .direction='updown')%>%
    fill(indigenous_agreement, .direction='updown')%>%
    fill(construction_jobs, .direction='updown')%>%
    fill(operating_jobs, .direction='updown')%>%
    fill(standardized_completion_date, .direction='updown')%>%
    fill(telephone, .direction='updown')%>%
    fill(project_website, .direction='updown')
}

fix_last_update <- function(tbbl){
  if("last_up_dt" %in% colnames(tbbl)){
    tbbl <- tbbl%>%
      rename(last_update=last_up_dt)
  }
  tbbl%>%
    mutate(last_update= Mode(last_update))
}
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
    geom_jitter(alpha=.25)+
    facet_wrap(~`get(var)`, scales = "free_y") +
    scale_y_continuous(labels = scales::comma) +
    scale_colour_brewer(palette = "Dark2")
  plt <- wrapR::fix_labs(plt)
  #  plotly::ggplotly(plt)
}



# possibly_abandoned <- function(tbbl) {
#   # find projects where they were previously present in the MPI, are now missing AND were NOT reported completed.
#   last_reported_stage <- tbbl %>%
#     filter(last_update == max(last_update)) %>%
#     pull(project_stage)
#   value <- max(tbbl$last_update) < global_last_date & !last_reported_stage == "Completed"
# }
# last_var <- function(tbbl, var) {
#   # get the last value of a variable.
#   tbbl %>%
#     filter(last_update == max(last_update)) %>%
#     pull({{ var }}) %>%
#     as.character()
# }
