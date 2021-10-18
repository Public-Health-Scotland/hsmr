
# Gets the difference between each row in df1 and df2.
#
# Any missing rows in either are assumed = 0.
# Only compares values in columns that are not specified in join_by.
# Difference is df1 - df2.
#
# Arguments
# df1, df2 - dataframes to compare
# join_by - character vector specifying the column names to join df1 and df2 by
#           i.e. these are the columns that identify what data is in each row
# df1_name, df2_name - name to give data frome each df in the output columns
#
# Value
# Dataframe containing the original data and the differences
compare_df = function(df1, df2, join_by, df1_name="df1", df2_name="df2"){


  # Join df1 and df2 ----

  # I suspect the comparison won't work properly if the columns don't match
  # (this checks for both different column names, different number of cols, different order)
  if (!isTRUE(identical(names(df1), names(df2)))) {
    stop("Columns in df1 and df2 don't match")
  }

  # Make sure columns are in the same order
  df2 = select(df2, names(df1))

  # For now just treat any missing rows as data = 0
  # (only to the columns not in join_by, we don't want to replace any missing data in there)
  # In future, could add a step using anti_join in both directions to see what rows are missing
  df_joined =
    full_join(df1, df2, by=join_by, suffix=c(glue(".{df1_name}"), glue(".{df2_name}"))) %>%
    mutate(across(!all_of(join_by), ~replace_na(., 0)))

  # Prep for calcs
  df_joined =
    df_joined %>%
    # Make sure the columns for comparing are numeric
    mutate(across(!all_of(join_by), as.numeric)) %>%
    # Replace any missing values that were introduced by conversion to 0
    mutate(across(!all_of(join_by), ~replace_na(., 0))) %>%
    # Put the join columns first - they are the labels
    relocate(all_of(join_by))



  # Find difference between data columns in df1 and df2 ----

  # We need to index into the data columns that originally came from df1 and df2
  # so they can be compered with each other.
  # This is going to get a vector of boolean values, specifying which columns
  # in df_joined end in .{df1_name} and .{df2_name}
  df1_compare_cols = grepl(glue("*\\.{df1_name}$"), names(df_joined))
  df2_compare_cols = grepl(glue("*\\.{df2_name}$"), names(df_joined))

  # Now calculate the difference between each cell
  # (assumes the columns for df1 and df2 are in the same order)
  df_diff = df_joined[, df1_compare_cols, drop=FALSE] - df_joined[, df2_compare_cols, drop=FALSE]



  # Find percentage difference ----

  df_diff_pc = (df_diff / df_joined[, df2_compare_cols, drop=FALSE]) * 100

  df_diff_pc = round_half_up(df_diff_pc, digits = 2)



  # Name difference columns and rejoin to rest of joined df ----

  # What are the columns that we just subtracted called in df1?
  new_col_names_base = names(df1)[!(names(df1) %in% join_by)]

  # Set up new names for the difference between the dfs
  new_col_names_diff = paste0(new_col_names_base, ".diff")

  # Set the new names
  names(df_diff) = new_col_names_diff

  # Join back together
  df_diff = cbind(df_joined, df_diff)

  # Repeat for % differences
  new_col_names_pc = paste0(new_col_names_base, ".diff_pc")
  names(df_diff_pc) = new_col_names_pc
  df_diff = cbind(df_diff, df_diff_pc)


  return(df_diff)

}


# Gets a dataframe showing the difference between subgroup totals and Scotland
#
# Arguments
# df - dataframe of data from one of the HSMR tables
# hsmr table - integer specifying which table df is from. Only 2 or 3 supported.
#
# Value
# A dataframe with subgroups aggregate and difference from Scotland totals
split_tot_df = function(df, hsmr_table){

  tot_loc = "Scotland"

  # Need a lookup of Scotland level totals, and the column for joining back to df
  if (hsmr_table == 2) {
    tot_lookup = filter(df, location_name == tot_loc, sub_grp == "All Admissions")
    join_cols = "quarter_short"
  } else if (hsmr_table == 3) {
    tot_lookup = filter(df, location_name == tot_loc)
    join_cols = c("quarter_short", "sub_grp")
  } else {
    stop("Only HSMR Tables 2 and 3 supported.")
  }

  # Drop the rows that have been put into the lookup - these are totals that
  # we don't want to include in the group sums
  df = anti_join(df, tot_lookup, by = names(df))

  # Finish setting up lookup
  tot_lookup =
    tot_lookup %>%
    select(all_of(join_cols), deaths, pats)

  # Get totals from each split, and join on Scotland total
  df_split_tot =
    df %>%
    group_by(agg_label, quarter_short, sub_grp) %>%
    summarise(deaths = sum(deaths), pats = sum(pats), .groups = "drop") %>%
    left_join(tot_lookup, by = join_cols, suffix = c(".split_tot", ".scot")) %>%
    mutate(deaths.diff = deaths.split_tot - deaths.scot,
           pats.diff = pats.split_tot - pats.scot,
           deaths.diff_pc = (deaths.diff / deaths.scot) * 100,
           pats.diff_pc = (pats.diff / pats.scot) * 100)

  return(df_split_tot)

}


# Makes a loglog plot for % change vs old value
#
# Arguments
# plot_data - dataframe produced by compare_df()
# measure - string specifying column prefix e.g. "pats"
#
# Value
# ggplot
make_change_plot = function(plot_data, measure){

  x_col = glue("{measure}.old")
  y_col = glue("{measure}.diff_pc")

  plot_data =
    plot_data %>%
    # Can't plot 0 on a log plot, so remove
    # (change = 0 is good, and a % change from 0 is not meaningful)
    filter(across(all_of(c(x_col, y_col)), ~(.x != 0))) %>%
    # Can't plot -ve values on a log plot
    mutate(across(all_of(y_col), abs))

  change_plot =
    # aes_string because we're working with quoted column names
    ggplot(plot_data, aes_string(x = x_col, y = y_col)) +
    geom_point(size = 0.5) +
    scale_x_continuous(trans = "log10",
                       labels = scales::number_format(), minor_breaks = NULL) +
    scale_y_continuous(trans = "log10",
                       labels = scales::number_format(accuracy = 0.01),
                       minor_breaks = NULL) +
    annotation_logticks() +
    ylab(glue("absolute value of {y_col}")) +
    theme_bw()

  return(change_plot)

}


# Makes a table
#
# Arguments
# table_data - dataframe to put in the table
# filename - filename to give to files produced by download buttons
# round_cols - charactor vector of column names to apply rounding to.
#               NULL to avoid rounding.
make_table = function(table_data, filename = "data", round_cols = NULL){

  # Much more convenient to avoid scrolling in table, but not found a way to
  # automatically set table height to fit all rows. Instead set long enough
  # to fit the tallest rows, and reduce a bit for smaller tables
  # Only affects table height on html output
  table_height = ifelse(nrow(table_data) <= 5, 300, 600)

  change_table =
  datatable(table_data, rownames = FALSE, filter = "top",
            fillContainer = TRUE, extensions = "Buttons",
            options =
              list(scrollY = table_height,
                   buttons = list('copy',
                                  list(extend = 'csv', filename = filename),
                                  list(extend = 'excel', filename = filename)),
                   # Need to specify layout of table to include buttons.
                   # Number of rows to show (l) looks odd without aligning
                   # right using the .dataTables_filter class
                   dom = 'B<".dataTables_filter"l>rtip')
            )

  if (!is.null(round_cols)) {
    change_table = formatRound(change_table, round_cols, 2)
  }

  return(change_table)

}


# Makes table for exploring changes in data
#
# Arguments
# table - dataframe produced by compare_df(), filtered to one measure
# measure - string specifying column prefix e.g. "pats"
# pc_cutoff - minimum absolute % change to include in table (NULL = no filter)
# quarter_filter - quarters to include in table: "first only", "last only"
#                                                "middle only", "all"
#
# Value
# datatable, or string if no changes > pc_cutoff
make_change_table = function(table_data, measure,
                             pc_cutoff = NULL, quarter_filter = "all") {

  # This assumes that the quarters in table_data are ordered
  first_quarter = unique(table_data$quarter_short)[1]
  last_quarter = tail(unique(table_data$quarter_short), 1)

  if (quarter_filter == "first only") {
    table_data = filter(table_data, quarter_short == first_quarter)
  } else if (quarter_filter == "last only") {
    table_data = filter(table_data, quarter_short == last_quarter)
  } else if (quarter_filter == "middle only") {
    table_data = filter(table_data,
                        quarter_short != first_quarter,
                        quarter_short != last_quarter)
  } else if (quarter_filter != "all") {
    stop("quarter_filter setting not recognised")
  }

  # No filtering when pc_cutoff = NULL
  if (!is.null(pc_cutoff)) {
    table_data = filter(table_data, across(all_of(glue("{measure}.diff_pc")),
                                           ~(abs(.x) >= pc_cutoff)))
  }

  # No point in returning a table if there's nothing in it
  if (nrow(table_data) > 0) {

    # For use by data download buttons in table
    filename = glue("{measure} {pc_cutoff}pc cutoff {quarter_filter} quarters")

    # All crude rate columns need rounded, otherwise just %
    if (measure == "crd_rate") {
      round_cols = paste0(measure, c(".new", ".old", ".diff", ".diff_pc"))
    } else {
      round_cols = glue("{measure}.diff_pc")
    }

    change_table = make_table(table_data, filename, round_cols)

  } else {
    change_table = glue("No differences greater than {pc_cutoff}%")
  }

  return(change_table)

}


# Makes table for exploring whether splits sum up to Scotland total
#
# Arguments
# table - dataframe produced by split_tot_df()
#
# Value
# datatable, or string if all splits sum to Scotland total
make_split_table = function(table_data) {

  # Only need rows where there is a difference
  table_data = filter(table_data, across(ends_with(".diff"), ~(.x != 0)))

  # No point in returning a table if there's nothing in it
  if (nrow(table_data) > 0) {
    # Need vector of % diff column names to specify rounding
    round_cols = names(table_data)[str_ends(names(table_data), ".diff_pc")]
    change_table = make_table(table_data, filename = "split totals",
                              round_cols = round_cols)
  } else {
    change_table = "All splits sum to Scotland totals"
  }

  return(change_table)

}
