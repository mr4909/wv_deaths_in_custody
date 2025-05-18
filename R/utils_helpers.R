load_ucla_csvs <- function(data_dir = "data",
                           folders = c("latest-data", "historical-data", "anchored-data")) {
  walk(folders, function(folder) {
    folder_path <- file.path(data_dir, folder)
    csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
    
    walk(csv_files, function(file) {
      object_name <- tools::file_path_sans_ext(basename(file))
      assign(object_name, read_csv(file, show_col_types = FALSE), envir = .GlobalEnv)
    })
  })
}

get_wv_subset <- function(df_name) {
  get(df_name) |>
    clean_names() |>
    filter(state == "West Virginia")
}

style_ucla_table <- function(gt_table, title = NULL) {
  gt_table |>
    tab_header(title = md(paste0("**", title, "**"))) |>
    tab_style(
      style = cell_text(
        weight = "bold"
      ),
      locations = cells_column_labels(everything())
    ) |>
    tab_options(
      heading.title.font.size = 18,
      data_row.padding = px(6)
    ) |>
    opt_table_outline()
}
