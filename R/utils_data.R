#' Déterminer le type de colonne d'in dataframe
#' @param column La colonne à analyser
#' Les types de colonne supportés sont: string, int, decimal et date
#' @return Le type de colonne
# get_column_type <- function(column) {
#  # Remove NA values temporarily
#  non_na_column <- na.omit(column)
#
#  if (is.character(non_na_column)) {
#    return("character")
#  } else if (inherits(non_na_column, "Date")) {
#    return("date")
#  } else if (is.numeric(non_na_column)) {
#    if (all(non_na_column == as.integer(non_na_column))) {
#      return("integer")
#    } else {
#      return("decimal")
#    }
#  } else {
#    return("unsupported")
#  }
# }

get_column_type <- function(column) {
  # Remove NA values temporarily
  non_na_column <- na.omit(column)
  if (is.integer(non_na_column)) {
    "integer"
  } else if (inherits(non_na_column, "Date")) {
    "date"
  } else if (is.double(non_na_column)) {
    "decimal"
  } else if (is.character(non_na_column)) {
    "character"
  } else {
    "unsupported"
  }
}

#' Merge multiple dataframes with schema union
#' @param df_list List of dataframes to merge
#' @keywords internal
merge_dataframes_with_schema_union <- function(df_list) {
  if (length(df_list) == 0) {
    return(tibble::tibble())
  }
  if (length(df_list) == 1) {
    return(df_list[[1]])
  }

  # Get the column order from the first dataframe
  first_df_cols <- names(df_list[[1]])

  # Get union of all column names
  all_cols <- unique(unlist(lapply(df_list, names)))

  # Arrange columns: first_df columns first, then others
  other_cols <- setdiff(all_cols, first_df_cols)
  final_col_order <- c(first_df_cols, other_cols)

  # Add missing columns to each dataframe and convert all to character
  df_list_standardized <- lapply(df_list, function(df) {
    # Add missing columns with NA
    missing_cols <- setdiff(final_col_order, names(df))
    if (length(missing_cols) > 0) {
      df[missing_cols] <- NA
    }

    # Convert all columns to character to avoid type conflicts
    df <- df %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

    # Reorder columns consistently
    df[final_col_order]
  })

  # Combine all dataframes
  merged_df <- dplyr::bind_rows(df_list_standardized)

  merged_df
}

#' Null coalescing operator
#' @param x Left-hand side
#' @param y Right-hand side
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
