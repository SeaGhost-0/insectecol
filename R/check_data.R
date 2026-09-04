## check_data <- function(df, n)
# Check whether the raw data are correct
# (1) Check the first n-1 columns for illegal entries:
#   - only trailing blanks are allowed (the individual died earlier)
#   - the 3rd and 4th columns before the sex column (prepupa-related)
#     may be skipped: a blank cell with data in later columns is legal
#   - a blank cell in any other column while later columns still contain
#     data is an illegal gap and is reported
# (2) Check that the oviposition days of every female match her adult
#   survival days: for a female (F) the number of oviposition records
#   (non-NA entries after the sex column) must equal the adult survival
#   days (the number in the column just before the sex column)
# df: the data frame to be checked
# n: the column index of the sex column; columns 1 to n-1 are checked

check_data <- function(df, n) {
  df_sub <- df[, 1:(n-1)]               # extract the data of columns 1 to n-1
  valid <- logical(nrow(df_sub))         # logical flag vector, one entry per row
  positions <- list()                    # list of error positions
  oviposition <- NULL                    # oviposition errors (row indices)

  # Column indices where gaps are allowed (the 3rd and 4th columns before
  # the sex column, i.e. the 3rd and 4th data columns from the end,
  # prepupa-related)
  # The data columns are 2 to n-1: 3rd from last = n-3, 4th from last = n-4
  skip_allowed_cols <- c(n-4, n-3)      # columns where intermediate gaps are allowed (prepupa-related)

  for (i in seq_len(nrow(df_sub))) {     # loop over the rows: check columns 1 to n-1 for errors
    row <- df_sub[i, ]                   # row i
    bad_cols <- integer(0)               # columns with errors in this row

    # All values of columns 2 to n-1 (as character)
    row_vals <- as.character(unlist(row[2:(n-1)]))
    # Which positions are filled (contain actual data)
    is_filled <- !(is.na(row_vals) | trimws(row_vals) == "")
    # Relative position (counted from column 2) of the last filled column
    last_filled <- if(any(is_filled)) max(which(is_filled)) else 0

    for (j in 2:(n-1)) {                 # check columns 2 to n-1 (column 1 is the ID)
      cell_val <- row[[j]]               # value of the cell
      rel_j <- j - 1                     # relative position (counted from column 2, starting at 1)

      if (is.na(cell_val) || trimws(as.character(cell_val)) == "") {
        # Trailing blanks are legal (early death): skip if rel_j > last_filled
        if (rel_j > last_filled) { next }
        # Gap-allowed (prepupa-related) columns: legal even if later columns contain data
        if (j %in% skip_allowed_cols) { next }
        # All other columns: a blank while later columns still contain data
        # is an illegal gap
        bad_cols <- c(bad_cols, j)       # record the illegal gap
        next
      }

      # Illegal characters: values that cannot be converted to numbers (e.g. ".", letters)
      num_val <- suppressWarnings(as.numeric(as.character(cell_val)))
      if (is.na(num_val)) {
        bad_cols <- c(bad_cols, j)        # record the illegal character
      }

      # Negative values (durations must not be negative)
      if (!is.na(num_val) && num_val < 0) {
        bad_cols <- c(bad_cols, j)        # record the negative value
      }
    }

    valid[i] <- length(bad_cols) == 0    # TRUE if the row contains no error
    # If the row contains errors, record their row and column positions
    positions[[i]] <- if (length(bad_cols) > 0) {
      data.frame(row = i, column = bad_cols)
    } else {
      NA
    }
  }

  for (o in 1:nrow(df)){                # loop over the rows: oviposition days vs adult survival days
    if(df[o,n] == "F"){                  # is the individual a female?
      days <- df[o,(n+1):ncol(df)]       # the oviposition records of the row
      check_days <- sum(!is.na(days))    # number of oviposition records (non-NA entries after the sex column)
      adult_days <- as.numeric(df[o, n-1]) # adult survival days (the number before the sex column)
      # The number of oviposition records must equal the survival days exactly
      if(!is.na(adult_days) && check_days != adult_days){
        oviposition[o] <- o              # save the row index of the mismatch
      }
    }
  }

  # The return list
  result_list <- list(
    valid = valid,                       # TRUE/FALSE per row: are columns 1 to n-1 error-free?
    # Error positions in columns 1 to n-1, NA entries dropped
    positions = do.call(rbind, positions[!is.na(positions)]),
    oviposition = oviposition[!is.na(oviposition)]) # rows with oviposition errors, NA entries dropped
  return(result_list)                    # return the list
}
