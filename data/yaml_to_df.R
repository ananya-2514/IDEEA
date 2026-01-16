yaml_to_df <- function(x,
                       key_col = "key",
                       numeric_cols = NULL,
                       keep_key_order = TRUE,
                       stringsAsFactors = FALSE) {
  # x can be:
  # - a file path to .yml/.yaml
  # - a YAML string (containing ":" and newlines)
  # - a named list already parsed from YAML

  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required. Install it with install.packages('yaml').")
  }

  # Parse input into a named list
  y <- NULL
  if (is.list(x)) {
    y <- x
  } else if (is.character(x) && length(x) == 1) {
    if (file.exists(x)) {
      y <- yaml::read_yaml(x)
    } else if (grepl(":\n|:\r\n", x)) {
      y <- yaml::yaml.load(x)
    } else {
      stop("Input 'x' is a string but is neither an existing file path nor a YAML string.")
    }
  } else {
    stop("Input 'x' must be a named list, a YAML file path, or a YAML string.")
  }

  if (is.null(names(y)) || any(names(y) == "")) {
    stop("Top-level YAML must be a *named* mapping (root keys present).")
  }

  # Ensure each top-level entry is a list-like (so we can bind rows)
  rows <- lapply(names(y), function(k) {
    v <- y[[k]]

    if (is.null(v)) v <- list()

    # If someone puts a scalar at root, wrap it
    if (!is.list(v) || is.null(names(v))) {
      v <- list(value = v)
    }

    # Coerce nested structures to JSON-ish strings to keep df rectangular
    v <- lapply(v, function(z) {
      if (is.list(z) || is.vector(z) && length(z) > 1) {
        paste0(deparse(z), collapse = "")
      } else {
        z
      }
    })

    v[[key_col]] <- k
    v
  })

  # Union of all sub-keys across roots (preserve key order if requested)
  all_cols <- unique(unlist(lapply(rows, names), use.names = FALSE))
  # Put key column first
  all_cols <- c(key_col, setdiff(all_cols, key_col))

  # Fill missing columns with NA, then rbind
  filled <- lapply(rows, function(r) {
    missing <- setdiff(all_cols, names(r))
    if (length(missing)) r[missing] <- NA
    r[all_cols]
  })

  df <- as.data.frame(
    do.call(rbind, lapply(filled, function(r)
      as.data.frame(r, stringsAsFactors = stringsAsFactors)
    )),
    stringsAsFactors = stringsAsFactors
  )
  # if (stringsAsFactors == FALSE) {
  #   df[[key_col]] <- as.character(df[[key_col]])
  # }

  # keep_key_order: make key column an ordered factor (optional)
  if (keep_key_order && stringsAsFactors) {
    df[[key_col]] <- factor(df[[key_col]], levels = names(y), ordered = TRUE)
  }

  # Optionally coerce some columns to numeric
  if (!is.null(numeric_cols)) {
    for (nm in intersect(numeric_cols, names(df))) {
      df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
    }
  }

  df
}
