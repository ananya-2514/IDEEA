#' (Interim solution to)  Update Scenario Paths - see readme.Rmd for details
#'
#' This function walks through all nested slots of a scenario object to find @path and @misc$path
#' in the entire object tree and replaces the old directory path with a new one.
#'
#' @param scenario A scenario object (S4 class)
#' @param scenarios_old_dir Character string of the old directory path to replace
#' @param scenarios_new_dir Character string of the new directory path to use
#' @param verbose Logical, if TRUE shows detailed progress messages (default: FALSE)
#' @return Updated scenario object with modified paths
#' @export
update_scenario_paths <- function(scenario, scenarios_old_dir, scenarios_new_dir, verbose = FALSE) {
  # Check if scenario is a valid scenario object
  if (!inherits(scenario, "scenario")) {
    stop("Input must be a scenario object")
  }

  # Normalize paths to handle different path separators (but keep relative paths relative)
  old_path_norm <- gsub("\\\\", "/", scenarios_old_dir) # Convert backslashes to forward slashes
  new_path_norm <- gsub("\\\\", "/", scenarios_new_dir) # Convert backslashes to forward slashes

  if (verbose) {
    cat("Path replacement settings:\n")
    cat("  Old pattern:", old_path_norm, "\n")
    cat("  New pattern:", new_path_norm, "\n\n")
  }

  # Counter for tracking updates
  path_updates <- 0

  # Helper function to update character paths
  update_character_paths <- function(char_vec, old_path, new_path) {
    if (is.character(char_vec) && length(char_vec) > 0) {
      original <- char_vec

      # Try multiple replacement strategies
      # 1. Direct replacement
      updated <- gsub(old_path, new_path, char_vec, fixed = TRUE)

      # 2. If no change, try with normalized slashes
      if (identical(original, updated)) {
        char_vec_norm <- gsub("\\\\", "/", char_vec)
        updated <- gsub(old_path, new_path, char_vec_norm, fixed = TRUE)
      }

      # 3. If still no change, try replacing just the beginning of the path
      if (identical(original, updated)) {
        for (i in seq_along(char_vec)) {
          if (startsWith(char_vec[i], old_path)) {
            updated[i] <- paste0(new_path, substring(char_vec[i], nchar(old_path) + 1))
          }
        }
      }

      # Check if any changes were made
      if (!identical(original, updated)) {
        path_updates <<- path_updates + 1
        if (verbose) {
          cat("  Updated path:", original[1], "->", updated[1], "\n")
        }
        return(updated)
      }
    }
    return(char_vec)
  }

  # Main recursive function to walk through the object tree
  walk_and_update <- function(obj, obj_name = "", depth = 0) {
    indent <- paste(rep("  ", depth), collapse = "")
    max_depth <- 15 # Prevent infinite recursion

    # Check depth limit
    if (depth > max_depth) {
      if (verbose) {
        cat(indent, "(max depth reached, skipping)\n")
      }
      return(obj)
    }

    # Check for NULL objects
    if (is.null(obj)) {
      return(obj)
    }

    # If it's an S4 object, check for @path and @misc slots, then recurse
    if (isS4(obj)) {
      if (depth == 0 && verbose) {
        cat("Walking through scenario object...\n")
      } else if (verbose) {
        cat(indent, "Processing S4 object:", class(obj)[1], "\n")
      }

      slot_names <- slotNames(obj)

      # Process @path slot if it exists
      if ("path" %in% slot_names && .hasSlot(obj, "path")) {
        path_value <- slot(obj, "path")
        if (verbose) {
          cat(indent, "Found @path slot with value:", path_value, "\n")
        }
        updated_path <- update_character_paths(path_value, old_path_norm, new_path_norm)
        slot(obj, "path") <- updated_path
      }

      # Process @misc slot if it exists
      if ("misc" %in% slot_names && .hasSlot(obj, "misc")) {
        misc_value <- slot(obj, "misc")
        if (verbose) {
          cat(indent, "Found @misc slot\n")
        }

        # If misc is a list and has a "path" element
        if (is.list(misc_value) && "path" %in% names(misc_value)) {
          if (verbose) {
            cat(indent, "  Found @misc$path with value:", misc_value$path, "\n")
          }
          misc_value$path <- update_character_paths(misc_value$path, old_path_norm, new_path_norm)
        }

        # Recursively process other elements in misc
        if (is.list(misc_value)) {
          for (name in names(misc_value)) {
            tryCatch(
              {
                misc_value[[name]] <- walk_and_update(
                  misc_value[[name]],
                  paste0(obj_name, "@misc$", name),
                  depth + 1
                )
              },
              error = function(e) {
                if (verbose) {
                  cat(indent, "    Error processing @misc$", name, ":", e$message, "\n")
                  cat(indent, "    Skipping this misc element\n")
                }
              }
            )
          }
        }

        slot(obj, "misc") <- misc_value
      }

      # Recursively process all other slots
      for (slot_name in slot_names) {
        if (slot_name %in% c("path", "misc")) next # Already processed above

        if (.hasSlot(obj, slot_name)) {
          tryCatch(
            {
              slot_value <- slot(obj, slot_name)
              updated_slot <- walk_and_update(
                slot_value,
                paste0(obj_name, "@", slot_name),
                depth + 1
              )
              slot(obj, slot_name) <- updated_slot
            },
            error = function(e) {
              if (verbose) {
                cat(indent, "  Error processing slot", slot_name, ":", e$message, "\n")
                cat(indent, "  Skipping this slot\n")
              }
            }
          )
        }
      }

      return(obj)
    }

    # If it's a list, process each element
    if (is.list(obj)) {
      if (length(obj) > 0 && verbose) {
        cat(indent, "Processing list with", length(obj), "elements\n")
      }

      if (length(obj) > 0) {
        # Use safe iteration with try-catch
        for (i in seq_along(obj)) {
          tryCatch(
            {
              # Get the name safely
              name <- if (!is.null(names(obj)) && i <= length(names(obj))) {
                names(obj)[i]
              } else {
                as.character(i)
              }

              # Skip NULL elements
              if (is.null(obj[[i]])) {
                if (verbose) {
                  cat(indent, "  Skipping NULL element:", name, "\n")
                }
                next
              }

              # Process the element
              obj[[i]] <- walk_and_update(
                obj[[i]],
                paste0(obj_name, "[[", name, "]]"),
                depth + 1
              )
            },
            error = function(e) {
              if (verbose) {
                cat(indent, "  Error processing list element", i, ":", e$message, "\n")
                cat(indent, "  Skipping this element\n")
              }
            }
          )
        }
      }
      return(obj)
    }

    # If it's a character vector, update paths
    if (is.character(obj)) {
      return(update_character_paths(obj, old_path_norm, new_path_norm))
    }

    # For other types, return as-is
    return(obj)
  }

  # Create a copy of the scenario to avoid modifying the original
  updated_scenario <- scenario

  # Start the recursive walk
  if (verbose) {
    cat("Starting path update process...\n")
    cat("Old path:", old_path_norm, "\n")
    cat("New path:", new_path_norm, "\n\n")
  }

  updated_scenario <- walk_and_update(updated_scenario, "scenario")

  # Get the final path for summary message
  final_path <- if (.hasSlot(updated_scenario, "path")) {
    slot(updated_scenario, "path")
  } else {
    "(no @path slot found)"
  }

  cat("Path updated to \n  \"", final_path, "\", ", path_updates,
    "\nreplacements made\n",
    sep = ""
  )

  return(updated_scenario)
}
