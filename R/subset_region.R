#' Subset S4 objects by region
#'
#' @description
#' Filter S4 objects (technology, storage, demand, supply, import, export, 
#' commodity, constraint, weather, repository) to keep only specified regions. 
#' This function filters both the `region` slot and any data.frame slots that 
#' contain a "region" column.
#'
#' @param obj An S4 object (technology, storage, demand, supply, import, export, 
#'   commodity, constraint, weather, or repository)
#' @param keep_regions A character vector of region names to keep
#' @param verbose Logical. If TRUE (default), reports the filtering process 
#'   (only applicable for repository objects)
#'
#' @return The same type of S4 object with filtered region data
#'
#' @details
#' The function filters:
#' - The `region` slot (character vector): keeps only regions in `keep_regions`
#' - Data.frame slots with "region" column: keeps rows where region is in
#'   `keep_regions` or is NA
#' - List slots containing data.frames with "region" column: filters each 
#'   data.frame in the list
#' 
#' For `repository` objects, the method is applied recursively to all objects
#' in the `@data` slot. Objects without a `subset_region` method are returned
#' unchanged. Verbose output indicates whether each object was actually changed
#' by the filtering operation.
#' 
#' For `commodity` objects, the method returns the object unchanged as commodities
#' typically don't have region-specific data.
#' 
#' For `constraint` objects, the method filters:
#' - `@for.each` data.frame if it has a "region" column
#' - `@rhs` data.frame if it has a "region" column
#' - `@lhs` list: filters the `mult` data.frame in each summand object
#'
#' @examples
#' \dontrun{
#' # Filter a technology to keep only specific regions
#' tech_filtered <- subset_region(tech_obj, keep_regions = c("USA", "Canada"))
#'
#' # Filter a demand object
#' demand_filtered <- subset_region(demand_obj, keep_regions = "USA")
#' 
#' # Filter a constraint object
#' constraint_filtered <- subset_region(constraint_obj, keep_regions = c("R1", "R2"))
#' 
#' # Filter a repository object (applies to all contained objects)
#' repo_filtered <- subset_region(repo_obj, keep_regions = c("USA", "Canada"))
#' 
#' # Filter a repository object without verbose output
#' repo_filtered <- subset_region(repo_obj, keep_regions = "USA", verbose = FALSE)
#' }
#'
#' @name subset_region
#' @export
setGeneric("subset_region", function(obj, keep_regions, verbose = TRUE) {
  standardGeneric("subset_region")
})

#' @rdname subset_region
#' @export
setMethod("subset_region", signature(obj = "technology"), function(obj, keep_regions, verbose = TRUE) {
  if (verbose) {
    cat("Filtering technology:", obj@name, "\n")
  }
  
  # Filter the region slot (character vector)
  if (length(obj@region) > 0) {
    old_regions <- obj@region
    obj@region <- obj@region[obj@region %in% keep_regions]
    if (verbose && length(old_regions) != length(obj@region)) {
      cat("  @region: ", length(old_regions), " -> ", length(obj@region), " regions\n", sep = "")
    }
  }
  
  # Get all slot names
  slot_names <- slotNames(obj)
  
  # Filter data.frame slots with "region" column
  for (slot_name in slot_names) {
    slot_value <- slot(obj, slot_name)
    
    # Check if slot is a data.frame and has "region" column
    if (is.data.frame(slot_value) && "region" %in% names(slot_value)) {
      old_nrow <- nrow(slot_value)
      # Keep rows where region is in keep_regions OR region is NA
      slot_value <- slot_value[slot_value$region %in% keep_regions | is.na(slot_value$region), , drop = FALSE]
      slot(obj, slot_name) <- slot_value
      if (verbose && old_nrow != nrow(slot_value)) {
        cat("  @", slot_name, ": ", old_nrow, " -> ", nrow(slot_value), " rows\n", sep = "")
      }
    }
  }
  
  return(obj)
})

#' @rdname subset_region
#' @export
setMethod("subset_region", signature(obj = "storage"), function(obj, keep_regions, verbose = TRUE) {
  if (verbose) {
    cat("Filtering storage:", obj@name, "\n")
  }
  
  # Filter the region slot (character vector)
  if (length(obj@region) > 0) {
    old_regions <- obj@region
    obj@region <- obj@region[obj@region %in% keep_regions]
    if (verbose && length(old_regions) != length(obj@region)) {
      cat("  @region: ", length(old_regions), " -> ", length(obj@region), " regions\n", sep = "")
    }
  }
  
  # Get all slot names
  slot_names <- slotNames(obj)
  
  # Filter data.frame slots with "region" column
  for (slot_name in slot_names) {
    slot_value <- slot(obj, slot_name)
    
    # Check if slot is a data.frame and has "region" column
    if (is.data.frame(slot_value) && "region" %in% names(slot_value)) {
      old_nrow <- nrow(slot_value)
      # Keep rows where region is in keep_regions OR region is NA
      slot_value <- slot_value[slot_value$region %in% keep_regions | is.na(slot_value$region), , drop = FALSE]
      slot(obj, slot_name) <- slot_value
      if (verbose && old_nrow != nrow(slot_value)) {
        cat("  @", slot_name, ": ", old_nrow, " -> ", nrow(slot_value), " rows\n", sep = "")
      }
    }
  }
  
  return(obj)
})

#' @rdname subset_region
#' @export
setMethod("subset_region", signature(obj = "demand"), function(obj, keep_regions, verbose = TRUE) {
  if (verbose) {
    cat("Filtering demand:", obj@name, "\n")
  }
  
  # Filter the region slot (character vector)
  if (length(obj@region) > 0) {
    old_regions <- obj@region
    obj@region <- obj@region[obj@region %in% keep_regions]
    if (verbose && length(old_regions) != length(obj@region)) {
      cat("  @region: ", length(old_regions), " -> ", length(obj@region), " regions\n", sep = "")
    }
  }
  
  # Get all slot names
  slot_names <- slotNames(obj)
  
  # Filter data.frame slots with "region" column
  for (slot_name in slot_names) {
    slot_value <- slot(obj, slot_name)
    
    # Check if slot is a data.frame and has "region" column
    if (is.data.frame(slot_value) && "region" %in% names(slot_value)) {
      old_nrow <- nrow(slot_value)
      # Keep rows where region is in keep_regions OR region is NA
      slot_value <- slot_value[slot_value$region %in% keep_regions | is.na(slot_value$region), , drop = FALSE]
      slot(obj, slot_name) <- slot_value
      if (verbose && old_nrow != nrow(slot_value)) {
        cat("  @", slot_name, ": ", old_nrow, " -> ", nrow(slot_value), " rows\n", sep = "")
      }
    }
  }
  
  return(obj)
})

#' @rdname subset_region
#' @export
setMethod("subset_region", signature(obj = "supply"), function(obj, keep_regions, verbose = TRUE) {
  if (verbose) {
    cat("Filtering supply:", obj@name, "\n")
  }
  
  # Filter the region slot (character vector)
  if (length(obj@region) > 0) {
    old_regions <- obj@region
    obj@region <- obj@region[obj@region %in% keep_regions]
    if (verbose && length(old_regions) != length(obj@region)) {
      cat("  @region: ", length(old_regions), " -> ", length(obj@region), " regions\n", sep = "")
    }
  }
  
  # Get all slot names
  slot_names <- slotNames(obj)
  
  # Filter data.frame slots with "region" column
  for (slot_name in slot_names) {
    slot_value <- slot(obj, slot_name)
    
    # Check if slot is a data.frame and has "region" column
    if (is.data.frame(slot_value) && "region" %in% names(slot_value)) {
      old_nrow <- nrow(slot_value)
      # Keep rows where region is in keep_regions OR region is NA
      slot_value <- slot_value[slot_value$region %in% keep_regions | is.na(slot_value$region), , drop = FALSE]
      slot(obj, slot_name) <- slot_value
      if (verbose && old_nrow != nrow(slot_value)) {
        cat("  @", slot_name, ": ", old_nrow, " -> ", nrow(slot_value), " rows\n", sep = "")
      }
    }
  }
  
  return(obj)
})

#' @rdname subset_region
#' @export
setMethod("subset_region", signature(obj = "import"), function(obj, keep_regions, verbose = TRUE) {
  # Note: import class doesn't have a @region slot, only data.frame with region column
  if (verbose) {
    cat("Filtering import:", obj@name, "\n")
  }
  
  # Get all slot names
  slot_names <- slotNames(obj)
  
  # Filter data.frame slots with "region" column
  for (slot_name in slot_names) {
    slot_value <- slot(obj, slot_name)
    
    # Check if slot is a data.frame and has "region" column
    if (is.data.frame(slot_value) && "region" %in% names(slot_value)) {
      old_nrow <- nrow(slot_value)
      # Keep rows where region is in keep_regions OR region is NA
      slot_value <- slot_value[slot_value$region %in% keep_regions | is.na(slot_value$region), , drop = FALSE]
      slot(obj, slot_name) <- slot_value
      if (verbose && old_nrow != nrow(slot_value)) {
        cat("  @", slot_name, ": ", old_nrow, " -> ", nrow(slot_value), " rows\n", sep = "")
      }
    }
  }
  
  return(obj)
})

#' @rdname subset_region
#' @export
setMethod("subset_region", signature(obj = "commodity"), function(obj, keep_regions, verbose = TRUE) {
  # Note: commodity class doesn't have region-specific data
  # Return unchanged as commodities are typically global
  if (verbose) {
    cat("Filtering commodity:", obj@name, "(no region data, unchanged)\n")
  }
  return(obj)
})

#' @rdname subset_region
#' @export
setMethod("subset_region", signature(obj = "constraint"), function(obj, keep_regions, verbose = TRUE) {
  # Filter data.frame slots with "region" column
  if (verbose) {
    cat("Filtering constraint:", obj@name, "\n")
  }
  
  # Track which slots become empty
  empty_slots <- character()
  
  # Filter for.each slot if it has region column
  if (is.data.frame(obj@for.each) && "region" %in% names(obj@for.each)) {
    old_nrow <- nrow(obj@for.each)
    obj@for.each <- obj@for.each[obj@for.each$region %in% keep_regions | is.na(obj@for.each$region), , drop = FALSE]
    if (verbose && old_nrow != nrow(obj@for.each)) {
      cat("  @for.each: ", old_nrow, " -> ", nrow(obj@for.each), " rows\n", sep = "")
    }
    if (nrow(obj@for.each) == 0) {
      empty_slots <- c(empty_slots, "for.each")
    }
  }
  
  # Filter rhs slot if it has region column
  if (is.data.frame(obj@rhs) && "region" %in% names(obj@rhs)) {
    old_nrow <- nrow(obj@rhs)
    obj@rhs <- obj@rhs[obj@rhs$region %in% keep_regions | is.na(obj@rhs$region), , drop = FALSE]
    if (verbose && old_nrow != nrow(obj@rhs)) {
      cat("  @rhs: ", old_nrow, " -> ", nrow(obj@rhs), " rows\n", sep = "")
    }
    if (nrow(obj@rhs) == 0) {
      empty_slots <- c(empty_slots, "rhs")
    }
  }
  
  # Filter lhs list - process each summand's mult data.frame
  if (is.list(obj@lhs) && length(obj@lhs) > 0) {
    for (i in seq_along(obj@lhs)) {
      summand <- obj@lhs[[i]]
      
      # Check if summand has mult slot that is a data.frame with region column
      if (is(summand, "summand") && is.data.frame(summand@mult) && "region" %in% names(summand@mult)) {
        old_nrow <- nrow(summand@mult)
        summand@mult <- summand@mult[summand@mult$region %in% keep_regions | is.na(summand@mult$region), , drop = FALSE]
        if (verbose && old_nrow != nrow(summand@mult)) {
          cat("  @lhs[[", i, "]]@mult: ", old_nrow, " -> ", nrow(summand@mult), " rows\n", sep = "")
        }
        if (nrow(summand@mult) == 0) {
          empty_slots <- c(empty_slots, paste0("lhs[[", i, "]]@mult"))
        }
        obj@lhs[[i]] <- summand
      }
    }
  }
  
  # Report empty slots
  if (length(empty_slots) > 0 && verbose) {
    cat("  \033[33mWARNING: The following slot(s) became empty after filtering: ",
        paste(empty_slots, collapse = ", "), "\033[0m\n", sep = "")
  }
  
  return(obj)
})

#' @rdname subset_region
#' @export
setMethod("subset_region", signature(obj = "weather"), function(obj, keep_regions, verbose = TRUE) {
  if (verbose) {
    cat("Filtering weather:", obj@name, "\n")
  }
  
  # Check if weather slot is empty before filtering
  if (nrow(obj@weather) == 0 && verbose) {
    cat("  \033[33mWARNING: @weather slot is empty (no data to filter)\033[0m\n")
    return(obj)
  }
  
  # Filter the region slot (character vector)
  if (length(obj@region) > 0) {
    old_regions <- obj@region
    obj@region <- obj@region[obj@region %in% keep_regions]
    if (verbose && length(old_regions) != length(obj@region)) {
      cat("  @region: ", length(old_regions), " -> ", length(obj@region), " regions\n", sep = "")
    }
  }
  
  # Get all slot names
  slot_names <- slotNames(obj)
  
  # Filter data.frame slots with "region" column
  for (slot_name in slot_names) {
    slot_value <- slot(obj, slot_name)
    
    # Check if slot is a data.frame and has "region" column
    if (is.data.frame(slot_value) && "region" %in% names(slot_value)) {
      old_nrow <- nrow(slot_value)
      # Keep rows where region is in keep_regions OR region is NA
      slot_value <- slot_value[slot_value$region %in% keep_regions | is.na(slot_value$region), , drop = FALSE]
      slot(obj, slot_name) <- slot_value
      if (verbose && old_nrow != nrow(slot_value)) {
        cat("  @", slot_name, ": ", old_nrow, " -> ", nrow(slot_value), " rows\n", sep = "")
      }
    }
  }
  
  # Check if weather slot became empty after filtering
  if (nrow(obj@weather) == 0 && verbose) {
    cat("  \033[33mWARNING: @weather slot is now empty after filtering\033[0m\n")
  }
  
  return(obj)
})

#' @rdname subset_region
#' @export
setMethod("subset_region", signature(obj = "export"), function(obj, keep_regions, verbose = TRUE) {
  # Note: export class doesn't have a @region slot, only data.frame with region column
  if (verbose) {
    cat("Filtering export:", obj@name, "\n")
  }
  
  # Get all slot names
  slot_names <- slotNames(obj)
  
  # Filter data.frame slots with "region" column
  for (slot_name in slot_names) {
    slot_value <- slot(obj, slot_name)
    
    # Check if slot is a data.frame and has "region" column
    if (is.data.frame(slot_value) && "region" %in% names(slot_value)) {
      old_nrow <- nrow(slot_value)
      # Keep rows where region is in keep_regions OR region is NA
      slot_value <- slot_value[slot_value$region %in% keep_regions | is.na(slot_value$region), , drop = FALSE]
      slot(obj, slot_name) <- slot_value
      if (verbose && old_nrow != nrow(slot_value)) {
        cat("  @", slot_name, ": ", old_nrow, " -> ", nrow(slot_value), " rows\n", sep = "")
      }
    }
  }
  
  return(obj)
})

#' @rdname subset_region
#' @param verbose Logical. If TRUE (default), reports the filtering process
#' @export
setMethod("subset_region", signature(obj = "repository"), function(obj, keep_regions, verbose = TRUE) {
  if (verbose) {
    cat("Filtering repository:", obj@name, "\n")
    cat("Keeping regions:", paste(keep_regions, collapse = ", "), "\n")
    cat("Processing", length(obj@data), "objects in @data slot...\n\n")
  }
  
  # Helper function to check if object was changed
  objects_equal <- function(obj1, obj2) {
    tryCatch({
      # For S4 objects, compare slot by slot
      if (isS4(obj1) && isS4(obj2)) {
        slot_names <- slotNames(obj1)
        for (sn in slot_names) {
          s1 <- slot(obj1, sn)
          s2 <- slot(obj2, sn)
          # Use all.equal for more robust comparison
          if (!isTRUE(all.equal(s1, s2))) {
            return(FALSE)
          }
        }
        return(TRUE)
      } else {
        # For non-S4 objects, use identical
        return(identical(obj1, obj2))
      }
    }, error = function(e) {
      # If comparison fails, assume they're different
      return(FALSE)
    })
  }
  
  # Process each object in the @data list
  for (i in seq_along(obj@data)) {
    item <- obj@data[[i]]
    item_name <- names(obj@data)[i]
    if (is.null(item_name) || item_name == "") {
      item_name <- paste0("[[", i, "]]")
    }
    item_class <- class(item)[1]
    
    # Try to apply subset_region method
    result <- tryCatch({
      # Call subset_region WITHOUT verbose parameter for non-repository objects
      filtered_item <- subset_region(item, keep_regions)
      
      # Check if object was actually changed
      was_changed <- !objects_equal(item, filtered_item)
      
      if (verbose) {
        status <- if (was_changed) "filtered (changed)" else "filtered (no change)"
        cat("  [", i, "]", item_name, "(", item_class, "): ", status, "\n", sep = "")
      }
      
      filtered_item
    }, error = function(e) {
      # If method doesn't exist or fails, return unchanged
      if (verbose) {
        cat("  [", i, "]", item_name, "(", item_class, "): unchanged (no method or error)\n", sep = "")
      }
      item
    })
    
    obj@data[[i]] <- result
  }
  
  if (verbose) {
    cat("\nFiltering complete.\n")
  }
  
  return(obj)
})
