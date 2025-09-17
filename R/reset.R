# A helper function to clean environment (soft or hard restart)
# helps to remove tracing (unintentionally loaded) data and packages from
# global environment for a "fresh start"
reset_session <- function(warn = TRUE, prompt = TRUE, restart = FALSE) {
  # Collect targets
  objs <- ls(envir = .GlobalEnv, all.names = TRUE)

  attached <- search()
  base_pkgs <- paste0("package:", c("stats","graphics","grDevices",
                                    "utils","datasets","methods","base"))
  pkg_attached <- attached[grepl("^package:", attached)]
  non_base_pkgs_search <- setdiff(pkg_attached, base_pkgs)
  non_base_pkgs <- sub("^package:", "", non_base_pkgs_search)

  # User-attached environments (from attach()), exclude protected entries
  user_envs <- setdiff(attached[!grepl("^package:", attached)],
                       c(".GlobalEnv","Autoloads","tools:rstudio"))

  # Preview
  if (warn && (length(objs) + length(non_base_pkgs) + length(user_envs) > 0 || restart)) {
    msg <- c(
      "About to clear session:",
      if (length(objs)) paste0("  - Objects: ", paste(objs, collapse = ", ")) else "  - No objects in .GlobalEnv.",
      if (length(non_base_pkgs)) paste0("  - Packages: ", paste(non_base_pkgs, collapse = ", ")) else "  - No non-base packages.",
      if (length(user_envs)) paste0("  - Attached environments: ", paste(user_envs, collapse = ", ")) else "  - No user-attached environments.",
      paste0("  - Will try to restart session: ", if (restart) "YES" else "NO")
    )
    message(paste(msg, collapse = "\n"))
  }

  # Confirm
  if (prompt && (length(objs) + length(non_base_pkgs) + length(user_envs) > 0 || restart)) {
    if (interactive()) {
      ans <- readline("Proceed? [y/N]: ")
      if (!tolower(ans) %in% c("y","yes")) {
        message("Aborted.")
        return(invisible(NULL))
      }
    } else {
      stop("Prompt requested but session is not interactive. Re-run with prompt = FALSE.")
    }
  }

  # 1) Remove objects
  if (length(objs)) rm(list = objs, envir = .GlobalEnv)

  # 2) Detach user-attached environments (reverse order)
  if (length(user_envs)) {
    for (env in rev(user_envs)) {
      try(detach(env, character.only = TRUE), silent = TRUE)
    }
  }

  # 3) Unload non-base packages
  if (length(non_base_pkgs)) {
    if (requireNamespace("pacman", quietly = TRUE)) {
      pacman::p_unload(char = non_base_pkgs, character.only = TRUE)
    } else {
      for (pkg in rev(non_base_pkgs)) {
        try(detach(paste0("package:", pkg), character.only = TRUE, unload = TRUE), silent = TRUE)
      }
    }
  }

  # 4) Optional restart
  if (restart) {
    # Ensure rstudioapi if we are in RStudio
    need_restart <- TRUE
    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE)) &&
        isTRUE(tryCatch(rstudioapi::hasFun("restartSession"), error = function(e) FALSE))) {
      rstudioapi::restartSession(clean = TRUE)
      need_restart <- FALSE
    }

    if (need_restart) {
      if (interactive()) {
        ans <- readline("Could not use rstudioapi::restartSession(). Quit R session now? [y/N]: ")
        if (tolower(ans) %in% c("y","yes")) {
          quit(save = "no")
        } else {
          message("Restart skipped. You may need to restart R manually.")
        }
      } else {
        message("Restart not available in this environment. Please restart R manually.")
      }
    }
  }

  invisible(list(
    removed_objects   = objs,
    detached_packages = non_base_pkgs,
    detached_envs     = user_envs,
    restart_attempted = restart
  ))
}

# pacman::p_unload(char = non_base_pkgs, character.only = TRUE)
