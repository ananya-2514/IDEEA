################################################################################
# Project setup:
# - [Don't edit 'R/setup-template.R']
# - save a copy of this file to R/setup.R
# - edit 'R/setup.R' to configure your sustem
#

## Packages
library(tidyverse) # data, charts, utils
library(data.table) # efficient data frames (tables)
library(sf) # geo-maps
library(glue) # stings
library(IDEEA)

## Directory with scenarios (solved and new)
set_scenarios_path("D:/WRI/ideea_scenarios")
# set_scenarios_path("PATH/to/shared/scenarios")
get_scenarios_path()

# Directory with IDEEA additional (extra) datasets
# set_ideea_extra("PATH/TO/ideea_extra")
ideea_extra() # if empty - use the row above to set (once)

# Set default solver (language of the model script and the linear solver)
# set_default_solver(solver_options$julia_highs)
set_default_solver(solver_options$julia_cplex_barrier)
# set_default_solver(solver_options$pyomo_cplex_barrier)
# set_default_solver(solver_options$gams_gdx_cplex_barrier)
# names(solver_options) # to see more options

# Output control
set_progress_bar(type = "progress") # output progress bar in the console
# show_progress_bar(show = FALSE) # to switch off

# load functions
source("R/ideea_snapshot2.r") # updated "snapshot" function

# === temporary patch to fix `.scen` error === #
if (!exists(".scen")) {
  .initiate_env <- function(e) {
    if (!exists(e, envir = .GlobalEnv)) {
      assign(e, new.env(parent = .GlobalEnv), envir = .GlobalEnv)
    }
  }
  .initiate_env(".scen")
  .initiate_env(".tmp")
}
# === end of .scen-patch === #
