# This script runs the model a set number of times (e.g. 200)
# It also compares the first and second half of model runs (to check stability of results)
# These computations are not memory-intensive (i.e. do not require a lot of RAM) but may take a long time to run on a single machine. You can set 'n_runs' to a smaller number of runs.

#  --------------
#  number of runs
#  ==============

n_runs <- 50 # set the number of runs here
seeds <- seq_len(n_runs) # this variable can also be changed to any set of seeds, but there is no real benefit to this

#  -----------------
#  general functions
#  =================

# extract key results from model
pe <- function (fm, start_day = 1) { 
  fm <- fm[,start_day:ncol(fm)]
  x.itu <- sum(rowSums(fm == 15) > 0)
  c(
    n = nrow(fm),
    peak_PROTECT = max(colSums((fm > 6) & (fm < 12))),
    peak_CARE = max(colSums((fm == 12) | (fm == 13))),
    cases = sum(rowSums(fm == 2 | fm == 8) > 0),
    deaths = sum(fm[, ncol(fm)] == 16) - sum(fm[,1] == 16),
    admissions_itu = sum(rowSums(fm == 14) > 0) + x.itu,
    itu = x.itu
  )
}

# number of new cases per day (input is raw output from 'run_model')

new_cases <- function(fm) {
  nd <- ncol(fm)
  x <- fm == 2 | fm == 8
  case_day <- max.col(x, ties.method = 'first')
  anyCase <- rowSums(x) == 0
  case_day[anyCase] <- NA
  sapply(seq_len(nd), function(x) sum(case_day == x, na.rm = T))
}

# function for doing multiple runs. returns a list of 2 objects
# the first is a matrix of key results defined in 'pe' (starting from day 'start_day'); the second is the number of new cases each day (starting from day 1)

mr <- function(..., start_day = 1) {
  ts <- proc.time()
  r <- NULL
  rc <- NULL
  for (i in seeds) {
    t0 <- proc.time()
    m <- run_model(..., seed = i)
    r <- rbind(r,pe(m, start_day = start_day))
    rc <- rbind(rc, new_cases(m))
    print(paste0(i, '; ', round((proc.time() - t0)[3], 2))) # prints off a number and time each time a model iteration is completed
  }
  print(proc.time() - ts) # total time to run all seeds
  print((proc.time() - ts) / length(seeds)) # average time to run model
  list(r, rc)
}

#  ----------
#  Scenario A: Base Case
#  ==========

sALA <- mr(model_duration = 396,
         interventions_start = 61,
         intervention_ends = NA,
         start_day = 1) # start_day is for 'pe' rather than run_model

#  ----------
#  Scenario B: No Hotel Housing Ever Used
#  ==========

sBLA <- mr(model_duration = 396,
         interventions_start = 1,
         intervention_ends = 1,
         start_day = 1) # start_day is for 'pe' rather than run_model

#  ----------
#  Scenario C: Hotel Housing Started Earlier
#  ==========

sCLA <- mr(model_duration = 396,
         interventions_start = 46,
         intervention_ends = 396,
         start_day = 1) # start_day is for 'pe' rather than run_model

#  ----------
#  Scenario D: Hotel Housing Started Even Earlier
#  ==========

sDLA <- mr(model_duration = 396,
         interventions_start = 15,
         intervention_ends = 396,
         start_day = 1) # start_day is for 'pe' rather than run_model

#  ----------
#  Scenario E: Hotel Housing Started Later
#  ==========

sELA <- mr(model_duration = 396,
         interventions_start = 76,
         intervention_ends = 396,
         start_day = 1) # start_day is for 'pe' rather than run_model

#  ----------
#  Scenario F: Hotel Housing Started Even Later
#  ==========

sFLA <- mr(model_duration = 396,
         interventions_start = 92,
         intervention_ends = 396,
         start_day = 1) # start_day is for 'pe' rather than run_model

#  ----------
#  Scenario G: Hotel Housing Started At the end of wave 1
#  ==========

sGLA <- mr(model_duration = 396,
           interventions_start = 98,
           intervention_ends = 396,
           start_day = 1) # start_day is for 'pe' rather than run_model

#  ----------
#  Scenario H: Hotel Housing Started Mid wave 2
#  ==========

sHLA <- mr(model_duration = 396,
           interventions_start = 149,
           intervention_ends = 396,
           start_day = 1) # start_day is for 'pe' rather than run_model
