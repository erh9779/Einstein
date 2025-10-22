# prepare tests

  # close open devices
    while (dev.cur() > 1) dev.off()
  # clear envrionment
    rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)

  # set working directory
  # define constants and functions, but be aware they might be overwritten by following loading of scripts

  # remove packages
    pkgs <- names(sessionInfo()$otherPkgs)
    while (length(pkgs) > 0) {
      lapply(pkgs, function(pkg) detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE))
      pkgs <- names(sessionInfo()$otherPkgs)
    }
  # don't install packages besides test suite in order not to influence script to be tested



# execute tests

  cat("\fEinstein\n")

  # don't load utility scripts besides script to be tested in order not to influence script to be tested
    source("Einstein_Calculation.R")
  # execute einstein calc
      cat("Einstein Calculation\n")
      # readline(prompt = "Drücke [Enter], um fortzufahren...")
      possible_fish_owners <- calculate_fish_owners()
      print(possible_fish_owners)
