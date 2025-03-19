## Run batches of R scripts. Handy if you want scripts to run after another finishes while you're away from the machine
rm(list = ls()) #reset

library(tidyverse)
library(MiMeMo.tools)
library(tictoc)

#### Batch process scripts ####

scripts <- c(                                           # List scripts in the order you want to run them
  # "./R scripts/1.1 NE EXTRACT BARENTS SEA.R",
  "./R scripts/1.2 NE EXTRACT EAST_GREENLAND.R",
  "./R scripts/2.1 NE EXTRACT ICE BARENTS SEA.R",
  "./R scripts/2.2 NE EXTRACT ICE EAST_GREENLAND.R"
) %>% 
  map(MiMeMo.tools::execute)                                                           # Run the scripts

# #### Plot run times ####
# 
# timings <- tictoc::tic.log(format = F) %>%                                             # Get the log of timings
#   lapply(function(x) data.frame("Script" = x$msg, Minutes = (x$toc - x$tic)/60)) %>%   # Get a dataframe of scripts and runtimes in minutes
#   bind_rows() %>%                                                                      # Get a single dataframe
#   separate(Script, into = c(NA, "Script"), sep = "/R scripts/") %>% 
#   separate(Script, into = c("Type", NA, NA), sep = "[.]", remove = F) %>% 
#   mutate(Script = factor(Script, levels = Script[order(rownames(.), decreasing = T)])) # Order the scripts
# saveRDS(timings, "./Objects/Run time.rds")