rm(list=ls())

# place all files in the same working directory and change to this directory 
setwd("../PPH-Model/Shiny App")

# load functions we need
source("sub_routines.R")

# some preliminary stuff
# these are not varied in sensitivity analyses
interventions <- c("Misoprostol", "No Misoprostol")

source("data_input.R")


# run basecase model
model <- RunModel(baseparms = baseinputs, 
                  basetransitions = basetransitions,
                  makePlots = TRUE)

print(model, what = "tables")
print(model, what = "plots")

dsa <- owsa(model = model,
            low_base = usa.low, low_transitions = dir_lowinputs,
            high_base = usa.high, high_transitions = dir_highinputs,
            max_vars = 20)

## PSA
psa <- RunPSA(model = model, nsims = 50, wtp = 2000, by = 200)

psa$pT["Incidence", ]


pblapply("Incidence",
         FUN = function(x) data.table::setDF(data.table::rbindlist(psa$pT[x, ])))
