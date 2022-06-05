
# place all files in the same working directory and change to this directory 
# setwd("../../PPH-Model/Shiny App")


# load functions we need
source("sub_routines.R")

# some preliminary stuff
# these are not varied in sensitivity analyses
interventions <- c("Misoprostol", "No Misoprostol")


# load data
data = read_excel("pph_data_input.xlsx", sheet = 1)

# colors
col <- brewer.pal(8,"Dark2")

## general parameters
# discounts
discFact <- 0.03
procDaysYr <- 264
procHrsDay <- 8
age <- c(18, 27, 42)
LifeExp <- 63

# parameters for misoprostol program costs
nWomen <- 10000
nHW <- 200
daysTraining <- 5
cTrain <- 10 #US$/day
nurseFTE <- 20
nNurse <- 50
cPackage <- 0.5 # 50% of cost of misoprostol


# pull out inputs for the main model but separate out the transition probabilities
baseinputs <- data %>%
  filter(!is.na(variable) & dist != "dirichlet") %>%   # remove NA from data
  pull(basecase, variable)

# process dirichlet inputs so I can input set of high and low values at the same time (also add label, so it is easy to reference in functions)
basetransitions <- data %>%
  filter(!is.na(variable) & dist == "dirichlet") %>%   # remove NA from data
  pull(basecase, variable)

## ONE WAY SENSITIVITY ANALYSES
# parameters
usa.low <- data %>%
  filter(!is.na(low) & dist != "dirichlet") %>%   # remove NA from data
  pull(low, variable)

usa.high <- data %>%
  filter(!is.na(low) & dist != "dirichlet") %>%   # remove NA from data
  pull(high, variable)

dir_lowinputs <- data %>%
  filter(dist == "dirichlet") %>%   # remove NA from data
  pull(low, variable) %>%
  split(., rep(1:2, c(5, 3)))
names(dir_lowinputs) <- c("Quintiles", "AgeDistribution")

dir_highinputs <- data %>%
  filter(dist == "Dirichlet") %>%   # remove NA from data
  pull(high, variable) %>%
  split(., rep(1:2, c(5, 3)))
names(dir_highinputs) <- c("Quintiles", "AgeDistribution")


TxTrjList <- list("Hospital", "HC", "Friend", "TBA", "Unassisted")
names(TxTrjList) <- c("Hospital", "HC", "Friend", "TBA", "Unassisted")

QuintileList <- list("Lowest", "Second", "Middle", "Fourth", "Highest")
names(QuintileList) <- c("Lowest", "Second", "Middle", "Fourth", "Highest")

# put outcomes in a list and run DSA and PSA on all, remember to add names so they are identifiable in the returned list
outcomes = list("Incidence", "Mortality", "DALYs")
names(outcomes) = c("Incidence", "Mortality", "DALYs")


# how many simulations? In furure, either wrap into function or pregenerate the data in the data_input file
nsims <- 20

# first identify fixed parameters
fixedparms <- data %>%
  filter(dist == "fixed") %>%
  pull(variable)

betaparms <- data %>%
  filter(dist == "beta") %>%
  pull(variable)

tnormparms <- data %>%
  filter(dist == "truncnormal") %>%
  pull(variable)

unifparms <- data %>%
  filter(dist == "uniform") %>%
  pull(variable)

lognormparms <- data %>%
  filter(dist == "lognormal") %>%
  pull(variable)

alphas <- data %>%
  pull(alpha, variable)

betas <- data %>%
  pull(beta, variable)

sems <- data %>%
  pull(sem, variable)

# random dirichlet transitions
randdirchinputs <- data %>%
  filter(dist == "dirichlet") %>%   # remove NA from data
  pull(alpha, variable) %>%
  split(., rep(1:2, c(5, 3)))
names(randdirchinputs) <- c("Quintiles", "AgeDistribution")

# generate variates and put in one database

# # draw values
# #betas
# randbeta <- mapply(rbeta, MoreArgs=list(n=nsims), alphas[betaparms], betas[betaparms])
# 
# # normals
# randtnormal <- mapply(rtruncnorm, MoreArgs=list(n=nsims), 
#                       mean = baseinputs[tnormparms], sd = sems[tnormparms], 
#                       a = usa.low[tnormparms], b = usa.high[tnormparms])
# 
# # durations (uniformly distributed)
# randunif <- mapply(runif, MoreArgs=list(n=nsims), usa.low[unifparms], usa.high[unifparms])
# 
# # log-normals
# randlognormal <- mapply(rlnorm, MoreArgs=list(n=nsims), 
#                         meanlog = log(baseinputs[lognormparms]), 
#                         sdlog = sems[lognormparms])
# 
# # use data.table for speed; avoid do.call("cbind, .)
# randdirichlets <- data.table::as.data.table(unname(lapply(randdirchinputs, vdirichlet, n=nsims)))
# randdirichlets <- data.table::setDF(randdirichlets)
# 
# 
# fixedinputs <- matrix(rep(baseinputs[fixedparms],each=nsims),nrow=nsims) %>%
#   magrittr::set_colnames(., fixedparms)
# 
# 
# # pool together into one dataframe
# psa_input <- data.frame(randbeta, randtnormal, randunif, randlognormal, randdirichlets, fixedinputs)

