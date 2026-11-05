

### make inputs
makeBaseCaseInputs <- function(data) {
  # pull out inputs for the main model but separate out the transition probabilities
  baseinputs <- data |>
    filter(!is.na(variable) & dist != "dirichlet") |>   # remove NA from data
    pull(basecase, variable)
  
  ## keep in list for sonsistency
  # basetransitions <- data |>
  #   filter(!is.na(variable) & dist == "dirichlet") |>   # remove NA from data
  #   pull(basecase, variable)
  
  basetransitions <- data |>
    filter(dist == "dirichlet") |>   # remove NA from data
    pull(basecase, variable) |>
    split(rep(1:2, c(5, 3)))
  names(basetransitions) <- c("Quintiles", "AgeDistribution")
  
  basetransitions <- lapply(basetransitions, FUN = function(x) x/sum(x))
  
  return(list(baseinputs = baseinputs,
              basetransitions = c(basetransitions$Quintiles, 
                                  basetransitions$AgeDistribution)))
}


### make inputs
makeOWSAInputs <- function(data) {
  
  # pull out inputs for the main model but separate out the transition probabilities
  # parameters
  usa.low <- data |>
    filter(!is.na(low) & dist != "dirichlet") |>   # remove NA from data
    pull(low, variable)
  
  usa.high <- data |>
    filter(!is.na(low) & dist != "dirichlet") |>   # remove NA from data
    pull(high, variable)
  
  basetransitions <- data |>
    filter(dist == "dirichlet") |>   # remove NA from data
    pull(basecase, variable) |>
    split(rep(1:2, c(5, 3)))
  names(basetransitions) <- c("Quintiles", "AgeDistribution")

  # 95% interval of each share, from its marginal Beta(a_i, sum(a) - a_i), where a = the counts
  # (the same counts are the Dirichlet parameters in the PSA)
  dir_lowinputs  <- lapply(basetransitions, function(a) qbeta(0.025, a, sum(a) - a))
  dir_highinputs <- lapply(basetransitions, function(a) qbeta(0.975, a, sum(a) - a))

  return(list(usa.low = usa.low,
              usa.high = usa.high,
              dir_lowinputs = dir_lowinputs, 
              dir_highinputs = dir_highinputs))
  
}


# function calculates care path depending on wealth quintile
# Intervention = "Miso" will apply the treatment effects (in this function on odds of HF delivery)
getCarePath <- function(params, Quintile, Intervention = "Miso") {

  stopifnot(Intervention %in% c("Miso", "NoMiso"))

  # Look up the four delivery-probability parameters for this quintile
  s <- switch(Quintile,
    Lowest  = list(pHfDel = params[["pHfDelLowQ"]],  pHospDel = params[["pHospDelLowQ"]],
                   pUnassDel = params[["pUnassDelLowQ"]],  pFriendRelDel = params[["pFriendRelDelLowQ"]]),
    Second  = list(pHfDel = params[["pHfDelSecQ"]],  pHospDel = params[["pHospDelSecQ"]],
                   pUnassDel = params[["pUnassDelSecQ"]],  pFriendRelDel = params[["pFriendRelDelSecQ"]]),
    Middle  = list(pHfDel = params[["pHfDelMidQ"]],  pHospDel = params[["pHospDelMidQ"]],
                   pUnassDel = params[["pUnassDelMidQ"]],  pFriendRelDel = params[["pFriendRelDelMidQ"]]),
    Fourth  = list(pHfDel = params[["pHfDelFourQ"]], pHospDel = params[["pHospDelFourQ"]],
                   pUnassDel = params[["pUnassDelFourQ"]], pFriendRelDel = params[["pFriendRelDelFourQ"]]),
    Highest = list(pHfDel = params[["pHfDelHighQ"]], pHospDel = params[["pHospDelHighQ"]],
                   pUnassDel = params[["pUnassDelHighQ"]], pFriendRelDel = params[["pFriendRelDelHighQ"]])
  )
  orHFDelivery <- params[["orHFDelivery"]]
  
  pHfDel        <- s$pHfDel
  pHospDel      <- s$pHospDel
  pUnassDel     <- s$pUnassDel
  pFriendRelDel <- s$pFriendRelDel
  
  # turn the survey shares (each a share of all births) into branch probabilities:
  # unassisted as a share of home births, relative/friend as a share of assisted home births
  pUnassDel     <- pUnassDel / (1 - pHfDel)
  pFriendRelDel <- pFriendRelDel / ((1 - pHfDel) * (1 - pUnassDel))
  
  # apply treatment effect: intervention increases the odds of HF delivery
  pHfDel <- switch(Intervention,
                   NoMiso = pHfDel,
                   Miso   = orHFDelivery*pHfDel/(1-pHfDel+(orHFDelivery*pHfDel)))
  
  # calculate product of path probabilities
  hosp   <- prod(pHfDel, pHospDel)
  hc     <- prod(pHfDel, (1-pHospDel))
  friend <- prod((1-pHfDel), (1-pUnassDel), pFriendRelDel)
  TBA    <- prod((1-pHfDel), (1-pUnassDel), (1-pFriendRelDel))
  unass  <- prod((1-pHfDel), pUnassDel)
  
  return(c(hospital = hosp, hc = hc, friend = friend, TBA = TBA, unass = unass))
  
}


# function calculates care path: 32 different possibilities per care pathway (5 care pathways)
# Intervention = "Miso" will apply the treatment effects, higher access to miso with intervention
getTxPath <- function(params, Where, Intervention = "Miso") {

  stopifnot(Intervention %in% c("Miso", "NoMiso"))

  # Look up per-setting parameters: oxytocin access, EmOC access, miso coverage
  # (intervention vs control), and PPH base rate + miso efficacy ratio
  s <- switch(Where,
    Hospital   = list(pOxy = params[["pOxyHosp"]],      pAccessEmOC = params[["pAccessEmOCHosp"]],
                      pMisoI = params[["pMisoAccessHospMisoPolicy"]], pMisoC = params[["pMisoAccessHospOxyPolicy"]],
                      pPPHbase = params[["pHospPPH"]],  rPPHMiso = params[["rPPHMisoHF"]]),
    HC         = list(pOxy = params[["pOxyHC"]],         pAccessEmOC = params[["pAccessEmOCHC"]],
                      pMisoI = params[["pMisoAccessHCMisoPolicy"]],   pMisoC = params[["pMisoAccessHCOxyPolicy"]],
                      pPPHbase = params[["pHospPPH"]],  rPPHMiso = params[["rPPHMisoHF"]]),
    Friend     = list(pOxy = params[["pOxyHomeFriend"]], pAccessEmOC = params[["pAccessEmOCFriend"]],
                      pMisoI = params[["pMisoAccessHomeFriendMisoPolicy"]], pMisoC = params[["pMisoAccessHomeOxyPolicy"]],
                      pPPHbase = params[["pHomePPH"]],  rPPHMiso = params[["rPPHMisoOxy"]]),
    TBA        = list(pOxy = params[["pOxyHomeTBA"]],    pAccessEmOC = params[["pAccessEmOCTBA"]],
                      pMisoI = params[["pMisoAccessHomeTBAMisoPolicy"]],    pMisoC = params[["pMisoAccessHomeOxyPolicy"]],
                      pPPHbase = params[["pHomePPH"]],  rPPHMiso = params[["rPPHMisoOxy"]]),
    Unassisted = list(pOxy = params[["pOxyHomeUnass"]],  pAccessEmOC = params[["pAccessEmOCUnass"]],
                      pMisoI = params[["pMisoAccessHomeUnassMisoPolicy"]],  pMisoC = params[["pMisoAccessHomeOxyPolicy"]],
                      pPPHbase = params[["pHomePPH"]],  rPPHMiso = params[["rPPHMisoOxy"]])
  )

  pOxy        <- s$pOxy
  pAccessEmOC <- s$pAccessEmOC
  pMisoAccess <- switch(Intervention, Miso = s$pMisoI, NoMiso = s$pMisoC)
  pPPHOxy     <- s$pPPHbase * params[["rPPHOxyPlacebo"]]
  pPPHMiso    <- s$pPPHbase * s$rPPHMiso
  pPPH        <- s$pPPHbase

  # misuse (a woman taking her own misoprostol before the birth) only happens with
  # prenatal distribution; without it, misoprostol is given by staff
  pMisuse     <- switch(Intervention, Miso = params[["pMisuseMiso"]], NoMiso = 0)
  # despite its name, pNoMisoAE is the probability of an adverse event after misuse
  pAE         <- params[["pNoMisoAE"]]

  # outcome probabilities used in the paths below
  pDieEmOC            <- params[["pDieEmOC"]]
  pDieNoEmOC          <- params[["pDieNoEmOC"]]
  pDieNoPPH           <- params[["pDieNoPPH"]]
  pUterineRuptureMiso <- params[["pUterineRuptureMiso"]]
  pDieUterineRupture  <- params[["pDieUterineRupture"]]
  pDieStillBirth      <- params[["pDieStillBirth"]]

  # calculate individual paths
  # PPH deaths: women who reach EmOC die at pDieEmOC, women who don't at pDieNoEmOC
  pathlist <- c(
    path01 = prod(pOxy, pPPHOxy, pAccessEmOC, pDieEmOC),
    path02 = prod(pOxy, pPPHOxy, pAccessEmOC, (1-pDieEmOC)),
    path03 = prod(pOxy, pPPHOxy, (1-pAccessEmOC), pDieNoEmOC),
    path04 = prod(pOxy, pPPHOxy, (1-pAccessEmOC), (1-pDieNoEmOC)),
    path05 = prod(pOxy, (1-pPPHOxy), pDieNoPPH),
    path06 = prod(pOxy, (1-pPPHOxy), (1-pDieNoPPH)),
    path07 = prod((1-pOxy), pMisoAccess, (1-pMisuse), pPPHMiso, pAccessEmOC, pDieEmOC),
    path08 = prod((1-pOxy), pMisoAccess, (1-pMisuse), pPPHMiso, pAccessEmOC, (1-pDieEmOC)),
    path09 = prod((1-pOxy), pMisoAccess, (1-pMisuse), pPPHMiso, (1-pAccessEmOC), pDieNoEmOC),
    path10 = prod((1-pOxy), pMisoAccess, (1-pMisuse), pPPHMiso, (1-pAccessEmOC), (1-pDieNoEmOC)),
    path11 = prod((1-pOxy), pMisoAccess, (1-pMisuse), (1-pPPHMiso), pDieNoPPH),
    path12 = prod((1-pOxy), pMisoAccess, (1-pMisuse), (1-pPPHMiso), (1-pDieNoPPH)),
    path15 = prod((1-pOxy), pMisoAccess, pMisuse, pAE, pUterineRuptureMiso, pDieUterineRupture),
    path16 = prod((1-pOxy), pMisoAccess, pMisuse, pAE, pUterineRuptureMiso, (1-pDieUterineRupture)),
    path17 = prod((1-pOxy), pMisoAccess, pMisuse, pAE, (1-pUterineRuptureMiso), pDieStillBirth),
    path18 = prod((1-pOxy), pMisoAccess, pMisuse, pAE, (1-pUterineRuptureMiso), (1-pDieStillBirth)),
    path19 = prod((1-pOxy), pMisoAccess, pMisuse, (1-pAE), pPPH, pAccessEmOC, pDieEmOC),
    path20 = prod((1-pOxy), pMisoAccess, pMisuse, (1-pAE), pPPH, pAccessEmOC, (1-pDieEmOC)),
    path21 = prod((1-pOxy), pMisoAccess, pMisuse, (1-pAE), pPPH, (1-pAccessEmOC), pDieNoEmOC),
    path22 = prod((1-pOxy), pMisoAccess, pMisuse, (1-pAE), pPPH, (1-pAccessEmOC), (1-pDieNoEmOC)),
    path23 = prod((1-pOxy), pMisoAccess, pMisuse, (1-pAE), (1-pPPH), pDieNoPPH),
    path24 = prod((1-pOxy), pMisoAccess, pMisuse, (1-pAE), (1-pPPH), (1-pDieNoPPH)),
    path27 = prod((1-pOxy), (1-pMisoAccess), pPPH, pAccessEmOC, pDieEmOC),
    path28 = prod((1-pOxy), (1-pMisoAccess), pPPH, pAccessEmOC, (1-pDieEmOC)),
    path29 = prod((1-pOxy), (1-pMisoAccess), pPPH, (1-pAccessEmOC), pDieNoEmOC),
    path30 = prod((1-pOxy), (1-pMisoAccess), pPPH, (1-pAccessEmOC), (1-pDieNoEmOC)),
    path31 = prod((1-pOxy), (1-pMisoAccess), (1-pPPH), pDieNoPPH),
    path32 = prod((1-pOxy), (1-pMisoAccess), (1-pPPH), (1-pDieNoPPH))
  )
  
  return(pathlist)
  
}


# DALY components for one condition at one age:
#   YLL = discounted years of life lost if the woman dies at `age`, counted up to LE
#   YLD = years lived with disability = disability weight x duration (in years)
# One entry per year of life lost: whole years, then the part-year left at the end
# (half a year when LE = 62.5). The first year loses the time already counted as YLD.
# Each year is discounted from its start, as in the Excel model.
getDALYS <- function(age, LE, disabilitywt, duration, discFact) {
  yrs_left <- LE - age
  index    <- 0:(ceiling(yrs_left) - 1)
  share    <- pmin(1, yrs_left - index)
  share[1] <- share[1] - duration
  disc_vec <- 1 / (1 + discFact)^index
  YLL      <- sum(disc_vec * share)
  YLD      <- disabilitywt * duration
  return(c(YLL = YLL, YLD = YLD))
}

getCareCost <- function(params, Perspective = "Societal", Intervention = "Miso") {

  stopifnot(Perspective %in% c("Societal", "Governmental"), Intervention %in% c("Miso", "NoMiso"))

  # calculate individual cost paths
  # to preserve structure with the paths module, set places where there is no cost to zero
  cOxy <- switch(Intervention,
                 Miso   = params[["cOxytocin"]] + params[["cDestruction"]],
                 NoMiso = params[["cOxytocin"]])
  # patient can still access Miso in HF if in Intervention; for intervention I add cMisoprostol to everyone
  cMisoAccess <- switch(Intervention, Miso = 0, NoMiso = params[["cMisoprostol"]])

  cAccessEmOC <- switch(Perspective,
                        Societal     = params[["cPPHManagementHospSocietal"]],
                        Governmental = params[["cPPHManagementHosp"]])
  cUterineRupture <- switch(Perspective,
                            Societal     = params[["cUterineRuptureSocietal"]],
                            Governmental = params[["cUterineRupture"]])
  cVaginalStillBirth <- switch(Perspective,
                               Societal     = params[["cVaginalStillBirthSocietal"]],
                               Governmental = params[["cVaginalStillBirth"]])
  cMortality <- params[["cMortality"]]

  costlist <- c(
    cost01 = sum(cOxy, cPPH = 0, cAccessEmOC, cDieEmOC = cMortality),
    cost02 = sum(cOxy, cPPH = 0, cAccessEmOC, cAliveEmOC = 0),
    cost03 = sum(cOxy, cPPH = 0, cNoAccessEmOC = 0, cDieEmOC = cMortality),
    cost04 = sum(cOxy, cPPH = 0, cNoAccessEmOC = 0, cDieEmOC = 0),
    cost05 = sum(cOxy, cNoPPH = 0, cDieNoPPH = cMortality),
    cost06 = sum(cOxy, cNoPPH = 0, cAliveNoPPH = 0),
    cost07 = sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cPPH = 0, cAccessEmOC, cDieEmOC = cMortality),
    cost08 = sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cPPH = 0, cAccessEmOC, cAliveEmOC = 0),
    cost09 = sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cPPH = 0, cNoAccessEmOC = 0, cDieEmOC = cMortality),
    cost10 = sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cPPH = 0, cNoAccessEmOC = 0, cAliveEmOC = 0),
    cost11 = sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cNoPPH = 0, cDieNoPPH = cMortality),
    cost12 = sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cNoPPH = 0, cAliveNoPPH = 0),
    cost15 = sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cMisoAE = 0, cUterineRupture, cDieUterineRupture = cMortality),
    cost16 = sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cMisoAE = 0, cUterineRupture, cAliveUterineRupture = 0),
    cost17 = sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cMisoAE = 0, cVaginalStillBirth, cDieStillBirth = cMortality),
    cost18 = sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cMisoAE = 0, cVaginalStillBirth, cAliveStillBirth = 0),
    cost19 = sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cNoMisoAE = 0, cPPH = 0, cAccessEmOC, cDieEmOC = cMortality),
    cost20 = sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cNoMisoAE = 0, cPPH = 0, cAccessEmOC, cAliveEmOC = 0),
    cost21 = sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cNoMisoAE = 0, cPPH = 0, cNoAccessEmOC = 0, cDieEmOC = cMortality),
    cost22 = sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cNoMisoAE = 0, cPPH = 0, cNoAccessEmOC = 0, cAliveEmOC = 0),
    cost23 = sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cNoMisoAE = 0, cNoPPH = 0, cDieNoPPH = cMortality),
    cost24 = sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cNoMisoAE = 0, cNoPPH = 0, cAliveNoPPH = 0),
    cost27 = sum(cOxy = 0, cNoMisoAccess = 0, cPPH = 0, cAccessEmOC, cDieEmOC = cMortality),
    cost28 = sum(cOxy = 0, cNoMisoAccess = 0, cPPH = 0, cAccessEmOC, cAliveEmOC = 0),
    cost29 = sum(cOxy = 0, cNoMisoAccess = 0, cPPH = 0, cNoAccessEmOC = 0, cDieEmOC = cMortality),
    cost30 = sum(cOxy = 0, cNoMisoAccess = 0, cPPH = 0, cNoAccessEmOC = 0, cAliveEmOC = 0),
    cost31 = sum(cOxy = 0, cNoMisoAccess = 0, cNoPPH = 0, cDieNoPPH = cMortality),
    cost32 = sum(cOxy = 0, cNoMisoAccess = 0, cNoPPH = 0, cAliveEmOC = 0)
  )
  
  return(costlist)
  
}


getIncidencePath <- function(params, Where, Intervention = "Miso") {

  stopifnot(Intervention %in% c("Miso", "NoMiso"))

  # Same lookup as getTxPath but without EmOC access (incidence only)
  s <- switch(Where,
    Hospital   = list(pOxy = params[["pOxyHosp"]],
                      pMisoI = params[["pMisoAccessHospMisoPolicy"]], pMisoC = params[["pMisoAccessHospOxyPolicy"]],
                      pPPHbase = params[["pHospPPH"]],  rPPHMiso = params[["rPPHMisoHF"]]),
    HC         = list(pOxy = params[["pOxyHC"]],
                      pMisoI = params[["pMisoAccessHCMisoPolicy"]],   pMisoC = params[["pMisoAccessHCOxyPolicy"]],
                      pPPHbase = params[["pHospPPH"]],  rPPHMiso = params[["rPPHMisoHF"]]),
    Friend     = list(pOxy = params[["pOxyHomeFriend"]],
                      pMisoI = params[["pMisoAccessHomeFriendMisoPolicy"]], pMisoC = params[["pMisoAccessHomeOxyPolicy"]],
                      pPPHbase = params[["pHomePPH"]],  rPPHMiso = params[["rPPHMisoOxy"]]),
    TBA        = list(pOxy = params[["pOxyHomeTBA"]],
                      pMisoI = params[["pMisoAccessHomeTBAMisoPolicy"]],    pMisoC = params[["pMisoAccessHomeOxyPolicy"]],
                      pPPHbase = params[["pHomePPH"]],  rPPHMiso = params[["rPPHMisoOxy"]]),
    Unassisted = list(pOxy = params[["pOxyHomeUnass"]],
                      pMisoI = params[["pMisoAccessHomeUnassMisoPolicy"]],  pMisoC = params[["pMisoAccessHomeOxyPolicy"]],
                      pPPHbase = params[["pHomePPH"]],  rPPHMiso = params[["rPPHMisoOxy"]])
  )

  pOxy        <- s$pOxy
  pMisoAccess <- switch(Intervention, Miso = s$pMisoI, NoMiso = s$pMisoC)
  pPPHOxy     <- s$pPPHbase * params[["rPPHOxyPlacebo"]]
  pPPHMiso    <- s$pPPHbase * s$rPPHMiso
  pPPH        <- s$pPPHbase
  pMisuse     <- switch(Intervention, Miso = params[["pMisuseMiso"]], NoMiso = 0)   # as in getTxPath
  pAE         <- params[["pNoMisoAE"]]                            # probability of an adverse event after misuse

  # calculate individual paths
  pathlist <- c(
    path01 = prod(pOxy, pPPHOxy),
    path06 = prod(pOxy, (1-pPPHOxy)),
    path07 = prod((1-pOxy), pMisoAccess, (1-pMisuse), pPPHMiso),
    path12 = prod((1-pOxy), pMisoAccess, (1-pMisuse), (1-pPPHMiso)),
    path15 = prod((1-pOxy), pMisoAccess, pMisuse, (1-pAE), pPPH),       # misuse, no adverse event, PPH
    path16 = prod((1-pOxy), pMisoAccess, pMisuse, 1 - (1-pAE) * pPPH),  # misuse, no PPH (adverse event, or no PPH)
    path27 = prod((1-pOxy), (1-pMisoAccess), pPPH),
    path32 = prod((1-pOxy), (1-pMisoAccess), (1-pPPH))
  )
  
  
  return(pathlist)
  
}



# Perspective is not used: incidence costs are the same from both perspectives
getIncidenceCost <- function(params, Perspective = "Societal", Intervention = "Miso") {

  stopifnot(Intervention %in% c("Miso", "NoMiso"))

  # calculate individual cost paths
  # to preserve structure with the paths module, set places where there is no cost to zero
  cOxy <- switch(Intervention,
                 Miso   = params[["cOxytocin"]] + params[["cDestruction"]],
                 NoMiso = params[["cOxytocin"]])
  # patient can still access Miso in HF if in Intervention; for intervention I add cMisoprostol to everyone
  cMisoAccess <- switch(Intervention, Miso = 0, NoMiso = params[["cMisoprostol"]])
  
  IncidCostlist <- c(
    cost01 = sum(cOxy, cPPH = 0),
    cost06 = sum(cOxy, cNoPPH = 0),
    cost07 = sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cPPH = 0),
    cost12 = sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cNoPPH = 0),
    cost15 = sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cPPH = 0),
    cost16 = sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cNoPPH = 0),
    cost27 = sum(cOxy = 0, cNoMisoAccess = 0, cPPH = 0),
    cost32 = sum(cOxy = 0, cNoMisoAccess = 0, cNoPPH = 0)
  )
  
  return(IncidCostlist)
  
}


# function calculates the cost of reaching each place of delivery
# Intervention = "Miso" adds the misoprostol programme cost
getAccessCost <- function(params, Perspective = "Societal", Intervention = "Miso") {

  stopifnot(Perspective %in% c("Societal", "Governmental"), Intervention %in% c("Miso", "NoMiso"))

  # cost of a normal delivery in each setting; home deliveries cost the government nothing
  cDel <- switch(Perspective,
    Societal     = c(hospital = params[["cNormalDeliveryHospSocietal"]],
                     hc       = params[["cNormalDeliveryHCSocietal"]],
                     friend   = params[["cNormalDeliveryHomeFriend"]],
                     TBA      = params[["cNormalDeliveryHomeTBA"]],
                     unass    = params[["cNormalDeliveryHomeUnass"]]),
    Governmental = c(hospital = params[["cNormalDeliveryHosp"]],
                     hc       = params[["cNormalDeliveryHC"]],
                     friend   = 0, TBA = 0, unass = 0))

  # every woman in the intervention arm carries the misoprostol programme cost
  ProgramCost <- switch(Intervention, Miso = params[["cMisoprostolProgram"]], NoMiso = 0)

  return(cDel + ProgramCost)
  
}


getMisoProgCosts <- function(nWomen, nHW, cTrain, daysTraining, cNurse, nNurse, nurseFTE, cMisoprostol, cPackage) {
  
  cHWTraining <- nHW * cTrain * daysTraining ## 200 HWs @ $10/day for 5 days
  cMotherTraining <- cNurse * 8 * 5 * nNurse * nurseFTE # 20 MidWife FTEs
  cPackaging <- cMisoprostol * cPackage * nWomen
  
  # put it together
  cMisoprostolProgram <- (cMisoprostol*nWomen + cHWTraining + cMotherTraining + cPackaging)/nWomen
  
  return(cMisoprostolProgram)
  
}


RunModel <- function(baseparms, 
                     basetransitions, 
                     makePlots = FALSE) {

  # every model parameter in one named vector, read with params[["name"]] and
  # passed explicitly to each helper function
  # (global.R constants such as age and TxTrjList are still read from global.R)
  params <- c(baseparms, basetransitions)

  # use mapply to iterate over lists of duration and disability weights
  # apply mapply function to the middle "ages" in the model
  dalysList <- lapply(age, FUN = function(x) {

    mapply(FUN = getDALYS,
           disabilitywt = list(params[["disabilitywtPPH"]], params[["disabilitywtRupture"]], params[["disabilitywtStillBirth"]]),
           duration = list(params[["durationPPH"]], params[["durationUterineRupture"]], params[["durationStillBirth"]]),
           MoreArgs = list(age = x, LE = params[["lifeexpectancy"]], discFact = params[["cDiscountCosts"]]))
    })


  # dalysList holds one 2 x 3 matrix per age group: rows YLL and YLD,
  # columns pph, rupture and stillbirth. Average over the age groups,
  # weighting each group by its share of births.
  proportions <- c(params[["isless20"]], params[["is20to34"]], params[["is35to49"]])
  age_wt <- Reduce(`+`, Map(`*`, dalysList, proportions))
  age_wt_DALYs <- list(pph = age_wt[, 1], rupture = age_wt[, 2], stillbirth = age_wt[, 3])
  

  ## opportunity cost calculations
  cProductivity     <- params[["cProductivity"]]
  averageTravelTime <- params[["averageTravelTime"]]

  cNormDelHomeFriend <- (cProductivity/procDaysYr)*params[["avgFriendTime"]]
  cOppCostPPHPatientTime <- ((params[["averageLOSPPH"]]*cProductivity)/procDaysYr)+((averageTravelTime*cProductivity)/(procDaysYr*procHrsDay))
  # the woman's own time recovering at home after a home birth (no travel)
  OppCostPatientTimeHome <- ((params[["averageLOSHome"]]*cProductivity)/procDaysYr)
  cOppCostRupturePatientTime <- ((params[["averageLOSUterineRupture"]]*cProductivity)/procDaysYr)+((averageTravelTime*cProductivity)/(procDaysYr*procHrsDay))
  cOppCostNormDelPatient <- ((params[["averageLOSNormDel"]]*cProductivity)/procDaysYr)+((averageTravelTime*cProductivity)/(procDaysYr*procHrsDay))

  dirNonMedHC <- params[["cTravelHC"]] + params[["cUpkeepHC"]]
  dirNonMedHosp <- params[["cTravelHospital"]] + params[["cUpkeepHospital"]]

  # costs of PPH management (at hospital rates, wherever the woman delivered, as in the Excel model)
  cPPHManagementHospSocietal <- params[["cPPHManagementHosp"]] + dirNonMedHosp + cOppCostPPHPatientTime

  # costs of Normal Delivery
  cNormalDeliveryHCSocietal <- params[["cNormalDeliveryHC"]] + dirNonMedHC + cOppCostNormDelPatient
  cNormalDeliveryHospSocietal <- params[["cNormalDeliveryHosp"]] + dirNonMedHosp + cOppCostNormDelPatient

  # costs of treating a uterine rupture (only in hospital)
  cUterineRuptureSocietal <- params[["cUterineRupture"]] +  dirNonMedHosp + cOppCostRupturePatientTime

  # cost of a vaginal still birth equal to costs of a normal delivery
  cVaginalStillBirthSocietal <- params[["cVaginalStillBirth"]] + dirNonMedHosp + cOppCostNormDelPatient

  # normal delivery by TBA: TBA fee + travel to the TBA + the helper's time + the woman's own time
  cNormalDeliveryHomeTBA <- params[["cNormDelHomeTBA"]] + params[["cTravelTBA"]] + cNormDelHomeFriend + OppCostPatientTimeHome

  # normal delivery by friend: the helper's time + the woman's own time
  cNormalDeliveryHomeFriend <- cNormDelHomeFriend + OppCostPatientTimeHome

  # normal delivery home unassisted: the woman's own time only
  cNormalDeliveryHomeUnass <- OppCostPatientTimeHome

  # calculate misoprostol program costs
  cMisoprostolProgram <- getMisoProgCosts(nWomen, nHW, cTrain, daysTraining, params[["cNurse"]], nNurse, nurseFTE, params[["cMisoprostol"]], cPackage)

  # add the derived costs to params, so the helper functions read them from params too
  params <- c(params,
              cPPHManagementHospSocietal  = cPPHManagementHospSocietal,
              cNormalDeliveryHospSocietal = cNormalDeliveryHospSocietal,
              cNormalDeliveryHCSocietal   = cNormalDeliveryHCSocietal,
              cUterineRuptureSocietal     = cUterineRuptureSocietal,
              cVaginalStillBirthSocietal  = cVaginalStillBirthSocietal,
              cNormalDeliveryHomeTBA      = cNormalDeliveryHomeTBA,
              cNormalDeliveryHomeFriend   = cNormalDeliveryHomeFriend,
              cNormalDeliveryHomeUnass    = cNormalDeliveryHomeUnass,
              cMisoprostolProgram         = cMisoprostolProgram)

  ### PAYOFF VECTORS, convert everything to matrix then tibble to ease calculations
  # # calculate incidence
  # # 1 = incident case, 0 = not an incident case
  # # avoid counting incidence multiple times, because model was built to estimate the incidence of PPH only
  incidence <- matrix(c(1, 0, 1, 0, 1, 0, 1, 0), ncol = 1)

  # calculate mortality
  mortality <- matrix(c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0), ncol = 1)
  
  # DALY payoffs, named and built as in the Excel model
  pphYLL     <- age_wt_DALYs$pph[["YLL"]];        pphYLD     <- age_wt_DALYs$pph[["YLD"]]
  ruptureYLL <- age_wt_DALYs$rupture[["YLL"]];    ruptureYLD <- age_wt_DALYs$rupture[["YLD"]]
  stillYLL   <- age_wt_DALYs$stillbirth[["YLL"]]; stillYLD   <- age_wt_DALYs$stillbirth[["YLD"]]

  # PPH: a death adds YLL; severe anaemia adds YLD (repeated for the two halves of the tree)
  pSevereAnemiaEmOC   <- params[["pSevereAnemiaEmOC"]]
  pSevereAnemianoEmOC <- params[["pSevereAnemianoEmOC"]]
  pph_payoff <- rep(c(pphYLL + pphYLD * pSevereAnemiaEmOC,      # PPH, EmOC, die
                      pphYLD * pSevereAnemiaEmOC,               # PPH, EmOC, live
                      pphYLL + pphYLD * pSevereAnemianoEmOC,    # PPH, no EmOC, die
                      pphYLD * pSevereAnemianoEmOC,             # PPH, no EmOC, live
                      0,                                        # no PPH, die
                      0), 2)                                    # no PPH, live

  # complications of misuse - uterine rupture / stillbirth
  compl_payoff <- c(ruptureYLL + ruptureYLD, ruptureYLD,
                    stillYLL + stillYLD, stillYLD)
  
  
  DALYlist <- matrix(c(pph_payoff, compl_payoff, pph_payoff), ncol = 1)
  
  apply_intervention <- c(NoMiso = "NoMiso", Miso = "Miso")

  # TREATMENT TRAJECTORIES
  # apply getTxPath function to each place of delivery, then to each intervention arm
  # each arm is a matrix: a row per path, a column per place of delivery
  TxTrajectory <- lapply(apply_intervention, FUN = function (x) {
    sapply(TxTrjList, FUN = getTxPath, params = params, Intervention = x)}
    )

  # # INCIDENCE ONLY TRAJECTORY
  IncidTrajectory <- lapply(apply_intervention, FUN = function (x) {
    sapply(TxTrjList, FUN = getIncidencePath, params = params, Intervention = x)}
  )
  
  # # CAREPATHWAY TRAJECTORIES
  # now apply getCarePath function to each Quintile list, then to each intervention arm
  # return TxTrajectory list with trajectory for each treatment arm
  CareTrajectory <- lapply(apply_intervention, FUN = function (x) {
    lapply(QuintileList, FUN = getCarePath, params = params, Intervention = x)}
  )
  
  # for each quintile, multiply each column (place of delivery) of the path matrix
  # by the share of women in that quintile who deliver there
  # (rep(x, each = ) repeats each share down its column)
  noMisoAll <- lapply(CareTrajectory$NoMiso, FUN = function(x) {
    TxTrajectory$NoMiso * rep(x, each = nrow(TxTrajectory$NoMiso))
  })

  MisoAll <- lapply(CareTrajectory$Miso, FUN = function(x) {
    TxTrajectory$Miso * rep(x, each = nrow(TxTrajectory$Miso))
  })

  # apply to incidence tables
  IncidnoMisoAll <- lapply(CareTrajectory$NoMiso, FUN = function(x) {
    IncidTrajectory$NoMiso * rep(x, each = nrow(IncidTrajectory$NoMiso))
  })

  IncidMisoAll <- lapply(CareTrajectory$Miso, FUN = function(x) {
    IncidTrajectory$Miso * rep(x, each = nrow(IncidTrajectory$Miso))
  })
  
  
  # # COST for TREATMENT TRAJECTORIES
  perspective <- c(Societal = "Societal", Governmental = "Governmental")

  CareCost <- lapply(apply_intervention, FUN = function(x) {
    sapply(perspective, getCareCost, params = params, Intervention = x)
  })
  
  IncidCost <- lapply(apply_intervention, FUN = function(x) {
    sapply(perspective, getIncidenceCost, params = params, Intervention = x)
  })

  AccessCost <- lapply(apply_intervention, FUN = function(x) {
    sapply(perspective, getAccessCost, params = params, Intervention = x)
  })
  
  CostCalc <- function(x) {
    
    totalCareNoMiso <- t(outer(AccessCost$NoMiso[, x], CareCost$NoMiso[, x], FUN = "+"))
    totalCareMiso <- t(outer(AccessCost$Miso[, x], CareCost$Miso[, x], FUN = "+"))
    IncidCareNoMiso <- t(outer(AccessCost$NoMiso[, x], IncidCost$NoMiso[, x], FUN = "+"))
    IncidCareMiso <- t(outer(AccessCost$Miso[, x], IncidCost$Miso[, x], FUN = "+"))
    
    # compute total care costs
    totCareMiso <- sapply(MisoAll, FUN = function(z) sum(z * c(totalCareMiso)))
    
    totCareNoMiso <- sapply(noMisoAll, FUN = function(z) sum(z * c(totalCareNoMiso)))
    
    # compute incidence costs
    IncCareMiso <- sapply(IncidMisoAll, FUN = function(z) sum(z * c(IncidCareMiso)))
    
    IncCareNoMiso <- sapply(IncidnoMisoAll, FUN = function(z) sum(z * c(IncidCareNoMiso)))
    
    return(list(totCareNoMiso = totCareNoMiso,
                totCareMiso = totCareMiso,
                IncCareNoMiso = IncCareNoMiso,
                IncCareMiso = IncCareMiso))
    
  }
  
  allCosts <- lapply(perspective, FUN = CostCalc)
  
  # # The rest of the final math here - outcomes by wealth quintile
  # # INCIDENCE
  IncidenceNoMiso <- sapply(IncidnoMisoAll, FUN = function(x) sum(x * c(incidence)))
  
  IncidenceMiso <- sapply(IncidMisoAll, FUN = function(x) sum(x * c(incidence)))

  # # MORTALITY
  mortalityNoMiso <- sapply(noMisoAll, FUN = function(x) sum(x * c(mortality)))
  
  mortalityMiso <- sapply(MisoAll, FUN = function(x) sum(x * c(mortality)))
  
  # # DALYs
  DALYsNoMiso <- sapply(noMisoAll, FUN = function(x) sum(x * c(DALYlist)))
  
  DALYsMiso <- sapply(MisoAll, FUN = function(x) sum(x * c(DALYlist)))
  
  # # sum everything, then weight by proportions in each quantile
  # # done above with lapply and sum
  # 
  # now weight outcomes and total
  wts <- c(params[["pLowQuintile"]], params[["pSecQuintile"]], params[["pMidQuintile"]], params[["pFourthQuintile"]], params[["pHighQuintile"]])
  
  #return(allCosts)
  
  # calculate a few differences here we need for the table
  SocCostsIncidNoMiso <- allCosts$Societal$IncCareNoMiso
  SocCostsIncidMiso <- allCosts$Societal$IncCareMiso
  GovCostsIncidNoMiso <- allCosts$Governmental$IncCareNoMiso
  GovCostsIncidMiso <- allCosts$Governmental$IncCareMiso

  # collect Incidence outcomes (a matrix: a row per quintile, a column per result)
  IncidOutcomes <- cbind(IncidenceNoMiso = IncidenceNoMiso, IncidenceMiso = IncidenceMiso, deltaOutcome = (IncidenceNoMiso - IncidenceMiso),
                              SocCostsIncidNoMiso, SocCostsIncidMiso, deltaSocCosts = (SocCostsIncidMiso - SocCostsIncidNoMiso),
                              GovCostsIncidNoMiso, GovCostsIncidMiso, deltaGovCosts = (GovCostsIncidMiso - GovCostsIncidNoMiso),
                              iCERSoc = (SocCostsIncidMiso - SocCostsIncidNoMiso)/(IncidenceNoMiso - IncidenceMiso),
                              iCERGov = (GovCostsIncidMiso - GovCostsIncidNoMiso)/(IncidenceNoMiso - IncidenceMiso))

  # compute weighted sum of outcomes and bind to quintile specific outcomes
  wtIncidOutcomes <- weightSumOutcomes(outcomesList = IncidOutcomes, weights = wts)
  
  SocCostsNoMiso <- allCosts$Societal$totCareNoMiso
  SocCostsMiso <- allCosts$Societal$totCareMiso
  GovCostsMiso <- allCosts$Governmental$totCareMiso
  GovCostsNoMiso = allCosts$Governmental$totCareNoMiso

  # collect Mortality outcomes
  MortOutcomes <- cbind(mortalityNoMiso = mortalityNoMiso, mortalityMiso = mortalityMiso, deltaOutcome = (mortalityNoMiso - mortalityMiso),
                             SocCostsNoMiso, SocCostsMiso, deltaSocCosts = (SocCostsMiso - SocCostsNoMiso),
                             GovCostsNoMiso, GovCostsMiso, deltaGovCosts = (GovCostsMiso - GovCostsNoMiso),
                             iCERSoc = (SocCostsMiso - SocCostsNoMiso)/(mortalityNoMiso - mortalityMiso),
                             iCERGov = (GovCostsMiso - GovCostsNoMiso)/(mortalityNoMiso - mortalityMiso))

  # compute weighted sum of outcomes and bind to quintile specific outcomes
  wtMortOutcomes <- weightSumOutcomes(outcomesList = MortOutcomes, weights = wts)


  # collect DALY outcomes
  DALYOutcomes <- cbind(DALYsNoMiso = DALYsNoMiso, DALYsMiso = DALYsMiso, deltaOutcome = (DALYsNoMiso - DALYsMiso),
                             SocCostsMiso, SocCostsNoMiso, deltaSocCosts = (SocCostsMiso - SocCostsNoMiso),
                             GovCostsMiso, GovCostsNoMiso, deltaGovCosts = (GovCostsMiso - GovCostsNoMiso),
                             iCERSoc = (SocCostsMiso - SocCostsNoMiso)/(DALYsNoMiso - DALYsMiso),
                             iCERGov = (GovCostsMiso - GovCostsNoMiso)/(DALYsNoMiso - DALYsMiso))

  # compute weighted sum of outcomes and bind to quintile specific outcomes
  wtDALYOutcomes <- weightSumOutcomes(outcomesList = DALYOutcomes, weights = wts)

  # also generate plots here too
  # put outcomes in their own list
  tabList <- list(Incidence = wtIncidOutcomes, Mortality = wtMortOutcomes, DALYS = wtDALYOutcomes)
  #parmsList = list(baseparms = baseparms, basetransitions = basetransitions)
  
  if(makePlots) {
    
    incidPlot <- outcomesPlot(outcome = "Incidence", whichTab = wtIncidOutcomes)
    mortsPlot <- outcomesPlot(outcome = "Mortality", whichTab = wtMortOutcomes)
    dalysPlot <- outcomesPlot(outcome = "DALYs", whichTab = wtDALYOutcomes)

  }

  # slowing dowm execution of the program
  # TableIncidence <- TabulateOutcomes(data = wtIncidOutcomes, decimals = 4, scale = 1)
  # TableMortality <- TabulateOutcomes(data = wtMortOutcomes, decimals = 4, scale = 1)
  # TableDALYs <- TabulateOutcomes(data = wtDALYOutcomes, decimals = 4, scale = 1)
  
  # the last expression is the return value of RunModel()
  if (makePlots) {
    structure(list(Incidence  = wtIncidOutcomes,
                   Mortality  = wtMortOutcomes,
                   DALYS      = wtDALYOutcomes,
                   plotsList  = list(Incidence = incidPlot,
                                     Mortality = mortsPlot,
                                     DALYs     = dalysPlot),
                   baseparms      = baseparms,
                   basetransitions = basetransitions),
              class = "modOut")
  } else {
    structure(list(Incidence  = wtIncidOutcomes,
                   Mortality  = wtMortOutcomes,
                   DALYS      = wtDALYOutcomes,
                   baseparms      = baseparms,
                   basetransitions = basetransitions),
              class = "modOut")
  }

}


# custom print method to supress baseparms an basetransitions from printing to the console

print.modOut <- function(x, ...) {
  
    x <- x["plotsList"]
    NextMethod()
}
  

## computed weighted sum of outcomes and returns the named dataframe
## INSTRUCTIONS
# 1) outcomesList: matrix of outcomes, a row per wealth quintile
# 2) weights: share of women in each quintile
weightSumOutcomes <- function(outcomesList, weights) {

  # each row times its quintile's weight, then add up each column
  wtSumOutcome <- colSums(outcomesList * weights)
  # the average ICER is the difference in average costs over the difference in average
  # outcomes, not a weighted average of the quintile ICERs
  wtSumOutcome["iCERSoc"] <- wtSumOutcome["deltaSocCosts"] / wtSumOutcome["deltaOutcome"]
  wtSumOutcome["iCERGov"] <- wtSumOutcome["deltaGovCosts"] / wtSumOutcome["deltaOutcome"]
  wtSumOutcome <- rbind(outcomesList, Average = wtSumOutcome)
  return(as.data.frame(wtSumOutcome))

}



## function to set one share of a dirichlet set and rescale the others proportionally,
## so the set still sums to 1
## INSTRUCTIONS
# 1) shares: named vector of the base-case shares in one set (sums to 1)
# 2) n: name of the share to change
# 3) value: its new value
rebalance <- function(shares, n, value) {
  others <- names(shares) != n
  shares[others] <- shares[others] * (1 - value) / (1 - shares[n])
  shares[n] <- value
  return(shares)
}


## function to run the one-way sensitivity analysis
## INSTRUCTIONS
# 1) model: the base-case model object from RunModel()
# 2) low_base, high_base: low and high values of the ordinary parameters, from makeOWSAInputs()
# 3) low_transitions, high_transitions: low and high values of each dirichlet share, by set
# 4) max_vars: number of variables shown in the tornado diagrams
# each run starts from a fresh copy of the base case with one variable changed
owsa <- function(model, 
                 low_base, low_transitions, 
                 high_base, high_transitions, 
                 max_vars) {
  
  # get the baseinputs and transitions from model object
  baseinputs <- model$baseparms
  basetransitions <- model$basetransitions
  base <- c(baseinputs, basetransitions)
  
  # identify basecase values
  baseIncid <- model$Incidence[6, c(3, 6, 9:11)]
  baseMort <- model$Mortality[6, c(3, 6, 9:11)]
  baseDALYs <- model$DALYS[6, c(3, 6, 9:11)]
  
  # 1) task list: for each variable, the values that change in its low run and in its high run
  # an ordinary parameter changes only itself
  baseTasks <- lapply(names(low_base), function(n) {
    list(varname = n, low = low_base[n], high = high_base[n])
  })
  
  # a dirichlet share changes its whole set: the share itself, and the other shares rescaled
  transitionTasks <- lapply(names(low_transitions), function(s) {
    shares <- basetransitions[names(low_transitions[[s]])]   # base-case shares in this set
    lapply(names(shares), function(n) {
      list(varname = n,
           low  = rebalance(shares, n, low_transitions[[s]][n]),
           high = rebalance(shares, n, high_transitions[[s]][n]))
    })
  })
  
  # one list of tasks: parameters, then every share of every set
  tasks <- c(baseTasks, unlist(transitionTasks, recursive = FALSE))
  
  # 2) the 5 results kept from one run, for each outcome
  runLive <- function(live) {
    model <- RunModel(baseparms = live[names(baseinputs)], 
                      basetransitions = live[names(basetransitions)])
    list(Incidence = unlist(model$Incidence[6, c(3, 6, 9:11)]),
         Mortality = unlist(model$Mortality[6, c(3, 6, 9:11)]),
         DALYs     = unlist(model$DALYS[6, c(3, 6, 9:11)]))
  }
  
  # 3) run each task: a fresh copy of the base case for its low run and for its high run
  p <- progressor(steps = length(tasks))
  
  results <- lapply(tasks, function(task) {
    p(paste0("Analyzing impact of ", task$varname))
    # a misspelled name would otherwise be added to live silently
    stopifnot(all(c(names(task$low), names(task$high)) %in% names(base)))
    
    live_low <- base
    live_low[names(task$low)] <- task$low
    
    live_high <- base
    live_high[names(task$high)] <- task$high
    
    list(low = runLive(live_low), high = runLive(live_high))
  })
  
  # 4) one table per outcome: a row per variable, its 5 low results then its 5 high results
  owsaList <- lapply(c(Incidence = "Incidence", Mortality = "Mortality", DALYs = "DALYs"), function(o) {
    out <- do.call(rbind, lapply(results, function(r) c(r$low[[o]], r$high[[o]])))
    dimnames(out) <- list(NULL, paste0("col", 1:10))
    return(out)
  })
  
  # labels, taken from the tasks so they stay in the same order as the results
  lowvals  <- sapply(tasks, function(task) task$low[[task$varname]])
  highvals <- sapply(tasks, function(task) task$high[[task$varname]])
  names(lowvals) <- names(highvals) <- sapply(tasks, function(task) task$varname)
  owsavars <- owsaLabels(lowvals, highvals, low_transitions)


  # post processing of data to return full results
  # split the table into respective components

  # loop through table and create list of tables for each outcome

  owsadata <- list()
  owsaplot <- list()

  basecase <- list(baseIncid, baseMort, baseDALYs)
  names(basecase) <- c("Incidence", "Mortality", "DALYs")
  
  genDSA <- function(owsalist, basecase, max_vars, named) {
    # print(named)
    isName <- switch(named,
                     Incidence = "Incident cases prevented",
                     Mortality = "Deaths averted",
                     DALYs = "DALYs averted")
    
    isPerName <- switch(named,
                     Incidence = "incident case prevented",
                     Mortality = "death averted",
                     DALYs = "DALY averted")
    
    outcomeName <- c(isName, 
                     "Incremental cost per woman, US$ (societal perspective)", 
                     "Incremental cost per woman, US$ (governmental perspective)", 
                     paste0("US$ per ", isPerName, " (societal perspective)"), 
                     paste0("US$ per ", isPerName, " (governmental perspective)"))

    for (i in 1:5) {
      
      #progress$inc(1/5, detail = "Generating plots")
      
      owsa_tab <- as_tibble(owsalist) |>
        select(c(0 + i, 5 + i)) |>
        rename("low" = 1, "high" = 2) |>
        mutate(range = abs(high - low), 
               highval = high - unlist(basecase[i]), 
               lowval = low - unlist(basecase[i])) |>
        add_column(variable = owsavars, .before = 1) |>
        arrange(range)
      
      # generate plot object (can deal with axis labels later)
      owsaplot[[i]] <- TornadoPlot(owsatab = owsa_tab, basecase = unlist(basecase[i]), 
                                   outcomeName = outcomeName[i], max_vars = max_vars,
                                   money = i > 1)   # outputs 2-5 are costs and ICERs
      names(owsaplot)[i] <- outcomeName[i]
      
      # also store owsadata in case I need to inspect it 
      owsadata[[i]] <- owsa_tab
      names(owsadata)[i] <- outcomeName[i]
    }
    
    return(list(Tables = owsadata, Plots = owsaplot))
    
  }
  

  # need to recover names of lists, so I do seq_along and add the name to the original function
  
  p <- progressor(along = seq_along(owsaList))
  
  dsaList <- pblapply(seq_along(owsaList), function(x) {
    
    p(paste0("Tornado diagrams for ", names(owsaList)[x]))
    genDSA(owsalist = owsaList[[x]], basecase = basecase[[x]], 
           max_vars = max_vars, named = names(basecase)[[x]])
    
  })
  names(dsaList) <- names(owsaList) # name the traces

  # 
  tablesList <- list(Incidence = dsaList$Incidence$Tables,
                     Mortality = dsaList$Mortality$Tables,
                     DALYs = dsaList$DALYs$Tables)
  # 
  plotsList <- list(Incidence = dsaList$Incidence$Plots,
                    Mortality = dsaList$Mortality$Plots,
                    DALYs = dsaList$DALYs$Plots)
  # 
  # apply structure to output to hide Tables when user prints the output
  dsaList <- structure(list(Tables = tablesList,
                             Plots = plotsList
                             ),
                        class = "owsaOut"
                        )
  
  return(dsaList)

  
} 

# custom print method to suppress baseparms an basetransitions from printing to the console
print.owsaOut <- function(x, ...) {

  x <- x["Plots"]
  NextMethod()

}





# functions to create intervals for the dirichlet distributions
idirichlet <- function(prior) {
  prior <- unlist(prior)
  posterior <- prior + rep(1, length=length(prior))
  lower <- qgamma(.025,shape=posterior,rate=1)/sum(qgamma(.025,shape=posterior,rate=1))
  upper <- qgamma(.975,shape=posterior,rate=1)/sum(qgamma(.975,shape=posterior,rate=1))
  return(list(lower = lower, upper = upper))
}

# function to generate and name random dirichlet variates
vdirichlet <- function(n, alpha) {
  variates <- rdirichlet(n, alpha)
  colnames(variates) <- names(alpha)
  return(variates)
}



## function to run the model on one PSA draw
## INSTRUCTIONS
# 1) model: the base-case model object from RunModel()
# 2) psalist: one draw, a named numeric vector of every parameter
genPSA <- function(model, psalist) {

  # get things we need
  basenames <- names(model$baseparms)
  transitnames <- names(model$basetransitions)
  stopifnot(all(c(basenames, transitnames) %in% names(psalist)))   # every parameter has a draw

  psamod <- RunModel(baseparms = psalist[basenames],
                     basetransitions = psalist[transitnames])

  psares <- list(Incidence = unlist(psamod$Incidence[6, c(3, 6, 9:11)]),
                 Mortality = unlist(psamod$Mortality[6, c(3, 6, 9:11)]),
                 DALYs     = unlist(psamod$DALYS[6, c(3, 6, 9:11)]))

  return(psares)

}


## function to run the probabilistic sensitivity analysis
## INSTRUCTIONS
# 1) model: the base-case model object from RunModel()
# 2) psalist: data frame of draws from makeSims(), a row per draw
# 3) wtp, by: maximum willingness to pay and its step, for the CEAC
RunPSA <- function(model, psalist, wtp, by) {

  # split the draws into rows: each row is a named numeric vector of every parameter
  draws <- asplit(as.matrix(psalist), MARGIN = 1)

  # update the progress bar 100 times, not once per draw
  every <- ceiling(length(draws) / 100)
  p <- progressor(steps = ceiling(length(draws) / every))

  results <- lapply(seq_along(draws), function(i) {
    if (i %% every == 0 || i == length(draws)) {
      p(message = paste0("PSA iteration ", i, " of ", length(draws)))
    }
    genPSA(model = model, psalist = draws[[i]])
  })

  # one table per outcome: a row per draw
  outList <- c(Incidence = "Incidence", Mortality = "Mortality", DALYs = "DALYs")

  psaTraces <- lapply(outList, function(o) {
    as.data.frame(do.call(rbind, unname(lapply(results, `[[`, o))))
  })
  
  # generate scatter plot
  # need to recover names of lists, so I do seq_along and add the name to the original function
  scatterPlots <- pblapply(seq_along(psaTraces), function(x) {
    ScatterPSA(psaTrace = psaTraces[[x]], named = names(psaTraces)[[x]])
    })
  names(scatterPlots) <- names(psaTraces) # name the traces

  # run CEAC analysis
  p <- progressor(along = seq_along(psaTraces))
  #progress$inc(detail = "Generating CEAC")
  ceacTraces <- pblapply(psaTraces, RunCEAC, wtp = wtp, by = by)
  p(message = "Analyzing probability of cost-effectiveness")
  

  # generate CEAC
  # need to recover names of lists
  ceacPlots <- pblapply(seq_along(ceacTraces), function(x) {
    drawCEAC(ceacTrace = ceacTraces[[x]], named = names(ceacTraces)[[x]])

  })
  names(ceacPlots) <- names(ceacTraces)

  psaList <- structure(list(psaTraces = psaTraces,
                            ceacTraces = ceacTraces,
                            Plots = list(scatterPlots = scatterPlots, ceacPlots = ceacPlots)),
                       class = "psaOut"
  )

  return(psaList)
  
}

# custom print method to suppress baseparms an basetransitions from printing to the console
print.psaOut <- function(x, ...) {

  x <- x["Plots"]
  NextMethod()

}


# function takes the maxwtp, a vector or dataframe of costs and outcomes for all interventions
# and returns probability that a given intervention is cost effective 
genProbCE <- function(wtp, psaTrace) {
  NMBSoc <- psaTrace[, 1] * wtp - psaTrace[, 2]                   # NMB = DALYs * wtp - COSTS
  NMBGov <- psaTrace[, 1] * wtp - psaTrace[, 3]                   # NMB = DALYs * wtp - COSTS
  pCESoc <- mean(NMBSoc > 0)        # find maximum for each row (nax nmb)
  pCEGov <- mean(NMBGov > 0)         # find maximum for each row (nax nmb)
  return(c(pCESoc = pCESoc, pCEGov = pCEGov))           # calculate probability of cost-effectiveness
}


RunCEAC <- function(psaTrace, wtp, by) {
  
  WTP <- seq(0, wtp, by = by)
  probCE <- data.frame(WTP, t(sapply(WTP, FUN = genProbCE, psaTrace = psaTrace)))
  
  return(probCE)
  
}











makeSims <- function(nsims, data, ...) {
  set.seed(12678)
  
  # pull out inputs for the main model but separate out the transition probabilities
  baseinputs <- data |>
    filter(!is.na(variable) & dist != "dirichlet") |>   # remove NA from data
    pull(basecase, variable)
  
  # pull out inputs for the main model but separate out the transition probabilities
  # parameters
  usa.low <- data |>
    filter(!is.na(low) & dist != "dirichlet") |>   # remove NA from data
    pull(low, variable)
  
  usa.high <- data |>
    filter(!is.na(low) & dist != "dirichlet") |>   # remove NA from data
    pull(high, variable)
  
  # first identify fixed parameters
  fixedparms <- data |>
    filter(dist == "fixed") |>
    pull(variable)
  
  betaparms <- data |>
    filter(dist == "beta") |>
    pull(variable)
  
  tnormparms <- data |>
    filter(dist == "truncnormal") |>
    pull(variable)
  
  unifparms <- data |>
    filter(dist == "uniform") |>
    pull(variable)
  
  lognormparms <- data |>
    filter(dist == "lognormal") |>
    pull(variable)
  
  alphas <- data |>
    pull(alpha, variable)
  
  betas <- data |>
    pull(beta, variable)
  
  sems <- data |>
    pull(sem, variable)
  
  # random dirichlet transitions
  randdirchinputs <- data |>
    filter(dist == "dirichlet") |>   # remove NA from data
    pull(basecase, variable) |>
    split(rep(1:2, c(5, 3)))
  names(randdirchinputs) <- c("Quintiles", "AgeDistribution")
  
  
  # takes inputs from the global environment
  # need nsims to go through function so user can set it
  #betas
  randbeta <- mapply(rbeta, MoreArgs=list(n=nsims), alphas[betaparms], betas[betaparms])
  
  # normals
  randtnormal <- mapply(rtruncnorm, MoreArgs=list(n=nsims), 
                        mean = baseinputs[tnormparms], sd = sems[tnormparms], 
                        a = usa.low[tnormparms], b = usa.high[tnormparms])
  
  # durations (uniformly distributed)
  randunif <- mapply(runif, MoreArgs=list(n=nsims), usa.low[unifparms], usa.high[unifparms])
  
  # log-normals
  randlognormal <- mapply(rlnorm, MoreArgs=list(n=nsims), 
                          meanlog = log(baseinputs[lognormparms]), 
                          sdlog = sems[lognormparms])
  
  # use data.table for speed; avoid do.call("cbind, .)
  randdirichlets <- as.data.table(unname(lapply(randdirchinputs, vdirichlet, n=nsims)))
  randdirichlets <- setDF(randdirichlets)
  
  fixedinputs <- matrix(rep(baseinputs[fixedparms],each=nsims),nrow=nsims) |>
    set_colnames(fixedparms)
  
  
  # pool together into one dataframe
  psa_input <- data.frame(randbeta, randtnormal, randunif, randlognormal, randdirichlets, fixedinputs)
  
  
  return(psaParms = psa_input)

}




#### PARAMETERS PAGE ####
# Helpers for the Parameters page: number formatting, readable distribution
# labels, the edit dialog, input checks and the preview of probabilistic draws.

# Compact number display: thousands get commas, small values keep 3 significant digits
fmt_val <- function(x) {
  vapply(x, function(v) {
    if (is.null(v) || length(v) == 0 || is.na(v)) return("—")
    a <- abs(v)
    if (a == 0)        "0"
    else if (a >= 100) formatC(v, format = "f", digits = 0, big.mark = ",")
    else if (a >= 1)   formatC(v, format = "f", digits = 2)
    else               formatC(signif(v, 3), format = "fg", digits = 3)
  }, character(1))
}

# Plain-language label for how a parameter is drawn in the probabilistic analysis
dist_label <- function(dist, alpha, beta, sem) {
  mapply(function(d, a, b, s) {
    switch(d,
      beta        = sprintf("Beta (α %s, β %s)", fmt_val(a), fmt_val(b)),
      truncnormal = sprintf("Truncated normal (SD %s)", fmt_val(s)),
      lognormal   = sprintf("Log-normal (SD of log %s)", fmt_val(s)),
      uniform     = "Uniform between low and high",
      dirichlet   = "Dirichlet (count)",
      fixed       = "Fixed",
      d)
  }, dist, alpha, beta, sem, USE.NAMES = FALSE)
}

# Beta shape parameters from a mean and standard error (method of moments) —
# the same derivation used for the published inputs
beta_mom <- function(mu, se) {
  k <- mu * (1 - mu) / se^2 - 1
  c(alpha = mu * k, beta = (1 - mu) * k)
}

# Which fields each distribution uses
dist_uses <- function(dist) {
  list(range = dist != "dirichlet",
       se    = dist %in% c("beta", "truncnormal", "lognormal"),
       draws = dist %in% c("beta", "truncnormal", "lognormal", "uniform"))
}

se_label <- function(dist) {
  switch(dist,
         beta        = "Standard error",
         truncnormal = "Standard deviation",
         lognormal   = "Standard deviation of the log value",
         "Standard error")
}

dist_note <- function(dist) {
  switch(dist,
    beta        = "Drawn from a beta distribution whose α and β are derived from the base case and standard error.",
    truncnormal = "Drawn from a normal distribution centred on the base case, truncated at the low and high values.",
    lognormal   = "Drawn from a log-normal distribution centred on the log of the base case.",
    uniform     = "Drawn uniformly between the low and high values.",
    fixed       = "Held at the base-case value in the probabilistic analysis.",
    dirichlet   = paste("One of a set of counts drawn together from a Dirichlet distribution.",
                        "The counts in the set are rescaled to proportions in the model."),
    "")
}

# Problems with a proposed edit; an empty vector means it can be saved
check_param <- function(dist, base, low, high, se) {
  u <- dist_uses(dist)
  bad <- function(v) is.null(v) || length(v) == 0 || is.na(v)
  errs <- character(0)
  if (bad(base)) return("Enter a base-case value.")
  if (dist == "dirichlet" && base <= 0) errs <- c(errs, "The count must be greater than 0.")
  if (u$range && !(bad(low) && bad(high))) {
    if (bad(low) || bad(high)) errs <- c(errs, "Enter both a low and a high value.")
    else {
      if (low > high)                 errs <- c(errs, "The low value must not be above the high value.")
      if (base < low || base > high)  errs <- c(errs, "The base case must lie between the low and high values.")
      if (dist == "uniform" && low == high) errs <- c(errs, "A uniform distribution needs low below high.")
    }
  }
  if (u$se) {
    if (bad(se) || se <= 0) errs <- c(errs, "The standard error must be greater than 0.")
    else if (dist == "beta") {
      if (base <= 0 || base >= 1) errs <- c(errs, "A beta parameter must lie strictly between 0 and 1.")
      else if (se^2 >= base * (1 - base))
        errs <- c(errs, sprintf("For a mean of %s the standard error must be below %s.",
                                fmt_val(base), fmt_val(sqrt(base * (1 - base)))))
    }
  }
  if (dist == "lognormal" && base <= 0) errs <- c(errs, "A log-normal parameter must be greater than 0.")
  errs
}

# Histogram of what the probabilistic analysis would draw with these values
param_preview <- function(dist, base, low, high, se, n = 4000) {
  set.seed(1)
  x <- switch(dist,
    beta        = { ab <- beta_mom(base, se); rbeta(n, ab[["alpha"]], ab[["beta"]]) },
    truncnormal = truncnorm::rtruncnorm(n, a = low, b = high, mean = base, sd = se),
    lognormal   = rlnorm(n, meanlog = log(base), sdlog = se),
    uniform     = runif(n, low, high))
  h  <- hist(x, breaks = 36, plot = FALSE)
  dp <- max(0, min(4, 2 - floor(log10(diff(range(x))))))   # decimals that suit the scale
  xf <- paste0("{value:,.", dp, "f}")
  q  <- quantile(x, c(.025, .975))
  pl <- function(v, lbl, dash = "Solid") list(value = v, color = chart_cols$ink2, width = 1,
          dashStyle = dash, zIndex = 5, label = list(text = lbl, rotation = 0, y = -4,
          style = list(color = chart_cols$ink2, fontSize = "11px")))
  lines <- list(pl(base, "Base case"))
  if (!is.na(low))  lines <- c(lines, list(pl(low,  "Low",  "ShortDash")))
  if (!is.na(high)) lines <- c(lines, list(pl(high, "High", "ShortDash")))

  highchart() |>
    hc_chart(type = "column", height = 210, spacingTop = 24) |>
    hc_add_series(name = "Share of draws", showInLegend = FALSE, color = chart_cols$gov,
                  data = list_parse2(data.frame(x = h$mids, y = 100 * h$counts / n)),
                  pointPadding = 0, groupPadding = 0, borderWidth = 0, borderRadius = 0) |>
    hc_xAxis(plotLines = lines, labels = list(format = xf)) |>
    hc_yAxis(title = list(text = ""), labels = list(format = "{value}%"), gridLineWidth = 0) |>
    hc_tooltip(headerFormat = "", pointFormat = paste0("Around {point.x:,.", dp + 1, "f}: {point.y:.1f}% of draws")) |>
    hc_exporting(enabled = FALSE) |>
    hc_subtitle(text = sprintf("95%% of draws fall between %s and %s", fmt_val(q[[1]]), fmt_val(q[[2]])))
}

# The edit dialog for one parameter (a row of the inputs table)
param_editor_modal <- function(row, edited) {
  u <- dist_uses(row$dist)
  num <- function(id, label, value) numericInput(id, label, value = value, width = "100%")
  src <- if (!is.na(row$Reference) && nzchar(row$Reference)) row$Reference else "Not recorded"

  modalDialog(
    title = div(class = "pe-title",
      div(row$Description),
      div(class = "pe-sub", tags$code(row$variable), " · ", row$group)),
    size = "l", easyClose = TRUE,

    div(class = "pe-section",
      div(class = "pe-head", "Values"),
      fluidRow(
        column(4, num("pe_base", if (row$dist == "dirichlet") "Count" else "Base case", row$basecase)),
        if (u$range) column(4, num("pe_low",  "Low (one-way analysis)",  row$low)),
        if (u$range) column(4, num("pe_high", "High (one-way analysis)", row$high))
      )),

    div(class = "pe-section",
      div(class = "pe-head", "Probabilistic analysis"),
      p(class = "pe-note", dist_note(row$dist)),
      if (u$se) fluidRow(column(4, num("pe_se", se_label(row$dist), row$sem)),
                         column(8, uiOutput("pe_derived"))),
      if (u$draws) highchartOutput("pe_preview", height = "210px")),

    uiOutput("pe_errors"),
    div(class = "pe-source", tags$strong("Source: "), src),

    footer = tagList(
      if (edited) actionButton("pe_reset", "Reset to published value", icon = icon("rotate-left"),
                               status = "secondary", outline = TRUE, class = "mr-auto"),
      modalButton("Cancel"),
      actionButton("pe_save", "Save", icon = icon("check"), status = "primary"))
  )
}


#### TORNADO ####
# Human-readable tornado labels: the parameter's description from the input
# spreadsheet plus the low–high range tested, e.g.
# "Baseline probability of PPH if health facility delivery (0.043–0.11)"
owsaLabels <- function(low, high, low_transitions) {
  rng <- paste0(" (", signif(low, 2), "–", signif(high, 2), ")")
  desc <- unname(param_labels[names(low)])
  desc[is.na(desc)] <- names(low)[is.na(desc)]
  # the sheet labels the quintile shares only "Lowest", "Second", ...
  q <- names(low) %in% names(low_transitions$Quintiles)
  desc[q] <- paste0("Share of women in the ", tolower(desc[q]), " wealth quintile")
  paste0(desc, rng)
}

# Tornado diagram: one bar pair per parameter, anchored at the base-case value.
# Bars show the outcome when the parameter is set to its low (light grey) or
# high (dark grey) value; parameters are ordered by the width of that range.
TornadoPlot <- function(owsatab, basecase, outcomeName, max_vars, money = FALSE) {

  basecase <- unname(as.numeric(basecase))   # a named value would be sent to Highcharts as an object

  owsa_torn <- owsatab |>
    slice_max(range, n = max_vars, with_ties = FALSE) |>
    arrange(desc(range))

  fmt <- if (money) "${point.y:,.2f}" else "{point.y:,.4f}"
  base_lbl <- if (money) sprintf("Base case: $%s", formatC(basecase, format = "f", digits = 2, big.mark = ","))
              else sprintf("Base case: %s", formatC(basecase, format = "f", digits = 4))

  highchart() |>
    hc_chart(type = "bar") |>
    hc_xAxis(categories = owsa_torn$variable, title = list(text = NULL),
             labels = list(style = list(width = "340px", textOverflow = "ellipsis"))) |>
    hc_yAxis(title = list(text = outcomeName),
             plotLines = list(list(value = basecase, color = chart_cols$ink, width = 1.5,
                                   zIndex = 5, label = list(text = base_lbl, rotation = 0,
                                   y = -6, style = list(color = chart_cols$ink2))))) |>
    hc_plotOptions(bar = list(grouping = FALSE, threshold = basecase)) |>
    hc_add_series(name = "Parameter at low value",  data = owsa_torn$low,  color = chart_cols$low) |>
    hc_add_series(name = "Parameter at high value", data = owsa_torn$high, color = chart_cols$high) |>
    hc_tooltip(headerFormat = "<b>{point.key}</b><br>",
               pointFormat = paste0("{series.name}: <b>", fmt, "</b>")) |>
    hc_legend(align = "left", verticalAlign = "top")
}

#### PSA: COST-EFFECTIVENESS PLANE ####
# Incremental cost vs outcome averted for every PSA draw, one colour per
# perspective. Axes are left free (draws that cost less or avert less are kept
# in view) with reference lines at zero; the mean of the draws is a larger marker.
ScatterPSA <- function(psaTrace, named) {

  isName <- switch(named,
                   Incidence = "PPH cases averted per woman",
                   Mortality = "Deaths averted per woman",
                   DALYs     = "DALYs averted per woman")

  mk_series <- function(cost_col, name, colour) {
    pts <- data.frame(x = psaTrace$deltaOutcome, y = psaTrace[[cost_col]])
    list(
      list(type = "scatter", name = name, color = colour, data = list_parse2(pts),
           marker = list(symbol = "circle", radius = 3,
                         fillColor = paste0(colour, "55"), lineColor = paste0(colour, "99"), lineWidth = 1)),
      list(type = "scatter", name = paste("Mean,", tolower(name)), linkedTo = ":previous",
           color = colour, zIndex = 5,
           data = list(list(x = mean(pts$x), y = mean(pts$y))),
           marker = list(symbol = "diamond", radius = 8, fillColor = colour,
                         lineColor = "#ffffff", lineWidth = 2))
    )
  }

  zero <- list(list(value = 0, color = chart_cols$axis, width = 1, zIndex = 3))

  highchart() |>
    hc_add_series_list(c(mk_series("deltaGovCosts", "Governmental perspective", chart_cols$gov),
                         mk_series("deltaSocCosts", "Societal perspective",     chart_cols$soc))) |>
    hc_xAxis(title = list(text = isName), plotLines = zero, gridLineWidth = 1) |>
    hc_yAxis(title = list(text = "Incremental cost per woman (US$)"), plotLines = zero,
             labels = list(format = "${value:,.0f}")) |>
    hc_tooltip(headerFormat = "<b>{series.name}</b><br>",
               pointFormat = paste0("Averted: <b>{point.x:.4f}</b><br>",
                                    "Incremental cost: <b>${point.y:,.2f}</b>")) |>
    hc_legend(align = "left", verticalAlign = "top") |>
    hc_title(text = "Cost-effectiveness plane") |>
    hc_subtitle(text = "Each point is one probabilistic draw; diamonds mark the mean")
}

#### PSA: COST-EFFECTIVENESS ACCEPTABILITY CURVE ####
# Share of PSA draws in which misoprostol is cost-effective at each
# willingness-to-pay value. Drawn as straight segments between computed points
# (a smoothed spline can overshoot 0% or 100%).
drawCEAC <- function(ceacTrace, named) {

  isPerName <- switch(named,
                      Incidence = "PPH case averted",
                      Mortality = "death averted",
                      DALYs     = "DALY averted")

  pts <- function(col) list_parse2(data.frame(x = ceacTrace$WTP, y = 100 * ceacTrace[[col]]))

  highchart() |>
    hc_chart(type = "line") |>
    hc_add_series(name = "Governmental perspective", data = pts("pCEGov"), color = chart_cols$gov,
                  lineWidth = 2, marker = list(enabled = FALSE)) |>
    hc_add_series(name = "Societal perspective",     data = pts("pCESoc"), color = chart_cols$soc,
                  lineWidth = 2, marker = list(enabled = FALSE)) |>
    hc_yAxis(min = 0, max = 100, tickInterval = 25, labels = list(format = "{value}%"),
             title = list(text = "Probability cost-effective")) |>
    hc_xAxis(min = 0, crosshair = TRUE, gridLineWidth = 0,
             labels = list(format = "${value:,.0f}"),
             title = list(text = paste0("Willingness to pay (US$ per ", isPerName, ")"))) |>
    hc_tooltip(shared = TRUE, headerFormat = "WTP <b>${point.key:,.0f}</b><br>",
               pointFormat = "{series.name}: <b>{point.y:.0f}%</b><br>") |>
    hc_legend(align = "left", verticalAlign = "top") |>
    hc_title(text = "Cost-effectiveness acceptability curve")
}

#### BASE CASE: BY WEALTH QUINTILE ####
# Two charts per outcome, both indexed by wealth quintile with the weighted
# average for all women as the last, shaded column:
#   outc — outcome averted per 1,000 women (per 100,000 for deaths), with the
#          values with and without misoprostol in the tooltip
#   cost — incremental cost per woman for each perspective, with costs with and
#          without misoprostol in the tooltip
# Columns are read by name, so the DALY table's different column order is safe.
outcomesPlot <- function(outcome, whichTab) {

  tab <- rownames_to_column(as.data.frame(whichTab), var = "Quintile")
  tab$Quintile[tab$Quintile == "Average"] <- "All women"
  nm <- names(tab)
  noMisoCol <- grep("NoMiso$", nm, value = TRUE)[1]
  misoCol   <- setdiff(grep("Miso$", nm, value = TRUE), c(noMisoCol, grep("Costs", nm, value = TRUE)))[1]
  costCol   <- function(persp, arm) grep(paste0("^", persp, "Costs.*", arm, "$"), nm, value = TRUE)[1]

  per <- if (outcome == "Mortality") 1e5 else 1e3
  per_lbl <- if (outcome == "Mortality") "100,000" else "1,000"
  what <- switch(outcome,
                 Incidence = "PPH cases averted",
                 Mortality = "Deaths averted",
                 DALYs     = "DALYs averted")

  avg_band <- list(list(from = nrow(tab) - 1.5, to = nrow(tab) - 0.5, color = chart_cols$band,
                        label = list(text = "Weighted average", verticalAlign = "top", y = 14,
                                     style = list(color = chart_cols$muted, fontSize = "11px"))))

  outc_pts <- lapply(seq_len(nrow(tab)), function(i) list(
    y      = per * tab$deltaOutcome[i],
    nomiso = per * tab[[noMisoCol]][i],
    miso   = per * tab[[misoCol]][i]))

  h1 <- highchart() |>
    hc_chart(type = "column") |>
    hc_xAxis(categories = tab$Quintile, title = list(text = "Wealth quintile"), plotBands = avg_band) |>
    hc_yAxis(title = list(text = paste(what, "per", per_lbl, "women")), min = 0) |>
    hc_add_series(name = what, data = outc_pts, color = chart_cols$outcome, showInLegend = FALSE,
                  dataLabels = list(enabled = TRUE, format = "{point.y:,.1f}",
                                    style = list(color = chart_cols$ink2, fontWeight = "500",
                                                 textOutline = "none"))) |>
    hc_tooltip(headerFormat = "<b>{point.key}</b><br>",
               pointFormat = paste0("Without misoprostol: {point.nomiso:,.1f}<br>",
                                    "With misoprostol: {point.miso:,.1f}<br>",
                                    what, ": <b>{point.y:,.1f}</b> per ", per_lbl, " women")) |>
    hc_title(text = paste(what, "per", per_lbl, "women")) |>
    hc_subtitle(text = "Misoprostol vs no misoprostol, by wealth quintile")

  cost_pts <- function(persp) lapply(seq_len(nrow(tab)), function(i) list(
    y      = tab[[paste0("delta", persp, "Costs")]][i],
    nomiso = tab[[costCol(persp, "NoMiso")]][i],
    miso   = tab[[setdiff(grep(paste0("^", persp, "Costs"), nm, value = TRUE), costCol(persp, "NoMiso"))[1]]][i]))

  h2 <- highchart() |>
    hc_chart(type = "column") |>
    hc_xAxis(categories = tab$Quintile, title = list(text = "Wealth quintile"), plotBands = avg_band) |>
    hc_yAxis(title = list(text = "Incremental cost per woman (US$)"),
             labels = list(format = "${value:,.0f}")) |>
    hc_add_series(name = "Governmental perspective", data = cost_pts("Gov"), color = chart_cols$gov) |>
    hc_add_series(name = "Societal perspective",     data = cost_pts("Soc"), color = chart_cols$soc) |>
    hc_tooltip(headerFormat = "<b>{point.key}</b><br>",
               pointFormat = paste0("<span style='color:{series.color}'>●</span> {series.name}<br>",
                                    "Without misoprostol: ${point.nomiso:,.2f}<br>",
                                    "With misoprostol: ${point.miso:,.2f}<br>",
                                    "Incremental: <b>${point.y:,.2f}</b><br>")) |>
    hc_legend(align = "left", verticalAlign = "top") |>
    hc_title(text = "Incremental cost per woman") |>
    hc_subtitle(text = "Misoprostol vs no misoprostol, by wealth quintile")

  list(outc = h1, cost = h2)
}

#### BASE CASE: TABLE ####
# Columns are selected by name (the outcome tables do not share a column order).
TabulateOutcomes <- function(data, decimals, scale) {
  rownames_to_column(data.frame(data), var = "Wealth Quintile") |>
    select(`Wealth Quintile`, deltaGovCosts, deltaSocCosts, deltaOutcome, iCERGov, iCERSoc) |>
    gt(rowname_col = "Wealth Quintile") |>
    cols_label(deltaGovCosts = "Governmental", deltaSocCosts = "Societal",
               deltaOutcome  = "Diff. in outcome",
               iCERGov       = "Governmental", iCERSoc = "Societal") |>
    fmt_currency(columns = c(deltaGovCosts, deltaSocCosts, iCERGov, iCERSoc), currency = "USD") |>
    tab_spanner(label = "Incremental Costs", columns = c(deltaGovCosts, deltaSocCosts)) |>
    tab_spanner(label = "ICER", columns = c(iCERGov, iCERSoc)) |>
    fmt_number(columns = deltaOutcome, decimals = decimals, scale_by = scale)
}
