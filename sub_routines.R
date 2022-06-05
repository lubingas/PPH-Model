# switch off file writing if in use
if(sink.number()>0) sink()

rm(list=ls())

# define function for detaching variables
detachAllData <- function()  {
  pos.to.detach <- (1:length(search()))[substring(search(), 
                                                  first = 1, last = 8) != "package:" & search() != ".GlobalEnv" & 
                                          search() != "Autoloads" & search() != "CheckExEnv" & search() != "tools:rstudio" & search() != "TempEnv"]
  for (i in 1:length(pos.to.detach)) {
    if (length(pos.to.detach) > 0) {
      detach(pos = pos.to.detach[1])
      pos.to.detach <- (1:length(search()))[substring(search(), 
                                                      first = 1, last = 8) != "package:" & search() != 
                                              ".GlobalEnv" & search() != "Autoloads" & search() != 
                                              "CheckExEnv" & search() != "tools:rstudio" & 
                                              search() != "TempEnv"]
    }
  }
}

# run detach function
detachAllData()

library(tidyverse)
library(readxl)
library(plotly)
library(RColorBrewer)
library(truncnorm)
library(gt)
library(pbapply)
library(highcharter)
library(echarts4r)
library(bezier) # to generate bezier points for smoother CEAC plot

#library(data.table)

pboptions(type = "txt")

# function calculates care path depending on wealth quintile
# Intervention = TRUE will apply the treatment effects (in this function on odds of HF delivery)
getCarePath <- function(Quintile, Intervention = TRUE) {
  
  # beta1 <- as.list(data)
  # attach(beta1)
  
  if (Quintile == "Lowest") {
    
    pHfDel <- pHfDelLowQ
    pHospDel <- pHospDelLowQ
    pUnassDel <- pUnassDelLowQ
    pFriendRelDel <- pFriendRelDelLowQ
    
  } else if (Quintile == "Second") {
    
    pHfDel <- pHfDelSecQ
    pHospDel <- pHospDelSecQ
    pUnassDel <- pUnassDelSecQ
    pFriendRelDel <- pFriendRelDelSecQ
    
  } else if (Quintile == "Middle") {
    
    pHfDel <- pHfDelMidQ
    pHospDel <- pHospDelMidQ
    pUnassDel <- pUnassDelMidQ
    pFriendRelDel <- pFriendRelDelMidQ
    
  } else if (Quintile == "Fourth") {
    
    pHfDel <- pHfDelFourQ
    pHospDel <- pHospDelFourQ
    pUnassDel <- pUnassDelFourQ
    pFriendRelDel <- pFriendRelDelFourQ
    
  } else if (Quintile == "Highest") {
    
    pHfDel <- pHfDelHighQ
    pHospDel <- pHospDelHighQ
    pUnassDel <- pUnassDelHighQ
    pFriendRelDel <- pFriendRelDelHighQ
    
  }
  
  # computations to make the probabilities work
  pUnassDel <- pUnassDel/(1-pHfDel)
  pFriendRelDel <- pFriendRelDel/(1-pUnassDel)
  
  # apply treatment effect: intervention increases the odds of HF delivery
  pHfDel <- pHfDel * (Intervention==FALSE) + orHFDelivery*pHfDel/(1-pHfDel+(orHFDelivery*pHfDel)) * (Intervention==TRUE)
  
  # calculate product of path probabilities
  hosp   <- prod(pHfDel, pHospDel)
  hc     <- prod(pHfDel, (1-pHospDel))
  friend <- prod((1-pHfDel), (1-pUnassDel), pFriendRelDel)
  TBA    <- prod((1-pHfDel), (1-pUnassDel), (1-pFriendRelDel))
  unass  <- prod((1-pHfDel), pUnassDel)
  
  
  # # don't forget to detach
  # detach(beta1)
  
  return(c(hospital = hosp, hc = hc, friend = friend, TBA = TBA, unass = unass))
  
}


# function calculates care path: 32 different possibilities per care pathway (5 care pathways)
# Intervention = TRUE will apply the treatment effects, higher access to miso with intervention
getTxPath <- function(Where, Intervention = TRUE) {
  
  # beta1 <- as.list(data)
  # attach(beta1)
  
  if (Where == "Hospital") {
    
    pOxy <- pOxyHosp
    pAccessEmOC <- pAccessEmOCHosp
    pMisoAccess <- pMisoAccessHospMisoPolicy * (Intervention==TRUE) + pMisoAccessHospOxyPolicy * (Intervention==FALSE)
    
    # hospital deliveries, apply pHospPPH
    pPPHOxy  <- pHospPPH*rPPHOxyPlacebo
    pPPHMiso <- pHospPPH*rPPHMisoHF
    pPPH     <- pHospPPH
    
  } else if (Where == "HC") {
    
    pOxy <- pOxyHC
    pAccessEmOC <- pAccessEmOCHC
    pMisoAccess <- pMisoAccessHCMisoPolicy * (Intervention==TRUE) + pMisoAccessHCOxyPolicy * (Intervention==FALSE)
    
    # hospital deliveries, apply pHospPPH
    pPPHOxy  <- pHospPPH*rPPHOxyPlacebo
    pPPHMiso <- pHospPPH*rPPHMisoHF
    pPPH     <- pHospPPH
    
  } else if (Where == "Friend") {
    
    pOxy <- pOxyHomeFriend
    pAccessEmOC <- pAccessEmOCFriend
    pMisoAccess <- pMisoAccessHomeFriendMisoPolicy * (Intervention==TRUE) + pMisoAccessHomeOxyPolicy * (Intervention==FALSE)
    
    # home deliveries, apply pHomePPH
    pPPHOxy  <- pHomePPH*rPPHOxyPlacebo
    pPPHMiso <- pHomePPH*rPPHMisoOxy
    pPPH     <- pHomePPH
    
  } else if (Where == "TBA") {
    
    pOxy <- pOxyHomeTBA
    pAccessEmOC <- pAccessEmOCTBA
    pMisoAccess <- pMisoAccessHomeTBAMisoPolicy * (Intervention==TRUE) + pMisoAccessHomeOxyPolicy * (Intervention==FALSE)
    
    # home deliveries, apply pHomePPH
    pPPHOxy  <- pHomePPH*rPPHOxyPlacebo
    pPPHMiso <- pHomePPH*rPPHMisoOxy
    pPPH     <- pHomePPH
    
  } else if (Where == "Unassisted") {
    
    pOxy <- pOxyHomeUnass
    pAccessEmOC <- pAccessEmOCUnass
    pMisoAccess <- pMisoAccessHomeUnassMisoPolicy * (Intervention==TRUE) + pMisoAccessHomeOxyPolicy * (Intervention==FALSE)
    
    # home deliveries, apply pHomePPH
    pPPHOxy  <- pHomePPH*rPPHOxyPlacebo
    pPPHMiso <- pHomePPH*rPPHMisoOxy
    pPPH     <- pHomePPH
    
  }
  
  # calculate individual paths
  pathlist <- c(
    path01 <- prod(pOxy, pPPHOxy, pAccessEmOC, pDieEmOC),
    path02 <- prod(pOxy, pPPHOxy, pAccessEmOC, (1-pDieEmOC)),
    path03 <- prod(pOxy, pPPHOxy, (1-pAccessEmOC), pDieEmOC),
    path04 <- prod(pOxy, pPPHOxy, (1-pAccessEmOC), (1-pDieEmOC)),
    path05 <- prod(pOxy, (1-pPPHOxy), pDieNoPPH),
    path06 <- prod(pOxy, (1-pPPHOxy), (1-pDieNoPPH)),
    path07 <- prod((1-pOxy), pMisoAccess, (1-pMisuseMiso), pPPHMiso, pAccessEmOC, pDieEmOC),
    path08 <- prod((1-pOxy), pMisoAccess, (1-pMisuseMiso), pPPHMiso, pAccessEmOC, (1-pDieEmOC)),
    path09 <- prod((1-pOxy), pMisoAccess, (1-pMisuseMiso), pPPHMiso, (1-pAccessEmOC), pDieEmOC),
    path10 <- prod((1-pOxy), pMisoAccess, (1-pMisuseMiso), pPPHMiso, (1-pAccessEmOC), (1-pDieEmOC)),
    path11 <- prod((1-pOxy), pMisoAccess, (1-pMisuseMiso), (1-pPPHMiso), pDieNoPPH),
    path12 <- prod((1-pOxy), pMisoAccess, (1-pMisuseMiso), (1-pPPHMiso), (1-pDieNoPPH)),
    path15 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, (1-pNoMisoAE), pUterineRuptureMiso, pDieUterineRupture),
    path16 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, (1-pNoMisoAE), pUterineRuptureMiso, (1-pDieUterineRupture)),
    path17 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, (1-pNoMisoAE), (1-pUterineRuptureMiso), pDieStillBirth),
    path18 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, (1-pNoMisoAE), (1-pUterineRuptureMiso), (1-pDieStillBirth)),
    path19 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, pNoMisoAE, pPPH, pAccessEmOC, pDieEmOC),
    path20 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, pNoMisoAE, pPPH, pAccessEmOC, (1-pDieEmOC)),
    path21 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, pNoMisoAE, pPPH, (1-pAccessEmOC), pDieEmOC),
    path22 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, pNoMisoAE, pPPH, (1-pAccessEmOC), (1-pDieEmOC)),
    path23 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, pNoMisoAE, (1-pPPH), pDieNoPPH),
    path24 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, pNoMisoAE, (1-pPPH), (1-pDieNoPPH)),
    path27 <- prod((1-pOxy), (1-pMisoAccess), pPPH, pAccessEmOC, pDieEmOC),
    path28 <- prod((1-pOxy), (1-pMisoAccess), pPPH, pAccessEmOC, (1-pDieEmOC)),
    path29 <- prod((1-pOxy), (1-pMisoAccess), pPPH, (1-pAccessEmOC), pDieEmOC),
    path30 <- prod((1-pOxy), (1-pMisoAccess), pPPH, (1-pAccessEmOC), (1-pDieEmOC)),
    path31 <- prod((1-pOxy), (1-pMisoAccess), (1-pPPH), pDieNoPPH),
    path32 <- prod((1-pOxy), (1-pMisoAccess), (1-pPPH), (1-pDieNoPPH))
  )
  
  # don't forget to detach
  #detach(beta1)
  
  return(pathlist)
  
}


getYLL <- function(disc, time, wt) {
  
  disc * time
  
}

getYLD <- function(wtD, time) {
  
  wtD * time
  
}


getDALYS <- function(age, LE, disabilitywt, duration, discFact) {
  
  ages = seq(age, LE, by = 1)
  index = (0:(length(ages)-1))
  discFact = (1 / (1 + discFact)^index)
  YLL = getYLD(disabilitywt, duration)
  YLD = sum(getYLL(disc = discFact, time = c(1-duration, rep(1, (length(ages)-2)),0.5)))
  return(c(YLL = YLL, YLD = YLD))
  
}

getCareCost <- function(Societal = TRUE, Intervention = TRUE) {
  
  # calculate individual cost paths
  # to preserve structure with the paths module, set places where there is no cost to zero
  cOxy <- ifelse(Intervention, (cOxytocin + cDestruction), cOxytocin)
  cMisoAccess <- ifelse(Intervention, 0, cMisoprostol) #  patient can still access Miso in HF if in Intervention; for intervention I add cMisoprostol to everyone
  
  cAccessEmOC <- ifelse(Societal, cPPHManagementHospSocietal, cPPHManagementHosp)
  cUterineRupture <- ifelse(Societal, cUterineRuptureSocietal, cUterineRupture)
  cVaginalStillBirth <- ifelse(Societal, cVaginalStillBirthSocietal, cVaginalStillBirth)
  
  costlist <- c(
    cost01 <- sum(cOxy, cPPH = 0, cAccessEmOC, cDieEmOC = cMortality),
    cost02 <- sum(cOxy, cPPH = 0, cAccessEmOC, cAliveEmOC = 0),
    cost03 <- sum(cOxy, cPPH = 0, cNoAccessEmOC = 0, cDieEmOC = cMortality),
    cost04 <- sum(cOxy, cPPH = 0, cNoAccessEmOC = 0, cDieEmOC = 0),
    cost05 <- sum(cOxy, cNoPPH = 0, cDieNoPPH = cMortality),
    cost06 <- sum(cOxy, cNoPPH = 0, cAliveNoPPH = 0),
    cost07 <- sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cPPH = 0, cAccessEmOC, cDieEmOC = cMortality),
    cost08 <- sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cPPH = 0, cAccessEmOC, cAliveEmOC = 0),
    cost09 <- sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cPPH = 0, cNoAccessEmOC = 0, cDieEmOC = cMortality),
    cost10 <- sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cPPH = 0, cNoAccessEmOC = 0, cAliveEmOC = 0),
    cost11 <- sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cNoPPH = 0, cDieNoPPH = cMortality),
    cost12 <- sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cNoPPH = 0, cAliveNoPPH = 0),
    cost15 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cMisoAE = 0, cUterineRupture, cDieUterineRupture = cMortality),
    cost16 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cMisoAE = 0, cUterineRupture, cAliveUterineRupture = 0),
    cost17 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cMisoAE = 0, cVaginalStillBirth, cDieStillBirth = cMortality),
    cost18 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cMisoAE = 0, cVaginalStillBirth, cAliveStillBirth = 0),
    cost19 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cNoMisoAE = 0, cPPH = 0, cAccessEmOC, cDieEmOC = cMortality),
    cost20 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cNoMisoAE = 0, cPPH = 0, cAccessEmOC, cAliveEmOC = 0),
    cost21 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cNoMisoAE = 0, cPPH = 0, cNoAccessEmOC = 0, cDieEmOC = cMortality),
    cost22 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cNoMisoAE = 0, cPPH = 0, cNoAccessEmOC = 0, cAliveEmOC = 0),
    cost23 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cNoMisoAE = 0, cNoPPH = 0, cDieNoPPH = cMortality),
    cost24 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cNoMisoAE = 0, cNoPPH = 0, cAliveNoPPH = 0),
    cost27 <- sum(cOxy = 0, cNoMisoAccess = 0, cPPH = 0, cAccessEmOC, cDieEmOC = cMortality),
    cost28 <- sum(cOxy = 0, cNoMisoAccess = 0, cPPH = 0, cAccessEmOC, cAliveEmOC = 0),
    cost29 <- sum(cOxy = 0, cNoMisoAccess = 0, cPPH = 0, cNoAccessEmOC = 0, cDieEmOC = cMortality),
    cost30 <- sum(cOxy = 0, cNoMisoAccess = 0, cPPH = 0, cNoAccessEmOC = 0, cAliveEmOC = 0),
    cost31 <- sum(cOxy = 0, cNoMisoAccess = 0, cNoPPH = 0, cDieNoPPH = cMortality),
    cost32 <- sum(cOxy = 0, cNoMisoAccess = 0, cNoPPH = 0, cAliveEmOC = 0)
  )
  
  return(costlist)
  
}


getIncidencePath <- function(Where, Intervention = TRUE) {
  
  if (Where == "Hospital") {
    
    pOxy <- pOxyHosp
    pMisoAccess <- pMisoAccessHospMisoPolicy * (Intervention==TRUE) + pMisoAccessHospOxyPolicy * (Intervention==FALSE)
    
    # hospital deliveries, apply pHospPPH
    pPPHOxy  <- pHospPPH*rPPHOxyPlacebo
    pPPHMiso <- pHospPPH*rPPHMisoHF
    pPPH     <- pHospPPH
    
  } else if (Where == "HC") {
    
    pOxy <- pOxyHC
    pMisoAccess <- pMisoAccessHCMisoPolicy * (Intervention==TRUE) + pMisoAccessHCOxyPolicy * (Intervention==FALSE)
    
    # hospital deliveries, apply pHospPPH
    pPPHOxy  <- pHospPPH*rPPHOxyPlacebo
    pPPHMiso <- pHospPPH*rPPHMisoHF
    pPPH     <- pHospPPH
    
  } else if (Where == "Friend") {
    
    pOxy <- pOxyHomeFriend
    pMisoAccess <- pMisoAccessHomeFriendMisoPolicy * (Intervention==TRUE) + pMisoAccessHomeOxyPolicy * (Intervention==FALSE)
    
    # home deliveries, apply pHomePPH
    pPPHOxy  <- pHomePPH*rPPHOxyPlacebo
    pPPHMiso <- pHomePPH*rPPHMisoOxy
    pPPH     <- pHomePPH
    
  } else if (Where == "TBA") {
    
    pOxy <- pOxyHomeTBA
    pMisoAccess <- pMisoAccessHomeTBAMisoPolicy * (Intervention==TRUE) + pMisoAccessHomeOxyPolicy * (Intervention==FALSE)
    
    # home deliveries, apply pHomePPH
    pPPHOxy  <- pHomePPH*rPPHOxyPlacebo
    pPPHMiso <- pHomePPH*rPPHMisoOxy
    pPPH     <- pHomePPH
    
  } else if (Where == "Unassisted") {
    
    pOxy <- pOxyHomeUnass
    pMisoAccess <- pMisoAccessHomeUnassMisoPolicy * (Intervention==TRUE) + pMisoAccessHomeOxyPolicy * (Intervention==FALSE)
    
    # home deliveries, apply pHomePPH
    pPPHOxy  <- pHomePPH*rPPHOxyPlacebo
    pPPHMiso <- pHomePPH*rPPHMisoOxy
    pPPH     <- pHomePPH
    
  }
  
  # calculate individual paths
  pathlist <- c(
    path01 <- prod(pOxy, pPPHOxy),
    path06 <- prod(pOxy, (1-pPPHOxy)),
    path07 <- prod((1-pOxy), pMisoAccess, (1-pMisuseMiso), pPPHMiso),
    path12 <- prod((1-pOxy), pMisoAccess, (1-pMisuseMiso), (1-pPPHMiso)),
    path15 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, pPPH = 0), # no one gets PPH
    path16 <- prod((1-pOxy), pMisoAccess, pMisuseMiso, noPPH = 1), # no one gets PPH because either a uterine rupture or still birth
    path27 <- prod((1-pOxy), (1-pMisoAccess), pPPH),
    path32 <- prod((1-pOxy), (1-pMisoAccess), (1-pPPH))
  )
  
  
  return(pathlist)
  
}



getIncidenceCost <- function(Societal = TRUE, Intervention = TRUE) {
  
  # calculate individual cost paths
  # to preserve structure with the paths module, set places where there is no cost to zero
  cOxy <- ifelse(Intervention, (cOxytocin + cDestruction), cOxytocin)
  cMisoAccess <- ifelse(Intervention, 0, cMisoprostol) #  patient can still access Miso in HF if in Intervention; for intervention I add cMisoprostol to everyone
  
  IncidCostlist <- c(
    cost01 <- sum(cOxy, cPPH = 0),
    cost06 <- sum(cOxy, cNoPPH = 0),
    cost07 <- sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cPPH = 0),
    cost12 <- sum(cOxy = 0, cMisoAccess, cNoMisuseMiso = 0, cNoPPH = 0),
    cost15 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cPPH = 0),
    cost16 <- sum(cOxy = 0, cMisoAccess, cMisuseMiso = 0, cNoPPH = 0),
    cost27 <- sum(cOxy = 0, cNoMisoAccess = 0, cPPH = 0),
    cost32 <- sum(cOxy = 0, cNoMisoAccess = 0, cNoPPH = 0)
  )
  
  return(IncidCostlist)
  
}


# function calculates care path depending on wealth quintile
# Intervention = TRUE will apply the treatment effects (in this function on odds of HF delivery)
getAccessCost <- function(Societal = TRUE, Intervention = TRUE) {
  
  cHospDel <- ifelse(Societal, cNormalDeliveryHospSocietal, cNormalDeliveryHosp)
  cHCDel <- ifelse(Societal, cNormalDeliveryHCSocietal, cNormalDeliveryHC)
  cFriendRelDel <- ifelse(Societal, cNormalDeliveryHomeFriend, 0)
  cDelTBA <- ifelse(Societal, cNormalDeliveryHomeTBA, 0)
  cDelUnass <- ifelse(Societal, cNormalDeliveryHomeUnass, 0)
  
  ProgramCost <- ifelse(Intervention, cMisoprostolProgram, 0)
  
  # calculate product of path probabilities
  hosp   <- cHospDel + ProgramCost
  hc     <- cHCDel + ProgramCost
  friend <- cFriendRelDel + ProgramCost
  TBA    <- cDelTBA + ProgramCost
  unass  <- cDelUnass + ProgramCost
  
  return(c(hospital = hosp, hc = hc, friend = friend, TBA = TBA, unass = unass))
  
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

  beta1 <- as.list(c(baseparms, basetransitions))
  attach(beta1)
  
  environment(getAccessCost) <- environment() # makes temp copy of getAccessCost function so variables created here are accessible to it without needing to plance entire mtrace function in the RunModel function
  environment(getCareCost) <- environment() # makes temp copy of getCareCost function so variables created here are accessible to it without needing to plance entire mtrace function in the RunModel function
  
  # use mapply to iterate over lists of duration and disability weights
  # apply mapply function to the middle "ages" in the model 
  dalysList <- lapply(age, FUN = function(x) {
    
    mapply(FUN = getDALYS, 
           disabilitywt = list(disabilitywtPPH, disabilitywtRupture, disabilitywtStillBirth),
           duration = list(durationPPH, durationUterineRupture, durationStillBirth),
           MoreArgs = list(age = x, LE = LifeExp, discFact = discFact))
    })
  

  proportions <- c(isless20, is20to34, is35to49)
  age_wt_DALYs <- lapply(dalysList, "%*%", proportions)
  names(age_wt_DALYs) <- c("pph", "rupture", "stillbirth")
  

  ## opportunity cost calculations
  cNormDelHomeFriend <- (cProductivity/procDaysYr)*avgFriendTime
  cOppCostPPHPatientTime <- ((averageLOSPPH*cProductivity)/procDaysYr)+((averageTravelTime*cProductivity)/(procDaysYr*procHrsDay))
  OppCostPatientTimeHome <- ((averageLOSHome*cProductivity)/procDaysYr)
  cOppCostRupturePatientTime <- ((averageLOSUterineRupture*cProductivity)/procDaysYr)+((averageTravelTime*cProductivity)/(procDaysYr*procHrsDay))
  cOppCostNormDelPatient <- ((averageLOSNormDel*cProductivity)/procDaysYr)+((averageTravelTime*cProductivity)/(procDaysYr*procHrsDay))

  dirNonMedHC <- cTravelHC + cUpkeepHC
  dirNonMedHosp <- cTravelHospital + cUpkeepHospital

  # costs of PPH management
  cPPHManagementHCSocietal <- cPPHManagementHC + dirNonMedHC + cOppCostPPHPatientTime
  cPPHManagementHospSocietal <- cPPHManagementHosp + dirNonMedHosp + cOppCostPPHPatientTime

  # costs of Normal Delivery
  cNormalDeliveryHCSocietal <- cNormalDeliveryHC + dirNonMedHC + cOppCostNormDelPatient
  cNormalDeliveryHospSocietal <- cNormalDeliveryHosp + dirNonMedHC + cOppCostNormDelPatient

  # costs of treating a uterine rupture (only in hospital)
  cUterineRuptureSocietal <- cUterineRupture +  dirNonMedHosp + cOppCostRupturePatientTime

  # cost of a vaginal still birth equal to costs of a normal delivery
  cVaginalStillBirthSocietal <- cVaginalStillBirth + dirNonMedHC + cOppCostNormDelPatient

  # normal delivery by TBA
  cNormalDeliveryHomeTBA <- cNormDelHomeTBA + cTravelTBA + cNormDelHomeFriend

  # normal delivery by friend
  cNormalDeliveryHomeFriend <- cNormDelHomeFriend

  # normal delivery home unassisted
  cNormalDeliveryHomeUnass <- cOppCostNormDelPatient

  # calculate misoprostol program costs
  cMisoprostolProgram <- getMisoProgCosts(nWomen, nHW, cTrain, daysTraining, cNurse, nNurse, nurseFTE, cMisoprostol, cPackage)

  ### PAYOFF VECTORS, convert everything to matrix then tibble to ease calculations
  # # calculate incidence
  # # 1 = incident case, 0 = not an incident case
  # # avoid counting incidence multiple times, because model was built to estimate the incidence of PPH only
  incidence <- matrix(c(1, 0, 1, 0, 1, 0, 1, 0), ncol = 1)

  # calculate mortality
  mortality <- matrix(c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0), ncol = 1)
  
  # order is die (apply YLL = age_wt_DALYs[1] & YLD = age_wt_DALYs[2]), live (apply YLD = age_wt_DALYs[2]) in model structure
  # pph pay offs - repeat twice
  pph_payoff <- rep(c(age_wt_DALYs$pph[1] + age_wt_DALYs$pph[2] * pSevereAnemiaEmOC,      # anemia, EmOC, die
                      age_wt_DALYs$pph[2] * pSevereAnemiaEmOC,                            # anemia, EmOC, live
                      age_wt_DALYs$pph[1] + age_wt_DALYs$pph[2] * pSevereAnemianoEmOC,    # anemia, noEmOC, die
                      age_wt_DALYs$pph[2] * pSevereAnemianoEmOC,                          # anemia, noEmOC, live
                      0,                                                                  # no pph, die
                      0), 2)                                                              # no pph, live
  
  # complications - uterine rupture/still birth
  compl_payoff <- c(sum(age_wt_DALYs$rupture), age_wt_DALYs$rupture[2],
                    sum(age_wt_DALYs$stillbirth), age_wt_DALYs$stillbirth[2])
  
  
  DALYlist <- matrix(c(pph_payoff, compl_payoff, pph_payoff), ncol = 1)
  
  apply_intervention <- c(NoMiso = FALSE, Miso = TRUE)
  
  # TREATMENT TRAJECTORIES
  # apply getTxPath function to each treatment trajectory list, then to each intervention arm
  # return TxTrajectory list with trajectory for each treatment arm
  TxTrajectory <- lapply(apply_intervention, FUN = function (x) {
    lapply(TxTrjList, FUN = getTxPath, Intervention = x)}
    )
  
  # TxTrajectory_NoMiso <- lapply(TxTrjList, FUN = getTxPath, Intervention = FALSE)
  # TxTrajectory_Miso <- lapply(TxTrjList, FUN = getTxPath, Intervention = TRUE)
  
  # # INCIDENCE ONLY TRAJECTORY
  IncidTrajectory <- lapply(apply_intervention, FUN = function (x) {
    lapply(TxTrjList, FUN = getIncidencePath, Intervention = x)}
  )
  
  # # CAREPATHWAY TRAJECTORIES
  # now apply getCarePath function to each Quintile list, then to each intervention arm
  # return TxTrajectory list with trajectory for each treatment arm
  CareTrajectory <- lapply(apply_intervention, FUN = function (x) {
    lapply(QuintileList, FUN = getCarePath, Intervention = x)}
  )
  
  # mapply applies "outer" function to 
  noMisoAll <- lapply(CareTrajectory$NoMiso, FUN = function(x) {
    mapply(FUN = outer, TxTrajectory$NoMiso, x)
  })
  
  MisoAll <- lapply(CareTrajectory$Miso, FUN = function(x) {
    mapply(FUN = outer, TxTrajectory$Miso, x)
  })
  
  # apply to incidence tables
  IncidnoMisoAll <- lapply(CareTrajectory$NoMiso, FUN = function(x) {
    mapply(FUN = outer, IncidTrajectory$NoMiso, x)
  })
  
  IncidMisoAll <- lapply(CareTrajectory$Miso, FUN = function(x) {
    mapply(FUN = outer, IncidTrajectory$Miso, x)
  })
  
  
  # # COST for TREATMENT TRAJECTORIES
  perspective <- c(Societal = TRUE, Governmental = FALSE)
  
  CareCost <- lapply(apply_intervention, FUN = function(x) {
    sapply(perspective, getCareCost, x)
  })
  
  IncidCost <- lapply(apply_intervention, FUN = function(x) {
    sapply(perspective, getIncidenceCost, x)
  })

  AccessCost <- lapply(apply_intervention, FUN = function(x) {
    sapply(perspective, getAccessCost, x)
  })
  
  perp <- c("Societal", "Governmental")
  names(perp) <- perp
  
  CostCalc <- function(x) {
    
    totalCareNoMiso <- t(outer(AccessCost$NoMiso[, x], CareCost$NoMiso[, x], FUN = "+"))
    totalCareMiso <- t(outer(AccessCost$Miso[, x], CareCost$Miso[, x], FUN = "+"))
    IncidCareNoMiso <- t(outer(AccessCost$NoMiso[, x], IncidCost$NoMiso[, x], FUN = "+"))
    IncidCareMiso <- t(outer(AccessCost$Miso[, x], IncidCost$Miso[, x], FUN = "+"))
    
    # compute total care costs
    totCareMiso <- sapply(lapply(MisoAll, FUN = function(z) {
      mapply("*", z, totalCareMiso)
    }), sum)
    
    totCareNoMiso <- sapply(lapply(noMisoAll, FUN = function(z) {
      mapply("*", z, totalCareNoMiso)
    }), sum)
    
    # compute incidence costs
    IncCareMiso <- sapply(lapply(IncidMisoAll, FUN = function(z) {
      mapply("*", z, IncidCareMiso)
    }), sum)
    
    IncCareNoMiso <- sapply(lapply(IncidnoMisoAll, FUN = function(z) {
      mapply("*", z, IncidCareNoMiso)
    }), sum)
    
    return(list(totCareNoMiso = totCareNoMiso,
                totCareMiso = totCareMiso,
                IncCareNoMiso = IncCareNoMiso,
                IncCareMiso = IncCareMiso))
    
  }
  
  allCosts <- lapply(perp, FUN = CostCalc)
  
  # # The rest of the final math here - outcomes by wealth quintile
  # # INCIDENCE
  IncidenceNoMiso <- sapply(lapply(IncidnoMisoAll, FUN = function(x) {
    mapply("*", x, incidence)}), sum)
  
  IncidenceMiso <- sapply(lapply(IncidMisoAll, FUN = function(x) {
    mapply("*", x, incidence)}), sum)

  # # MORTALITY
  mortalityNoMiso <- sapply(lapply(noMisoAll, FUN = function(x) {
    mapply("*", x, mortality)}), sum)
  
  mortalityMiso <- sapply(lapply(MisoAll, FUN = function(x) {
    mapply("*", x, mortality)}), sum)
  
  # # DALYs
  DALYsNoMiso <- sapply(lapply(noMisoAll, FUN = function(x) {
    mapply("*", x, DALYlist)}), sum)
  
  DALYsMiso <- sapply(lapply(MisoAll, FUN = function(x) {
    mapply("*", x, DALYlist)}), sum)
  
  # # sum everything, then weight by proportions in each quantile
  # # done above with lapply and sum
  # 
  # now weight outcomes and total
  wts <- c(pLowQuintile, pSecQuintile, pMidQuintile, pFourthQuintile, pHighQuintile)
  
  #return(allCosts)
  
  # calculate a few differences here we need for the table
  SocCostsIncidNoMiso <- allCosts$Societal$IncCareNoMiso
  SocCostsIncidMiso <- allCosts$Societal$IncCareMiso
  GovCostsIncidNoMiso <- allCosts$Governmental$IncCareNoMiso
  GovCostsIncidMiso <- allCosts$Governmental$IncCareMiso

  # collect Incidence outcomes
  IncidOutcomes <- data.frame(IncidenceNoMiso = IncidenceNoMiso, IncidenceMiso = IncidenceMiso, deltaOutcome = (IncidenceNoMiso - IncidenceMiso),
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
  MortOutcomes <- data.frame(mortalityNoMiso = mortalityNoMiso, mortalityMiso = mortalityMiso, deltaOutcome = (mortalityNoMiso - mortalityMiso),
                             SocCostsNoMiso, SocCostsMiso, deltaSocCosts = (SocCostsMiso - SocCostsNoMiso),
                             GovCostsNoMiso, GovCostsMiso, deltaGovCosts = (GovCostsMiso - GovCostsNoMiso),
                             iCERSoc = (SocCostsMiso - SocCostsNoMiso)/(mortalityNoMiso - mortalityMiso),
                             iCERGov = (GovCostsMiso - GovCostsNoMiso)/(mortalityNoMiso - mortalityMiso))

  # compute weighted sum of outcomes and bind to quintile specific outcomes
  wtMortOutcomes <- weightSumOutcomes(outcomesList = MortOutcomes, weights = wts)


  # collect DALY outcomes
  DALYOutcomes <- data.frame(DALYsNoMiso = DALYsNoMiso, DALYsMiso = DALYsMiso, deltaOutcome = (DALYsNoMiso - DALYsMiso),
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

  # don't forget to detach
  detach(beta1)


  # slowing dowm execution of the program
  # TableIncidence <- TabulateOutcomes(data = wtIncidOutcomes, decimals = 4, scale = 1)
  # TableMortality <- TabulateOutcomes(data = wtMortOutcomes, decimals = 4, scale = 1)
  # TableDALYs <- TabulateOutcomes(data = wtDALYOutcomes, decimals = 4, scale = 1)
  
  if(makePlots) {
    out <- structure(list(Incidence = wtIncidOutcomes, Mortality = wtMortOutcomes, DALYS = wtDALYOutcomes, 
                          plotsList = list(Incidence = incidPlot, Mortality = mortsPlot, DALYs = dalysPlot), 
                          baseparms = baseparms, basetransitions = basetransitions),
                     class = "modOut"
    )
    
  } else {
    
    out <- structure(list(Incidence = wtIncidOutcomes, Mortality = wtMortOutcomes, DALYS = wtDALYOutcomes, 
                          baseparms = baseparms, basetransitions = basetransitions),
                     class = "modOut"
    )
    
  }

  out <- structure(list(Incidence = wtIncidOutcomes, Mortality = wtMortOutcomes, DALYS = wtDALYOutcomes,
                        #gTabs = list(IncidTab = TableIncidence, MortTab = TableMortality, DALYsTab = TableDALYs),
                        baseparms = baseparms, basetransitions = basetransitions
  ),
  class = "modOut"
  )


  return(out)
  
}


# custom print method to supress baseparms an basetransitions from printing to the console

print.modOut <- function(x, what = TRUE...) {
  
  if (what == "plots") {
    x <- x["plotsList"]
  } else if (what == "tables") {
    x <- c(x["Incidence"], x["Mortality"], x["DALYS"])
  }
}
  

## computed weighted sum of outcomes and returns the named dataframe
weightSumOutcomes <- function(outcomesList, weights) {
  
  wtOutcome <- sapply(outcomesList, "*", weights)  # 
  wtSumOutcome <- apply(wtOutcome, MARGIN = 2, sum)
  wtSumOutcome <- rbind(outcomesList, wtSumOutcome)
  rownames(wtSumOutcome) <- c(rownames(outcomesList), "Average")
  return(wtSumOutcome)
  
}



## loop through base-case, high and low values and calculate model
# function requires user to have run the incremental analysis, if not, run incremental within the function and extrace the things we need
owsa <- function(model, low_base, low_transitions, high_base, high_transitions, 
                 outcome, max_vars) {
  
  # get the baseinputs and transitions from model object
  baseinputs <- model$baseparms
  basetransitions <- model$basetransitions
  
  # identify basecase values
  baseIncid <- model$Incidence[6, c(3, 6, 9:11)]
  baseMort <- model$Mortality[6, c(3, 6, 9:11)]
  baseDALYs <- model$DALYS[6, c(3, 6, 9:11)]
  
  
  # set up matrix to collect the results
  owsaIncid <- matrix(NA,
                      nrow = length(low_base) + length(low_transitions),
                      ncol=10)

  # make three of them
  owsaMort <- owsaDALYs <- owsaIncid
  
  lowinputs <- low_base
  highinputs <- high_base
  owsavars <- c(paste0(names(low_base), " (", sprintf(lowinputs, fmt = "%#.2f"),", ", sprintf(highinputs, fmt = "%#.2f"),")"),
                paste0("Distribution of - ", names(low_transitions)))
  
  i = 0
  # set all live values to the base case
  live <- baseinputs
  for (n in names(lowinputs)) {
    i = i + 1
    # counter
    message("Analyzing impact of ", n, ", variable ", i, "/", length(low_base) + length(low_transitions))
    # message("Running DSA on ", n, ", variable ", i, " of ", length(names(lowinputs)))
    # replace the current parameter with its low value and run the model
    low.input <- lowinputs[n]
    live[n] <- low.input
    
    model <- RunModel(baseparms = live, 
                      basetransitions = basetransitions)
    
    lowIncid <- model$Incidence[6, c(3, 6, 9:11)]
    lowMort <- model$Mortality[6, c(3, 6, 9:11)]
    lowDALYs <- model$DALYS[6, c(3, 6, 9:11)]
    
    # replace the current parameter with its high value and run the model
    high.input <- highinputs[n]
    live[n] <- high.input
    
    model <- RunModel(baseparms = live, 
                      basetransitions = basetransitions)
    
    highIncid <- model$Incidence[6, c(3, 6, 9:11)]
    highMort <- model$Mortality[6, c(3, 6, 9:11)]
    highDALYs <- model$DALYS[6, c(3, 6, 9:11)]
    
    owsaIncid[i, ] <- c(unlist(lowIncid), unlist(highIncid))
    owsaMort[i, ] <- c(unlist(lowMort), unlist(highMort))
    owsaDALYs[i, ] <- c(unlist(lowDALYs), unlist(highDALYs))
    
    
    
    # reset to the original value
    live[n] <- baseinputs[n]
  }
  
  # do the same for the transition probabilities, but put all in at once
  # set all live values to the base case, the 4th element of list returned by the RunModel function
  live <- basetransitions
  
  for (i in 1:length(low_transitions)) {
    
    message("Analyzing impact of dirichlet input set ", names(low_transitions)[i], ", variable ", i + length(low_base), "/", length(low_base) + length(low_transitions))
    
    # replace the current parameter-set with its low value and run the model
    low.input <- low_transitions[[i]]
    live[names(low_transitions[[i]])] <- low.input
    
    model <- RunModel(baseparms = baseinputs, 
                      basetransitions = live)
    
    lowIncid <- model$Incidence[6, c(3, 6, 9:11)]
    lowMort <- model$Mortality[6, c(3, 6, 9:11)]
    lowDALYs <- model$DALYS[6, c(3, 6, 9:11)]
    
    
    # replace the current parameter-set with its high value and run the model
    high.input <- high_transitions[[i]]
    live[names(high_transitions[[i]])] <- high.input
    
    model <- RunModel(baseparms = baseinputs, 
                      basetransitions = live)
    
    highIncid <- model$Incidence[6, c(3, 6, 9:11)]
    highMort <- model$Mortality[6, c(3, 6, 9:11)]
    highDALYs <- model$DALYS[6, c(3, 6, 9:11)]
    
    owsaIncid[i+length(low_base), ] <- c(unlist(lowIncid), unlist(highIncid))
    owsaMort[i+length(low_base), ] <- c(unlist(lowMort), unlist(highMort))
    owsaDALYs[i+length(low_base), ] <- c(unlist(lowDALYs), unlist(highDALYs))
    
    # reset to the original value in basetransitions
    live[names(high_transitions[[i]])] <- basetransitions[names(high_transitions[[i]])]
    
  }
  
  
  owsaList <-  list(owsaIncid, owsaMort, owsaDALYs)
  names(owsaList) <- c("Incidence", "Mortality", "DALYs")
  
  owsavars <- c(paste0(names(usa.low), " (", sprintf(usa.low, fmt = "%#.2f"),", ", sprintf(usa.high, fmt = "%#.2f"),")"),
                paste0("Distribution of - ", names(dir_lowinputs)))


  # post processing of data to return full results
  # split the table into respective components

  # loop through table and create list of tables for each outcome

  owsadata <- list()
  owsaplot <- list()

  basecase <- list(baseIncid, baseMort, baseDALYs)
  names(basecase) <- c("Incidence", "Mortality", "DALYs")
  
  genDSA <- function(owsalist, basecase, max_vars, named) {
    print(named)
    isName <- switch(named,
                     Incidence = "incident cases prevented",
                     Mortality = "deaths averted",
                     DALYs = "DALYs averted")
    
    isPerName <- switch(named,
                     Incidence = "incident case prevented",
                     Mortality = "death averted",
                     DALYs = "DALY averted")
    
    outcomeName <- c(isName, 
                     "Incremental costs (Societal Perspective)", 
                     "Incremental costs (Government Perspective)", 
                     paste0("Incremental cost per ", isPerName, " (Societal Perspective)"), 
                     paste0("Incremental cost per ", isPerName, " (Governmental Perspective)"))

    for (i in 1:5) {
      
      owsa_tab <- as_tibble(owsalist) %>%
        select(c(0 + i, 5 + i)) %>%
        rename("low" = 1, "high" = 2) %>%
        mutate(range = abs(high - low), 
               highval = high - unlist(basecase[i]), 
               lowval = low - unlist(basecase[i])) %>%
        add_column(variable = owsavars, .before = 1) %>%
        arrange(range)
      
      # generate plot object (can deal with axis labels later)
      owsaplot[[i]] <- TornadoPlot(owsatab = owsa_tab, basecase = unlist(basecase[i]), 
                                   outcomeName = outcomeName[i], max_vars = max_vars)
      names(owsaplot)[i] <- outcomeName[i]
      
      # also store owsadata in case I need to inspect it 
      owsadata[[i]] <- owsa_tab
      names(owsadata)[i] <- outcomeName[i]
    }
    
    return(list(Tables = owsadata, Plots = owsaplot))
    
  }
  

  # need to recover names of lists, so I do seq_along and add the name to the original function
  dsaList <- pblapply(seq_along(owsaList), function(x) {
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


# function takes output from DSA as a list, and plots the results (user inputs max vars)
TornadoPlot <- function(owsatab, basecase, outcomeName, max_vars) {
  
  # filter for those with perc impact on outcome
  owsa_torn <- owsatab |>
    slice_max(range, n = max_vars) |>
    arrange(desc(range)) |>
    select(1, 2, 3)
  
  p <- owsa_torn |>
    #mutate_if(is.character, as.factor) |>
    pivot_longer(!variable) |> # pivot longer 
    hchart('bar', hcaes(x = variable, y = value, group = name), 
           color = brewer.pal(3, "Dark2")[1:2],
           threshold = basecase) |>
    hc_yAxis(title = list(text = outcomeName),
             gridLineWidth = 0,
             lineWidth = 1,
             tickWidth = 1, 
             crosshair = TRUE,
             minorTicks = TRUE,
             minorGridLineWidth = 0,
             minorTickLength = 5,
             minorTickWidth = 1) |>
    hc_xAxis(title = list(text = ""),
             lineWidth = 1,
             tickWidth = 1, 
             crosshair = TRUE,
             tickmarkPlacement = 'on') |>
    hc_chart(zoomType = "x") |>
    hc_plotOptions(bar = list(grouping = FALSE))

  return(p)
  
}



# functions to create intervals for the dirichlet distributions
dir_interval <- function(prior) {
  prior <- unlist(prior)
  posterior <- prior + rep(1, length=length(prior))
  lower <- qgamma(.025,shape=posterior,rate=1)/sum(qgamma(.025,shape=posterior,rate=1))
  upper <- qgamma(.975,shape=posterior,rate=1)/sum(qgamma(.975,shape=posterior,rate=1))
  return(list(lower,upper))
}

# function to generate and name random dirichlet variates
vdirichlet <- function(n, alpha) {
  variates <- MCMCpack::rdirichlet(n, alpha)
  colnames(variates) <- names(alpha)
  return(variates)
}


# this function calls the RunModel function on psalist
# use it to extract the PSA trace of costs and outcomes
genPSA <- function(model, psalist) {
  
  # get things we need
  basenames <- names(model$baseparms)
  transitnames <- names(model$basetransitions)
  
  psamod <- RunModel(baseparms = psalist[basenames],
                     basetransitions = psalist[transitnames])
  
  psares <- list(Incidence = psamod$Incidence[6, c(3, 6, 9:11)],
                 Mortality = psamod$Mortality[6, c(3, 6, 9:11)],
                 DALYs = psamod$DALYS[6, c(3, 6, 9:11)])
  
  return(psares)
  
}


RunPSA <- function(model, nsims, wtp, by) {
  
  # generate draws
  psalist <- makeSims(nsims)
  
  # run PSA: split PSA inputs (psalist) into row by row lists, calculate PSA (using genPSA function)
  psaTrace <- transpose(psalist) |>
    pbsapply(FUN = genPSA, model = model)
  
  outList <- c("Incidence", "Mortality", "DALYs")
  names(outList) <- outList
  
  psaTraces <- pblapply(outList, 
                FUN = function(x) data.table::setDF(data.table::rbindlist(psaTrace[x, ])))
  
  # generate scatter plot
  # need to recover names of lists, so I do seq_along and add the name to the original function
  scatterPlots <- pblapply(seq_along(psaTraces), function(x) {
    ScatterPSA(psaTrace = psaTraces[[x]], named = names(psaTraces)[[x]])
    
    })
  names(scatterPlots) <- names(psaTraces) # name the traces

  # run CEAC analysis
  ceacTraces <- pblapply(psaTraces, RunCEAC, wtp = wtp, by = by)
  
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



ScatterPSA <- function(psaTrace, named) {
  
  min <- with(psaTrace, ifelse((min(deltaSocCosts) > 0 | min(deltaGovCosts) > 0) , 0, NULL))
  
  isName <- switch(named,
                   Incidence = "incident cases prevented",
                   Mortality = "deaths averted",
                   DALYs = "DALYs averted")

  h1 <- highchart() |>
    hc_add_series(data = psaTrace,
                  type = 'scatter',
                  name = "Societal Perspective",
                  hcaes(x = deltaOutcome, y = deltaSocCosts),
                  marker = list(symbol = "circle", 
                                fillColor = "rgba(27,158,119,0.3)",
                                lineColor = "rgba(27,158,119,0.5)",
                                lineWidth = 1,
                                radius = 4)) |>
    hc_add_series(data = psaTrace,
                  type = 'scatter',
                  name= "Governmental Perspective",
                  hcaes(x = deltaOutcome, y = deltaGovCosts),
                  marker = list(symbol = "circle", 
                                fillColor = "rgba(217,95,2,0.3)",
                                lineColor = "rgba(2217,95,2,0.5)",
                                lineWidth = 1,
                                radius = 4)) |>
    hc_yAxis(lineWidth = 1, # show y-axis line
             tickWidth = 1, # show y-axis ticks
             gridLineWidth = 0, # remove gridlines
             #tickLength = 1,
             #gridLineColor = 'transparent', # removes gridlines
             startOnTick = TRUE,
             title = list(text = "Incremental costs", align = "high"),
             min = min) |>
    hc_xAxis(title = list(text = isName, align = "high"),
             startOnTick = TRUE) |>
    hc_title(text = paste0("Incremental cost and ", isName, " pairs"))

  
  return(h1)

}


drawCEAC <- function(ceacTrace, named) {
  
  isName <- switch(named,
                   Incidence = "incident cases prevented",
                   Mortality = "deaths averted",
                   DALYs = "DALYs averted")
  
  isPerName <- switch(named,
                      Incidence = "incident case prevented",
                      Mortality = "death averted",
                      DALYs = "DALY averted")
  
  # bezier points for smooth curve
  bezCEAC <- as_tibble(bezier(t=seq(0, 1, length=200), p=ceacTrace))
  colnames(bezCEAC) <- c("WTP", "pCESoc", "pCEGov")
  
  h1 <- highchart() |>
    hc_add_series(data = bezCEAC,
                  type = 'line',
                  name = "Societal Perspective",
                  hcaes(x = WTP, y = pCESoc)) |>
    hc_add_series(data = bezCEAC,
                  type = 'line',
                  name= "Governmental Perspective",
                  hcaes(x = WTP, y = pCEGov)) |>
    hc_yAxis(max = 1,
             lineWidth = 1, # show y-axis line
             tickWidth = 1, # show y-axis ticks
             gridLineWidth = 0, # remove gridlines
             #tickLength = 1,
             #gridLineColor = 'transparent', # removes gridlines
             startOnTick = TRUE,
             title = list(text = "Probability Cost Effective", align = "high")) |>
    hc_xAxis(title = list(text = paste0("WTP ($/", isPerName, ")"), align = "high"),
             startOnTick = TRUE,
             min = 0) |>
    hc_title(text = paste0("Cost Effective Acceptability Curve (", named, ")"))


  return(h1)
  
}

outcomesPlot <- function(outcome, whichTab) {
  
    whichTab <- rownames_to_column(whichTab, var = "Quintile")
    
    name1 <- switch(outcome, 
                    Incidence = "Incidence (Misoprostol)",
                    Mortality = "Mortality (Misoprostol)",
                    DALYs = "DALYS (Misoprostol)")
    
    name2 <- switch(outcome, 
                    Incidence = "Additional cases",
                    Mortality = "Deaths prevented",
                    DALYs = "DALYs averted")
    
    h1 <- highchart() |> 
      hc_chart(type = "column") |>
      hc_plotOptions(column = list(stacking = "normal")) |>
      hc_xAxis(categories = whichTab[, 1],
               crosshair = TRUE) |>
      hc_add_series(name=paste0(name2, " (No Miso)"),
                    data = whichTab[, 4],
                    stack = "Quintile") |>
      hc_add_series(name=name1,
                    data = whichTab[, 3],
                    stack = "Quintile") |>
      hc_title(text = outcome)
    
    h2 <- highchart() |> 
      hc_chart(type = "column") |>
      hc_plotOptions(column = list(stacking = "normal")) |>
      hc_xAxis(categories = whichTab[, 1],
               crosshair = TRUE) |>
      hc_add_series(name="Incremental Cost (Governmental)",
                    data = whichTab[, 10],
                    stack = "Quintile") |>
      hc_add_series(name="No Miso Costs (Governmental)",
                    data = whichTab[, 8],
                    stack = "Quintile") |>
      hc_add_series(name="Incremental Cost (Societal)",
                    data = whichTab[ ,7],
                    stack = "Quintile2") |>
      hc_add_series(name="No Miso Costs (Societal)",
                    data = whichTab[, 5],
                    stack = "Quintile2")
    
    return(hw_grid(list(h1, h2)))
    
}



TabulateOutcomes <- function(data, decimals, scale) {
  iTab <- rownames_to_column(data.frame(data), var = "Wealth Quintile") %>%
    as_tibble() %>%
    select(c(1, 10, 7, 4, 12, 11)) %>%
    rename(., "Governmental" = 2,
           "Societal" = 3,
           "Diff. in outcome" = 4,
           "iSocietal" = 5,
           "iGovernmental" = 6) %>%
    gt(rowname_col = 1) %>%
    fmt_currency(
      columns = c(2, 3, 5, 6),
      currency = "USD"
    ) %>%
    tab_spanner(
      label = "Incremental Costs",
      columns = c(2, 3)
    ) %>%
    tab_spanner(
      label = "ICER",
      columns = c(5, 6)
    )  %>%
    fmt_number(
      columns = 4,
      decimals = decimals,
      scale_by = scale
    )
  
}



makeSims <- function(nsims, ...) {
  set.seed(12678)
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
  randdirichlets <- data.table::as.data.table(unname(lapply(randdirchinputs, vdirichlet, n=nsims)))
  randdirichlets <- data.table::setDF(randdirichlets)
  
  fixedinputs <- matrix(rep(baseinputs[fixedparms],each=nsims),nrow=nsims) %>%
    magrittr::set_colnames(., fixedparms)
  
  
  # pool together into one dataframe
  psa_input <- data.frame(randbeta, randtnormal, randunif, randlognormal, randdirichlets, fixedinputs)
  
  return(psa_input)
  
  
}






