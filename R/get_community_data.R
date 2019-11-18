#' Get Communities and Crime Unnormalized Data Set from the UCI respository
#' @export

get_community_data <- 
function() {

  ## Pull data from UCI respository
  file <- paste0("https://archive.ics.uci.edu/ml/machine-learning-databases",
                 "/00211/CommViolPredUnnormalizedData.txt")
  community <- read.csv(file, header = F)
  
  ## txt file does not include header names, add those manually
  c_names <- "communityname, state, countyCode, communityCode, fold, 
               population, householdsize, racepctblack, racePctWhite, 
               racePctAsian, racePctHisp, agePct12t21, agePct12t29, agePct16t24, 
               agePct65up, numbUrban, pctUrban, medIncome, pctWWage, 
               pctWFarmSelf, pctWInvInc, pctWSocSec, pctWPubAsst, pctWRetire, 
               medFamInc, perCapInc, whitePerCap, blackPerCap, indianPerCap, 
               AsianPerCap, OtherPerCap, HispPerCap, NumUnderPov, 
               PctPopUnderPov, PctLess9thGrade, PctNotHSGrad, PctBSorMore, 
               PctUnemployed, PctEmploy, PctEmplManu, PctEmplProfServ, 
               PctOccupManu, PctOccupMgmtProf, MalePctDivorce, MalePctNevMarr, 
               FemalePctDiv, TotalPctDiv, PersPerFam, PctFam2Par, 
               PctKids2Par, PctYoungKids2Par, PctTeen2Par, PctWorkMomYoungKids, 
               PctWorkMom, NumKidsBornNeverMar, PctKidsBornNeverMar, NumImmig, 
               PctImmigRecent, PctImmigRec5, PctImmigRec8, PctImmigRec10, 
               PctRecentImmig, PctRecImmig5, PctRecImmig8, PctRecImmig10, 
               PctSpeakEnglOnly, PctNotSpeakEnglWell, PctLargHouseFam, 
               PctLargHouseOccup, PersPerOccupHous, PersPerOwnOccHous, 
               PersPerRentOccHous, PctPersOwnOccup, PctPersDenseHous, 
               PctHousLess3BR, MedNumBR, HousVacant, PctHousOccup, 
               PctHousOwnOcc, PctVacantBoarded, PctVacMore6Mos, MedYrHousBuilt, 
               PctHousNoPhone, PctWOFullPlumb, OwnOccLowQuart, OwnOccMedVal, 
               OwnOccHiQuart, OwnOccQrange, RentLowQ, RentMedian, RentHighQ, 
               RentQrange, MedRent, MedRentPctHousInc, MedOwnCostPctInc, 
               MedOwnCostPctIncNoMtg, NumInShelters, NumStreet, PctForeignBorn, 
               PctBornSameState, PctSameHouse85, PctSameCity85, PctSameState85, 
               LemasSwornFT, LemasSwFTPerPop, LemasSwFTFieldOps, 
               LemasSwFTFieldPerPop, LemasTotalReq, LemasTotReqPerPop, 
               PolicReqPerOffic, PolicPerPop, 
               RacialMatchCommPol, PctPolicWhite, PctPolicBlack, PctPolicHisp, 
               PctPolicAsian, PctPolicMinor, OfficAssgnDrugUnits, 
               NumKindsDrugsSeiz, PolicAveOTWorked, LandArea, PopDens, 
               PctUsePubTrans, PolicCars, PolicOperBudg, LemasPctPolicOnPatr, 
               LemasGangUnitDeploy, LemasPctOfficDrugUn, PolicBudgPerPop, 
               murders, murdPerPop, rapes, rapesPerPop, robberies, 
               robbbPerPop, assaults, assaultPerPop, burglaries, burglPerPop, 
               larcenies, larcPerPop, autoTheft, autoTheftPerPop, arsons, 
               arsonsPerPop, ViolentCrimesPerPop, nonViolPerPop"
  
  ## Convert to a character vector
  c_names <- unlist(strsplit(c_names, split = ","))
  
  ## Remove new line characters, remove spaces, add to data
  c_names <- gsub(pattern = "\n", "", x = c_names)
  c_names <- gsub(pattern = " ", "", x = c_names)
  colnames(community) <- c_names
  
  ## Replace "?" with NA
  community[community == "?"] <- NA
  community <- community[, colSums(is.na(community)) == 0]
  
  ## Convert all columns to numeric, add outcome
  col_start <- which(names(community) == "population")
  end_start <- which(names(community) == "LemasPctOfficDrugUn")
  community2 <- as.data.frame(sapply(community[, col_start:end_start], 
                                     as.numeric))
  community2$murdPerPop <- community$murdPerPop

  ## Return data
  community2
}
