library(rpact)
#------------------------------
# TTE endpoint
# no FA
allocationRatio <- 1

powerNoFA <- getPowerSurvival(
  design = getDesignGroupSequential(kMax = 1, alpha = 0.025, sided = 1),
  hazardRatio = 0.65,
  median2 = 11,
  dropoutRate1 = 0.15,
  dropoutRate2 = 0.15,
  dropoutTime = 12,
  accrualTime = 27,
  maxNumberOfEvents = 227,
  maxNumberOfSubjects = 400,
  allocationRatioPlanned = allocationRatio,
  directionUpper = F
)

powerNoFA$overallReject
#------------------------------
# Conversion of futility boundary from HR to Z scale
informationRate <- 0.7
futilityBoundaryHR <- 0.85
numberOfEvents_interim <- 227 * informationRate
factor <- (1 + allocationRatio)^2/allocationRatio # = 4 for 1:1 allocation
futilityZ <-  -log(futilityBoundaryHR)/sqrt(factor/numberOfEvents_interim) 

# Updated design with futility interim
designFA <-
  getDesignGroupSequential(
    kMax = 2,
    informationRates = c(informationRate, 1),
    alpha = 0.025,
    typeOfDesign = "noEarlyEfficacy",
    futilityBounds = futilityZ
  )

# Calculate power and other design characteristics of updated design
powerFA <-
  getPowerSurvival(
    design = designFA,
    hazardRatio = 0.65,
    median2 = 11,
    dropoutRate1 = 0.15,
    dropoutRate2 = 0.15,
    dropoutTime = 12,
    accrualTime = 27,
    maxNumberOfSubjects = 400,
    allocationRatioPlanned = allocationRatio, 
    maxNumberOfEvents = 227,
    directionUpper = FALSE
  )

summary(powerFA)

# use futile package to plot the operating characteristics vs. the futility boundary for a given information fraction
install.packages("futile", repos = "https://rspm.roche.com/Non-Validated-Preview/latest")
library(futile)
chooseBoundary(t = 0.3, boundary = 0.95, alternative = 0.65, mdd = 0.771, survival = TRUE)
