#-----------------------------------------------
# continuous endpoint
# no FA
allocationRatio <- 1
stDev <- 1

design <- getDesignGroupSequential(kMax = 1, alpha = 0.025, sided = 1)

powerNoFA <- getPowerMeans(
  design = design,
  maxNumberOfSubjects = 750,
  allocationRatioPlanned = allocationRatio,
  alternative = 0.24,
  stDev = stDev,
  normalApproximation = TRUE
)

powerNoFA$overallReject
MDD <- powerNoFA$criticalValuesEffectScale

# add a futility analysis
# Conversion of futility boundary from mean difference to Z scale
futility_meanDiff <- 0.1
informationRate <- 0.5
#informationRate <- c(0.3, 0.5, 0.7)
n_interim <- 750 * informationRate
pi <- allocationRatio/(1+allocationRatio)
se <- stDev/sqrt(n_interim*pi*(1-pi))
futilityZ <- futility_meanDiff/se
# probability of stopping for futility under H0 can be calculated as pnorm(futilityZ)
pnorm(futilityZ)

# Updated design with futility interim
designFA <-
  getDesignGroupSequential(
    alpha = 0.025,
    sided = 1,
    informationRates = c(informationRate, 1),
    typeOfDesign = "noEarlyEfficacy",
    futilityBounds = futilityZ
  )

# Calculate power and other design characteristics of updated design
powerFA <-
  getPowerMeans(
    design = designFA,
    maxNumberOfSubjects = 750,
    allocationRatioPlanned = allocationRatio,
    alternative = 0.24,
    stDev = stDev,
    normalApproximation = TRUE
  )
summary(powerFA)

(MDD - informationRate*futility_meanDiff)/(1-informationRate)
