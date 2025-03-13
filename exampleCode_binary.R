# Binary endpoint
# no FA
allocationRatio <- 1
pi1 <- 0.4 
pi2 <- 0.2 

design <- getDesignGroupSequential(kMax = 1, alpha = 0.025, sided = 1)

powerNoFA <- getPowerRates(
  design=design,
  allocationRatioPlanned = allocationRatio,
  maxNumberOfSubjects = 200, 
  pi1 = pi1, 
  pi2 = pi2
  )

powerNoFA$overallReject
MDD <- powerNoFA$criticalValuesEffectScale
MDD

# ------------------------------
# now add futility
informationRate <- 0.5
futility_rateDiff <- 0.07 
n_interim <- 200 * informationRate
pi <- allocationRatio/(1+allocationRatio)

# Estimated response rate in all patient under the null hypothesis if observed difference in proportions = futility bound
piNull <- ((1-pi)*pi2 + pi*(pi2+futility_rateDiff))
se <- sqrt(piNull * (1 - piNull))/sqrt(n_interim*pi*(1-pi))

# Conversion of futility boundary from response rate difference to Z scale
futilityZ <- futility_rateDiff/se 

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
  getPowerRates(
    design = designFA,
    allocationRatioPlanned = allocationRatio,
    maxNumberOfSubjects = 200, 
    pi1 = pi1, 
    pi2 = pi2
    )
summary(powerFA)

# If the interim result is right on the futility boundary, what effect do I need to see at the final analysis
# for the study to be positive?
(MDD - informationRate * futility_rateDiff)/(1-informationRate)
