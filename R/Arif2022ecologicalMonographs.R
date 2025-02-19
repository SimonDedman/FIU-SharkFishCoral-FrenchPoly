# Simulate forest species abundance data ####
# Generating simulated data set for factors influencing forest species abundance, given the DAG in Fig 3.

# install simstudy: user friendly package for simulating data
install.packages("simstudy")
library(simstudy)

# set seed for reproducibility
set.seed(5)

# define variables for protected area, fire, poaching, logging, distance to roads and cities, slope, elevation, carbon sequestration and forest species abundance
def <- defData(varname = "slope", dist = "normal", formula = 0, variance = 1)

def <- defData(def, varname = "elevation", dist = "normal", formula = 0, variance = 1)

def <- defData(def, varname = "distancetoroadsandcities", dist = "normal", formula = 0, variance = 1)

def <- defData(def, varname = "protectedarea", dist = "binary", 	formula = "0.9 * slope + 0.9 * elevation + -1.2 *	distancetoroadsandcities", link = "logit", variance = 1)

def <- defData(def, varname = "fire", dist = "normal", formula = "-0.5 * protectedarea + 0.6 * distancetoroadsandcities", variance = 1)

def <- defData(def, varname = "poaching", dist = "normal", formula = "-1.1 * protectedarea + 0.8 * distancetoroadsandcities", variance = 1)

def <- defData(def, varname = "logging", dist = "normal", formula = "-0.7 * slope + -0.7 * elevation + -1.6 * protectedarea + 0.6 * distancetoroadsandcities", variance = 1)

def <- defData(def, varname = "forestspeciesabundance", dist = "normal", formula = "-0.4 * fire + -0.6 * poaching + -0.7 * logging", variance = 1)

def <- defData(def, varname = "carbonsequestration", dist = "normal", formula = "-0.8 * logging + 0.4 * forestspeciesabundance", variance = 1)

# create 10000 observations
ForestSpeciesAbundanceData <- genData(10000, def)

# Test DAG-data consistency ####
# Install dagitty
install.packages("dagitty")
library(dagitty)

# download specified DAG from dagitty.net
DAG <- downloadGraph("dagitty.net/m18S_bV")

# evaluate the d-separation implications of our DAG with our simulated dataset
test <- localTests(DAG, ForestSpeciesAbundanceData)

# perform Holm-Bonferrino correction to mitigate problems around multiple testing
test$p.value <- p.adjust(test$p.value)

test # should show all p values above 0.05, suggesting DAG-data consistency

# Statistical models in Figure 5 ####
## Linear regression models ####
# that employ DAG-informed vs DAG-uniformed covariate selection. Results are presented in Figure 5.
# Protected Area Model [known effect: 1.98]
M1 <- glm(forestspeciesabundance ~ protectedarea + 	distancetoroadsandcities + elevation + slope, data = ForestSpeciesAbundanceData)
summary(M1)

# Fire Model [known effect: -0.4]
# Option 1:
M2a <- glm(forestspeciesabundance ~ fire + logging + poaching, 	data = ForestSpeciesAbundanceData)
summary(M2a) # provides point estimate, standard errors, and AIC values

# Option 2:
M2b <- glm(forestspeciesabundance ~ fire + 	distancetoroadsandcities + protectedarea, data = ForestSpeciesAbundanceData)
summary(M2b)


## Poaching Models [known effect: -0.6] ####
# Option 1:
M3a <- glm(forestspeciesabundance ~ poaching + fire + logging, 	data = ForestSpeciesAbundanceData)
summary(M3a)

# Option 2:
M3b <- glm(forestspeciesabundance ~ poaching + 	distancetoroadsandcities + protectedarea, data = ForestSpeciesAbundanceData)
summary(M3b)

## Logging Models ####
# [known effect: -0.7]
# Option 1:
M4a <- glm(forestspeciesabundance ~ logging + fire + poaching, 	data = ForestSpeciesAbundanceData)
summary(M4a)

# Option 2:
M4b <- glm(forestspeciesabundance ~ logging + 	distancetoroadsandcities + protectedarea, data = ForestSpeciesAbundanceData)
summary(M4b)

## Model 5: Causal Salad Model ####
# Include all variables as covariates and subsequently interpreting their coefficients as their effect on forest species richness.
M5 <- glm(forestspeciesabundance ~ fire + poaching + logging + 	protectedarea + distancetoroadsandcities + elevation + 	slope + carbonsequestration, data = ForestSpeciesAbundanceData)
summary(M5)

# Simulate shark-bivalve data ####
# Generating simulated data set for the effect of sharks on bivalves, given the DAG in Fig 7.
# use the set.seed() function for reproducibility
set.seed(5)

# create a sample size of 100000
N = 100000

# generates a dataset for fishing pressure
Fishing_Pressure = rnorm(N)

# generates a dataset for sharks
Sharks = -2.2 * Fishing_Pressure + rnorm(N)

# generates a dataset for rays
Rays = -0.1 * Sharks + rnorm(N)

# generates a dataset for bivalves
Bivalves = -0.2 * Rays + -1.5 * Fishing_Pressure + rnorm(N)

# creates data frame of all data, called sharkdata
sharkdata <- data.frame(Fishing_Pressure = Fishing_Pressure,
                        Sharks = Sharks,
                        Rays = Rays,
                        Bivalves = Bivalves)


# Statistical models in Figure 7 ####
# Employing the front-door criterion to determine effect of sharks on bivalves.
# First, a model to determine the effect of sharks on rays:
M1 <- glm(Rays~Sharks, data = sharkdata)
summary(M1) # provides point estimate and standard error for sharks, which is -0.1 + 0.003SE

# Next, a model to determine the effect of rays on bivalves:
M2 <- glm(Bivalves ~ Rays + Sharks, data = sharkdata)
summary(M2) # provides point estimate and standard error for rays, which is -0.2 + 0.003SE

# The product of the effect of sharks on rays and the effect of rays on bivalves gives the estimate of sharks on bivalves. In our case this is -0.1 x -0.2 = 0.02
