library(ggplot2)
library(R.matlab)
library(latex2exp)
library(wesanderson)

directory = "~/Documents/Morphorods/Code/MATLAB/MechanochemicalGrowth/Solutions/"
values = "k_0p02_L0_0p125_sigma0p1L_area1_mu0_inext"
mutantValues = "k_0p02_L0_0p125_sigma0p1L_mutant_0p5L_area7_mu0_inext"

# Load the solutions
solsList <- readMat(paste(directory, "sols_", values, ".mat", sep=""))
# Load the growth profiles
gammaList <- readMat(paste(directory, "gamma_", values, ".mat", sep=""))
# Load the times
timesList <- readMat(paste(directory, "times_", values, ".mat", sep=""))

# Load the mutant sols
mutantSolsList <- readMat(paste(directory, "sols_", mutantValues, ".mat", sep=""))
# Load the growth profiles
mutantGammaList <- readMat(paste(directory, "gamma_", mutantValues, ".mat", sep=""))
# Load the times
mutantTimesList <- readMat(paste(directory, "times_", mutantValues, ".mat", sep=""))

numSols <- length(solsList[[1]])
numMutantSols <- length(mutantSolsList[[1]])
solData <- NULL # Create data frame for solutions
gammaData <- NULL # Create data frame for growth profiles

# Some parameters
solCount <- 1

# Add the "base solution" 
baseSol <- solsList[[1]][[numSols - 1]][[1]]
baseGammaProfile <- gammaList[[1]][[numSols - 1]][[1]]

solDataFrame <- data.frame(S=baseSol[2,], x=baseSol[3,], y=baseSol[4,], nX=baseSol[5,], nY=baseSol[6,], theta=baseSol[7,], m=baseSol[8,])
solDataFrame$time = toString(1)
solData <- rbind(solData, solDataFrame)

gammaDataFrame <- data.frame(S=baseGammaProfile[1,], gamma=baseGammaProfile[2,])
gammaDataFrame$time = toString(1)
gammaData <-rbind(gammaData, gammaDataFrame)

# Plot solutions
for ( i in seq(1, numMutantSols - 1, by=solCount) )
{
  # Extract the solution, growth and time
  sol <- mutantSolsList[[1]][[i]][[1]]
  gammaProfile <- mutantGammaList[[1]][[i]][[1]]
  currentTime = mutantTimesList[[1]][[i]]

  # Update the solutions
  solDataFrame <- data.frame(S=sol[2,], x=sol[3,], y=sol[4,], nX=sol[5,], nY=sol[6,], theta=sol[7,], m=sol[8,])
  solDataFrame$time = toString(i + 1)
  solData <- rbind(solData, solDataFrame)

  gammaDataFrame <- data.frame(S=gammaProfile[1,], gamma=gammaProfile[2,])
  gammaDataFrame$time = toString(i + 1)
  gammaData <-rbind(gammaData, gammaDataFrame)

}

# Tweaking the plots
solPlot <- ggplot(solData) + geom_path(aes(x, -y, colour=time), size=1.0) + ylab(TeX('$y$')) + xlab(TeX('$x$')) + theme(legend.position="none", panel.background = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_blank(), text=element_text(size=18)) 
gammaPlot <- ggplot(gammaData) + geom_path(aes(S, gamma, colour=time), size=1.0) + ylab(TeX('$\\gamma$')) + xlab(TeX('$S_0$')) + theme(legend.position="none", panel.background = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_blank(), text=element_text(size=18))
