library(ggplot2)
library(R.matlab)
library(latex2exp)
library(wesanderson)

directory = "~/Documents/Morphorods/Code/MATLAB/MechanochemicalGrowth/Solutions/"
values = "k_0p02_L0_0p125_sigma0p2L_area1_mu0_inext_variablefoundation"

# Load the solutions
solsList <- readMat(paste(directory, "sols_", values, ".mat", sep=""))
# Load the growth profiles
gammaList <- readMat(paste(directory, "gamma_", values, ".mat", sep=""))
# Load the times
timesList <- readMat(paste(directory, "times_", values, ".mat", sep=""))

numGammaProfiles = length(gammaList[[1]])
numSols <- length(solsList[[1]])
solData <- NULL # Create data frame for solutions
gammaData <- NULL # Create data frame for growth profiles

# Some parameters
solCount <- 1

# Plot solutions
for ( i in seq(2, numSols, by=solCount) )
{
  # Extract the solution, growth and time
  sol <- solsList[[1]][[i]][[1]]
  gammaProfile <- gammaList[[1]][[i]][[1]]
  currentTime = timesList[[1]][[i]]
  
  # Update the solutions
  solDataFrame <- data.frame(S=sol[2,], x=sol[3,], y=sol[4,], nX=sol[5,], nY=sol[6,], theta=sol[7,], m=sol[8,])
  solDataFrame$time = toString(currentTime)
  solData <- rbind(solData, solDataFrame)
  
  gammaDataFrame <- data.frame(S=gammaProfile[1,], gamma=gammaProfile[2,])
  gammaDataFrame$time = toString(i)
  gammaData <-rbind(gammaData, gammaDataFrame)
  
}

# Tweaking the plots
solPlot <- ggplot(solData) + geom_path(aes(x, -y, colour=time), size=1.0) + ylab(TeX('$y$')) + 
            xlab(TeX('$x$')) + 
            theme(legend.position="none", panel.background = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_blank(), text=element_text(size=18)) + 
            scale_colour_manual(values=colorRampPalette(wes_palette("GrandBudapest"))(numSols)) +
            scale_x_continuous(expand = c(0, 0), limits=c(0,31))
gammaPlot <- ggplot(gammaData) + geom_path(aes(S, gamma, colour=time), size=1.0) + 
              ylab(TeX('$\\gamma$')) + xlab(TeX('$S_0$')) + 
              theme(legend.position="none", panel.background = element_blank(), axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_blank(), text=element_text(size=18)) +
              scale_colour_manual(values=colorRampPalette(wes_palette("GrandBudapest"))(numSols)) +
              scale_x_continuous(expand = c(0, 0), limits=c(0,31))
