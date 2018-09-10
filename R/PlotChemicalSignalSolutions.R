library(ggplot2)
library(R.matlab)
library(latex2exp)

# Some parameters
solCount <- 1

# Load the solutions
solsList <- readMat("~/Documents/Morphorods/Code/MATLAB/ChemicalSignalling/Solutions/gaussiangrowth_bvpsols_k0p04_L20_sigma1p25.mat")

numSols <- length(solsList[[1]])
solPlot <- NULL

# Plot solutions
for ( i in seq(1, 25, by=solCount) )
{
  # Extract the solution
  sol <- solsList[[1]][[i]][[1]]
  
  # Convert the relevant solutions to a data frame
  rodSol <- cbind(sol[3,], -sol[4,])
  solData <- as.data.frame(rodSol)
  colnames(solData) <- c("x","y")
  
  if (i == 1)
  {
    solPlot <- ggplot(solData, aes(x=x, y=y)) + geom_path()
  }
  else
  {
    # Add it to the plots
    solPlot <- solPlot + geom_path(data=solData, aes(x=x, y=y))
  }
  
}

# Load the growth profiles
gammaList <- readMat("~/Documents/Morphorods/Code/MATLAB/ChemicalSignalling/Solutions/gaussiangrowth_gamma_k0p04_L20_sigma1p25.mat")

numGammaProfiles = length(gammaList[[1]])

gammaPlot <- NULL

# Plot solutions
for ( i in seq(1, numGammaProfiles, by=solCount) )
{
  gammaProfile <- gammaList[[1]][[i]][[1]]

  # Convert the relevant solutions to a data frame
  gammaSol <- cbind(gammaProfile[1,], gammaProfile[2,])
  
  gammaData <- as.data.frame(gammaSol)
  colnames(gammaData) <- c("S","gamma")
  
  if (i == 1)
  {
    gammaPlot <- ggplot(gammaData, aes(x=S, y=gamma)) + geom_path()

  }
  else
  {
    # Add it to the plots
    gammaPlot <- gammaPlot + geom_path(data=gammaData, aes(x=S,y=gamma))

  }
}


# Tweaking the plots
solPlot <- solPlot + theme_classic() + ylab(TeX('$y$')) + xlab(TeX('$x$')) + theme(axis.ticks = element_blank(), text=element_text(size=18, family="serif"))
gammaPlot <- gammaPlot + theme_classic() + ylab(TeX('$\\gamma$')) + xlab(TeX('$S_0$')) + theme(axis.ticks = element_blank(), text=element_text(size=18, family="serif"))
