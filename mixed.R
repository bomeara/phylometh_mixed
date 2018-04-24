install.packages("yearn")
library(yearn)
yearn(phylolm)
yearn(geiger)
yearn(adephylo)

# To understand what you're doing here, read:

# Ho, L. S. T. and Ane, C. 2014. "A linear-time algorithm for Gaussian and non-Gaussian trait evolution models". Systematic Biology 63(3):397-408.

# Ives, A. R. and T. Garland, Jr. 2010. "Phylogenetic logistic regression for binary dependent variables". Systematic Biology 59:9-26.

# Paradis E. and Claude J. 2002. "Analysis of Comparative Data Using Generalized Estimating Equations". Journal of Theoretical Biology 218:175-185.

# Hansen, T. F. 1997. "Stabilizing selection and the comparative analysis of adaptation". Evolution 51:1341-1351.

# Basically, we're going to try to fit regressions, correcting for phylogeny.

# If the dependent trait is discrete, use phyloglm; if it's continuous, use phylolm.


load("primateplay.rda") # Data from https://doi.org/10.1177/1059712315611733 and tree from https://doi.org/10.1002/evan.20251 (pruned by https://doi.org/10.1177/1059712315611733). Read both papers before using the tree or data for any published work so you make sure to cite the underlying data sources appropriately

# Look at what data is available:

print(colnames(primate.data.pruned))

# And plot it

adephylo::bullseye(primate.phy, primate.data.pruned, legend=FALSE, circ.n=1, show.tip.label=TRUE, cex=0.2,
traits.cex=0.5, open.angle=45, rotate=90)

# Some functions to make it easier to analyze the data:

#' Drop rows that have any missing data
#'
#' @param data data.frame with taxon names as rownames, fields as colnames
#' @param traits character vector of traits to keep
#' @return pruned data.frame
PruneFactors <- function(data, traits) {
	data<-data[,colnames(data) %in% traits]
	data<-data[complete.cases(data),]
	return(data)
}

#' Clean the tree and data so that all taxa have all the traits you want
#'
#' @param traits character vector of traits to keep
#' @param data data.frame with taxon names as rownames, fields as colnames
#' @param phy a phylo object
#' @return a list of two elements, phy and data
CleanAll <- function(traits, data, phy) {
	data <- PruneFactors(data, traits)
	pruned <- geiger::treedata(phy, data, sort=TRUE, warnings=FALSE)
	pruned$data <- data.frame(pruned$data)
	return(list(phy=pruned$phy, data=pruned$data))
}

# An example analysis

traits.example1 <- c("overall.social.play", "BMR_mean")
pruned.example1 <- CleanAll(traits=traits.example1, data=primate.data.pruned, phy=primate.phy)
example1.result <- phylolm::phyloglm(overall.social.play~BMR_mean, phy=pruned.example1$phy, data=pruned.example1$data, boot=100)

# Look at results

print(example1.result)

print(example1.result$bootconfint95)

stop("What do the results mean?")

# Now do similar analyses to compare other correlations. Remember: phylolm for continuous dependent variables, phyloglm for discrete. ?phylolm and ?phyloglm for help, ?formula for how to write formulas
