#https://besjournals.onlinelibrary.wiley.com/doi/10.1111/j.1365-2656.2009.01639.x
#https://juliengamartin.github.io/wam_tuto/
#https://wildanimalmodels.org/
#example: https://www.journals.uchicago.edu/doi/full/10.1086/730261

#-----------------------

df<- tpc.sel[which(tpc.sel$period=="past"),-which(colnames(tpc.sel)%in% c("FecEggCount"))]
df<- na.omit(df)
names(df)[c(1,3)]<- c("dam","id")

#install.packages("qgg")
library(qgg)

ped <- create_pedigree(df$id, df$dam)

traits <- c("RGR11","RGR17","RGR23","RGR29","RGR35")
models <- lapply(traits, function(trait) {
  animal_model(y = your_data[[trait]], 
               random = ~ped(individual),
               data = df)
})

#----

library(gremlin)
library(nadiv)



pedigree<- df[,c("f.ind","Mom")]
names(pedigree)<- c("id", "dam")
pedigree$sire<- "NA"
pedigree<- prepPed(pedigree)
Ainv <- makeAinv(pedigree)$Ainv

gr_model <- gremlin(cbind(RGR11, RGR17, RGR23, RGR29, RGR35) ~ pupal_massmg,
                    random = ~ idv,
                    data = df,
                    ginverse = list(idv = Ainv),
                    Gstart = diag(5) * 0.1,  # Starting values for G matrix
                    Rstart = diag(5) * 0.5)  # Starting values for R matrix

G_matrix <- gr_model$G
P_matrix <- gr_model$P

#--------------

library(evolqg)

old <- options(contrasts=c("contr.sum","contr.poly"))

#tpc.lm = lm(as.matrix(tpc[,c(5,6,3,4,7)])~as.factor(tpc[,"FEMALE"]))

# cov.matrix <- CalculateMatrix(tpc.lm)
# 
# options(old)
# #To obtain a correlation matrix, use:
# cor.matrix <- cov2cor(cov.matrix)
# 
# cor.matrix <- cov2cor(cov.matrix)
# 
# cor.matrix.m <- melt(cor.matrix)
# plot.cov2= ggplot(data = cor.matrix.m, aes(x=Var1, y=Var2, fill=value)) + 
#   geom_tile()+scale_fill_viridis()
# 
# #tutorial
# #  https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2656.2009.01639.x

#--------------
library(MCMCglmm)
#install.packages("pedigree")
library(pedigree)

df<- tpc.sel[which(tpc.sel$period=="past"),-which(colnames(tpc.sel)%in% c("FecEggCount"))]
df<- na.omit(df)
names(df)[c(1)]<- c("mother")
#make new id
df$id<- 1: nrow(df)

#make pedigree matrix
pedigree<- df[,c("id", "mother")]
pedigree$father<- "NA"
ped_matrix <- orderPed(pedigree)

#inverse_A <- inverseA(pedigree = ped_matrix)$Ainv

# Set up the prior
trait_prior <- list(
  R = list(V = diag(5), nu = 0.002, fix = 5),  # Fix residual variance for fitness
  G = list(
    G1 = list(V = diag(5)*0.02, nu = 5,    # Individual genetic effects
              alpha.mu = rep(0,5), alpha.V = diag(25^2,5,5)),
    G2 = list(V = diag(5)*0.02, nu = 5,    # Maternal effects
              alpha.mu = rep(0,5), alpha.V = diag(25^2,5,5))
  )
)

# Model for traits
trait_model <- MCMCglmm(
  cbind(RGR11, RGR17, RGR23, RGR29, RGR35) ~ trait - 1 + 
    at.level(trait, 1:5):mean,
  random = ~ us(trait):id + us(trait):mother,
  rcov = ~ us(trait):units,
  family = rep("gaussian", 5),
  prior = trait_prior,
  pedigree = pedigree,
  data = df,
  nitt = 130000,
  burnin = 30000,
  thin = 100
)

# Extract G matrix (additive genetic variance-covariance matrix)
G_matrix <- matrix(colMeans(model$VCV[,grep("animal", colnames(model$VCV))]), 6, 6)

# Extract P matrix (phenotypic variance-covariance matrix)
P_matrix <- matrix(colMeans(model$VCV), 6, 6)

#or this?
# Genetic (G) matrix (6x6)
G_matrix <- apply(model$VCV[, grep("individual", colnames(model$VCV))], 
                  2, posterior.mode)
G_matrix <- matrix(G_matrix, nrow=6)

# Phenotypic (P) matrix = G + residual
P_matrix <- G_matrix + apply(model$VCV[, grep("units", colnames(model$VCV))], 
                             2, posterior.mode)
P_matrix <- matrix(P_matrix, nrow=6)

#----
#https://rdrr.io/cran/evolqg/man/RandomSkewers.html
# Compare G and P matrices using Random Skewers
matrix_similarity <- RandomSkewers(G_matrix, P_matrix)
# Decompose G-matrix to identify major axes of variation
srd_results <- SRD(G_matrix, P_matrix)

#Mantel Test: Tests matrix element correlations while accounting for trait dependencies:
mantel_results <- MantelCor(G_matrix, P_matrix)

#Krzanowski's Comparison: Compares subspaces spanned by matrices' eigenvectors:
krz_results <- KrzCor(G_matrix, P_matrix)

#PCA Similarity: Quantifies overlap in major axes of variation:
pca_sim <- PCASimilarity(G_matrix, P_matrix)

#Bayesian Framework: Compare posterior distributions of matrices:
bayes_compare <- BayesianMatrixCompare(G_samples, P_samples)

#--------------------
## Variance covariance analysis
library(sommer)

#Past
# Estimate G matrix
df<- tpc.sel[which(tpc.sel$period=="past"),-which(colnames(tpc.sel)%in% c("FecEggCount"))]
df<- na.omit(df)
# Model specification
model <- mmer(
  cbind(RGR11,RGR17,RGR23,RGR29,RGR35)~1,
  random = ~ vsr(isc(f.ind), Gtc = unsm(5)) +   # Genetic effects
    vsr(isc(Mom), Gtc = diag(5)),
  data = df
)

# Genetic variance-covariance matrix (G)
G.h <- model$sigma$`u:f.ind`

#---
#Estimate P matrix by not accounting for family
# Model specification
model <- mmer(
  cbind(RGR11,RGR17,RGR23,RGR29,RGR35)~1,
  random = ~ vsr(isc(f.ind), Gtc = unsm(5)),
  data = df
)

# Genetic variance-covariance matrix (G)
P.h <- model$sigma$`u:f.ind`

#------------
#recent
# Estimate G matrix
df<- tpc.sel[which(tpc.sel$period=="recent"),-which(colnames(tpc.sel)%in% c("ID","FecEggCount"))]
df<- na.omit(df)
# Model specification
model <- mmer(
  cbind(RGR11,RGR17,RGR23,RGR29,RGR35)~1,
  random = ~ vsr(isc(f.ind), Gtc = unsm(5)) +   # Genetic effects
    vsr(isc(Mom), Gtc = diag(5)),
  data = df
)

# Genetic variance-covariance matrix (G)
G <- model$sigma$`u:f.ind`

#---
#Estimate P matrix by not accounting for family
# Model specification
model <- mmer(
  cbind(RGR11,RGR17,RGR23,RGR29,RGR35)~1,
  random = ~ vsr(isc(f.ind), Gtc = unsm(5)),
  data = df
)

# Genetic variance-covariance matrix (G)
P <- model$sigma$`u:f.ind`

#---
#melt matrices
gm.h <- melt(G.h*10^6)
pm.h <- melt(P.h*10^6)
gm <- melt(G*10^6)
pm <- melt(P*10^6)

#Combine matrices
gm.h$type<- "G"; gm.h$time<- "past"
pm.h$type<- "P"; pm.h$time<- "past"
gm$type<- "G"; gm$time<- "recent"
pm$type<- "P"; pm$time<- "recent"

var.all<- rbind(gm.h, pm.h, gm, pm)

#----
# Variance Covariance plot

plot.var= ggplot(data = var.all, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  facet_grid(type~time)+
  scale_fill_viridis()
#scale_fill_gradient2(low ="orange", high = "blue", space = "Lab")

#save figure 
if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/figures/")
if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/PrapaeGardenExpt/figures/")

pdf("PrapaeTPC_cov_REML.pdf",height = 10, width = 10)
plot.var
dev.off()

#----
#compare through time
tpc.sel$mom_per <- paste(tpc.sel$Mom, tpc.sel$period, sep="_")

df<- tpc.sel[,-which(colnames(tpc.sel)%in% c("ID","FecEggCount"))]
df<- na.omit(df)
# Model specification
model <- mmer(
  cbind(RGR11,RGR17,RGR23,RGR29,RGR35)~1,
  random = ~ vsr(isc(f.ind), Gtc = unsm(5)) +   # Genetic effects
    vsr(isc(mom_per), Gtc = diag(5))+
    vsr(isc(period), Gtc = diag(5)),
  data = df
)

#library(evolqg)
# #tutorial
# #  https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2656.2009.01639.x

CalculateMatrix
