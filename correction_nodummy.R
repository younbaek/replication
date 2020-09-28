# Result for column (5) in Baek(2020)
setwd("C:/Users/pc/Desktop/mebane");
library(eforensics);
# Korea 2020
dat <- read.csv("pagg_democ.csv");
names(dat);
dim(dat);
dat$NAbst <- dat$NVoters-dat$NValid;
kidx <- !is.na(dat$NVoters) & (dat$NVoters >= dat$NValid) &
  (dat$NValid > 0) & (dat$NValid >= dat$Votes);
if (any(is.na(kidx))) kidx[is.na(kidx)] <- FALSE;
table(kidx);
vnames <- c("uid","name","district","NVoters","NValid","Votes");
dat[!kidx,vnames];
tab <- table(dat[!kidx,"name"]);
tab[tab>0];
dat <- dat[kidx,];
dim(dat);
dat$isprevote <- dat$isprevote
table(dat$isprevote);
table(dat$isabroad <- dat$name=="abroad");
dat$district <- factor(dat$district);
dat$constit <- factor(dat$constit);
## mcmc parameters
## ---------------
mcmc    = list(burn.in=5000, n.adapt=1000, n.iter=1000, n.chains=4)
efout <- eforensics(
  Votes ~ 1 + isabroad + constit,
  NAbst ~ 1 + isabroad + constit,  data=dat,
  eligible.voters="NVoters",
  model="qbl",  mcmc=mcmc,
  parameters = "all",  parComp = TRUE,  autoConv = TRUE,  max.auto = 10,
  mcmc.conv.diagnostic = "MCMCSE",
  mcmc.conv.parameters = c("pi"),  mcmcse.conv.precision = .05,  mcmcse.combine = FALSE
)
save(efout,file="pagg_democ_nodummy_result.RData");
summary(efout);
#Look at the estimated fraud proportions for each observation
#attr(efout,"frauds")

rm(list = ls())

# Result for column (10) in Baek(2020)
# eforensics model applied to various datasets
setwd("C:/Users/pc/Desktop/mebane");
library(eforensics);
set.seed(0)
# Korea 2020
dat <- read.csv("pagg_const.csv");
names(dat);
dim(dat);
dat$NAbst <- dat$NVoters-dat$NValid;
kidx <- !is.na(dat$NVoters) & (dat$NVoters >= dat$NValid) &
  (dat$NValid > 0) & (dat$NValid >= dat$Votes);
if (any(is.na(kidx))) kidx[is.na(kidx)] <- FALSE;
table(kidx);
vnames <- c("uid","name","district","NVoters","NValid","Votes");
dat[!kidx,vnames];
tab <- table(dat[!kidx,"name"]);
tab[tab>0];
dat <- dat[kidx,];
dim(dat);
dat$isprevote <- dat$isprevote
table(dat$isprevote);
table(dat$isabroad <- dat$name=="abroad");
dat$district <- factor(dat$district);
dat$constit <- factor(dat$constit);
## mcmc parameters
## ---------------
mcmc    = list(burn.in=5000, n.adapt=1000, n.iter=1000, n.chains=4)
efout <- eforensics(
  Votes ~ 1 + isabroad + constit,
  NAbst ~ 1 + isabroad + constit,  data=dat,
  eligible.voters="NVoters",
  model="qbl",  mcmc=mcmc,
  parameters = "all",  parComp = TRUE,  autoConv = TRUE,  max.auto = 10,
  mcmc.conv.diagnostic = "MCMCSE",
  mcmc.conv.parameters = c("pi"),  mcmcse.conv.precision = .05,  mcmcse.combine = FALSE
)
save(efout,file="pagg_const_nodummy_result.RData");
#summary(efout);
#Look at the estimated fraud proportions for each observation
#attr(efout,"frauds")