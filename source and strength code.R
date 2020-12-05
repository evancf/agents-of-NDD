# load packages ----------------------------------------------------------------

library("lme4")

# read in data -----------------------------------------------------------------

pr.dat <- read.csv("pr.dat.csv") # Premna serratifolia
mo.dat <- read.csv("mo.dat.csv") # Morinda citrifolia
ag.dat <- read.csv("ag.dat.csv") # Aglaia mariannensis



# analysis ---------------------------------------------------------------------

# Analysis for germination (coded 1) and seedling survival stages (coded 2)
# Note that "f" indicates fungicide treatment, "i" indicates insecticide, 
# "r" indicates rodent exclosure, "a" indicates all treatments

# Premna
Y.pr1 <- cbind(pr.dat$seed_input-pr.dat$total_germinants,pr.dat$total_germinants)
mod.pr1 <- glmer(Y.pr1 ~ f*near_far + i*near_far + r*near_far + dens + (1|site), family=binomial, data=pr.dat)

Y.pr2 <- cbind(pr.dat$total_germinants-pr.dat$tot_sdlng_surv,pr.dat$tot_sdlng_surv)
mod.pr2 <- glmer(Y.pr2 ~ f*near_far + i*near_far + r*near_far + dens + (1|site), family=binomial, data=pr.dat)


# Aglaia
Y.ag1 <- cbind(ag.dat$seed_input-ag.dat$total_germinants,ag.dat$total_germinants)
mod.ag1 <- glmer(Y.ag1 ~ f*near_far + i*near_far + r*near_far + dens + (1|site), family=binomial, data=ag.dat)

Y.ag2 <- cbind(ag.dat$total_germinants-ag.dat$tot_sdlng_surv,ag.dat$tot_sdlng_surv)
mod.ag2 <- glmer(Y.ag2 ~ f*near_far + i*near_far + r*near_far + dens + (1|site), family=binomial, data=ag.dat)


# Morinda
Y.mo1 <- cbind(mo.dat$seed_input-mo.dat$total_germinants,mo.dat$total_germinants)
mod.mo1 <- glmer(Y.mo1 ~ f*near_far + i*near_far + r*near_far + dens + (1|site), family=binomial, data=mo.dat)

Y.mo2 <- cbind(mo.dat$total_germinants-mo.dat$tot_sdlng_surv,mo.dat$tot_sdlng_surv)
mod.mo2 <- glmer(Y.mo2 ~ f*near_far + i*near_far + r*near_far + dens + (1|site), family=binomial, data=mo.dat)
