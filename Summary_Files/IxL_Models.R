##### Island x Lineage GLMM testing #####
library(lme4)

dat <- read.csv("Summary_Files/IxL.csv")

# Scale the columns that should be scaled
dat_scale <- scale(dat[,2:13])
dat_scale <- cbind(dat$name, dat_scale, dat$lineage, dat$dispersal, dat$n_habitat, dat$archipelago)
dat_scale <- as.data.frame(dat_scale)
colnames(dat_scale)[1] <- "name"
colnames(dat_scale)[14] <- "lineage"
colnames(dat_scale)[15] <- "dispersal"
colnames(dat_scale)[16] <- "n_habitat"
colnames(dat_scale)[17] <- "archipelago"

# Make dispersal (but not n_habitat now?) factor
dat_scale$dispersal <- as.factor(dat_scale$dispersal)
dat_scale$n_habitat <- as.factor(dat_scale$n_habitat)

# They're all characters for some reason
dat_scale[,2:13] <- sapply(dat_scale[,2:13], as.numeric)
dat_scale$n_habitat <- as.numeric(dat_scale$n_habitat)

# I don't think we want to scale richness though...
dat_scale$richness <- dat$richness
# Don't scale presence
dat_scale$presence <- dat$presence

# Use the random effect of (1 | archipelago / lineage) because the lineages
#  are nested within the archipelagos
# Exclude nearest_occ because Caribbean taxa are the only ones that aren't
#  zero-inflated
model1 <- glmer(richness ~ TRI + mean_elev + median_elev + min_elev + max_elev + Nearest_Dist + mean_csi + sd_csi + dispersal + n_habitat + (1 | archipelago / lineage), 
                data = dat_scale, family = poisson)

summary(model1)
# Singular
# Significant variables: mean_elev, min_elev, max_elev, mean_csi, 
#  dispersallow, dispersalmoderate, n_habitat33, n_habitat38

# Check for overdispersion?
# https://easystats.github.io/performance/reference/check_overdispersion.html
library(performance)
check_overdispersion(model1)
# Overdispersion detected: dispersion ratio = 2.512

# There might not be enough data to use (1 | archipelago / lineage), so use only (1 | archipelago)?
model2 <- glmer(richness ~ TRI + mean_elev + median_elev + min_elev + max_elev + Nearest_Dist + mean_csi + sd_csi + dispersal + n_habitat + (1 | archipelago), 
                data = dat_scale, family = poisson)
summary(model2)
# Still singular
# Same variables are significant

check_overdispersion(model2)
# Overdispersion detected

# Wait what about this from https://cran.r-project.org/web/packages/glmmTMB/vignettes/glmmTMB.pdf
# For example, the formula would be 1|block for a random-intercept model or
# time|block for a model with random variation in slopes through time
# across groups specified by block. A model of nested random effects
# (block within site) could be 1|site/block if block labels are reused
# across multiple sites, or (1|site) + (1|block) if the nesting structure
# is explicit in the data and each level of block only occurs within one
# site. A model of crossed random effects (block and year) would be
# (1|block)+(1|year).

# Since each lineage (block) only occurs within one archipelago (site), should
# it be (1 | archipelago) + (1 | lineage)?

model3 <- glmer(richness ~ TRI + mean_elev + median_elev + min_elev + max_elev + Nearest_Dist + mean_csi + sd_csi + dispersal + n_habitat + (1 | archipelago) + (1 | lineage), 
                data = dat_scale, family = poisson)
summary(model3)
check_overdispersion(model3)
# It's the same result, nevermind


##### Try a negative binomial model instead #####
model4 <- glmer.nb(richness ~ TRI + mean_elev + median_elev + min_elev + max_elev + Nearest_Dist + mean_csi + sd_csi + dispersal + n_habitat + (1 | archipelago / lineage), data = dat_scale)
summary(model4)
# Singular
# Significant: max_elev, Nearest_Dist, sd_csi, n_habitat29

##### Try it all again, but simplify the model to be binomial #####
modelb <- glmer(presence ~ TRI + mean_elev + median_elev + min_elev + max_elev + Nearest_Dist + mean_csi + sd_csi + dispersal + n_habitat + (1 | archipelago / lineage), 
                data = dat_scale, family = binomial)
summary(modelb)
# Singular
# Significant: mean_elev, median_elev, min_elev, Nearest_Dist, sd_csi

# Not just binomial, but include the logit link?
model_b2 <- glmer(presence ~ TRI + mean_elev + median_elev + min_elev + max_elev + Nearest_Dist + mean_csi + sd_csi + dispersal + n_habitat + (1 | archipelago / lineage), 
                  data = dat_scale, family = binomial(link = "logit"))
summary(model_b2)
# Singular
# Significant: mean_elev, median_elev, min_elev, Nearest_Dist, sd_csi

check_overdispersion(model_b2)
# No overdispersion this time, at least

# Maybe it wouldn't be singular if archipelago was the only random effect?
model_b3 <- glmer(presence ~ TRI + mean_elev + median_elev + min_elev + max_elev + Nearest_Dist + mean_csi + sd_csi + dispersal + n_habitat + (1 | archipelago), 
                  data = dat_scale, family = binomial(link = "logit"))
summary(model_b3)
# Nevermind, still singular

# Run the binomial(link = "logit") model with variables removed due to correlation?
cor_matrix_scale <- cor(dat_scale[, c("TRI", "mean_elev", "median_elev", 
                                      "min_elev", "max_elev", "Nearest_Dist", 
                                      "mean_csi", "sd_csi", "area")])
# mean_elev: median_elev, max_elev
# median_elev: mean_elev, max_elev
# min_elev: area is 0.67
# max_elev: mean_elev, median_elev
# Maybe I should remove median_elev and mean_elev

# Not just binomial, but include the logit link?
model_b4 <- glmer(presence ~ TRI + min_elev + max_elev + Nearest_Dist + mean_csi + sd_csi + dispersal + n_habitat + area + (1 | archipelago / lineage), 
                  data = dat_scale, family = binomial(link = "logit"))
summary(model_b4)
# Singular
# Significant: max_elev, Nearest_Dist, n_habitat38

check_overdispersion(model_b4)

# Maybe just keep max_elev and remove min_elev?
model_b5 <- glmer(presence ~ TRI + max_elev + Nearest_Dist + mean_csi + sd_csi + dispersal + area + (1 | archipelago), 
                  data = dat_scale, family = binomial(link = "logit"))
summary(model_b5)
# Still singular, but a couple of new significant terms (sd_csi, area)

# Try a stepwise regression procedure to see if one variable is causing the singularity issue?
model_t1 <- glmer(presence ~ dispersal + (1 | archipelago),
                  data = dat_scale, family = binomial)
# If you do it like this, everything is Singular

# As discussed for model2, there might not be enough data to use 
#  (1 | archipelago / lineage), so use only (1 | archipelago) here too?

##### THIS ONE #####
dat_scale$presence <- as.factor(dat_scale$presence)

model_b6 <- glmer(presence ~ TRI + max_elev + Nearest_Dist + mean_csi + sd_csi + dispersal + n_habitat + area + (1 | archipelago), data = dat_scale, family = binomial) # (link = "logit")
summary(model_b6)

# model_b6 didn't converge...
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
# Check for Singularity
isSingular(model_b6) # FALSE!

# Restart from a previous fit, with more iterations
ss <- getME(model_b6, c("theta","fixef"))
model_b6a <- update(model_b6, start=ss, control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(model_b6a)
# Significant: Nearest_Dist, mean_csi, n_habitat

# Get pseudo R-squared 
library(MuMIn)
r.squaredGLMM(model_b6a)
# R2m       R2c
# theoretical 0.8461498 0.8697797
# delta       0.8158991 0.8386842

# Logit link
model_b7 <- glmer(presence ~ TRI + max_elev + Nearest_Dist + mean_csi + sd_csi + dispersal + n_habitat + area + (1 | archipelago), data = dat_scale, family = binomial(link = "logit"))

# Restart from a previous fit, with more iterations
ss <- getME(model_b7, c("theta","fixef"))
model_b7a <- update(model_b7, start=ss, control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(model_b7a)
# Significant: Nearest_Dist, mean_csi, n_habitat

# Would including archipelago as a fixed effect make the model do what I think it should?
model_b7 <- glm(presence ~ TRI + max_elev + Nearest_Dist + mean_csi + sd_csi + dispersal + n_habitat + area + archipelago, data = dat_scale, family = binomial)
summary(model_b7)
# Warning message:
#   glm.fit: fitted probabilities numerically 0 or 1 occurred 

# It appears that there is no variation between the random effect groups,
#  regardless of what variables are used in the model

# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html

# Too few groups, so maybe the random effect needs to be an interaction term instead?
model_b8 <- glmer(presence ~ TRI + max_elev + Nearest_Dist + mean_csi + sd_csi + dispersal + n_habitat + area + (1 | archipelago:lineage), 
                              data = dat_scale, family = binomial(link = "logit"))
summary(model_b8)
# Still Singular

##### Try brms #####
# https://paulbuerkner.com/brms/
library(brms)

# Try poisson again
model_bay1 <- brm(richness ~ TRI + max_elev + Nearest_Dist + mean_csi + sd_csi + dispersal + n_habitat + area + (1 | archipelago:lineage),
  data = dat_scale, family = poisson())
summary(model_bay1)
# Parts of the model have not converged (some Rhats are > 1.05). Be careful when analysing the results! We recommend running more iterations and/or setting stronger priors. 