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

# Make dispersal and n_habitat factors
dat_scale$dispersal <- as.factor(dat_scale$dispersal)
dat_scale$n_habitat <- as.factor(dat_scale$n_habitat)

# They're all characters for some reason
dat_scale[,2:13] <- sapply(dat_scale[,2:13], as.numeric)

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
cor(dat_scale[, c("TRI", "mean_elev", "median_elev", "min_elev", "max_elev", "Nearest_Dist", "mean_csi", "sd_csi", "area")])
# Maybe I should remove median_elev and mean_elev

# Not just binomial, but include the logit link?
model_b4 <- glmer(presence ~ TRI + min_elev + max_elev + Nearest_Dist + mean_csi + sd_csi + dispersal + n_habitat + area + (1 | archipelago / lineage), 
                  data = dat_scale, family = binomial(link = "logit"))
summary(model_b4)
# Singular
# Significant: max_elev, Nearest_Dist, n_habitat38

check_overdispersion(model_b4)

# Try a stepwise regression procedure to see if a variable is causing the singularity issue?


