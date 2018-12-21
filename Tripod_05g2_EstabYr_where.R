min_estab_yr_individ <- tripod %>%
  filter(! spp == "SEEDLING") %>%
  group_by(., site, spp) %>%
  summarize_at(c("estab_yr"),
               funs(min(., na.rm=TRUE)))

# For figure and comparison purposes, drop any NONEs and any Inf (i.e., don't have yr recorded)
drops <- c("NONE", "UNKN")
min_estab_yr_individ <-min_estab_yr_individ[! min_estab_yr_individ$spp %in% drops,]
drops <- Inf
min_estab_yr_individ <-min_estab_yr_individ[! min_estab_yr_individ$estab_yr %in% drops,]
min_estab_yr_individ$min_estab_yr <- min_estab_yr_individ$estab_yr
min_estab_yr_individ$estab_yr <- NULL

###################### ABLA ###################### 
estab_min <- min_estab_yr_individ %>%
  filter(spp == "ABLA") %>%
  dplyr::select(-spp)

# Pull out topo and spp-specific predictors
data <- abla_sum_all %>%
  dplyr::select(site, dem100, slp100,  hli100, tpi100, cti100, shrub_perc, abla_dist_bin) %>%
  rename(dist_bin = abla_dist_bin)
data$shrub_perc <- (data$shrub_perc/100)
snowfree <- snowfree %>% dplyr::select(site, snowfree)
data <- data %>%
  left_join(snowfree, by = "site")

# Join height summary to these predictors
data <- data %>%
  left_join(estab_min, by = "site")

# Scale the predictor variables 
data.raw <- data # for safe-keeping
colnames(data.raw)
var.scale <- scale(data[, c(2, 3, 4, 5, 6, 7, 9)]) # centers and divides by SD
data <- data.frame(var.scale, site=data.raw$site,
                   dist_bin=data.raw$dist_bin,
                   min_estab_yr=data.raw$min_estab_yr)
colnames(data)

# Run glm to see what predicts min estab yr.
model <- glm(min_estab_yr ~ dem100 + slp100 + hli100 + tpi100 + cti100
             + shrub_perc + snowfree + dist_bin, data = data, maxit = 100)
summary(model)

# 49 observations deleted due to missingness


###################### LAOC ###################### 
estab_min <- min_estab_yr_individ %>%
  filter(spp == "LAOC") %>%
  dplyr::select(-spp)

# Pull out topo and spp-specific predictors
data <- laoc_sum_all %>%
  dplyr::select(site, dem100, slp100,  hli100, tpi100, cti100, shrub_perc, laoc_dist_bin) %>%
  rename(dist_bin = laoc_dist_bin)
data$shrub_perc <- (data$shrub_perc/100)
snowfree <- snowfree %>% dplyr::select(site, snowfree)
data <- data %>%
  left_join(snowfree, by = "site")

# Join height summary to these predictors
data <- data %>%
  left_join(estab_min, by = "site")

# Scale the predictor variables 
data.raw <- data # for safe-keeping
colnames(data.raw)
var.scale <- scale(data[, c(2, 3, 4, 5, 6, 7, 9)]) # centers and divides by SD
data <- data.frame(var.scale, site=data.raw$site,
                   dist_bin=data.raw$dist_bin,
                   min_estab_yr=data.raw$min_estab_yr)
colnames(data)

# Run glm to see what predicts min estab yr.
model <- glm(min_estab_yr ~ dem100 + slp100 + hli100 + tpi100 + cti100
             + shrub_perc + snowfree + dist_bin, data = data, maxit = 100)
summary(model)

# 45 deleted due to missingness




###################### PICO ###################### 
estab_min <- min_estab_yr_individ %>%
  filter(spp == "PICO") %>%
  dplyr::select(-spp)

# Pull out topo and spp-specific predictors
data <- pico_sum_all %>%
  dplyr::select(site, dem100, slp100,  hli100, tpi100, cti100, shrub_perc, site_air_seed, pico_dist_bin) %>%
  rename(air_seed=site_air_seed, dist_bin = pico_dist_bin)
data$shrub_perc <- (data$shrub_perc/100)
snowfree <- snowfree %>% dplyr::select(site, snowfree)
data <- data %>%
  left_join(snowfree, by = "site")

# Join height summary to these predictors
data <- data %>%
  left_join(estab_min, by = "site")

# Scale the predictor variables 
data.raw <- data # for safe-keeping
colnames(data.raw)
var.scale <- scale(data[, c(2, 3, 4, 5, 6, 7, 10)]) # centers and divides by SD
data <- data.frame(var.scale, site=data.raw$site,
                   min_estab_yr=data.raw$min_estab_yr,
                   dist_bin=data.raw$dist_bin,
                   air_seed=data.raw$air_seed) # add site, ttl_ht, air back on
colnames(data)

data <- na.omit(data)

# Run glm to see what predicts min estab yr.
model <- glm(min_estab_yr ~ dem100 + slp100 + hli100 + tpi100 + cti100
             + shrub_perc + snowfree + dist_bin + air_seed, data = data, maxit = 100)
summary(model)
model.step <- stepAIC(model, direction="backward")
summary(model.step)

# Call:
#   glm(formula = min_estab_yr ~ dem100 + air_seed, data = data, 
#       maxit = 100)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.8557  -0.9517  -0.1950   0.4929   3.2166  
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) 2008.1244     0.3330 6030.647  < 2e-16 ***
#   dem100         0.5949     0.2380    2.500  0.01742 *  
#   air_seed      -1.3718     0.4753   -2.886  0.00673 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for gaussian family taken to be 1.73445)
# 
# Null deviance: 77.243  on 36  degrees of freedom
# Residual deviance: 58.971  on 34  degrees of freedom
# AIC: 130.25
# 
# Number of Fisher Scoring iterations: 2


# Run glm to see what predicts min estab yr w/o snowfree
model <- glm(min_estab_yr ~ dem100 + slp100 + hli100 + tpi100 + cti100
             + shrub_perc + dist_bin + air_seed, data = data, maxit = 100)
summary(model)
model.step <- stepAIC(model, direction="backward")
summary(model.step)

# Call:
#   glm(formula = min_estab_yr ~ dem100 + air_seed, data = data, 
#       maxit = 100)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.8557  -0.9517  -0.1950   0.4929   3.2166  
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) 2008.1244     0.3330 6030.647  < 2e-16 ***
#   dem100         0.5949     0.2380    2.500  0.01742 *  
#   air_seed      -1.3718     0.4753   -2.886  0.00673 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for gaussian family taken to be 1.73445)
# 
# Null deviance: 77.243  on 36  degrees of freedom
# Residual deviance: 58.971  on 34  degrees of freedom
# AIC: 130.25
# 
# Number of Fisher Scoring iterations: 2

###################### PIEN ###################### 
estab_min <- min_estab_yr_individ %>%
  filter(spp == "PIEN") %>%
  dplyr::select(-spp)

# Pull out topo and spp-specific predictors
data <- pien_sum_all %>%
  dplyr::select(site, dem100, slp100,  hli100, tpi100, cti100, shrub_perc, pien_dist_bin) %>%
  rename(dist_bin = pien_dist_bin)
data$shrub_perc <- (data$shrub_perc/100)
snowfree <- snowfree %>% dplyr::select(site, snowfree)
data <- data %>%
  left_join(snowfree, by = "site")

# Join height summary to these predictors
data <- data %>%
  left_join(estab_min, by = "site")

# Scale the predictor variables 
data.raw <- data # for safe-keeping
colnames(data.raw)
var.scale <- scale(data[, c(2, 3, 4, 5, 6, 7, 9)]) # centers and divides by SD
data <- data.frame(var.scale, site=data.raw$site,
                   dist_bin=data.raw$dist_bin,
                   min_estab_yr=data.raw$min_estab_yr)
colnames(data)

# Run glm to see what predicts min estab yr.
model <- glm(min_estab_yr ~ dem100 + slp100 + hli100 + tpi100 + cti100
             + shrub_perc + snowfree + dist_bin, data = data, maxit = 100)
summary(model)

# 42 deleted due to missingness


###################### PIPO ###################### 
estab_min <- min_estab_yr_individ %>%
  filter(spp == "PIPO") %>%
  dplyr::select(-spp)

# Pull out topo and spp-specific predictors
data <- pipo_sum_all %>%
  dplyr::select(site, dem100, slp100,  hli100, tpi100, cti100, shrub_perc, pipo_dist_bin) %>%
  rename(dist_bin = pipo_dist_bin)
data$shrub_perc <- (data$shrub_perc/100)
snowfree <- snowfree %>% dplyr::select(site, snowfree)
data <- data %>%
  left_join(snowfree, by = "site")

# Join height summary to these predictors
data <- data %>%
  left_join(estab_min, by = "site")

# Scale the predictor variables 
data.raw <- data # for safe-keeping
colnames(data.raw)
var.scale <- scale(data[, c(2, 3, 4, 5, 6, 7, 9)]) # centers and divides by SD
data <- data.frame(var.scale, site=data.raw$site,
                   dist_bin=data.raw$dist_bin,
                   min_estab_yr=data.raw$min_estab_yr)
colnames(data)

data <- na.omit(data)

# Run glm to see what predicts min estab yr.
model <- glm(min_estab_yr ~ dem100 + slp100 + hli100 + tpi100 + cti100
             + shrub_perc + snowfree + dist_bin, data = data, maxit = 100)
summary(model)
model.step <- stepAIC(model, direction="backward")
summary(model.step)

# Call:
#   glm(formula = min_estab_yr ~ dem100 + tpi100 + cti100 + snowfree + 
#         dist_bin, data = data, maxit = 100)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.1041  -0.7906  -0.3128   0.8291   2.2764  
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) 2012.1606     1.2618 1594.657   <2e-16 ***
#   dem100         1.6237     0.7448    2.180   0.0468 *  
#   tpi100        -1.3085     0.4967   -2.634   0.0196 *  
#   cti100        -0.9964     0.4122   -2.417   0.0299 *  
#   snowfree      -0.9410     0.4968   -1.894   0.0791 .  
# dist_bin      -0.4345     0.1794   -2.421   0.0296 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for gaussian family taken to be 1.982187)
# 
# Null deviance: 48.800  on 19  degrees of freedom
# Residual deviance: 27.751  on 14  degrees of freedom
# AIC: 77.308
# 
# Number of Fisher Scoring iterations: 2

# Run glm to see what predicts min estab yr w/o snowfree
model <- glm(min_estab_yr ~ dem100 + slp100 + hli100 + tpi100 + cti100
             + shrub_perc + dist_bin, data = data, maxit = 100)
summary(model)
model.step <- stepAIC(model, direction="backward")
summary(model.step)

# Call:
#   glm(formula = min_estab_yr ~ dem100 + tpi100 + cti100 + dist_bin, 
#       data = data, maxit = 100)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.8268  -0.9479  -0.0148   1.0535   3.2947  
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) 2011.1256     1.2315 1633.036   <2e-16 ***
#   dem100         1.5599     0.8056    1.936   0.0719 .  
# tpi100        -0.9447     0.4960   -1.905   0.0762 .  
# cti100        -0.7589     0.4252   -1.785   0.0945 .  
# dist_bin      -0.3347     0.1857   -1.802   0.0917 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for gaussian family taken to be 2.324064)
# 
# Null deviance: 48.800  on 19  degrees of freedom
# Residual deviance: 34.861  on 15  degrees of freedom
# AIC: 79.87
# 
# Number of Fisher Scoring iterations: 2


###################### PSME ###################### 
estab_min <- min_estab_yr_individ %>%
  filter(spp == "PSME") %>%
  dplyr::select(-spp)

# Pull out topo and spp-specific predictors
data <- psme_sum_all %>%
  dplyr::select(site, dem100, slp100,  hli100, tpi100, cti100, shrub_perc, psme_dist_bin) %>%
  rename(dist_bin = psme_dist_bin)
data$shrub_perc <- (data$shrub_perc/100)
snowfree <- snowfree %>% dplyr::select(site, snowfree)
data <- data %>%
  left_join(snowfree, by = "site")

# Join height summary to these predictors
data <- data %>%
  left_join(estab_min, by = "site")

# Scale the predictor variables 
data.raw <- data # for safe-keeping
colnames(data.raw)
var.scale <- scale(data[, c(2, 3, 4, 5, 6, 7, 9)]) # centers and divides by SD
data <- data.frame(var.scale, site=data.raw$site,
                   dist_bin=data.raw$dist_bin,
                   min_estab_yr=data.raw$min_estab_yr)
colnames(data)

data <- na.omit(data)

# Run glm to see what predicts min estab yr.
model <- glm(min_estab_yr ~ dem100 + slp100 + hli100 + tpi100 + cti100
             + shrub_perc + snowfree + dist_bin, data = data, maxit = 100)
summary(model)
model.step <- stepAIC(model, direction="backward")
summary(model.step)

# Call:
#   glm(formula = min_estab_yr ~ dem100 + tpi100 + snowfree, data = data, 
#       maxit = 100)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.3416  -1.2383  -0.2871   1.0915   3.4439  
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) 2010.0781     0.3700 5433.040   <2e-16 ***
#   dem100         0.9582     0.4844    1.978   0.0612 .  
# tpi100         0.7243     0.3099    2.337   0.0294 *  
#   snowfree       0.8372     0.4999    1.675   0.1088    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for gaussian family taken to be 2.710829)
# 
# Null deviance: 91.040  on 24  degrees of freedom
# Residual deviance: 56.927  on 21  degrees of freedom
# AIC: 101.52
# 
# Number of Fisher Scoring iterations: 2

# Run glm to see what predicts min estab yr w/o snowfree
model <- glm(min_estab_yr ~ dem100 + slp100 + hli100 + tpi100 + cti100
             + shrub_perc + dist_bin, data = data, maxit = 100)
summary(model)
model.step <- stepAIC(model, direction="backward")
summary(model.step)


# Call:
#   glm(formula = min_estab_yr ~ hli100 + tpi100, data = data, maxit = 100)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.4627  -1.1950   0.1072   0.8277   3.2974  
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) 2009.8972     0.3481 5774.016  < 2e-16 ***
#   hli100         0.4090     0.2894    1.413  0.17153    
# tpi100         0.8642     0.2992    2.888  0.00853 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for gaussian family taken to be 2.83363)
# 
# Null deviance: 91.04  on 24  degrees of freedom
# Residual deviance: 62.34  on 22  degrees of freedom
# AIC: 101.79
# 
# Number of Fisher Scoring iterations: 2