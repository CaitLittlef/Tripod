## Does PICO HEIGHT VARY IN SPACE?

# Grab max height
height <- tripod %>%
  filter(! spp == "SEEDLING", spp == "PICO", ! is.na(ttl_ht)) %>%
  group_by(., site) %>%
  summarize_at(c("ttl_ht"),
               funs(max(., na.rm=TRUE)))

# Pull out topo and pico-specific predictors
data <- pico_sum_all %>%
  dplyr::select(site, dem100, slp100,  hli100, tpi100, cti100, shrub_perc, tpha, site_air_seed, pico_dist_bin)
data$shrub_perc <- (data$shrub_perc/100)

# Join height summary to these predictors
data <- data %>%
  right_join(height, by = "site")

# Scale the predictor variables 
data.raw <- data # for safe-keeping
colnames(data.raw)
var.scale <- scale(data[, c(2, 3, 4, 5, 6, 7, 8, 10)]) # centers and divides by SD
data <- data.frame(var.scale, site=data.raw$site, ttl_ht=data.raw$ttl_ht, air_seed=data.raw$site_air_seed) # add site, ttl_ht, air back on

# Run glm to see what predicts maximum height.
model <- glm(ttl_ht ~ dem100 + slp100 + hli100 + tpi100 + cti100 + tpha + shrub_perc + pico_dist_bin + air_seed, data = data, maxit = 100)
summary(model)

# Call:
#   glm(formula = ttl_ht ~ dem100 + slp100 + hli100 + tpi100 + cti100 + 
#         tpha + shrub_perc + pico_dist_bin + air_seed, data = data, 
#       maxit = 100)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -127.04   -51.92     3.37    42.41   168.93  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    185.239     22.958   8.069 4.12e-09 ***
#   dem100         -44.261     17.622  -2.512   0.0174 *  
#   slp100         -31.442     18.579  -1.692   0.1006    
# hli100          12.936     14.979   0.864   0.3945    
# tpi100          15.256     17.300   0.882   0.3846    
# cti100           1.068     19.860   0.054   0.9574    
# tpha            13.078     14.112   0.927   0.3612    
# shrub_perc      -7.278     14.781  -0.492   0.6259    
# pico_dist_bin  -15.830     14.232  -1.112   0.2746    
# air_seed        44.768     31.428   1.424   0.1643    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for gaussian family taken to be 6553.483)
# 
# Null deviance: 314098  on 40  degrees of freedom
# Residual deviance: 203158  on 31  degrees of freedom
# AIC: 487.19
# 
# Number of Fisher Scoring iterations: 2


################################################################
## DOES PICO AGE VARY IN SPACE?

# Grab mean estab yr
estab_yr <- tripod %>%
  filter(! spp == "SEEDLING", spp == "PICO", ! is.na(estab_yr)) %>%
  group_by(., site) %>%
  summarize_at(c("estab_yr"),
               funs(mean(., na.rm=TRUE)))

# Pull out topo and pico-specific predictors
data <- pico_sum_all %>%
  dplyr::select(site, dem100, slp100,  hli100, tpi100, cti100, shrub_perc, tpha, site_air_seed, pico_dist_bin)
data$shrub_perc <- (data$shrub_perc/100)

# Join height summary to these predictors
data <- data %>%
  right_join(estab_yr, by = "site")

# Scale the predictor variables
data.raw <- data # for safe-keeping
colnames(data.raw)
var.scale <- scale(data[, c(2, 3, 4, 5, 6, 7, 8, 10)]) # centers and divides by SD
data <- data.frame(var.scale, site=data.raw$site, estab_yr=data.raw$estab_yr, air_seed=data.raw$site_air_seed) # add site, ttl_ht, air back on
head(data)

# Run glm to see what predicts mean estab yr.
model <- glm(estab_yr ~ dem100 + slp100 + hli100 + tpi100 + cti100 + tpha + shrub_perc + pico_dist_bin + air_seed, data = data, maxit = 100)
summary(model)

# Call:
#   glm(formula = estab_yr ~ dem100 + slp100 + hli100 + tpi100 + 
#         cti100 + tpha + shrub_perc + pico_dist_bin + air_seed, data = data, 
#       maxit = 100)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -2.38984  -0.63402   0.02272   0.61042   1.63057  
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)   2010.2670     0.2788 7211.423  < 2e-16 ***
#   dem100           0.9344     0.2140    4.367  0.00013 ***
#   slp100           0.5881     0.2256    2.607  0.01392 *  
#   hli100          -0.1687     0.1819   -0.928  0.36074    
# tpi100          -0.2071     0.2101   -0.986  0.33181    
# cti100           0.2202     0.2411    0.913  0.36823    
# tpha             0.1293     0.1713    0.755  0.45614    
# shrub_perc       0.1365     0.1795    0.761  0.45253    
# pico_dist_bin    0.1964     0.1728    1.136  0.26451    
# air_seed        -0.3204     0.3816   -0.840  0.40757    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for gaussian family taken to be 0.9662021)
# 
# Null deviance: 54.564  on 40  degrees of freedom
# Residual deviance: 29.952  on 31  degrees of freedom
# AIC: 125.48
# 
# Number of Fisher Scoring iterations: 2


################################################################
## DOES PICO AGE INFLUENCE MAX HEIGHT, TOO?

# Grab mean estab yr
estab_yr <- tripod %>%
  filter(! spp == "SEEDLING", spp == "PICO", ! is.na(estab_yr)) %>%
  group_by(., site) %>%
  summarize_at(c("estab_yr"),
               funs(mean(., na.rm=TRUE)))

# Grab max height
height <- tripod %>%
  filter(! spp == "SEEDLING", spp == "PICO", ! is.na(ttl_ht)) %>%
  group_by(., site) %>%
  summarize_at(c("ttl_ht"),
               funs(max(., na.rm=TRUE)))

# Pull out topo and pico-specific predictors
data <- pico_sum_all %>%
  dplyr::select(site, dem100, slp100,  hli100, tpi100, cti100, shrub_perc, tpha, site_air_seed, pico_dist_bin)
data$shrub_perc <- (data$shrub_perc/100)

# Join height summary and age summary to these predictors
data <- data %>%
  right_join(height, by = "site") %>%
  right_join(estab_yr, by = "site")

# Scale the predictor variables INCLUDING AGE
data.raw <- data # for safe-keeping
colnames(data.raw)
var.scale <- scale(data[, c(2, 3, 4, 5, 6, 7, 8, 10, 12)]) # centers and divides by SD
data <- data.frame(var.scale, site=data.raw$site, ttl_ht=data.raw$ttl_ht, air_seed=data.raw$site_air_seed) # add site, ttl_ht, air back on
colnames(data)

# Run glm to see what predicts maximum height.
model <- glm(ttl_ht ~ dem100 + slp100 + hli100 + tpi100 + cti100 + tpha + shrub_perc + pico_dist_bin + air_seed
             + estab_yr, data = data, maxit = 100)
summary(model)
model.step <- stepAIC(model, direction =  "backward")
summary(model.step)

# Call:
#   glm(formula = ttl_ht ~ dem100 + cti100 + tpha + air_seed + estab_yr, 
#       data = data, maxit = 100)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -70.869  -29.429   -1.529   38.849   79.003  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  198.622     11.425  17.385  < 2e-16 ***
#   dem100        18.150      8.267   2.195  0.03486 *  
#   cti100         9.808      6.758   1.451  0.15559    
# tpha          20.661      6.925   2.984  0.00516 ** 
#   air_seed      22.819     15.365   1.485  0.14644    
# estab_yr     -78.187      7.736 -10.106 6.42e-12 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for gaussian family taken to be 1752.968)
# 
# Null deviance: 314098  on 40  degrees of freedom
# Residual deviance:  61354  on 35  degrees of freedom
# AIC: 430.1
# 
# Number of Fisher Scoring iterations: 2


###PICO##########################################################
## DOES MAX ANNUAL GROWTH RATE VARY IN SPACE OR WITH AGE??
## TOO FEW FOR ABLA, PIEN, LAOC

# Grab mean estab yr
estab_yr <- tripod %>%
  filter(! spp == "SEEDLING", spp == "PICO", ! is.na(estab_yr)) %>%
  group_by(., site) %>%
  summarize_at(c("estab_yr"),
               funs(mean(., na.rm=TRUE)))

# Grab max grwth in any given yr at a site
max <- tripod %>%
  filter(!spp == "SEEDLING", spp == "PICO") %>%
  dplyr::select(site, spp, starts_with("grwth"), 
                -grwth_2016, -grwth_avg_Ngerm_N16, -grwth_avg_Ngerm_Y16) %>%
  gather(key = yr, value = grwth, -spp, -site) %>%
  filter(! is.na(grwth))
max <- max %>%
  dplyr::select(-spp) %>%
  group_by(site) %>%
  summarize(max = max(grwth))

# Pull out topo and pico-specific predictors
data <- pico_sum_all %>%
  dplyr::select(site, dem100, slp100,  hli100, tpi100, cti100, shrub_perc, tpha, site_air_seed, pico_dist_bin)
data$shrub_perc <- (data$shrub_perc/100)

# Join grwth summary and age summary to these predictors
data <- data %>%
  left_join(max, by = "site") %>%
  left_join(estab_yr, by = "site")

# Scale the predictor variables INCLUDING AGE
data.raw <- data # for safe-keeping
colnames(data.raw)
var.scale <- scale(data[, c(2, 3, 4, 5, 6, 7, 8, 10, 12)]) # centers and divides by SD
data <- data.frame(var.scale, site=data.raw$site,
                   max=data.raw$max,
                   air_seed=data.raw$site_air_seed) # add site, ttl_ht, air back on
colnames(data)

# Run glm to see what predicts maximum grwth (Same as multiple linear regression lm)
model <- glm(max ~ dem100 + slp100 + hli100 + tpi100 + cti100 + tpha + shrub_perc + pico_dist_bin + air_seed
             + estab_yr, data = data, maxit = 100)
summary(model)
model.step <- stepAIC(model, direction =  "backward")
summary(model.step)

# Call:
#   glm(formula = max ~ dem100 + tpha + estab_yr, data = data, maxit = 100)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -19.000   -5.699   -0.655    5.926   15.957  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   40.832      1.401  29.147  < 2e-16 ***
#   dem100         5.422      1.585   3.421  0.00153 ** 
#   tpha           3.675      1.284   2.862  0.00690 ** 
#   estab_yr     -14.884      1.536  -9.692 1.07e-11 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for gaussian family taken to be 74.60889)
# 
# Null deviance: 10504.2  on 40  degrees of freedom
# Residual deviance:  2760.5  on 37  degrees of freedom
# (10 observations deleted due to missingness)
# AIC: 298.95
# 
# Number of Fisher Scoring iterations: 2





###PIPO##########################################################
## DOES MAX ANNUAL GROWTH RATE VARY IN SPACE OR WITH AGE??
# Grab mean estab yr
estab_yr <- tripod %>%
  filter(! spp == "SEEDLING", spp == "PIPO", ! is.na(estab_yr)) %>%
  group_by(., site) %>%
  summarize_at(c("estab_yr"),
               funs(mean(., na.rm=TRUE)))

# Grab max grwth in any given yr at a site
max <- tripod %>%
  filter(!spp == "SEEDLING", spp == "PIPO") %>%
  dplyr::select(site, spp, starts_with("grwth"),
                -grwth_2016, -grwth_avg_Ngerm_N16, -grwth_avg_Ngerm_Y16) %>%
  gather(key = yr, value = grwth, -spp, -site) %>%
  filter(! is.na(grwth))
max <- max %>%
  dplyr::select(-spp) %>%
  group_by(site) %>%
  summarize(max = max(grwth))


# Pull out topo and pipo-specific predictors
data <- pipo_sum_all %>%
  dplyr::select(site, dem100, slp100,  hli100, tpi100, cti100, shrub_perc, tpha, pipo_dist_bin)
data$shrub_perc <- (data$shrub_perc/100)

# Join grwth summary and age summary to these predictors
data <- data %>%
  left_join(max, by = "site") %>%
  left_join(estab_yr, by = "site")

# Scale the predictor variables INCLUDING AGE
data.raw <- data # for safe-keeping
colnames(data.raw)
var.scale <- scale(data[, c(2, 3, 4, 5, 6, 7, 8, 9, 11)]) # centers and divides by SD
data <- data.frame(var.scale, site=data.raw$site,
                   max=data.raw$max) 
colnames(data)

# Run glm to see what predicts maximum grwth
model <- glm(max ~ dem100 + slp100 + hli100 + tpi100 + cti100 + tpha + shrub_perc + pipo_dist_bin
             + estab_yr, data = data, maxit = 100)
summary(model)
model.step <- stepAIC(model, direction =  "backward")
summary(model.step)
 
# Call:
#   glm(formula = max ~ hli100 + tpi100 + tpha + shrub_perc + pipo_dist_bin + 
#         estab_yr, data = data, maxit = 100)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -9.5259  -4.5210   0.4144   3.7875   9.5693  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     20.071      2.125   9.444 6.05e-08 ***
#   hli100           2.304      1.259   1.830  0.08595 .  
# tpi100           3.137      1.393   2.252  0.03874 *  
#   tpha             3.009      1.212   2.482  0.02452 *  <- I kinda take this as more trees more likelihood tall one?
#   shrub_perc      -2.838      1.587  -1.788  0.09272 .  ^ also, TPHA is for PIPO. Should do ALL TPHA
# pipo_dist_bin   -5.946      2.736  -2.173  0.04513 *  
#   estab_yr        -7.677      1.683  -4.561  0.00032 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for gaussian family taken to be 42.47645)
# 
# Null deviance: 2254.61  on 22  degrees of freedom
# Residual deviance:  679.62  on 16  degrees of freedom
# (28 observations deleted due to missingness)
# AIC: 159.15
# 
# Number of Fisher Scoring iterations: 2

###PSME##########################################################
## DOES MAX ANNUAL GROWTH RATE VARY IN SPACE OR WITH AGE??
# Grab mean estab yr
estab_yr <- tripod %>%
  filter(! spp == "SEEDLING", spp == "PSME", ! is.na(estab_yr)) %>%
  group_by(., site) %>%
  summarize_at(c("estab_yr"),
               funs(mean(., na.rm=TRUE)))

# Grab max grwth in any given yr at a site
max <- tripod %>%
  filter(!spp == "SEEDLING", spp == "PSME") %>%
  dplyr::select(site, spp, starts_with("grwth"),
                -grwth_2016, -grwth_avg_Ngerm_N16, -grwth_avg_Ngerm_Y16) %>%
  gather(key = yr, value = grwth, -spp, -site) %>%
  filter(! is.na(grwth))
max <- max %>%
  dplyr::select(-spp) %>%
  group_by(site) %>%
  summarize(max = max(grwth))

# Pull out topo and psme-specific predictors
data <- psme_sum_all %>%
  dplyr::select(site, dem100, slp100,  hli100, tpi100, cti100, shrub_perc, tpha, psme_dist_bin)
data$shrub_perc <- (data$shrub_perc/100)

# Join grwth summary and age summary to these predictors
data <- data %>%
  left_join(max, by = "site") %>%
  left_join(estab_yr, by = "site")

# Scale the predictor variables INCLUDING AGE
data.raw <- data # for safe-keeping
colnames(data.raw)
var.scale <- scale(data[, c(2, 3, 4, 5, 6, 7, 8, 9, 11)]) # centers and divides by SD
data <- data.frame(var.scale, site=data.raw$site,
                   max=data.raw$max) 
colnames(data)

# Run glm to see what predicts maximum grwth
model <- glm(max ~ dem100 + slp100 + hli100 + tpi100 + cti100 + tpha + shrub_perc + psme_dist_bin
             + estab_yr, data = data, maxit = 100)
summary(model)
model.step <- stepAIC(model, direction =  "backward")
summary(model.step)

# Call:
#   glm(formula = max ~ tpha + estab_yr, data = data, maxit = 100)
# 
# Deviance Residuals: 
#   Min      1Q  Median      3Q     Max  
# -4.946  -2.429  -1.466   2.563  10.055  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  11.8222     0.8503  13.904 2.24e-12 ***
#   tpha          2.9393     0.6394   4.597  0.00014 ***
#   estab_yr     -2.3125     0.9008  -2.567  0.01758 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for gaussian family taken to be 15.21311)
# 
# Null deviance: 732.24  on 24  degrees of freedom
# Residual deviance: 334.69  on 22  degrees of freedom
# (26 observations deleted due to missingness)
# AIC: 143.81
# 
# Number of Fisher Scoring iterations: 2


#### TOO FEW FOR ABLA, PIEN, LAOC