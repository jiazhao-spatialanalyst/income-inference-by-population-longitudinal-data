library("lavaan")
library("tidyverse")

load("./us_clean.RData") 

usw

usl

# check variables
hist(usw$sf12mcs_1)
hist(usw$sf12mcs_2)
hist(usw$sf12mcs_3)
hist(usw$sf12mcs_4)

# correlation matrix
usw |>
  select(matches("sf12mcs")) |> 
  na.omit() |> 
  cor() |> 
  round(2)

# model setup
model <- 'sf12mcs_2 ~ 1 + sf12mcs_1'

# estimate and save results
fit <- sem(model, data = usw)

# print summary
summary(fit, standardized = TRUE)

# create new centered variables
usw <- usw |>
  mutate_at(vars(starts_with("sf12mcs")),
            list("test" = ~ . - mean(., na.rm = T)))

# check new variables
usw |>
  select(matches("mcs")) |>
  summary()

# rename variables
usw <- usw |>
  rename_at(vars(ends_with("test")),
            ~ str_replace(., "sf12mcs", "mcsc") |> 
              str_remove("_test"))

# check results
usw |>
  select(matches("mcs")) |>
  summary()

# define the model
model <- 'mcsc_2 ~ 1 + mcsc_1'

fit <- sem(model, data = usw)

summary(fit, standardized = TRUE)

model <- 'mcsc_2 ~ 1 + mcsc_1
          mcsc_3 ~ 1 + mcsc_2'

fit <- sem(model, data = usw)

summary(fit, standardized = TRUE)

# Three wave autoregressive + lag 2
model <- 'mcsc_2 ~ 1 + mcsc_1
          mcsc_3 ~ 1 + mcsc_2 + mcsc_1'

fit <- sem(model, data = usw)

summary(fit, standardized = TRUE)

# Three wave autoregressive + corr
model <- 'mcsc_2 ~ 1 + mcsc_1
          mcsc_3 ~ 1 + mcsc_2
          mcsc_1 ~~ mcsc_3'

fit <- sem(model, data = usw)

summary(fit, standardized = TRUE)

model <- 'mcsc_2 ~ 1 + a*mcsc_1
          mcsc_3 ~ 1 + b*mcsc_2
          ind := a*b'

fita <- sem(model, data = usw)

summary(fita, standardized = TRUE)

# Indirect and total effects
model <- 'mcsc_2 ~ 1 + a*mcsc_1
          mcsc_3 ~ 1 + b*mcsc_2 + c*mcsc_1
          ind := a*b
          total := c + (a*b)'

fitb <- sem(model, data = usw)

summary(fitb, standardized = TRUE)

anova(fita, fitb)

model <- 'mcsc_2 ~ 1 + a*mcsc_1 
          mcsc_3 ~ 1 + a*mcsc_2 
          ind := a*a'

fitc <- sem(model, data = usw)

summary(fitc, standardized = TRUE)

anova(fitc, fita)

# model 4 wave and calculate indirect effect of wave 1 on wave 4
model <- 'mcsc_2 ~ 1 + a*mcsc_1
          mcsc_3 ~ 1 + b*mcsc_2
          mcsc_4 ~ 1 + c*mcsc_3
          ind := a*b*c'

fit1 <- sem(model, data = usw)

summary(fit1, standardized = TRUE)

model <- 'mcsc_2 ~ 1 + a*mcsc_1
          mcsc_3 ~ 1 + a*mcsc_2
          mcsc_4 ~ 1 + c*mcsc_3
          ind := a*a*c'

fit2 <- sem(model, data = usw)

summary(fit2, standardized = TRUE)

model <- 'mcsc_2 ~ 1 + a*mcsc_1
          mcsc_3 ~ 1 + a*mcsc_2
          mcsc_4 ~ 1 + a*mcsc_3
          ind := a*a*a'

fit3 <- sem(model, data = usw)

summary(fit3, standardized = TRUE)

model <- 'mcsc_2 ~ 1 + a*mcsc_1
          mcsc_3 ~ 1 + b*mcsc_2 + d*mcsc_1
          mcsc_4 ~ 1 + c*mcsc_3 + e*mcsc_2
          ind := a*b*c + d*c + a*e'

fit4 <- sem(model, data = usw)

summary(fit4, standardized = TRUE)

anova(fit1, fit4)

# create missing value indicators
usw <- usw |> 
  mutate_at(vars(matches("mcsc")),
            list("miss" = ~is.na(.)))

count(usw, mcsc_1_miss, mcsc_2_miss, mcsc_3_miss, mcsc_4_miss)

model <- 'mcsc_2 ~ 1 + a*mcsc_1
          mcsc_3 ~ 1 + b*mcsc_2 + d*mcsc_1
          mcsc_4 ~ 1 + c*mcsc_3 + e*mcsc_2
          ind := a*b*c + d*c + a*e'

fit6 <- sem(model, data = usw, missing = "ML")

summary(fit6, standardized = TRUE)

# Practical 2 - Cross-lagged models ----

# create centered variables
usw <- usw |>
  mutate_at(vars(starts_with("sf12pcs")),
            list("test" = ~ . - mean(., na.rm = T)))

# rename variables
usw <- usw |>
  rename_at(vars(ends_with("test")),
            ~ str_replace(., "sf12pcs", "pcsc") |> 
              str_remove("_test"))

# check new vars
usw |>
  select(matches("pcs")) |>
  summary()

# correlation matrix
usw |>
  select(matches("pcsc")) |> 
  na.omit() |> 
  cor() |> 
  round(2)

# Cross-lagged model
model <- 'pcsc_2 ~ 1 + pcsc_1 + mcsc_1
          pcsc_3 ~ 1 + pcsc_2 + mcsc_2
          mcsc_2 ~ 1 + mcsc_1 + pcsc_1
          mcsc_3 ~ 1 + mcsc_2 + pcsc_2
          pcsc_1 ~~ mcsc_1
          pcsc_2 ~~ mcsc_2
          pcsc_3 ~~ mcsc_3'

m1 <- sem(model, data = usw, missing = "ML")

summary(m1, standardized = TRUE)

library(lavaanPlot)
lavaanPlot2(
  m1,
  include = "covs",
  coef_labels = TRUE,
  graph_options = list(label = "my first graph", rankdir = "LR")
)

# Controlling for confounders ----

# check gender coding
count(usw, gndr)

# Cross-lagged model with gender control
model <- 'pcsc_2 ~ 1 + pcsc_1 + mcsc_1 + gndr
          pcsc_3 ~ 1 + pcsc_2 + mcsc_2 + gndr
          mcsc_2 ~ 1 + mcsc_1 + pcsc_1 + gndr
          mcsc_3 ~ 1 + mcsc_2 + pcsc_2 + gndr
          pcsc_1 ~ 1 + gndr
          mcsc_1 ~ 1 + gndr
          pcsc_1 ~~ mcsc_1
          pcsc_2 ~~ mcsc_2
          pcsc_3 ~~ mcsc_3'

m2 <- sem(model, data = usw, missing = "ML")

summary(m2, standardized = TRUE)

model <- 'pcsc_2 ~ 1 + pcsc_1 + mcsc_1 + gndr + age
          pcsc_3 ~ 1 + pcsc_2 + mcsc_2 + gndr + age
          mcsc_2 ~ 1 + mcsc_1 + pcsc_1 + gndr + age
          mcsc_3 ~ 1 + mcsc_2 + pcsc_2 + gndr + age
          pcsc_1 ~ 1 + gndr + age
          mcsc_1 ~ 1 + gndr + age
          pcsc_1 ~~ mcsc_1
          pcsc_2 ~~ mcsc_2
          pcsc_3 ~~ mcsc_3'

m3 <- sem(model, data = usw, missing = "ML")

summary(m3, standardized = TRUE)

# Cross-lagged model with time varying var.
model <- 'pcsc_2 ~ 1 + pcsc_1 + mcsc_1 + single_2
          pcsc_3 ~ 1 + pcsc_2 + mcsc_2 + single_3
          mcsc_2 ~ 1 + mcsc_1 + pcsc_1 + single_2
          mcsc_3 ~ 1 + mcsc_2 + pcsc_2 + single_3
          pcsc_1 ~ 1 + single_1
          mcsc_1 ~ 1 + single_1
          pcsc_1 ~~ mcsc_1
          pcsc_2 ~~ mcsc_2
          pcsc_3 ~~ mcsc_3'

m4 <- sem(model, data = usw, missing = "ML")

summary(m4, standardized = TRUE)

# Cross-lagged model with multiple controls
model <- 'pcsc_2 ~ 1 + pcsc_1 + mcsc_1 + single_2 + logincome_2 + sati_2
          pcsc_3 ~ 1 + pcsc_2 + mcsc_2 + single_3 + logincome_3 + sati_3
          mcsc_2 ~ 1 + mcsc_1 + pcsc_1 + single_2 + logincome_2 + sati_2
          mcsc_3 ~ 1 + mcsc_2 + pcsc_2 + single_3 + logincome_3 + sati_3
          pcsc_1 ~ 1 + single_1 + logincome_1 + sati_1
          mcsc_1 ~ 1 + single_1 + logincome_1 + sati_1
          pcsc_1 ~~ mcsc_1
          pcsc_2 ~~ mcsc_2
          pcsc_3 ~~ mcsc_3'

m5 <- sem(model, data = usw)

summary(m5, standardized = TRUE, fit.measures = TRUE)

# Investigating differences in coefficients ----

# Cross-lagged model with equal cross-effects w1
model <- 'pcsc_2 ~ 1 + pcsc_1 + a*mcsc_1
          pcsc_3 ~ 1 + pcsc_2 + mcsc_2
          mcsc_2 ~ 1 + mcsc_1 + a*pcsc_1
          mcsc_3 ~ 1 + mcsc_2 + pcsc_2
          pcsc_1 ~~ mcsc_1
          pcsc_2 ~~ mcsc_2
          pcsc_3 ~~ mcsc_3'

m6 <- sem(model, data = usw, missing = "ML")

summary(m6, standardized = TRUE)

anova(m1, m6)

# Cross-lagged model with equal cross-effects w2
model <- 'pcsc_2 ~ 1 + pcsc_1 + mcsc_1
          pcsc_3 ~ 1 + pcsc_2 + b*mcsc_2
          mcsc_2 ~ 1 + mcsc_1 + pcsc_1 
          mcsc_3 ~ 1 + mcsc_2 + b*pcsc_2
          pcsc_1 ~~ mcsc_1
          pcsc_2 ~~ mcsc_2
          pcsc_3 ~~ mcsc_3'

m7 <- sem(model, data = usw, missing = "ML")

summary(m7, standardized = TRUE)

anova(m1, m7)

# Controlling for between variation ----

# cross-lagged model with random intercept for mental health
model <- 'pcsc_2 ~ 1 + pcsc_1 + mcsc_1 
          pcsc_3 ~ 1 + pcsc_2 + mcsc_2 
          mcsc_2 ~ 1 + mcsc_1 + pcsc_1 
          mcsc_3 ~ 1 + mcsc_2 + pcsc_2 
          pcsc_1 ~~ mcsc_1
          pcsc_2 ~~ mcsc_2
          pcsc_3 ~~ mcsc_3
          rm =~ 1*mcsc_1 + 1*mcsc_2 + 1*mcsc_3'

m13 <- sem(model, data = usw)

summary(m13, standardized = TRUE)

anova(m1, m13)

# Optional: stability, stationarity and equilibrium ----

# Cross-lagged model with stability on physical
model <- 'pcsc_2 ~ 1 + b*pcsc_1 + mcsc_1 
          pcsc_3 ~ 1 + b*pcsc_2 + mcsc_2 
          mcsc_2 ~ 1 + mcsc_1 + pcsc_1 
          mcsc_3 ~ 1 + mcsc_2 + pcsc_2 
          pcsc_1 ~~ mcsc_1
          pcsc_2 ~~ mcsc_2
          pcsc_3 ~~ mcsc_3'

m8 <- sem(model, data = usw, missing = "ML")

summary(m8, standardized = TRUE)

anova(m1, m8)

# Cross-lagged model with stability physical and mental 
model <- 'pcsc_2 ~ 1 + pcsc_1 + mcsc_1
          pcsc_3 ~ 1 + pcsc_2 + mcsc_2
          mcsc_2 ~ 1 + c*mcsc_1 + pcsc_1
          mcsc_3 ~ 1 + c*mcsc_2 + pcsc_2
          pcsc_1 ~~ mcsc_1
          pcsc_2 ~~ mcsc_2
          pcsc_3 ~~ mcsc_3'

m9 <- sem(model, data = usw, missing = "ML")

summary(m9, standardized = TRUE)

anova(m9, m1)

# stationarity of the effect of mental on physical
model <- 'pcsc_2 ~ 1 + pcsc_1 + c*mcsc_1
          pcsc_3 ~ 1 + pcsc_2 + c*mcsc_2
          mcsc_2 ~ 1 + mcsc_1 + pcsc_1
          mcsc_3 ~ 1 + mcsc_2 + pcsc_2
          pcsc_1 ~~ mcsc_1
          pcsc_2 ~~ mcsc_2
          pcsc_3 ~~ mcsc_3'

m10 <- sem(model, data = usw, missing = "ML")

summary(m10, standardized = TRUE)

anova(m10, m1)

# stationarity of the effect of mental on physical
model <- 'pcsc_2 ~ 1 + b*pcsc_1 + mcsc_1
          pcsc_3 ~ 1 + b*pcsc_2 + mcsc_2
          mcsc_2 ~ 1 + mcsc_1 + d*pcsc_1 
          mcsc_3 ~ 1 + mcsc_2 + d*pcsc_2 
          pcsc_1 ~~ mcsc_1
          pcsc_2 ~~ mcsc_2
          pcsc_3 ~~ mcsc_3'

m11 <- sem(model, data = usw, missing = "ML")

summary(m11, standardized = TRUE)

anova(m10, m11)

# Testing equilibrium wave 2 and 3
model <- 'pcsc_2 ~ 1 + pcsc_1 + mcsc_1
          pcsc_3 ~ 1 + pcsc_2 + mcsc_2
          mcsc_2 ~ 1 + mcsc_1 + pcsc_1
          mcsc_3 ~ 1 + mcsc_2 + pcsc_2
          pcsc_1 ~~ mcsc_1
          pcsc_2 ~~ c*mcsc_2
          pcsc_3 ~~ c*mcsc_3
          pcsc_2 ~~ f*pcsc_2
          pcsc_3 ~~ f*pcsc_3
          mcsc_2 ~~ g*mcsc_2
          mcsc_3 ~~ g*mcsc_3'

m12 <- sem(model, data = usw, missing = "ML")

summary(m12, standardized = TRUE)

anova(m1, m12)
