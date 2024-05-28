
library("tidyverse")
library("lme4")
library("plm")

load("./data/us_clean.RData")

# group by wave, calculate mean and variance of logincome
usl |> 
  group_by(wave) |> 
  summarise(mean_income = mean(logincome, na.rm = T), 
            income_var = var(logincome, na.rm = T))

# group by gender and wave, calculate mean and variance of logincome
usl |> 
  group_by(gndr, wave) |> 
  summarise(mean_income = mean(logincome, na.rm = T), 
            income_var = var(logincome, na.rm = T))

# plot unconditional change graph
usl |> 
  ggplot(aes(wave, fimngrs)) + 
  geom_point(alpha = .005) +
  stat_summary(fun = mean, group = 1, geom = "line", lwd = 1.5) +
  theme_bw() +
  labs(x = "Wave", y = "Gross income")

# plot conditional change graph by sex
usl |> 
  ggplot(aes(wave, fimngrs)) + geom_point(alpha = .005) +
  stat_summary(aes(color = gndr, group = gndr), 
               fun = mean, geom = "line", lwd = 1.5) +
  theme_bw() +
  labs(x = "Wave", y = "Gross income", color = "Sex")

# plot unconditional change graph
usl |> 
  ggplot(aes(wave, logincome)) + geom_point(alpha = .005) +
  stat_summary(fun = mean, group = 1, geom = "line", lwd = 1.5) +
  theme_bw() +
  labs(x = "Wave", y = "Log income")

# plot conditional change graph by sex
usl |> 
  ggplot(aes(wave, logincome)) + geom_point(alpha = .005) +
  stat_summary(aes(color = gndr, group = gndr), 
               fun = mean, geom = "line", lwd = 1.5) +
  theme_bw() +
  labs(x = "Wave", y = "Log income", color = "Sex")

# fixed effects model
fe <- plm(data = usl, logincome ~ gndr + sati, 
          index = "pidp", model = "within")

# random effects model
re <- plm(data = usl, logincome ~ gndr + sati, 
          index = "pidp", model = "random")

# check solution
summary(fe) 
summary(re) 

# Hausman test
phtest(fe, re)

# empty model with within and between variation
m0 <- lmer(data = usl, logincome ~ 1 + (1 | pidp))
summary(m0)

# create wave0
usl <- mutate(usl, wave0 = as.numeric(wave) - 1)

# check result
count(usl, wave, wave0)

# unconditional change model
m1 <- lmer(data = usl, logincome ~ 1 + wave0 + 
             (1 + wave0 | pidp))
summary(m1)

# predict logincome based on the model
usl$m1fit <- predict(m1)

# plot the predicted change
usl |> 
  ggplot(aes(wave, m1fit, group = pidp)) +
  geom_line(alpha = 0.005) + 
  stat_summary(fun = mean, geom = "line", lwd = 1.5, group = 1) +
  theme_bw() +
  labs(x = "Wave", y = "Predicted log income")

# model with "gndr" with different starting points
m2 <- lmer(data = usl, logincome ~ 1 + wave0 + gndr + 
             (1 + wave0 | pidp))
summary(m2)

# model with time varying effect of gender
m3 <- lmer(data = usl, logincome ~ 1 + wave0 + 
             gndr + gndr:wave0 + 
             (1 + wave0 | pidp))
summary(m3)

# compare models
anova(m3, m2)

# predict logincome based on the model
usl$m3fit <- predict(m3)

# plot the predicted change
usl |> 
  ggplot(aes(wave, m3fit, group = gndr, color = gndr)) +
  stat_summary(fun = mean, geom = "line", lwd = 1.5) +
  theme_bw() +
  labs(x = "Wave", y = "Predicted log income",
       color = "Sex")

# include time varying variables: explaining starting point
m4 <- lmer(data = usl, logincome ~ 1 + wave0 + gndr + sati + 
             (1 + wave0 | pidp))
summary(m4)

# explaining starting point and change
m5 <- lmer(data = usl, logincome ~ 1 + wave0 + gndr +  
             sati + sati:wave0 +
             (1 + wave0 | pidp))
summary(m5)

# compare models
anova(m4, m5)

# create new variables
usl <- usl |> 
  group_by(pidp) |> 
  mutate(sati_mean = mean(sati, na.rm = T), 
         sati_dev = sati - sati_mean) 

# check results 
usl |> 
  select(pidp, wave, sati, sati_mean, sati_dev) |> 
  print(n = 50)

m6 <- lmer(data = usl, logincome ~ 1 + wave0 + gndr + 
             sati_mean + sati_dev +  
             (1 + wave0 | pidp))
summary(m6)

m7 <- lmer(data = usl, logincome ~ 1 + wave0 + gndr + 
             sati_mean + sati_dev + 
             sati_mean:wave0 + sati_dev:wave0 + 
             (1 + wave0 | pidp))
summary(m7)

# compare models
anova(m6, m7)

# create lag variable
usl <- usl |> 
  group_by(pidp) |> 
  mutate(sati_lag = dplyr::lag(sati)) |> 
  ungroup()

# check results 
usl |> 
  select(pidp, wave, logincome, sati, sati_lag) |>  
  print(n = 50)

m8 <- lmer(data = usl, logincome ~ 1 + I(wave0-1) + gndr +  
             sati_lag  +
             (1 + I(wave0-1) | pidp))
summary(m8)

m9 <- lmer(data = usl, logincome ~ 1 + wave0 + gndr + sati_lag +
             sati_lag:wave0  +
             (1 + wave0 | pidp))
summary(m9)

# compare models
anova(m8, m9)

# model with intercept measuring wave 1
summary(m2)

# model with intercept measuring middle of study
m10 <- lmer(data = usl, logincome ~ 1 + 
              I(wave0 - 1.5) + gndr + 
              (1 + I(wave0 - 1.5) | pidp))
summary(m10)

# model with intercept measuring wave 4
m11 <- lmer(data = usl, logincome ~ 1 + I(wave0 - 3) + gndr + 
              (1 + I(wave0 - 3) | pidp))
summary(m11)

# compare models
anova(m2, m10, m11)

# create new variable for non-linear change
usl <- mutate(usl, wave0_2 = wave0^2)

# check new variable
count(usl, wave0, wave0_2)

# unconditional non-linear change
m12 <- lmer(data = usl, logincome ~ 1 + wave0 + I(wave0^2) + 
              (1 + wave0 + I(wave0^2) | pidp))
summary(m12)

# compare models
anova(m1, m12)

# predict logincome based on the model
usl$m12fit <- predict(m12)

# plot the predicted change
usl |> 
  ggplot(aes(wave, m12fit, group = 1)) +
  stat_summary(fun = mean, geom = "line", 
               lwd = 1.5, color = "red") +
  stat_summary(aes(wave, m1fit), fun = mean, 
               geom = "line", lwd = 1.5) +
  theme_bw() +
  labs(x = "Wave", y = "Predicted log income")

# model with dummy variables for time
m13 <- lmer(data = usl, logincome ~ 1 + I(as.factor(wave)) + 
              (1 + wave0 | pidp))
summary(m13)

# predict logincome based on the model
usl$m13fit <- predict(m13)

# plot the predicted change
usl |> 
  ggplot(aes(wave, group = 1)) +
  stat_summary(aes(y = m13fit), fun = mean, geom = "line", 
               lwd = 1.5, color = "red") +
  stat_summary(aes(y = m12fit), fun = mean, geom = "line", 
               lwd = 1.5, color = "blue") +
  theme_bw() +
  labs(x = "Wave", y = "Predicted log income")

# table of date and wave
usl |> 
  ungroup() |> 
  count(istrtdaty, wave)

# code negative values as missing
usl <- mutate(usl, 
              istrtdaty = haven::zap_label(istrtdaty),
              istrtdaty = ifelse(istrtdaty < 0, NA, istrtdaty))

# change time variable
m14 <- lmer(data = usl, logincome ~ 1 + I(istrtdaty - 2009) + 
              gndr + (1 + I(istrtdaty - 2009) | pidp))
summary(m14)

# check counts of time variable
count(usl, istrtdaty)

# make smaller dataset with no missing on vars of interest
usl_small <- usl |> 
  filter(istrtdaty > 0)

# predict logincome based on the model
usl_small$m14fit <- predict(m14)

# plot the predicted change
usl_small |> 
  ggplot(aes(istrtdaty, m14fit, group = gndr, color = gndr)) +
  geom_point(alpha = 0.01, position = position_jitter()) +
  stat_summary(fun = mean, geom = "line", lwd = 1.5) +
  theme_bw() +
  labs(x = "Year", y = "Predicted log income", color = "Sex")

# create new variables
usl <- usl |> 
  group_by(pidp) |> 
  mutate(age2 = age + wave0,
         age0 = age2 - 16)

# check new variables
usl |> 
  select(pidp, wave, logincome, age, age2, age0) |> 
  print(n = 50)

# change time variable
m15 <- lmer(data = usl, logincome ~ 1 + 
              age0 + I(age0^2) + I(age0^3) + 
              gndr + age0:gndr + I(age0^2):gndr + I(age0^3):gndr + 
              (1 + age | pidp))
summary(m15)

# predict logincome based on the model
usl$m15fit <- predict(m15)

# plot the predicted change
usl |> 
  ggplot(aes(age2, m15fit, group = gndr, color = gndr)) +
  geom_point(alpha = 0.03) +
  stat_summary(fun = mean, geom = "line", lwd = 1.5) +
  theme_bw() +
  labs(x = "Age", y = "Predicted log income", color = "Sex")

# plot predicted gross income
usl |>
  ggplot(aes(age2, exp(m15fit), group = gndr, color = gndr)) +
  geom_point(alpha = 0.03) +
  stat_summary(fun = mean, geom = "line", lwd = 1.5) +
  theme_bw() +
  labs(x = "Age", y = "Predicted gross income", color = "Sex")
