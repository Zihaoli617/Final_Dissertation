# set up the final assessment working directory
setwd("/Users/lizihao/Desktop/final_analysis/")

# ====================================================================================
# Libraries
# ====================================================================================
library(tidyverse)
library(spatstat)
library(here)
library(tmap)
library(maptools)

library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)
library(tidypredict)
library(ggplot2)
library(corrr)
library(rsample)
library(lattice)
library(caret)
library(boot)
library(spatialreg)

# ====================================================================================
# London wards information from the London datastore

Londonlsoa<-dir_info(here::here("Desktop","final_analysis","ESRI"))%>%

  dplyr::filter(str_detect(path, 
                           "LSOA_2011_London_gen_MHW.shp$"))%>%
  dplyr::select(path)%>%
  pull()%>%
  st_read()  

qtm(Londonlsoa)
# ====================================================================================
# Data cleaning
final_london <- read_csv("analysislzh2.csv", 
                               na = c("", "NA", "n/a"), 
                               locale = locale(encoding = 'Latin1'), 
                               col_names = TRUE)

Datatypelist <- final_london %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")


Datatypelist
#merge boundaries and data
final_lon <- Londonlsoa%>%
  left_join(.,
            final_london, 
            by = c("LSOA11CD" = "LSOAcode2011"))

#Regression Basics
q <- qplot(x = `imd`, 
           y = `unhealthy`, 
           data=final_lon)

q + stat_smooth(method="lm", se=FALSE, size=0.8) + 
  geom_jitter()

#run the linear regression model and store its outputs in an object called model1
Regressiondata<- final_lon%>%
  clean_names()%>%
  dplyr::select(unhealthy,
                imd,
                income,
                employment,
                health,
                crime,
                education,
                services,
                environment,)

# model 1
model1 <- Regressiondata %>%
  lm(unhealthy ~
       imd,

     data=.)
# summary model 1, it might occupy a large area
summary(model1)
#Call:
#lm(formula = unhealthy ~ imd, data = .)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-1.6406 -0.5387 -0.0481  0.4866  1.8357 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.1347508  0.0262358  -5.136 2.93e-07 ***
#  imd          0.0001926  0.0001340   1.438    0.151    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.739 on 4319 degrees of freedom
#Multiple R-squared:  0.0004782,	Adjusted R-squared:  0.0002468 
#F-statistic: 2.066 on 1 and 4319 DF,  p-value: 0.1506

tidy(model1)
#  term         estimate std.error statistic     p.value
# <chr>           <dbl>     <dbl>     <dbl>       <dbl>
#1 (Intercept) -0.135     0.0262       -5.14    0.000000293
#2 imd          0.000193  0.000134      1.44    0.151 

glance(model1)
#r.squared adj.r.squared sigma statistic p.value    df logLik   AIC   BIC deviance df.residual  nobs
#<dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>       <int> <int>
#0.000478      0.000247 0.739      2.07   0.151     1 -4823. 9652. 9672.    2359.        4319  4321

#model 2 for mutiple linear regression
regression2<- final_london%>%
  clean_names()%>%
  dplyr::select(unhealthy,
                imd,
                income,
                employment,
                health,
                crime,
                education,
                services,
                environment,)


model2 <- Regressiondata %>%
  lm(unhealthy ~
     income +
     employment +
     health +
     crime + 
     education +
     services +
     environment,
     
     data=.)

summary(model2)
#Call:
#  lm(formula = unhealthy ~ income + employment + health + crime + 
#       education + services + environment, data = .)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-2.21491 -0.45576 -0.06325  0.41216  2.17324 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.0631069  0.0263342   2.396 0.016600 *  
#income       0.0017735  0.0021335   0.831 0.405869    
#employment  -0.0094746  0.0025108  -3.774 0.000163 ***
#health      -0.0007293  0.0014915  -0.489 0.624912    
#crime        0.0013137  0.0005972   2.200 0.027878 *  
#education    0.0219086  0.0014682  14.922  < 2e-16 ***
#services     0.0032579  0.0006696   4.865 1.18e-06 ***
#environment -0.0147476  0.0006157 -23.951  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.6457 on 4313 degrees of freedom
#(514 observations deleted due to missingness)
#Multiple R-squared:  0.2381,	Adjusted R-squared:  0.2368 
#F-statistic: 192.5 on 7 and 4313 DF,  p-value: < 2.2e-16


#Assumption 1
final_lon <- final_lon %>%
  clean_names()

#let's check the distribution of these variables first
#income
ggplot(final_lon, aes(x=income)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 2) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

qplot(x = income, 
      y = unhealthy, 
      data=final_lon)


#employment
ggplot(final_lon, aes(x=employment)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 2) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

ggplot(final_lon, aes(x=log(employment))) + 
  geom_histogram()



ggplot(final_lon, aes(x=log(health))) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.1) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

ggplot(final_lon, aes(x=log(crime))) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.1) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

ggplot(final_lon, aes(x=log(services))) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.1) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

ggplot(final_lon, aes(x=log(education))) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.2) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

ggplot(final_lon, aes(x=log(environment))) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.1) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

qplot(x = environment, 
      y = unhealthy,
      data=final_lon)

#Assumption 2 - The residuals in your model should be normally distributed
model_data <- model2 %>%
  augment(., regression2)

#plot residuals
model_data%>%
  dplyr::select(.resid)%>%
  pull()%>%
  qplot()+ 
  geom_histogram() 


#Assumption 3 - No Multicolinearity in the independent variables
tidy(model2)
#term         estimate std.error statistic   p.value
#<chr>           <dbl>     <dbl>     <dbl>     <dbl>
#1 (Intercept)  0.0631    0.0263       2.40  1.66e-  2
#2 income       0.00177   0.00213      0.831 4.06e-  1
#3 employment  -0.00947   0.00251     -3.77  1.63e-  4
#4 health      -0.000729  0.00149     -0.489 6.25e-  1
#5 crime        0.00131   0.000597     2.20  2.79e-  2
#6 education    0.0219    0.00147     14.9   3.90e- 49
#7 services     0.00326   0.000670     4.87  1.18e-  6
#8 environment -0.0147    0.000616   -24.0   3.99e-119

glance(model2)
#r.squared adj.r.squared sigma statistic   p.value    df logLik   AIC   BIC deviance df.residual  nobs
#<dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>       <int> <int>
#0.238         0.237 0.646      193. 4.19e-249     7 -4237. 8492. 8549.    1798.        4313  4321

model_data2 <- model2 %>%
  augment(., regression2)

# also add them to the shapelayer
final_london <- final_london %>%
  mutate(model2resids = residuals(model2))

library(corrr)

Correlation <- final_london %>%

  dplyr::select(unhealthy,
                income,
                employment,
                health,
                crime,
                education,
                services,
                environment) %>%
  correlate() %>%
  
  # just focus on GCSE and house prices
  focus(-unhealthy, mirror = TRUE) 


#visualise the correlation matrix
rplot(Correlation)


#Variance Inflation Factor (VIF)
vif(model2)
#income      employment      health       crime     education    services   environment 
#14.139976   13.599931       3.702969    1.453199    2.603071    1.988167    1.36

Correlation_all<- final_london %>%
  dplyr::select(unhealthy,
                income,
                employment,
                health,
                crime,
                education,
                services,
                environment)%>%
  correlate()

rplot(Correlation_all)

#Assumption 4 - Homoscedasticity
#print some model diagnositcs. 
par(mfrow=c(2,2))    #plot to 2 by 2 array
plot(model2)

#Assumption 5 - Independence of Errors
DW <- durbinWatsonTest(model2)
tidy(DW)
#statistic p.value autocorrelation method             alternative
#<dbl>   <dbl>           <dbl> <chr>              <chr>      
#  1      1.50       0           0.251 Durbin-Watson Test two.sided  

#spatial-autocorrelation.
#now plot the residuals
tmap_mode("view")
#qtm(final_london, fill = "model2_resids")

tm_shape(final_london) +
  tm_polygons("model2resids",
              palette = "RdYlBu") +
  tm_shape(imd) + tm_dots(col = "TYPE")



#Spatial Regression Models

model3 <- lm(unhealthy ~ 
               environment + 
               education +
               employment +
               services +
               
               , data = final_london)

tidy(model3)
#term        estimate std.error statistic   p.value
#<chr>          <dbl>     <dbl>     <dbl>     <dbl>
# (Intercept)  0.0634   0.0262        2.42 1.54e-  2
#2 environment -0.0148   0.000596    -24.8  1.25e-126
#3 education    0.0222   0.00144      15.4  2.91e- 52
#4 employment  -0.00828  0.00118      -7.00 2.89e- 12
#5 services     0.00343  0.000637      5.39 7.51e-  8
#6 crime        0.00131  0.000593      2.21 2.69e-  2
summary(model3)
#Call:
#  lm(formula = unhealthy ~ environment + education + employment + 
 #      services + crime, data = final_london)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-2.2305 -0.4554 -0.0645  0.4166  2.1857 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.0634361  0.0261824   2.423   0.0154 *  
#  environment -0.0147647  0.0005964 -24.756  < 2e-16 ***
#  education    0.0221762  0.0014382  15.420  < 2e-16 ***
#  employment  -0.0082785  0.0011821  -7.003 2.89e-12 ***
#  services     0.0034301  0.0006366   5.388 7.51e-08 ***
#  crime        0.0013138  0.0005933   2.214   0.0269 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.6456 on 4315 degrees of freedom
#Multiple R-squared:  0.2379,	Adjusted R-squared:  0.237 
#F-statistic: 269.4 on 5 and 4315 DF,  p-value: < 2.2e-16
coordsW <- final_lon%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW)

library(spatialreg)
Llsoa <- final_lon %>%
  poly2nb(., queen=T)


slag_dv_model2_queen <- lagsarlm(unhealthy ~ 
                                   environment + 
                                   education +
                                   employment +
                                   services +
                                   crime, 
                                 data = final_lon, 
                                 nb2listw(Llsoa, style="C"), 
                                 method = "eigen")
#what do the outputs show?
tidy(slag_dv_model2_queen)




model4 <- lm(unhealthy ~ 
               environment + 
               education +
               employment +
               services
               
               , data = final_london)


summary(model4)
#Call:
#  lm(formula = unhealthy ~ environment + education + employment + 
#       services, data = final_london)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-2.2351 -0.4570 -0.0608  0.4148  2.1400 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.0785880  0.0252839   3.108  0.00189 ** 
#  environment -0.0143240  0.0005625 -25.466  < 2e-16 ***
#  education    0.0224442  0.0014337  15.655  < 2e-16 ***
# employment  -0.0077384  0.0011572  -6.687 2.57e-11 ***
#  services     0.0035051  0.0006360   5.511 3.78e-08 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.6459 on 4316 degrees of freedom
#Multiple R-squared:  0.237,	Adjusted R-squared:  0.2363 
#F-statistic: 335.2 on 4 and 4316 DF,  p-value: < 2.2e-16

model_data <- model4 %>%
  augment(., regression2)

# also add them to the shapelayer
final_london <-final_london %>%
  mutate(model4resids = residuals(model4))

#plot residuals
model_data%>%
  dplyr::select(.resid)%>%
  pull()%>%
  qplot()+ 
  geom_histogram() 


Correlation1 <- final_london %>%
  
  dplyr::select(unhealthy,
                employment,
                education,
                services,
                environment) %>%
  correlate() %>%
  
  # just focus on GCSE and house prices
  focus(-unhealthy, mirror = TRUE) 


#visualise the correlation matrix
rplot(Correlation1)

par(mfrow=c(2,2))    #plot to 2 by 2 array
plot(model4)
#
tmap_mode("view")
#qtm(LonWardProfiles, fill = "model1_resids")

tm_shape(final_lon) +
  tm_polygons("model4resids",
              palette = "RdYlBu") +
  tm_shape() + tm_dots(col = "TYPE")

wm <- nb2mat(w, style='B')
rwm <- mat2listw(wm, style='W')

lm.morantest(model2, rwm, alternative="two.sided")

# Global Moran I for regression residuals
# 
# data:  
#   model: lm(unhealthy,
#employment,
#education,
#services,
#environment)
# weights: rwm
# 
# Moran I statistic standard deviate = 20.463, p-value < 2.2e-16
# alternative hypothesis: two.sided
# sample estimates:
# Observed Moran I      Expectation         Variance 
# 0.1726919149    -0.0018717852     0.0005464598 

ggplot(final_lon, aes(x = model4)) + 
  geom_density() 

lm.LMtests(model4, rwm, test = c("LMerr","LMlag","RLMerr","RLMlag","SARMA"))
# Lagrange multiplier diagnostics for spatial dependence
# 
# data:  
#   model: lm(unhealthy,
#employment,
#education,
#services,
#environment)
# weights: rwm
# 
# LMerr = 397.66, df = 1, p-value < 2.2e-16
#
# Lagrange multiplier diagnostics for spatial dependence
# 
# weights: rwm
# 
# LMlag = 273.39, df = 1, p-value < 2.2e-16
# 
# 
# Lagrange multiplier diagnostics for spatial dependence
# 
# RLMerr = 156.35, df = 1, p-value < 2.2e-16
# 
# 
# Lagrange multiplier diagnostics for spatial dependence
# 
# weights: rwm
# 
# RLMlag = 32.076, df = 1, p-value = 1.482e-08
# 
# 
# Lagrange multiplier diagnostics for spatial dependence
# 
# data:  
#   model: lm(formula = percent_with_households_owned ~ general_fertility_rate +
#               population_density_persons_per_sq_km + employment_rate + log(crime_rate) +
#               log(average_public_transport_accessibility_score) + average_gcse_capped_point_scores, data =
#               Regressiondata_2)
# weights: rwm
# 
# SARMA = 429.74, df = 2, p-value < 2.2e-16
# ====================================================================================
#fit and interpret a spatially lagged model
model4_lag <- lagsarlm(unhealthy,
                       employment,
                       education,
                       services,
                       environment,
                       
                       data=final_lon, rwm)

glance(model4_lag)
# r.squared   AIC   BIC deviance logLik  nobs
# <dbl> <dbl> <dbl>    <dbl>  <dbl> <int>
# 0.872 4210. 4250.   28166. -2096.   626
summary(model4_lag)
# Call:lagsarlm(unhealthy,employment,education,services,environment, data = final_lon, listw = rwm)
# 
# Residuals:
#   Min        1Q      Median        3Q       Max 
# -18.89361  -4.59418   0.08083   4.27835  19.47971 
# 
# Type: lag 
# Coefficients: (asymptotic standard errors) 
# Estimate  Std. Error z value  Pr(>|z|)
# (Intercept)                                       -1.5655e+01  7.2410e+00 -2.1619   0.03062
# general_fertility_rate                            -8.8260e-03  2.0024e-02 -0.4408   0.65938
# population_density_persons_per_sq_km              -4.3825e-04  9.1895e-05 -4.7690 1.851e-06
# employment_rate                                    4.8675e-01  5.6014e-02  8.6899 < 2.2e-16
# log(crime_rate)                                   -5.6714e+00  8.8185e-01 -6.4312 1.266e-10
# log(average_public_transport_accessibility_score) -1.0252e+01  1.5493e+00 -6.6174 3.657e-11
# average_gcse_capped_point_scores                   1.4592e-01  1.4816e-02  9.8490 < 2.2e-16
# 
# Rho: 0.50469, LR test value: 252.28, p-value: < 2.22e-16
# Asymptotic standard error: 0.028907
# z-value: 17.459, p-value: < 2.22e-16
# Wald statistic: 304.81, p-value: < 2.22e-16
# 
# Log likelihood: -2095.82 for lag model
# ML residual variance (sigma squared): 44.994, (sigma: 6.7078)
# Number of observations: 626 
# Number of parameters estimated: 9 
# AIC: 4209.6, (AIC for lm: 4459.9)
# LM test for residual autocorrelation
# test value: 73.288, p-value: < 2.22e-16
tidy(model4_lag)
# term                                              estimate std.error statistic     p.value
# <chr>                                                <dbl>     <dbl>     <dbl>       <dbl>
#   1 rho                                                  0.543    0.0272     20.0  0          
# 2 (Intercept)                                          -29.3      6.02       -4.86 0.00000117 
# 3 employment                                           0.541    0.0541      9.99 0          
# 4 environment                                          -4.03     0.820      -4.92 0.000000857
# 5 education                                            -14.3      1.27      -11.3  0          
# 6 services                                             0.152    0.0148     10.2  0      

W <- as(rwm, "CsparseMatrix")
trMC <- trW(W, type="MC")
im<-impacts(model2_lag, tr=trMC, R=100)
sums<-summary(im,  zstats=T)
#To print the coefficients
data.frame(sums$res)
# direct      indirect         total
# 1 -9.326886e-03 -0.0084924092 -1.781929e-02
# 2 -4.631194e-04 -0.0004216841 -8.848036e-04
# 3  5.143720e-01  0.4683511660  9.827232e-01
# 4 -5.993196e+00 -5.4569845970 -1.145018e+01
# 5 -1.083424e+01 -9.8649013732 -2.069914e+01
# 6  1.542051e-01  0.1404084017  2.946135e-01


# ====================================================================================
#Assumptions Underpinning Linear Regression
#data transformtion
final_lon <- final_lon %>%
  clean_names()


#imd distribution
ggplot(final_lon, aes(x=imd)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 15) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

#unhealthy distribution
ggplot(final_lon, aes(x=unhealthy)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.1) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

#===============================================
#imd is a not normal and/or positively ‘skewed’ distribution
qplot(x = imd, 
      y = unhealthy, 
      data=final_lon)

symbox(~imd, 
       final_lon, 
       na.rm=T,
       powers=seq(-3,3,by=.5))







#END
# ====================================================================================
