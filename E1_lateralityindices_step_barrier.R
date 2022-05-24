#setwd("G:/VPHI/Welfare/2- Research Projects/Kathy Broekmeulen/E1/Experiment/Data/csv files")
setwd("/Users/kathybroekmeulen/Documents/OFH_E1_Data/csv files") #on laptop 

lat <- read.csv("E1_sdt_latin.csv", sep=";")

str(lat)

#change character,int variables to factors
lat$trt <- as.factor(lat$trt)                     #set treatment as factor
lat$chick_no <- as.factor(lat$chick_no)           #set chick no as factor 
lat$trt_pen <- as.factor(lat$trt_pen)
lat$woa <- as.factor(lat$woa)           

str(lat)                          #check new structure
head(lat)

#load packages

library(graphics)
library(dplyr)
library(ggplot2)
library(lme4)
library(DHARMa)
library(multcomp)
library(ggpubr)
library(lmerTest)
library(emmeans)
library(effects)
library(car)
library(stats)

codes = unique(lat$trt)
codes

dim(lat)
nrow(lat)
ncol(lat)
names(lat)

#lat$trt <- droplevels(lat$trt)

summary(lat)
#chick_no      trt_pen    trt     woa      left_step       right_step      step_index        left_barrier   right_barier  barrier_index   
#1      :  2   A      :10   ALL:30   5:75   Min.   :0.000   Min.   :0.000   Min.   :-1.00000   Min.   :0.00   Min.   :0.00   Min.   :-1.000  
#2      :  2   B      :10   CON:30   7:75   1st Qu.:2.000   1st Qu.:2.000   1st Qu.:-0.30000   1st Qu.:1.00   1st Qu.:1.00   1st Qu.:-0.700  
#3      :  2   C      :10   FW :30          Median :3.000   Median :3.000   Median : 0.00000   Median :2.00   Median :4.00   Median :-0.300  
#4      :  2   D      :10   L  :30          Mean   :2.887   Mean   :3.113   Mean   :-0.03733   Mean   :2.76   Mean   :3.24   Mean   :-0.082  
#35      :  2   E      :10   LIT:30          3rd Qu.:4.000   3rd Qu.:4.000   3rd Qu.: 0.30000   3rd Qu.:5.00   3rd Qu.:5.00   3rd Qu.: 0.700  
#6      :  2   F      :10                   Max.   :6.000   Max.   :6.000   Max.   : 1.00000   Max.   :6.00   Max.   :6.00   Max.   : 1.000  
#(Other):138   (Other):90                                                                                                                    
#avgt_start      avgt_finish    
#Min.   : 1.000   Min.   :  2.17  
#1st Qu.: 1.000   1st Qu.:  4.21  
#Median : 1.170   Median :  6.67  
#Mean   : 2.724   Mean   : 15.57  
#3rd Qu.: 2.458   3rd Qu.: 14.62  
#Max.   :28.670   Max.   :151.50  

#visualize
ggboxplot(lat, x="trt", y="step_index", color ="woa")


ggplot(lat,aes(x=trt,y=step_index))+
  geom_boxplot(aes(fill=trt), outlier.shape = NA)+
  geom_jitter(alpha=0.5)+
  facet_wrap(~woa)+
  labs(y="Laterality index for step", x="Treatment", fill="Treatment", )+
  theme_bw()+ theme(text = element_text(size = 18))


fit  <- lmer(step_index ~ trt * woa +(1|trt_pen/chick_no),lat)
summary(fit)

#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: step_index ~ trt * woa + (1 | trt_pen/chick_no)
#   Data: lat

#REML criterion at convergence: 185.8

#Scaled residuals: 
#     Min       1Q   Median       3Q      Max 
#-2.13136 -0.64995  0.03228  0.52746  2.36955 

#Random effects:
# Groups           Name        Variance Std.Dev.
# chick_no:trt_pen (Intercept) 0.00000  0.00000 
# trt_pen          (Intercept) 0.00865  0.09301 
# Residual                     0.17676  0.42043 
#Number of obs: 150, groups:  chick_no:trt_pen, 75; trt_pen, 15

#Fixed effects:
#              Estimate Std. Error         df t value Pr(>|t|)
#(Intercept)   0.006667   0.121110  27.000336   0.055    0.957
#trtCON       -0.273333   0.171276  27.000336  -1.596    0.122
#trtFW        -0.080000   0.171276  27.000336  -0.467    0.644
#trtL         -0.073333   0.171276  27.000336  -0.428    0.672
#trtLIT        0.173333   0.171276  27.000336   1.012    0.321
#woa7          0.060000   0.153521 130.000000   0.391    0.697
#trtCON:woa7  -0.066667   0.217111 130.000000  -0.307    0.759
#trtFW:woa7    0.020000   0.217111 130.000000   0.092    0.927
#trtL:woa7     0.040000   0.217111 130.000000   0.184    0.854
#trtLIT:woa7  -0.226667   0.217111 130.000000  -1.044    0.298

#Correlation of Fixed Effects:
#            (Intr) trtCON trtFW  trtL   trtLIT woa7   tCON:7 trFW:7 trtL:7
#trtCON      -0.707                                                        
#trtFW       -0.707  0.500                                                 
#trtL        -0.707  0.500  0.500                                          
#trtLIT      -0.707  0.500  0.500  0.500                                   
#woa7        -0.634  0.448  0.448  0.448  0.448                            
#trtCON:woa7  0.448 -0.634 -0.317 -0.317 -0.317 -0.707                     
#trtFW:woa7   0.448 -0.317 -0.634 -0.317 -0.317 -0.707  0.500              
#trtL:woa7    0.448 -0.317 -0.317 -0.634 -0.317 -0.707  0.500  0.500       
#trtLIT:woa7  0.448 -0.317 -0.317 -0.317 -0.634 -0.707  0.500  0.500  0.500
#optimizer (nloptwrap) convergence code: 0 (OK)
#boundary (singular) fit: see help('isSingular')


#check distribution of variance
plot(fit) #run this code a couple of times in a row to see the next plot


# Residuenanalysen #
par (mfrow= c (3, 2))
qqnorm (resid (fit))
scatter.smooth (fitted (fit), resid (fit))
boxplot (split (resid (fit), lat [, 'trt']))
boxplot (split (resid (fit), lat[, 'woa']))

leveneTest(step_index ~ trt * woa, data=lat)
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value  Pr(>F)  
#group   9  1.8318 0.06753 .
#      140                  
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#normal distribution
ggqqplot(lat,"step_index", ggtheme =theme_bw()) +facet_grid(trt ~ woa)

library(stats)
summary(lm1 <- lm(step_index ~ lat$trt * lat$woa, data = lat))

#Call:
#  lm(formula = step_index ~ lat$trt * lat$woa, data = lat)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.93333 -0.30667 -0.00667  0.26667  0.99333 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)          0.006667   0.110436   0.060   0.9519  
#lat$trtCON          -0.273333   0.156181  -1.750   0.0823 .
#lat$trtFW           -0.080000   0.156181  -0.512   0.6093  
#lat$trtL            -0.073333   0.156181  -0.470   0.6394  
#lat$trtLIT           0.173333   0.156181   1.110   0.2690  
#lat$woa7             0.060000   0.156181   0.384   0.7014  
#lat$trtCON:lat$woa7 -0.066667   0.220873  -0.302   0.7632  
#lat$trtFW:lat$woa7   0.020000   0.220873   0.091   0.9280  
#lat$trtL:lat$woa7    0.040000   0.220873   0.181   0.8566  
#lat$trtLIT:lat$woa7 -0.226667   0.220873  -1.026   0.3066  
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.4277 on 140 degrees of freedom
#Multiple R-squared:  0.09533,	Adjusted R-squared:  0.03717 
#F-statistic: 1.639 on 9 and 140 DF,  p-value: 0.1097

slm1 <- stepAIC(lm1,direction="backward")                #lowest AIC value shows best model --> step_index ~ lat$trt + lat$woa 
#Start:  AIC=-245.14
#step_index ~ lat$trt * lat$woa

#Df Sum of Sq    RSS     AIC
#- lat$trt:lat$woa  4     0.352 25.964 -251.09
#<none>                         25.612 -245.14

#Step:  AIC=-251.09
#step_index ~ lat$trt + lat$woa

#Df Sum of Sq    RSS     AIC
#- lat$woa  1   0.00667 25.971 -253.05
#<none>                 25.964 -251.09
#- lat$trt  4   2.34027 28.304 -246.14

#Step:  AIC=-253.05
#step_index ~ lat$trt

#Df Sum of Sq    RSS     AIC
#<none>                 25.971 -253.05
#- lat$trt  4    2.3403 28.311 -248.11


summary(slm1)
#Call:
#  lm(formula = step_index ~ lat$trt, data = lat)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.98333 -0.28333  0.01667  0.27000  1.03333 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)  0.03667    0.07727   0.475   0.6358   
#lat$trtCON  -0.30667    0.10927  -2.806   0.0057 **
#  lat$trtFW   -0.07000    0.10927  -0.641   0.5228   
#lat$trtL    -0.05333    0.10927  -0.488   0.6262   
#lat$trtLIT   0.06000    0.10927   0.549   0.5838   
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.4232 on 145 degrees of freedom
#Multiple R-squared:  0.08266,	Adjusted R-squared:  0.05736 
#F-statistic: 3.267 on 4 and 145 DF,  p-value: 0.01343

slm1$anova
#Stepwise Model Path 
#Analysis of Deviance Table

#Initial Model:
#  step_index ~ lat$trt * lat$woa

#Final Model:
#  step_index ~ lat$trt


#Step Df    Deviance Resid. Df Resid. Dev       AIC
#1                                        140   25.61200 -245.1361
#2 - lat$trt:lat$woa  4 0.352000000       144   25.96400 -251.0886
#3         - lat$woa  1 0.006666667       145   25.97067 -253.0501


kruskal.test(step_index ~ trt ,data=lat)
#Kruskal-Wallis rank sum test

#data:  step_index by trt
#Kruskal-Wallis chi-squared = 9.8091, df = 4, p-value = 0.0437

kruskal.test(step_index ~ woa ,data=lat)

#Kruskal-Wallis rank sum test

#data:  step_index by woa
#Kruskal-Wallis chi-squared = 0.00045664, df = 1, p-value = 0.983

summary(lm1 <- lm(step_index ~ lat$trt * lat$woa, data = lat))
summary(lm4 <- lm(step_index ~ lat$trt + lat$woa, data = lat))

anova(lm2 <- lm(step_index ~ lat$trt, data = lat))
anova(lm3 <- lm(step_index ~ lat$woa, data = lat))

#Analysis of Variance Table

#Response: step_index
#Df  Sum Sq Mean Sq F value  Pr(>F)  
#lat$trt     4  2.3403 0.58507  3.2666 0.01343 *
#  Residuals 145 25.9707 0.17911                  
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

summary(lm2)
#Call:
#  lm(formula = step_index ~ lat$trt, data = lat)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.98333 -0.28333  0.01667  0.27000  1.03333 

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)   
#(Intercept)  0.03667    0.07727   0.475   0.6358   
#lat$trtCON  -0.30667    0.10927  -2.806   0.0057 **
#lat$trtFW   -0.07000    0.10927  -0.641   0.5228   
#lat$trtL    -0.05333    0.10927  -0.488   0.6262   
#lat$trtLIT   0.06000    0.10927   0.549   0.5838   
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.4232 on 145 degrees of freedom
#Multiple R-squared:  0.08266,	Adjusted R-squared:  0.05736 
#F-statistic: 3.267 on 4 and 145 DF,  p-value: 0.01343

##########################################################################

#visualize
ggboxplot(lat, x="trt", y="barrier_index", color ="woa")


ggplot(lat,aes(x=trt,y=barrier_index))+
  geom_boxplot(aes(fill=trt), outlier.shape = NA)+
  geom_jitter(alpha=0.5)+
  facet_wrap(~woa)+
  labs(y="Laterality index for barrier", x="Treatment", fill="Treatment", )+
  theme_bw()+ theme(text = element_text(size = 18))




fit1  <- lmer(barrier_index ~ trt * woa +(1|trt_pen/chick_no),lat)
summary(fit1)
#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: barrier_index ~ trt * woa + (1 | trt_pen/chick_no)
#   Data: lat

#REML criterion at convergence: 276.6

#Scaled residuals: 
#     Min       1Q   Median       3Q      Max 
#-1.69098 -0.49091 -0.02408  0.60141  1.90674 

#Random effects:
# Groups           Name        Variance Std.Dev.
# chick_no:trt_pen (Intercept) 0.4294   0.6553  
# trt_pen          (Intercept) 0.0000   0.0000  
# Residual                     0.1233   0.3511  
#Number of obs: 150, groups:  chick_no:trt_pen, 75; trt_pen, 15

#Fixed effects:
#              Estimate Std. Error         df t value Pr(>|t|)   
#(Intercept)  2.467e-01  1.919e-01  8.730e+01   1.285   0.2022   
#trtCON       6.204e-15  2.715e-01  8.730e+01   0.000   1.0000   
##trtFW       -5.600e-01  2.715e-01  8.730e+01  -2.063   0.0421 * 
#trtL        -5.600e-01  2.715e-01  8.730e+01  -2.063   0.0421 * 
#trtLIT      -7.800e-01  2.715e-01  8.730e+01  -2.873   0.0051 **
#woa7        -4.000e-02  1.282e-01  7.000e+01  -0.312   0.7560   
#trtCON:woa7  4.000e-02  1.813e-01  7.000e+01   0.221   0.8260   
##trtFW:woa7   2.333e-01  1.813e-01  7.000e+01   1.287   0.2024   
##trtL:woa7    1.533e-01  1.813e-01  7.000e+01   0.846   0.4006   
t#rtLIT:woa7  2.867e-01  1.813e-01  7.000e+01   1.581   0.1184   
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Correlation of Fixed Effects:
#            (Intr) trtCON trtFW  trtL   trtLIT woa7   tCON:7 trFW:7 trtL:7
#trtCON      -0.707                                                        
#trtFW       -0.707  0.500                                                 
#trtL        -0.707  0.500  0.500                                          
#trtLIT      -0.707  0.500  0.500  0.500                                   
#woa7        -0.334  0.236  0.236  0.236  0.236                            
#trtCON:woa7  0.236 -0.334 -0.167 -0.167 -0.167 -0.707                     
#trtFW:woa7   0.236 -0.167 -0.334 -0.167 -0.167 -0.707  0.500              
#trtL:woa7    0.236 -0.167 -0.167 -0.334 -0.167 -0.707  0.500  0.500       
#trtLIT:woa7  0.236 -0.167 -0.167 -0.167 -0.334 -0.707  0.500  0.500  0.500
#optimizer (nloptwrap) convergence code: 0 (OK)
#boundary (singular) fit: see help('isSingular')


#check distribution of variance
plot(fit1)
#run this code a couple of times in a row to see the next plot


# Residuenanalysen #
par (mfrow= c (3, 2))
qqnorm (resid (fit1))
scatter.smooth (fitted (fit1), resid (fit1))
boxplot (split (resid (fit1), lat [, 'trt']))
boxplot (split (resid (fit1), lat[, 'woa']))

leveneTest(barrier_index ~ trt * woa, data=lat)


#normal distribution
ggqqplot(lat,"barrier_index", ggtheme =theme_bw()) +facet_grid(trt ~ woa)

library(stats)
summary(lmb1 <- lm(barrier_index ~ lat$trt * lat$woa, data = lat))
#Call:
#  lm(formula = barrier_index ~ lat$trt * lat$woa, data = lat)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-1.2467 -0.5800 -0.1000  0.6133  1.3133 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)          2.467e-01  1.919e-01   1.285  0.20088   
#lat$trtCON          -1.586e-16  2.715e-01   0.000  1.00000   
#lat$trtFW           -5.600e-01  2.715e-01  -2.063  0.04096 * 
#  lat$trtL            -5.600e-01  2.715e-01  -2.063  0.04096 * 
#  lat$trtLIT          -7.800e-01  2.715e-01  -2.873  0.00469 **
#  lat$woa7            -4.000e-02  2.715e-01  -0.147  0.88306   
#lat$trtCON:lat$woa7  4.000e-02  3.839e-01   0.104  0.91716   
#lat$trtFW:lat$woa7   2.333e-01  3.839e-01   0.608  0.54430   
#lat$trtL:lat$woa7    1.533e-01  3.839e-01   0.399  0.69019   
#lat$trtLIT:lat$woa7  2.867e-01  3.839e-01   0.747  0.45647   
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.7434 on 140 degrees of freedom
#Multiple R-squared:  0.1307,	Adjusted R-squared:  0.0748 
#F-statistic: 2.338 on 9 and 140 DF,  p-value: 0.01738

slmb1 <- stepAIC(lmb1,direction="backward")                #lowest AIC value shows best model --> step_index ~ lat$trt + lat$woa 
#Start:  AIC=-79.3
#barrier_index ~ lat$trt * lat$woa

#Df Sum of Sq    RSS     AIC
#- lat$trt:lat$woa  4   0.44973 77.820 -86.435
#<none>                         77.371 -79.304

#Step:  AIC=-86.43
#barrier_index ~ lat$trt + lat$woa

#Df Sum of Sq    RSS     AIC
#- lat$woa  1    0.3953 78.216 -87.675
#<none>                 77.820 -86.435
#- lat$trt  4   10.7857 88.606 -74.965

#Step:  AIC=-87.67
#barrier_index ~ lat$trt

#Df Sum of Sq    RSS     AIC
#<none>                 78.216 -87.675
#- lat$trt  4    10.786 89.001 -76.297

summary(slmb1)
#Call:
#  lm(formula = barrier_index ~ lat$trt, data = lat)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.24667 -0.59000 -0.08333  0.67167  1.41000 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)   0.2267     0.1341   1.690  0.09310 . 
#lat$trtCON    0.0200     0.1896   0.105  0.91615   
#lat$trtFW    -0.4433     0.1896  -2.338  0.02076 * 
#  lat$trtL     -0.4833     0.1896  -2.549  0.01185 * 
#  lat$trtLIT   -0.6367     0.1896  -3.357  0.00101 **
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.7345 on 145 degrees of freedom
#Multiple R-squared:  0.1212,	Adjusted R-squared:  0.09694 
#F-statistic: 4.999 on 4 and 145 DF,  p-value: 0.0008378


slmb1$anova
#Stepwise Model Path 
#Analysis of Deviance Table

#Initial Model:
#  barrier_index ~ lat$trt * lat$woa

#Final Model:
#  barrier_index ~ lat$trt


#Step Df  Deviance Resid. Df Resid. Dev       AIC
#1                                      140   77.37067 -79.30414
#2 - lat$trt:lat$woa  4 0.4497333       144   77.82040 -86.43475
#3         - lat$woa  1 0.3952667       145   78.21567 -87.67480

kruskal.test(barrier_index ~ trt ,data=lat)
#Kruskal-Wallis rank sum test

#data:  barrier_index by trt
#Kruskal-Wallis chi-squared = 15.426, df = 4, p-value = 0.003894

kruskal.test(barrier_index ~ woa ,data=lat)



summary(lmb1 <- lm(barrier_index ~ lat$trt * lat$woa, data = lat))
#Call:
#  lm(formula = barrier_index ~ lat$trt * lat$woa, data = lat)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-1.2467 -0.5800 -0.1000  0.6133  1.3133 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)          2.467e-01  1.919e-01   1.285  0.20088   
#lat$trtCON          -1.586e-16  2.715e-01   0.000  1.00000   
#lat$trtFW           -5.600e-01  2.715e-01  -2.063  0.04096 * 
#  lat$trtL            -5.600e-01  2.715e-01  -2.063  0.04096 * 
#  lat$trtLIT          -7.800e-01  2.715e-01  -2.873  0.00469 **
#  lat$woa7            -4.000e-02  2.715e-01  -0.147  0.88306   
#lat$trtCON:lat$woa7  4.000e-02  3.839e-01   0.104  0.91716   
#lat$trtFW:lat$woa7   2.333e-01  3.839e-01   0.608  0.54430   
#lat$trtL:lat$woa7    1.533e-01  3.839e-01   0.399  0.69019   
#lat$trtLIT:lat$woa7  2.867e-01  3.839e-01   0.747  0.45647   
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.7434 on 140 degrees of freedom
#Multiple R-squared:  0.1307,	Adjusted R-squared:  0.0748 
#F-statistic: 2.338 on 9 and 140 DF,  p-value: 0.01738

summary(lm4 <- lm(barrier_index ~ lat$trt + lat$woa, data = lat))




