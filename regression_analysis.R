## Loading libraries
library(ggplot2)
library(e1071)
library(nnet)
library(reshape2)
library(caret)
library(car)
library(plm)
library(stargazer)
library(mlogit)
library(jtools)

### Reading in the dataset
f = "cleaned_data_v17.csv"
df_Mig <- read.table(f, sep = ",", header = T)
colnames(df_Mig) <- gsub("\\.", "", colnames(df_Mig))

### Plotting the proportion of each outcome category
barplot(prop.table(table(df_Mig$Migration))* 100)

### Checking and adjusting type of variables  
str(df_Mig)
df_Mig$Migration <- as.factor(df_Mig$Migration)
df_Mig$Internalmigration <- as.factor(df_Mig$Internalmigration)
df_Mig$Internationalmigration <- as.factor(df_Mig$Internationalmigration)
# df_Mig$kebele <- as.factor(df_Mig$kebele)

### Definition of the models (multinomial logit)
model_test <- multinom(Migration ~ AgeHHhead + Ethnicminority + Sexratio + 
                        Marriedfemales + Tempanomaly*zone + Precanomaly*zone,data=df_Mig)

# Total migration
model1 <- multinom(Migration ~ AgeHHhead + FemaleHHhead + SecondaryeducationHHhead + HHsize + Ethnicminority 
                   + Sexratio + Marriedfemales + Marriedmales + lnsavings1 + Nonfarmincome
                   + Tempanomaly +  Tempanomaly1 + Precanomaly + Precanomaly1 + zone, data=df_Mig)

# Internal migration
model2 <- multinom(Internalmigration ~ AgeHHhead + FemaleHHhead + SecondaryeducationHHhead + HHsize + Ethnicminority 
                   + Sexratio + Marriedfemales + Marriedmales + lnsavings1 + Nonfarmincome
                   + Tempanomaly +  Tempanomaly1 + Precanomaly + Precanomaly1 + zone, data=df_Mig)

# International migration
model3 <- multinom(Internationalmigration ~ AgeHHhead + FemaleHHhead + SecondaryeducationHHhead + HHsize + Ethnicminority 
                   + Sexratio + Marriedfemales + Marriedmales + lnsavings1 + Nonfarmincome
                   + Tempanomaly +  Tempanomaly1 + Precanomaly + Precanomaly1 + zone, data=df_Mig)

### Adding interaction terms (savings x weather shocks for all three model definitions)
model1_inter = multinom(Migration ~ AgeHHhead + FemaleHHhead + SecondaryeducationHHhead + HHsize + Ethnicminority 
                      + Sexratio + Marriedfemales + Marriedmales + lnsavings1 + Nonfarmincome
                      + Tempanomaly +  Tempanomaly1 + Precanomaly + Precanomaly1 
                      + Tempanomaly:lnsavings1 +  Tempanomaly1:lnsavings1 + Precanomaly:lnsavings1 + Precanomaly1:lnsavings1
                      + zone, data=df_Mig)

model2_inter = multinom(Internalmigration ~ AgeHHhead + FemaleHHhead + SecondaryeducationHHhead + HHsize + Ethnicminority 
                        + Sexratio + Marriedfemales + Marriedmales + lnsavings1 + Nonfarmincome
                        + Tempanomaly +  Tempanomaly1 + Precanomaly + Precanomaly1 
                        + Tempanomaly:lnsavings1 +  Tempanomaly1:lnsavings1 + Precanomaly:lnsavings1 + Precanomaly1:lnsavings1
                        + zone, data=df_Mig)

model3_inter = multinom(Internationalmigration ~ AgeHHhead + FemaleHHhead + SecondaryeducationHHhead + HHsize + Ethnicminority 
                        + Sexratio + Marriedfemales + Marriedmales + lnsavings1 + Nonfarmincome
                        + Tempanomaly +  Tempanomaly1 + Precanomaly + Precanomaly1 
                        + Tempanomaly:lnsavings1 +  Tempanomaly1:lnsavings1 + Precanomaly:lnsavings1 + Precanomaly1:lnsavings1
                        + zone, data=df_Mig)

### Testing alternative model definitions 
# First: without zone-specific fixed effects
# Second: only significant variables
# Third: only with intercept
model1_nozone <- multinom(Migration ~ AgeHHhead + FemaleHHhead + SecondaryeducationHHhead + HHsize + Ethnicminority 
                   + Sexratio + Marriedfemales + Marriedmales + lnsavings1 + Nonfarmincome
                   + Tempanomaly +  Tempanomaly1 + Precanomaly + Precanomaly1, data=df_Mig)

model1_sig <- multinom(Migration ~ AgeHHhead + FemaleHHhead + HHsize  
                        + Marriedmales + Nonfarmincome
                        + Tempanomaly +  Tempanomaly1 + Precanomaly + Precanomaly1 + zone, data=df_Mig)

OIM <- multinom(Migration ~ 1, data = df_Mig)

### Summarizing  the model results 
summary(model1)

broom::tidy(model1)
broom::tidy(model1_inter)
broom::tidy(model3)

### Anova
Anova(model1_sig)
Anova(model1_inter)
Anova(model3_inter)

# E.g. results of model 1 with interaction terms:
# Analysis of Deviance Table (Type II tests)
#                           LR Chisq Df Pr(>Chisq)    
#   AgeHHhead                  51.444  2  6.746e-12 ***
#   FemaleHHhead               64.184  2  1.155e-14 ***
#   SecondaryeducationHHhead    0.000  2  0.9997768    
#   HHsize                    109.037  2  < 2.2e-16 ***
#   Ethnicminority              2.521  2  0.2834510    
#   Sexratio                    3.821  2  0.1479939    
#   Marriedfemales              1.242  2  0.5373002    
#   Marriedmales               16.120  2  0.0003159 ***
#   lnsavings1                  4.200  2  0.1224565    
#   Nonfarmincome              13.482  2  0.0011813 ** 
#   Tempanomaly                 0.000  2  1.0000000    
#   Tempanomaly1                0.000  2  0.9999385    
#   Precanomaly                 0.000  2  1.0000000    
#   Precanomaly1                0.000  2  1.0000000    
#   zone                       18.369 16  0.3027280    
#   lnsavings1:Tempanomaly     10.931  2  0.0042308 **  --> interaction with savings significant
#   lnsavings1:Tempanomaly1     2.363  2  0.3068885    
#   lnsavings1:Precanomaly     12.361  2  0.0020693 **  --> interaction with savings significant
#   lnsavings1:Precanomaly1     0.421  2  0.8102093 


### Comparing the Akaike information criteria
AIC(OIM) # only with intercept: 2418.63
AIC(model1) # 2151.264
AIC(model1_sig) # 2146.35
AIC(model1_inter) # 2146.642
AIC(model2_inter) # 2157.805
AIC(model3_inter) # 1700.358 # model with international migration as dependent variable has the best fit


### Validation based on confusion matrix
test <- predict(model1,data=df_Mig)
real_pred <- data.frame(real=df_Mig$Migration,test=as.character(test))
cmat <- caret::confusionMatrix(real_pred$real,real_pred$test)

test <- predict(model3,data=df_Mig)
real_pred <- data.frame(real=df_Mig$Internationalmigration,test=as.character(test))
cmat <- caret::confusionMatrix(real_pred$real,real_pred$test)

# E.g. results for model 3: 
# Confusion Matrix and Statistics
# 
#             Reference
# Prediction   0   1   2
#          0 706   1  77
#          1  88   1  11
#          2 181   1 136
# 
# Overall Statistics
#                                           
#                Accuracy : 0.7013          
#                  95% CI : (0.6746, 0.7271)
#     No Information Rate : 0.8111          
#     P-Value [Acc > NIR] : 1               
#                   Kappa : 0.2913          
#  Mcnemar's Test P-Value : <2e-16          
# 
# Statistics by Class:
#   
#                       Class: 0  Class: 1 Class: 2
# Sensitivity            0.7241 0.3333333   0.6071
# Specificity            0.6564 0.9174312   0.8139
# Pos Pred Value         0.9005 0.0100000   0.4277
# Neg Pred Value         0.3565 0.9981851   0.9005
# Prevalence             0.8111 0.0024958   0.1864
# Detection Rate         0.5874 0.0008319   0.1131
# Detection Prevalence   0.6522 0.0831947   0.2646
# Balanced Accuracy      0.6902 0.6253823   0.7105


### Creating output tables for LaTex
pred_names <- c("Age (head)", "Female (head)", "Sec. education (head)",
                "HH size", "Ethnic minority",  "Sex ratio", "Married females", "Married males",
                "Savings", "Nonfarm income",
                "Temp. shocks (+)", "Temp. shocks (-)",
                "Precip. shocks (+)", "Precip. shocks (-)")

dep_names=c("Migrants (+)", "Migrants (-)")
all_dep_names=c("Internal (+)", "Internal (-)", "Internat.(+)", "Internat.(-)")

stargazer(model1, type="text", out = "R_migrants1.tex", 
          covariate.labels=pred_names, dep.var.labels=dep_names,
          nobs=TRUE, no.space=TRUE)

stargazer(model2, model3, type="text", out = "R_migrants23.tex", 
          covariate.labels=pred_names, dep.var.labels=all_dep_names,
          nobs=TRUE, omit.stat = "aic", no.space=TRUE)

stargazer(model1_inter, type="text", out = "R_migrants1_inter.tex", 
          covariate.labels=pred_names, dep.var.labels=dep_names,
          nobs=TRUE, omit.stat = "aic", no.space=TRUE)

stargazer(model2_inter, model3_inter, type="text", out = "R_migrants23_inter.tex", 
          covariate.labels=pred_names, dep.var.labels=all_dep_names,
          nobs=TRUE, omit.stat = "aic", no.space=TRUE)

