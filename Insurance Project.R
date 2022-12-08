# Insurance Project

Ins <- Insurance_factor_identification

View(Ins)

str(Ins)

# Convert the categorical variables in factor

#Km, Zone, Make convert them in factor

Ins$Kilometres <- as.factor(Ins$Kilometres)

Ins$Zone <- as.factor(Ins$Zone)

Ins$Make <- as.factor(Ins$Make)

str(Ins)
  
summary(Ins)

#Exploratory Data analysis

# Payment relationship with Bonus, claims and insured

plot(Ins$Insured,Ins$Payment)

plot(Ins$Bonus,Ins$Payment)

plot(Ins$Claims,Ins$Payment)

#Payment is related to the number of
#claims and the number of insured policy years

cor(Ins$Payment,Ins$Claims)

cor(Ins$Payment,Ins$Bonus)

cor(Ins$Payment,Ins$Insured)

library('caTools')

sel <- sample.split(Ins$Insured,SplitRatio = 0.7)

train_Ins <- subset(Ins,sel==TRUE)

test_Ins <- subset(Ins,sel==FALSE)

#linear regression

ins_model <- lm(Payment ~.,data = Ins)
summary(ins_model)

#for each var
h0:coeff=0
ha:coeff<>0

# P value is less than alpha so we will reject the null hypothesis
# P value <0.05, so we reject the h0;

# KM is significant

# Zone, make and Bonus are insignificant so we will not include
# them in the model

ins_model <- lm(Payment~Kilometres+Insured+Claims,data = Ins)
summary(ins_model)

#The insurance company is planning to establish a new branch office,
#so they are interested to find at what location, kilometer, and bonus
#level their insured amount, claims, and payment gets increased. (Hint: Aggregate Dataset) 

library(dplyr)
Ins %>% group_by(Zone) %>% summarise(avg_amt=mean(Insured),avg_claims=mean(Claims),
                                     avg_payment=mean(Payment))

Ins %>% group_by(Kilometres) %>% summarise(avg_amt=mean(Insured),avg_claims=mean(Claims),
                                     avg_payment=mean(Payment))

Ins %>% group_by(Bonus) %>% summarise(avg_amt=mean(Insured),avg_claims=mean(Claims),
                                           avg_payment=mean(Payment))

# The committee wants to understand what affects their claim rates so
#as to decide the right premiums for a certain set of situations.
#Hence, they need to find whether the insured amount, zone, kilometer,
#bonus, or make affects the claim rates and to what extent.

claim_model <- lm(Claims~Insured+Bonus+Make+ Kilometres,data = train_Ins)
summary(claim_model)
