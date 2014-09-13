# data quality indicators
# for population synthesis
# comparing synthetic (PopGen) and actual (ACS)
# created by Prof. Liming Wang and Kihong Kim
# created in 6/24/2013

setwd("/home/kihong/PopGen/Outputs")

library(sqldf)
library(reshape)

##################
# PopGen outputs #
##################

# synthetic households
household_synthesis <- read.csv("housing_synthetic_data.csv", sep=",", header=F)
household_synthesis_meta <- read.table("housing_synthetic_data_meta.txt", header=F)
names(household_synthesis) <- c(as.character(household_synthesis_meta$V4))

# synthetic persons
person_synthesis <- read.csv("person_synthetic_data.csv", sep=",", header=F)
person_synthesis_meta <- read.table("person_synthetic_data_meta.txt", header=F)
names(person_synthesis) <- c(as.character(person_synthesis_meta$V4))

# summary
summary_synthesis <- read.csv("summary.csv", sep=",", header=F, na.strings=c("\\N"))
summary_synthesis_meta <- read.table("summary_meta.txt", header=F)
names(summary_synthesis) <- c(as.character(summary_synthesis_meta$V4))


#################
# PopGen inputs #
#################

# household marginals (2007-2011 5-year ACS SF)
household_marginals <- read.csv("../Inputs/household_marginals.csv", sep=",", header=T)
household_marginals$num_households <- household_marginals$hh_children1 + household_marginals$hh_children2

# person marginals (2007-2011 5-year ACS SF)
person_marginals <- read.csv("../Inputs/person_marginals.csv", sep=",", header=T)
person_marginals$num_persons <- person_marginals$sex1 + person_marginals$sex2

# household sample (2007-2011 5-year ACS PUMS)
household_sample <- read.csv("../Inputs/household_sample.csv", sep=",", header=T)

# person sample (2007-2011 5-year ACS PUMS)
person_sample <- read.csv("../Inputs/person_sample.csv", sep=",", header=T)


#################################
# Data Quality Indicator (RMSE) #
#################################

# compute RMSE for numbers of households
rmse_h <- merge(summary_synthesis, household_marginals, by=c("state","county","tract","bg"))
plot(rmse_h$num_households, rmse_h$housing_syn_sum,
main="synthetic vs. actual\n (total number of households in block group)")
print(rmse.h <- round(sqrt(mean((rmse_h$housing_syn_sum-rmse_h$num_households)^2, na.rm=TRUE)),2))

# compute RMSE for numbers of persons
rmse_p <- merge(summary_synthesis, person_marginals, by=c("state","county","tract","bg"))
plot(rmse_p$num_persons, rmse_p$person_syn_sum,
main="synthetic vs. actual\n (total number of persons in block group)")
print(rmse.p <- round(sqrt(mean((rmse_p$person_syn_sum-rmse_p$num_persons)^2, na.rm=TRUE)),2))


###################################
# Data Quality Indicator (S-RMSE) #
###################################

# household-level S-RMSE
# for the joint distribution of household size and housing type
# by tract

# make dummies for household variables
household_synthesis$hh_children1 <- ifelse(household_synthesis$hh_children==1, 1, 0)
household_synthesis$hh_children2 <- ifelse(household_synthesis$hh_children==2, 1, 0)
household_synthesis$hh_income1 <- ifelse(household_synthesis$hh_income==1, 1, 0)
household_synthesis$hh_income2 <- ifelse(household_synthesis$hh_income==2, 1, 0)
household_synthesis$hh_income3 <- ifelse(household_synthesis$hh_income==3, 1, 0)
household_synthesis$hh_income4 <- ifelse(household_synthesis$hh_income==4, 1, 0)
household_synthesis$hh_income5 <- ifelse(household_synthesis$hh_income==5, 1, 0)
household_synthesis$hh_income6 <- ifelse(household_synthesis$hh_income==6, 1, 0)
household_synthesis$hh_income7 <- ifelse(household_synthesis$hh_income==7, 1, 0)
household_synthesis$hh_income8 <- ifelse(household_synthesis$hh_income==8, 1, 0)
household_synthesis$hh_size1 <- ifelse(household_synthesis$hh_size==1, 1, 0)
household_synthesis$hh_size2 <- ifelse(household_synthesis$hh_size==2, 1, 0)
household_synthesis$hh_size3 <- ifelse(household_synthesis$hh_size==3, 1, 0)
household_synthesis$hh_size4 <- ifelse(household_synthesis$hh_size==4, 1, 0)
household_synthesis$hh_size5 <- ifelse(household_synthesis$hh_size==5, 1, 0)
household_synthesis$hh_size6 <- ifelse(household_synthesis$hh_size==6, 1, 0)
household_synthesis$hh_size7 <- ifelse(household_synthesis$hh_size==7, 1, 0)
household_synthesis$housing_type1 <- ifelse(household_synthesis$housing_type==1, 1, 0)
household_synthesis$housing_type2 <- ifelse(household_synthesis$housing_type==2, 1, 0)
household_synthesis$housing_type3 <- ifelse(household_synthesis$housing_type==3, 1, 0)

# make dummies for the joint distributions of the hh_size variable and the housing_type variable
household_synthesis$hs1ht1 <- ifelse(household_synthesis$hh_size==1&household_synthesis$housing_type==1, 1, 0)
household_synthesis$hs1ht2 <- ifelse(household_synthesis$hh_size==1&household_synthesis$housing_type==2, 1, 0)
household_synthesis$hs1ht3 <- ifelse(household_synthesis$hh_size==1&household_synthesis$housing_type==3, 1, 0)
household_synthesis$hs2ht1 <- ifelse(household_synthesis$hh_size==2&household_synthesis$housing_type==1, 1, 0)
household_synthesis$hs2ht2 <- ifelse(household_synthesis$hh_size==2&household_synthesis$housing_type==2, 1, 0)
household_synthesis$hs2ht3 <- ifelse(household_synthesis$hh_size==2&household_synthesis$housing_type==3, 1, 0)
household_synthesis$hs3ht1 <- ifelse(household_synthesis$hh_size==3&household_synthesis$housing_type==1, 1, 0)
household_synthesis$hs3ht2 <- ifelse(household_synthesis$hh_size==3&household_synthesis$housing_type==2, 1, 0)
household_synthesis$hs3ht3 <- ifelse(household_synthesis$hh_size==3&household_synthesis$housing_type==3, 1, 0)
household_synthesis$hs4ht1 <- ifelse(household_synthesis$hh_size==4&household_synthesis$housing_type==1, 1, 0)
household_synthesis$hs4ht2 <- ifelse(household_synthesis$hh_size==4&household_synthesis$housing_type==2, 1, 0)
household_synthesis$hs4ht3 <- ifelse(household_synthesis$hh_size==4&household_synthesis$housing_type==3, 1, 0)
household_synthesis$hs5ht1 <- ifelse(household_synthesis$hh_size>=5&household_synthesis$housing_type==1, 1, 0)
household_synthesis$hs5ht2 <- ifelse(household_synthesis$hh_size>=5&household_synthesis$housing_type==2, 1, 0)
household_synthesis$hs5ht3 <- ifelse(household_synthesis$hh_size>=5&household_synthesis$housing_type==3, 1, 0)

# group the joint distrutions by state, county, and tract
joint_synthesis_hhsize_housingtype <- sqldf("select state, county, tract,
sum(hs1ht1*frequency) 'syn_size1_type1',
sum(hs1ht2*frequency) 'syn_size1_type2',
sum(hs1ht3*frequency) 'syn_size1_type3',
sum(hs2ht1*frequency) 'syn_size2_type1',
sum(hs2ht2*frequency) 'syn_size2_type2',
sum(hs2ht3*frequency) 'syn_size2_type3',
sum(hs3ht1*frequency) 'syn_size3_type1',
sum(hs3ht2*frequency) 'syn_size3_type2',
sum(hs3ht3*frequency) 'syn_size3_type3',
sum(hs4ht1*frequency) 'syn_size4_type1',
sum(hs4ht2*frequency) 'syn_size4_type2',
sum(hs4ht3*frequency) 'syn_size4_type3',
sum(hs5ht1*frequency) 'syn_size5_type1',
sum(hs5ht2*frequency) 'syn_size5_type2',
sum(hs5ht3*frequency) 'syn_size5_type3'
from household_synthesis group by state, county, tract")

# load the joint distribution from ACS SF
joint_marginals_hhsize_housingtype <- read.csv("../Inputs/ACSSF5yrs2011_tenure_hhsize_hstype_tract.csv", sep=",", header=T)
attach(joint_marginals_hhsize_housingtype)
joint_marginals_hhsize_housingtype$obs_size1_type1 <- B25124_004E+B25124_040E
joint_marginals_hhsize_housingtype$obs_size1_type2 <- B25124_005E+B25124_006E+B25124_007E+B25124_008E+B25124_041E+B25124_042E+B25124_043E+B25124_044E
joint_marginals_hhsize_housingtype$obs_size1_type3 <- B25124_009E+B25124_045E
joint_marginals_hhsize_housingtype$obs_size2_type1 <- B25124_011E+B25124_047E
joint_marginals_hhsize_housingtype$obs_size2_type2 <- B25124_012E+B25124_013E+B25124_014E+B25124_015E+B25124_048E+B25124_049E+B25124_050E+B25124_051E
joint_marginals_hhsize_housingtype$obs_size2_type3 <- B25124_016E+B25124_052E
joint_marginals_hhsize_housingtype$obs_size3_type1 <- B25124_018E+B25124_054E
joint_marginals_hhsize_housingtype$obs_size3_type2 <- B25124_019E+B25124_020E+B25124_021E+B25124_022E+B25124_055E+B25124_056E+B25124_057E+B25124_058E
joint_marginals_hhsize_housingtype$obs_size3_type3 <- B25124_023E+B25124_059E
joint_marginals_hhsize_housingtype$obs_size4_type1 <- B25124_025E+B25124_061E
joint_marginals_hhsize_housingtype$obs_size4_type2 <- B25124_026E+B25124_027E+B25124_028E+B25124_029E+B25124_062E+B25124_063E+B25124_064E+B25124_065E
joint_marginals_hhsize_housingtype$obs_size4_type3 <- B25124_030E+B25124_066E
joint_marginals_hhsize_housingtype$obs_size5_type1 <- B25124_032E+B25124_068E
joint_marginals_hhsize_housingtype$obs_size5_type2 <- B25124_033E+B25124_034E+B25124_035E+B25124_036E+B25124_069E+B25124_070E+B25124_071E+B25124_072E
joint_marginals_hhsize_housingtype$obs_size5_type3 <- B25124_037E+B25124_073E
detach(joint_marginals_hhsize_housingtype)

# merge joint_synthesis and joint_marginals
joint_marginals_hhsize_housingtype <- rename(joint_marginals_hhsize_housingtype, c(ST="state"))
joint_marginals_hhsize_housingtype <- rename(joint_marginals_hhsize_housingtype, c(COUNTY="county"))
joint_marginals_hhsize_housingtype <- rename(joint_marginals_hhsize_housingtype, c(TRACT="tract"))
merge_hhsize_housingtype_tract <- merge(joint_synthesis_hhsize_housingtype, joint_marginals_hhsize_housingtype, by=c("state","county","tract"))

# compute S-RMSE
attach(merge_hhsize_housingtype_tract)
numerator_hhsize_housingtype_tract <- sqrt(mean(
(syn_size1_type1-obs_size1_type1)^2+
(syn_size1_type2-obs_size1_type2)^2+
(syn_size1_type3-obs_size1_type3)^2+
(syn_size2_type1-obs_size2_type1)^2+
(syn_size2_type2-obs_size2_type2)^2+
(syn_size2_type3-obs_size2_type3)^2+
(syn_size3_type1-obs_size3_type1)^2+
(syn_size3_type2-obs_size3_type2)^2+
(syn_size3_type3-obs_size3_type3)^2+
(syn_size4_type1-obs_size4_type1)^2+
(syn_size4_type2-obs_size4_type2)^2+
(syn_size4_type3-obs_size4_type3)^2+
(syn_size5_type1-obs_size5_type1)^2+
(syn_size5_type2-obs_size5_type2)^2+
(syn_size5_type3-obs_size5_type3)^2))
denominator_hhsize_housingtype_tract <- mean(
obs_size1_type1+
obs_size1_type2+
obs_size1_type3+
obs_size2_type1+
obs_size2_type2+
obs_size2_type3+
obs_size3_type1+
obs_size3_type2+
obs_size3_type3+
obs_size4_type1+
obs_size4_type2+
obs_size4_type3+
obs_size5_type1+
obs_size5_type2+
obs_size5_type3)
print(srmse_hhsize_housingtype_tract <- numerator_hhsize_housingtype_tract/denominator_hhsize_housingtype_tract)
detach(merge_hhsize_housingtype_tract)

# person-level S-RMSE
# for the joint distribution of sex and age group
# by block group

# make dummies for person variables
person_synthesis$sex1 <- ifelse(person_synthesis$sex==1, 1, 0)
person_synthesis$sex2 <- ifelse(person_synthesis$sex==2, 1, 0)
person_synthesis$age_group1 <- ifelse(person_synthesis$age_group==1, 1, 0)
person_synthesis$age_group2 <- ifelse(person_synthesis$age_group==2, 1, 0)
person_synthesis$age_group3 <- ifelse(person_synthesis$age_group==3, 1, 0)
person_synthesis$age_group4 <- ifelse(person_synthesis$age_group==4, 1, 0)
person_synthesis$age_group5 <- ifelse(person_synthesis$age_group==5, 1, 0)
person_synthesis$age_group6 <- ifelse(person_synthesis$age_group==6, 1, 0)
person_synthesis$age_group7 <- ifelse(person_synthesis$age_group==7, 1, 0)

# make dummies for the joint distributions of the sex variable and the age_group variable
person_synthesis$s1a1 <- ifelse(person_synthesis$sex==1&person_synthesis$age_group==1, 1, 0)
person_synthesis$s1a2 <- ifelse(person_synthesis$sex==1&person_synthesis$age_group==2, 1, 0)
person_synthesis$s1a3 <- ifelse(person_synthesis$sex==1&person_synthesis$age_group==3, 1, 0)
person_synthesis$s1a4 <- ifelse(person_synthesis$sex==1&person_synthesis$age_group==4, 1, 0)
person_synthesis$s1a5 <- ifelse(person_synthesis$sex==1&person_synthesis$age_group==5, 1, 0)
person_synthesis$s1a6 <- ifelse(person_synthesis$sex==1&person_synthesis$age_group==6, 1, 0)
person_synthesis$s1a7 <- ifelse(person_synthesis$sex==1&person_synthesis$age_group==7, 1, 0)
person_synthesis$s2a1 <- ifelse(person_synthesis$sex==2&person_synthesis$age_group==1, 1, 0)
person_synthesis$s2a2 <- ifelse(person_synthesis$sex==2&person_synthesis$age_group==2, 1, 0)
person_synthesis$s2a3 <- ifelse(person_synthesis$sex==2&person_synthesis$age_group==3, 1, 0)
person_synthesis$s2a4 <- ifelse(person_synthesis$sex==2&person_synthesis$age_group==4, 1, 0)
person_synthesis$s2a5 <- ifelse(person_synthesis$sex==2&person_synthesis$age_group==5, 1, 0)
person_synthesis$s2a6 <- ifelse(person_synthesis$sex==2&person_synthesis$age_group==6, 1, 0)
person_synthesis$s2a7 <- ifelse(person_synthesis$sex==2&person_synthesis$age_group==7, 1, 0)

# group the joint distrutions by state, county, tract, and bg
joint_synthesis_sex_age_bg <- sqldf("select state, county, tract, bg,
sum(s1a1*frequency) 'syn_sex1_age1',
sum(s1a2*frequency) 'syn_sex1_age2',
sum(s1a3*frequency) 'syn_sex1_age3',
sum(s1a4*frequency) 'syn_sex1_age4',
sum(s1a5*frequency) 'syn_sex1_age5',
sum(s1a6*frequency) 'syn_sex1_age6',
sum(s1a7*frequency) 'syn_sex1_age7',
sum(s2a1*frequency) 'syn_sex2_age1',
sum(s2a2*frequency) 'syn_sex2_age2',
sum(s2a3*frequency) 'syn_sex2_age3',
sum(s2a4*frequency) 'syn_sex2_age4',
sum(s2a5*frequency) 'syn_sex2_age5',
sum(s2a6*frequency) 'syn_sex2_age6',
sum(s2a7*frequency) 'syn_sex2_age7'
from person_synthesis group by state, county, tract, bg")

# load the joint distribution from ACS SF
joint_marginals_sex_age_bg <- read.csv("../Inputs/ACSSF5yrs2011_sex_age_bg.csv", sep=",", header=T)
attach(joint_marginals_sex_age_bg)
joint_marginals_sex_age_bg$obs_sex1_age1 <- B01001_003E
joint_marginals_sex_age_bg$obs_sex1_age2 <- B01001_004E + B01001_005E
joint_marginals_sex_age_bg$obs_sex1_age3 <- B01001_006E
joint_marginals_sex_age_bg$obs_sex1_age4 <- B01001_007E + B01001_008E + B01001_009E + B01001_010E + B01001_011E
joint_marginals_sex_age_bg$obs_sex1_age5 <- B01001_012E + B01001_013E + B01001_014E + B01001_015E
joint_marginals_sex_age_bg$obs_sex1_age6 <- B01001_016E + B01001_017E + B01001_018E + B01001_019E
joint_marginals_sex_age_bg$obs_sex1_age7 <- B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E
joint_marginals_sex_age_bg$obs_sex2_age1 <- B01001_027E
joint_marginals_sex_age_bg$obs_sex2_age2 <- B01001_028E + B01001_029E
joint_marginals_sex_age_bg$obs_sex2_age3 <- B01001_030E
joint_marginals_sex_age_bg$obs_sex2_age4 <- B01001_031E + B01001_032E + B01001_033E + B01001_034E + B01001_035E
joint_marginals_sex_age_bg$obs_sex2_age5 <- B01001_036E + B01001_037E + B01001_038E + B01001_039E
joint_marginals_sex_age_bg$obs_sex2_age6 <- B01001_040E + B01001_041E + B01001_042E + B01001_043E
joint_marginals_sex_age_bg$obs_sex2_age7 <- B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E
detach(joint_marginals_sex_age_bg)

# merge joint_synthesis and joint_marginals
joint_marginals_sex_age_bg <- rename(joint_marginals_sex_age_bg, c(ST="state"))
joint_marginals_sex_age_bg <- rename(joint_marginals_sex_age_bg, c(COUNTY="county"))
joint_marginals_sex_age_bg <- rename(joint_marginals_sex_age_bg, c(TRACT="tract"))
joint_marginals_sex_age_bg <- rename(joint_marginals_sex_age_bg, c(BLKGRP="bg"))
merge_sex_age_bg <- merge(joint_synthesis_sex_age_bg, joint_marginals_sex_age_bg, by=c("state","county","tract","bg"))

# compute SRMSE
attach(merge_sex_age_bg)
numerator_sex_age_bg <- sqrt(mean(
(syn_sex1_age1-obs_sex1_age1)^2+
(syn_sex1_age2-obs_sex1_age2)^2+
(syn_sex1_age3-obs_sex1_age3)^2+
(syn_sex1_age4-obs_sex1_age4)^2+
(syn_sex1_age5-obs_sex1_age5)^2+
(syn_sex1_age6-obs_sex1_age6)^2+
(syn_sex1_age7-obs_sex1_age7)^2+
(syn_sex2_age1-obs_sex2_age1)^2+
(syn_sex2_age2-obs_sex2_age2)^2+
(syn_sex2_age3-obs_sex2_age3)^2+
(syn_sex2_age4-obs_sex2_age4)^2+
(syn_sex2_age5-obs_sex2_age5)^2+
(syn_sex2_age6-obs_sex2_age6)^2+
(syn_sex2_age7-obs_sex2_age7)^2))
denominator_sex_age_bg <- mean(
obs_sex1_age1+
obs_sex1_age2+
obs_sex1_age3+
obs_sex1_age4+
obs_sex1_age5+
obs_sex1_age6+
obs_sex1_age7+
obs_sex2_age1+
obs_sex2_age2+
obs_sex2_age3+
obs_sex2_age4+
obs_sex2_age5+
obs_sex2_age6+
obs_sex2_age7)
print(srmse_sex_age_bg <- numerator_sex_age_bg/denominator_sex_age_bg)
detach(merge_sex_age_bg)

# person-level S-RMSE
# for the joint distribution of sex and age group
# by tract

# group the joint distrutions by state, county, and tract
joint_synthesis_sex_age_tract <- sqldf("select state, county, tract,
sum(s1a1*frequency) 'syn_sex1_age1',
sum(s1a2*frequency) 'syn_sex1_age2',
sum(s1a3*frequency) 'syn_sex1_age3',
sum(s1a4*frequency) 'syn_sex1_age4',
sum(s1a5*frequency) 'syn_sex1_age5',
sum(s1a6*frequency) 'syn_sex1_age6',
sum(s1a7*frequency) 'syn_sex1_age7',
sum(s2a1*frequency) 'syn_sex2_age1',
sum(s2a2*frequency) 'syn_sex2_age2',
sum(s2a3*frequency) 'syn_sex2_age3',
sum(s2a4*frequency) 'syn_sex2_age4',
sum(s2a5*frequency) 'syn_sex2_age5',
sum(s2a6*frequency) 'syn_sex2_age6',
sum(s2a7*frequency) 'syn_sex2_age7'
from person_synthesis group by state, county, tract")

# load the joint distribution from ACS SF
joint_marginals_sex_age_tract <- sqldf("select state, county, tract,
sum(obs_sex1_age1) 'obs_sex1_age1',
sum(obs_sex1_age2) 'obs_sex1_age2',
sum(obs_sex1_age3) 'obs_sex1_age3',
sum(obs_sex1_age4) 'obs_sex1_age4',
sum(obs_sex1_age5) 'obs_sex1_age5',
sum(obs_sex1_age6) 'obs_sex1_age6',
sum(obs_sex1_age7) 'obs_sex1_age7',
sum(obs_sex2_age1) 'obs_sex2_age1',
sum(obs_sex2_age2) 'obs_sex2_age2',
sum(obs_sex2_age3) 'obs_sex2_age3',
sum(obs_sex2_age4) 'obs_sex2_age4',
sum(obs_sex2_age5) 'obs_sex2_age5',
sum(obs_sex2_age6) 'obs_sex2_age6',
sum(obs_sex2_age7) 'obs_sex2_age7'
from joint_marginals_sex_age_bg group by state,county,tract")

# to merge joint_synthesis and joint_marginals
merge_sex_age_tract <- merge(joint_synthesis_sex_age_tract, joint_marginals_sex_age_tract, by=c("state","county","tract"))

# to compute SRMSE
attach(merge_sex_age_tract)
numerator_sex_age_tract <- sqrt(mean(
(syn_sex1_age1-obs_sex1_age1)^2+
(syn_sex1_age2-obs_sex1_age2)^2+
(syn_sex1_age3-obs_sex1_age3)^2+
(syn_sex1_age4-obs_sex1_age4)^2+
(syn_sex1_age5-obs_sex1_age5)^2+
(syn_sex1_age6-obs_sex1_age6)^2+
(syn_sex1_age7-obs_sex1_age7)^2+
(syn_sex2_age1-obs_sex2_age1)^2+
(syn_sex2_age2-obs_sex2_age2)^2+
(syn_sex2_age3-obs_sex2_age3)^2+
(syn_sex2_age4-obs_sex2_age4)^2+
(syn_sex2_age5-obs_sex2_age5)^2+
(syn_sex2_age6-obs_sex2_age6)^2+
(syn_sex2_age7-obs_sex2_age7)^2))
denominator_sex_age_tract <- mean(
obs_sex1_age1+
obs_sex1_age2+
obs_sex1_age3+
obs_sex1_age4+
obs_sex1_age5+
obs_sex1_age6+
obs_sex1_age7+
obs_sex2_age1+
obs_sex2_age2+
obs_sex2_age3+
obs_sex2_age4+
obs_sex2_age5+
obs_sex2_age6+
obs_sex2_age7)
print(srmse_sex_age_tract <- numerator_sex_age_tract/denominator_sex_age_tract)
detach(merge_sex_age_tract)
