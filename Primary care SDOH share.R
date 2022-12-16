# Cost of addressing social risks via screening/referral from primary care
# 2022, Sanjay Basu
library(nhanesA)
library(survey)
library(ipumsr)
library(tidyverse)
library(readr)
library(mice)
library(MatchIt)
library(sociome)
library(tableone)
library(tidycensus)
library(gtsummary)
options(survey.lonely.psu="adjust")


# National Health and Nutrition Examination Survey, 2015-2016
demo <- nhanes('DEMO_I')
fsq <- nhanes('FSQ_I')
hiq <- nhanes('HIQ_I')
huq <- nhanes('HUQ_I')
dis <- nhanes('DLQ_I')
hosp <- nhanes('HUQ_I')
cond <- nhanes('MCQ_I')

# National Health and Nutrition Examination Survey, 2017-2018
demo2 <- nhanes('DEMO_J')
fsq2 <- nhanes('FSQ_J')
hiq2 <- nhanes('HIQ_J')
huq2 <- nhanes('HUQ_J')
dis2 <- nhanes('DLQ_J')
hosp2 <- nhanes('HUQ_J')
cond2 <- nhanes('MCQ_J')

# construct 2015-2018 NHANES sample
df = merge(demo,fsq)
df = merge(df,hiq)
df = merge(df,huq)
df = merge(df,dis)
df = merge(df,hosp)
df = merge(df,cond)
df2 = merge(demo2, fsq2)
df2 = merge(df2, hiq2)
df2 = merge(df2, huq2)
df2 = merge(df2, dis2)
df2 = merge(df2, hosp2)
df2 = merge(df2, cond2)

# age, sex, race/ethnicity, income, education, health insurance, usual source of care
df$age = df$RIDAGEYR
df$sex[df$RIAGENDR==1] = "Male"
df$sex[df$RIAGENDR==2] = "Female"
df$male = df$sex=="Male"
df$race[df$RIDRETH1==1 | df$RIDRETH1==2] = "Hispanic"
df$race[df$RIDRETH1==3] = "White"
df$race[df$RIDRETH1==4] = "Black"
df$race[df$RIDRETH1==5] = "Other"
df$white = df$race=="White"
df$black = df$race=="Black"
df$hisp = df$race=="Hispanic"
df$inc = df$INDFMPIR
df$preg = (df$RIDEXPRG==1)
df$childu5 = (df$DMDHHSZA>0)
df$educ[df$DMDEDUC2<3] = "Less than high school"
df$educ[df$DMDEDUC2==3] = "High school"
df$educ[df$DMDEDUC2==4] = "Some college"
df$educ[df$DMDEDUC2==5] = "College"
df$educ = as.factor(df$educ)
df$unins = 1*(df$HIQ031AA=="40") 
df$unins2 = 1*(df$HIQ011=="2")
df$mcaid = 1*(df$HIQ031D=="17")
df$mcare = 1*(df$HIQ031B=="15")
df$priv = 1*(df$HIQ031A=="14")
df$ins[df$unins==1 | df$unins2==1] = "Uninsured"
df$ins[df$mcaid==1] = "Medicaid"
df$ins[df$mcare==1] = "Medicare"
df$ins[df$priv==1] = "Private"
df$ins[df$ins==0] = "Uninsured"
df$ins = as.factor(df$ins)
df$pricare = df$HUQ030==1 & (df$HUQ041==1 | df$HUQ041==2 | df$HUQ041==4) # has usual source of care, and it's a clinic/outpt location
df$WTMEC4YR = df$WTINT2YR/2
df$prepshop = (df$DLQ080==1)
df$hosp = (df$HUQ071==1)
df$chrdz = (df$MCQ160B==1) +
  (df$MCQ160C==1) +
  (df$MCQ160E==1) +
  (df$MCQ160F==1) +
  (df$MCQ160G==1) +
  (df$MCQ160K==1) +
  (df$MCQ160O==1) 
df$chrdz= (df$chrdz>=2)



df2$age = df2$RIDAGEYR
df2$sex[df2$RIAGENDR==1] = "Male"
df2$sex[df2$RIAGENDR==2] = "Female"
df2$male = df2$sex=="Male"
df2$race[df2$RIDRETH1==1 | df2$RIDRETH1==2] = "Hispanic"
df2$race[df2$RIDRETH1==3] = "White"
df2$race[df2$RIDRETH1==4] = "Black"
df2$race[df2$RIDRETH1==5] = "Other"
df2$white = df2$race=="White"
df2$black = df2$race=="Black"
df2$hisp = df2$race=="Hispanic"
df2$inc = df2$INDFMPIR
df2$preg = (df2$RIDEXPRG==1)
df2$childu5 = (df2$DMDHHSZA>0)
df2$educ[df2$DMDEDUC2<3] = "Less than high school"
df2$educ[df2$DMDEDUC2==3] = "High school"
df2$educ[df2$DMDEDUC2==4] = "Some college"
df2$educ[df2$DMDEDUC2==5] = "College"
df2$educ = as.factor(df2$educ)
df2$unins = 1*(df2$HIQ031AA=="40") 
df2$unins2 = 1*(df2$HIQ011=="2")
df2$mcaid = 1*(df2$HIQ031D=="17")
df2$mcare = 1*(df2$HIQ031B=="15")
df2$priv = 1*(df2$HIQ031A=="14")
df2$ins[df2$unins==1 | df2$unins2==1] = "Uninsured"
df2$ins[df2$mcaid==1] = "Medicaid"
df2$ins[df2$mcare==1] = "Medicare"
df2$ins[df2$priv==1] = "Private"
df2$ins[df2$ins==0] = "Uninsured"
df2$ins = as.factor(df2$ins)
df2$pricare = df2$HUQ030==1 & (df2$HUQ041==1 | df2$HUQ041==2 | df2$HUQ041==4) # has usual source of care, and it's a clinic/outpt location
df2$WTMEC4YR = df2$WTINT2YR/2
df2$prepshop = (df2$DLQ080==1)
df2$hosp = (df2$HUQ071==1)
df2$chrdz = (df2$MCQ160B==1) +
  (df2$MCQ160C==1) +
  (df2$MCQ160E==1) +
  (df2$MCQ160F==1) +
  (df2$MCQ160G==1) +
  (df2$MCQ160K==1) +
  (df2$MCQ160O==1) 
df2$chrdz  = (df2$chrdz >=2)

# food, housing, transport insecurity
# household food insecurity and whether on SNAP/WIC currently
df$food[df$FSDHH==1] = "Full food security"
df$food[df$FSDHH==2] = "Marginal food security"
df$food[df$FSDHH==3] = "Low food security"
df$food[df$FSDHH==4] = "Very low food security"
df$foodins = df$food=="Low food security" | df$food=="Very low food security"
df$snap = df$FSD230==1
df$wic = df$FSD660ZC==1

df2$food[df2$FSDHH==1] = "Full food security"
df2$food[df2$FSDHH==2] = "Marginal food security"
df2$food[df2$FSDHH==3] = "Low food security"
df2$food[df2$FSDHH==4] = "Very low food security"
df2$foodins = df2$food=="Low food security" | df2$food=="Very low food security"
df2$snap = df2$FSD230==1
df2$snap = df2$snap=="Receiving SNAP"
df2$wic = df2$FSD660ZC==1

nhanes15 = df %>%
  select(age,male,white,black,hisp,inc,preg,childu5,educ,pricare,ins,prepshop,hosp,chrdz,foodins,snap,wic,WTMEC4YR,SDMVSTRA,SDMVPSU)

nhanes17 = df2 %>%
  select(age,male,white,black,hisp,inc,preg,childu5,educ,pricare,ins,prepshop,hosp,chrdz,foodins,snap,wic,WTMEC4YR,SDMVSTRA,SDMVPSU)

nhanes = rbind(nhanes15,nhanes17) %>%
  mutate(inAnalysis = pricare==1,
         baseset = 1)

NHANES_all = svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, 
          nest = TRUE, survey.lonely.psu = "adjust", data = nhanes)


sum(svytable(~male,design = NHANES_all))



# National Health Interview Survey, 2015-2018
# Lynn A. Blewett, Julia A. Rivera Drew, Miriam L. King and Kari C.W. Williams. IPUMS Health Surveys: National Health Interview Survey, Version 6.4 [dataset]. Minneapolis, MN: IPUMS, 2019. https://doi.org/10.18128/D070.V6.4
setwd("~/Downloads")
ddi <- read_ipums_ddi("nhis_00007.xml")
df3 <- read_ipums_micro(ddi)

# housing insecurity and in a rent subsidy program enrollment, via NHIS data; 
# "Type of living quarters": housing unit is not house, apartment, flat or condo, and not mobile home and not student dorm and not un-ascertained (leaving transient/hotel and non-permanent/other)
df3$house[df3$LIVINGQTR!= 21 & df3$LIVINGQTR!=34 & df3$LIVINGQTR!=54 & df3$LIVINGQTR!=33 & df3$LIVINGQTR!=98] = "Low housing security"
df3$house[is.na(df3$house)] = "Moderate or high housing security"

# "Family pays lower rent due to government program"
df3$rent[df3$LOWRENT==2] = "Receiving government housing assistance"
df3$rent[is.na(df3$rent)] = "Not receiving government housing assistance"

# transportation insecurity, via NHIS data; 
# "There are many reasons people delay getting medical care. Have you delayed getting care for any of the following reasons in the past 12 months? . . . you didn’t have transportation?” 
df3$trans[df3$DELAYTRANS==2] = "Low transportation security"
df3$trans[is.na(df3$trans)] = "Moderate or high transportation security"

# label NHIS demographics data
df3$age = df3$AGE
df3$male = (df3$SEX==1)
df3$white = (df3$RACEA==100)
df3$black = (df3$RACEA==200)
df3$hisp = (df3$HISPETH!=10)
df3$inc = df3$POVIMPPOINT1
df3$educ[(df3$EDUC==400 | df3$EDUC==500 | df3$EDUC==501 | df3$EDUC==502 | df3$EDUC==503 | df3$EDUC==504)] = "College"
df3$educ[(df3$EDUC==200) | (df3$EDUC==201)] = "High school"
df3$educ[(df3$EDUC<200)] = "Less than high school"
df3$educ[(df3$EDUC==300) | (df3$EDUC==301)] = "Some college"
df3$educ = as.factor(df3$educ)
df3$pricare = (df3$PLACECAR==20 |
                 df3$PLACECAR==21 | 
                 df3$PLACECAR==22 | 
                 df3$PLACECAR==23 | 
                 df3$PLACECAR==24 | 
                 df3$PLACECAR==21 | 
                 df3$PLACECAR==26 | 
                 df3$PLACECAR==30 | 
                 df3$PLACECAR==31 | 
                 df3$PLACECAR==32) 
df3$ins[df3$HIPRIVATEE==2 | df3$HIPRIVATEE==3] = "Private"
df3$ins[df3$HICHIPE==20 | df3$HICHIPE==21| df3$HICHIPE==22| df3$HIMCAIDE==2 | df3$HIMCAIDE==3]  = "Medicaid"
df3$ins[df3$HIMCAREE==2 | df3$HIMCAREE==3]  = "Medicare"
df3$ins[is.na(df3$ins)] = "Uninsured"
df3$ins = as.factor(df3$ins)
nhis15 = df3 %>%
  select(age,male,white,black,hisp,inc,educ,pricare,ins,house,rent,trans,NHISPID,STRATA,PSU,PERWEIGHT)

# define subset usually going to primary care, construct survey weights
nhis = (nhis15) %>%
  mutate(inAnalysis = pricare==1,
         yearwt = PERWEIGHT/4,
         baseset = 0)

NHIS_all<-svydesign(id = ~NHISPID, strata = ~STRATA, weight = ~yearwt, data = nhis,nest=TRUE)

sum(svytable(~male,design = NHIS_all))

# Create a survey design object for the subset of interest: adults aged 20 and over with a valid depression score 
# Subsetting the original survey design object ensures we keep the design information about the number of clusters and strata
NHIS <- subset(NHIS_all, inAnalysis)

# create a sample with blank food insecurity variables to match demographics with NHANES and replace food insecurity variables with NHANES values
nhis_alt = nhis %>%
  mutate(WTMEC4YR=0,
         SDMVSTRA=0,
         SDMVPSU=0,
         preg=0,
         childu5=0,
         prepshop=0,
         hosp=0,
         chrdz=0,
         foodins = 0,
         snap = 0,
         wic = 0) %>%
  select(age,male,white,black,hisp,inc,preg,childu5,educ,pricare,ins,prepshop,hosp,chrdz,foodins,snap,wic,house,rent,trans,WTMEC4YR,SDMVSTRA,SDMVPSU,baseset)

# similarly, create a nhanes sample with blank housing/transport variables to match demographics with NHIS and impute housing/transport variables from NHIS values
alldat = nhanes %>%
  mutate(house=0,
         rent=0,
         trans=0) %>%
  select(age,male,white,black,hisp,inc,preg,childu5,educ,pricare,ins,prepshop,hosp,chrdz,foodins,snap,wic,house,rent,trans,WTMEC4YR,SDMVSTRA,SDMVPSU,baseset) %>%
  rbind(nhis_alt) %>%
  filter(pricare==1)




# multiple imputation with chained equations
set.seed(1)
completedata = mice(alldat,1)
completedData <- complete(completedata,1)

m.out1 <- matchit(baseset ~ age+male+white+black+hisp+inc+educ+ins,
                  data = completedData, replace = T)
summary(m.out1)
g.matches1 <- get_matches(m.out1, data = completedData)

recon = g.matches1


# impute missing values from matched comparison data [nhis for nhanes values]
for (i in 1:dim(g.matches1)[1]){
  recon[i,'house'][recon[i,'baseset']==1] = recon[i+1,'house']
  recon[i,'rent'][recon[i,'baseset']==1] = recon[i+1,'rent']
  recon[i,'trans'][recon[i,'baseset']==1] = recon[i+1,'trans']

}

# subset to those going to primary care
recon_sub = recon %>%
  filter(baseset==1) %>%
  select(house,rent,trans)

df = nhanes %>%
  filter(pricare==1)

df$house = recon_sub$house
df$rent = recon_sub$rent
df$trans = recon_sub$trans

set.seed(1)
completedata = mice(df,1)
df <- complete(completedata,1)


# get ADI data from census
census_api_key('526de8855a78f2fb4fa424b37031e1a775ad7a96',overwrite-T)

adis = get_adi(geography = "tract", year = 2018) 

valid_df = get_acs(geography = "tract", year = 2018, variables = c("B03001_003E",
                                                                    "B02001_002E",
                                                                    "B02001_003E",
                                                                    "B01001_001E",
                                                                    "B01001_026E",
                                                                    "B01001_003E",
                                                                    "B01001_004E",
                                                                    "B01001_005E",
                                                                    "B01001_006E",
                                                                    "B01001_027E",
                                                                    "B01001_028E",
                                                                    "B01001_029E",
                                                                    "B01001_030E",
                                                                    "B01001_020E",
                                                                    "B01001_021E",
                                                                    "B01001_022E",
                                                                    "B01001_023E",
                                                                    "B01001_024E",
                                                                    "B01001_025E",
                                                                    "B01001_044E",
                                                                    "B01001_045E",
                                                                    "B01001_046E",
                                                                    "B01001_047E",
                                                                    "B01001_048E",
                                                                    "B01001_049E",
                                                                    "B01001I_001E",
                                                                    "C27006_001E",
                                                                    "C27007_001E",
                                                                    "C27013_001E"))

valid_df = valid_df %>%
  select(!moe) %>%
  pivot_wider(names_from = variable, values_from = estimate) 

valid_df = valid_df %>%
  mutate(female = B01001_026/B01001_001,
         kids = (B01001_027+B01001_028+B01001_029+B01001_030+B01001_003+B01001_004+B01001_005+B01001_006)/B01001_001,
         older = (B01001_020+B01001_021+B01001_022+B01001_023+B01001_024+B01001_025+
                   B01001_044+B01001_045+B01001_046+B01001_047+B01001_048+B01001_049)/B01001_001,
         hisp = B01001I_001/B01001_001,
         white = B02001_002/B01001_001,
         black = B02001_003/B01001_001,
         mcare = C27006_001/B01001_001,
         mcaid = C27007_001/B01001_001,
         priv = C27013_001/B01001_001)

valid_df = left_join(adis,valid_df)

cutpoints = quantile(valid_df$ADI,na.rm=T)
valid_df$adi_quant = 1*(valid_df$ADI<=cutpoints[2])+
  2*(valid_df$ADI>cutpoints[2] & valid_df$ADI<=cutpoints[3])+
  3*(valid_df$ADI>cutpoints[3] & valid_df$ADI<=cutpoints[4])+
  4*(valid_df$ADI>cutpoints[4])

valid_df %>%
  tbl_summary(statistic = list(all_continuous() ~ "{mean} "), digits = list( ~ c(3)), by = "adi_quant",
                 include = c("female","kids","older","white","black","hisp","mcare","mcaid","priv"))





tract_pov = get_acs(geography = "tract", year = 2018, variables = "B17010_002") %>%
  mutate(belowpov = estimate) %>%
  select(-c(variable,moe,estimate))
tract_fam = get_acs(geography = "tract", year = 2018, variables = "B17010_001")  %>%
  mutate(families = estimate) %>%
  select(-c(variable,moe,estimate))
tract_povrate = left_join(tract_fam,tract_pov) %>%
  mutate(belowpovrate = belowpov/families) %>%
  select(-c(belowpov,families))
adis = left_join(adis,tract_povrate)


tract_inc = get_acs(geography = "tract", year = 2018, variables = "B05010_001") %>%
  mutate(inc = scale(estimate,center=T,scale=T)) %>%
  select(-c(variable,moe,estimate))
tract_priv = get_acs(geography = "tract", year = 2018, variables = "B27002_001") %>%
  mutate(priv = estimate) %>%
  select(-c(variable,moe,estimate))

adis = left_join(adis,tract_inc)

# impute ADI values from sociodemographics in the sample, centered, scaled and standardized
adi_mod = lm(ADI ~ inc , data = adis)
adi_df = df %>%
  mutate(inc = scale(inc,center=T,scale=T))
adi_df$adi = predict(adi_mod, adi_df)



# cut into quantiles
cutpoints = quantile(adi_df$adi,na.rm=T)
adi_df$adi_quant = 1*(adi_df$adi<=cutpoints[2])+
  2*(adi_df$adi>cutpoints[2] & adi_df$adi<=cutpoints[3])+
  3*(adi_df$adi>cutpoints[3] & adi_df$adi<=cutpoints[4])+
  4*(adi_df$adi>cutpoints[4])
df$adi_quant = adi_df$adi_quant


prac4cut = summary(adis$ADI[(adis$belowpovrate<0.2)])[4]
adi_df$prac = 0
adi_df$prac[adi_df$adi<prac4cut] = 4
adi_lo = table(adi_df$adi<prac4cut)[2]/sum(table(adi_df$adi<prac4cut))
set.seed(1)
for (i in 1:dim(df)[1]){
  # https://www.kff.org/other/state-indicator/chc-patients-by-payer-source/?dataView=1&currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
  adi_df$prac[i] = 3*(adi_df$prac[i]!=4)*rbinom(1,1,0.14/(1-adi_lo))+
    1*rbinom(1,1,.5)*(adi_df$prac[i]!=4 & adi_df$prac[i]!=3)*(adi_df$ins[i]=="Medicaid")+
    1*rbinom(1,1,.2)*(adi_df$prac[i]!=4 & adi_df$prac[i]!=3)*(adi_df$ins[i]=="Medicare")+
    1*rbinom(1,1,.1)*(adi_df$prac[i]!=4 & adi_df$prac[i]!=3)*(adi_df$ins[i]=="Private")+
    1*rbinom(1,1,.52)*(adi_df$prac[i]!=4 & adi_df$prac[i]!=3)*(adi_df$ins[i]=="Uninsured")
  adi_df$prac[adi_df$adi<prac4cut] = 4
  adi_df$prac[adi_df$prac==0] = 2
}
df$prac = adi_df$prac


# screening and referral costs
# $5 PMPM (95% CI: $4, $6)
set.seed(i)
df$screening_cost = rnorm(dim(df)[1], mean = 5, sd = (5-4)/1.96)
df$screening_cost[df$screening_cost<0]=0


# food insecurity interventions
# SNAP
# eligibility <130% FPL	
# Benefit $212 PMPM (95% CI: $183, $234)
# Overhead $441 once at enrollment (95% CI: $217, $945), w/ typical duration of enrollment of 12 mos https://www.chn.org/wp-content/uploads/2016/07/SNAP-Outcomes-2016-Update.pdf
# % of those eligible who enroll	83% (95% CI: 64%, 100%)
set.seed(i)
df$snap_elig = (df$inc<1.3)&(df$foodins==1)
df$snap_enroll = (df$snap_elig==1)*rbinom(dim(df)[1],1,0.83)
df$diff_snap_elig_enroll = df$snap_elig - df$snap_enroll
set.seed(i)
df$snap_cost = (df$snap_enroll==1)*rnorm(dim(df)[1], mean = (212+441/12), sd = (234+945/12-212-441/12)/1.96)
df$snap_cost[df$snap_cost<0]=0

# WIC	
# eligibility <185% FPL + pregnant or child under 5	Benefit 
# $39 PMPM (95% CI: $36, $41)
# Overhead $25 PMPM (95% CI: $23, $27)
# % of those eligible who enroll	57% (95% CI: 56%, 59%)
set.seed(i)
df$wic_elig = (df$inc<1.85)&((df$preg==1)|(df$childu5>0))&(df$foodins==1)
df$wic_enroll = (df$wic_elig==1)*rbinom(dim(df)[1],1,0.57)
df$diff_wic_elig_enroll = df$wic_elig - df$wic_enroll

set.seed(i)
df$wic_cost = (df$wic_enroll==1)*rnorm(dim(df)[1], mean = (39+25), sd = (41+27-39-25)/1.96) 
df$wic_cost[df$wic_cost<0]=0

# Child Nutrition Programs (School Breakfast/Lunch/etc)	
# eligibility <185% FPL	Benefit and overhead $51 PMPM (95% CI: $50, $53)
# % of those eligible who enroll	95% (95% CI: 83%, 100%)
set.seed(i)
df$lunch_elig = (df$inc<1.85)&(df$age<19)&(df$foodins==1)
df$lunch_enroll = (df$lunch_elig==1)*rbinom(dim(df)[1],1,0.95)
df$diff_lunch_elig_enroll = df$lunch_elig - df$lunch_enroll

set.seed(i)
df$lunch_cost = (df$lunch_enroll==1)*rnorm(dim(df)[1], mean = (51), sd = (53-51)/1.96) 
df$lunch_cost[df$lunch_cost<0]=0

# Emergency Food Assistance (food banks, food pantries, farmers market programs)	
# <400% FPL	Benefit and overhead $1.3 PMPM (95% CI: $1.2, $1.4)
# % of those eligible who enroll	27% (95% CI: 17%, 37%)
set.seed(i)
df$bank_elig = (df$inc<4)&(df$foodins==1)
df$bank_enroll = (df$bank_elig==1)*rbinom(dim(df)[1],1,0.27)
df$diff_bank_elig_enroll = df$bank_elig - df$bank_enroll

set.seed(i)
df$bank_cost = (df$bank_enroll==1)*rnorm(dim(df)[1], mean = (1.3), sd = (1.4-1.3)/1.96) 
df$bank_cost[df$bank_cost<0]=0

# Meal delivery service (non-medically-tailored)	
# 60+ years old and unable to prepare meals or shop for self, with a chronic condition or hospitalization in last year	
# Benefit and overhead $208 PMPM (95% CI: $149, $276)
# % of those eligible who enroll	68% (95% CI: 41%, 94%)
set.seed(i)
df$del_left = (df$age>=60)&(df$prepshop==1)&((df$chrdz==1)|(df$hosp==1))&(df$foodins==1)
df$del_enroll_left =  (df$del_left==1)*rbinom(dim(df)[1],1,0.68)
df$diff_del_elig_enroll_left = df$del_left - df$del_enroll_left

set.seed(i)
df$del_cost_left = (df$del_enroll_left==1)*rnorm(dim(df)[1], mean = 208, sd = (276-208)/1.96)
df$del_cost_left[df$del_cost_left<0]=0

# Medically-tailored meal delivery	
# Chronic conditions or a hospitalization in last year	
# Benefit and overhead $300 PMPM (95% CI: $208, $350)
# % of those eligible who enroll	68% (95% CI: 41%, 94%)
set.seed(i)
df$medmeal_waiver = (df$ins=='Medicaid')*rbinom(dim(df)[1],1,0.19) #oregon, mass, cali medicaid waivers are 19% of medicaid pop
df$medmeal_elig = (df$medmeal_waiver==1)*((df$chrdz==1)|(df$hosp==1))&(df$foodins==1)
set.seed(1)
df$medmeal_enroll =  (df$medmeal_elig==1)*rbinom(dim(df)[1],1,0.68)
df$diff_medmeal_elig_enroll = df$medmeal_elig - df$medmeal_enroll

set.seed(i)
df$medmeal_cost = (df$medmeal_enroll==1)*rnorm(dim(df)[1], mean = 300, sd = (350-300)/1.96)
df$medmeal_cost[df$medmeal_cost<0]=0


set.seed(i)
df$medmeal_left = ((df$medmeal_waiver==0)|(df$medmeal_waiver==1 & df$medmeal_enroll==0))*(df$chrdz==1)|(df$hosp==1)&(df$foodins==1)
set.seed(i)
df$medmeal_enroll_left =  (df$medmeal_left==1)*rbinom(dim(df)[1],1,0.68)
df$diff_medmeal_elig_enroll_left = df$medmeal_left - df$medmeal_enroll_left

set.seed(i)
df$medmeal_cost_left = (df$medmeal_enroll_left==1)*rnorm(dim(df)[1], mean = 300, sd = (350-300)/1.96)
df$medmeal_cost_left[df$medmeal_cost_left<0]=0


# federally funded versus not, accounting

df$food_elig_paid = (df$snap_elig==1)|
  (df$wic_elig==1)|
  (df$lunch_elig==1)|
  (df$bank_elig==1)|
  (df$medmeal_elig==1)


df$food_enroll_paid = (df$snap_enroll==1)|
  (df$wic_enroll==1)|
  (df$lunch_enroll==1)|
  (df$bank_enroll==1)|
  (df$medmeal_enroll==1)


df$diff_food_elig_enroll_paid = df$food_elig_paid-df$food_enroll_paid


df$food_cost_paid = (df$snap_cost)+
  (df$wic_cost)+
  (df$lunch_cost)+
  (df$bank_cost)+
  (df$medmeal_cost)

# Food vouchers	
# Moderate v severe food insecurity	
# Benefit and overhead $21 PMPM (95% CI: $20, $22)
# % of those eligible who enroll	31% (95% CI: 21%, 41%)
set.seed(i)
df$foodvou_left = (df$foodins==1)*(df$del_enroll_left==0)*(df$food_enroll_paid==0)
df$foodvou_enroll_left = (df$foodvou_left==1)* rbinom(dim(df)[1],1,0.31)
df$diff_foodvou_elig_enroll_left = df$foodvou_left - df$foodvou_enroll_left

set.seed(i)
df$foodvou_cost_left = (df$foodvou_enroll_left==1)*rnorm(dim(df)[1], mean = 21, sd = (22-21)/1.96)
df$foodvou_cost_left[df$foodvou_cost_left<0]=0


df$food_cost_unpaid = (df$del_cost_left)+
  (df$medmeal_cost_left)+
  (df$foodvou_cost_left)

df$food_cost = df$food_cost_paid+df$food_cost_unpaid

df$food_elig_any = (df$snap_elig==1)|
  (df$wic_elig==1)|
  (df$lunch_elig==1)|
  (df$bank_elig==1)|
  (df$medmeal_elig==1)|
  (df$del_left==1)|
  (df$medmeal_left==1)|
  (df$foodvou_left==1)


df$food_enroll_any = (df$snap_enroll==1)|
  (df$wic_enroll==1)|
  (df$lunch_enroll==1)|
  (df$bank_enroll==1)|
  (df$medmeal_enroll==1)|
  (df$del_enroll_left==1)|
  (df$medmeal_enroll_left==1)|
  (df$foodvou_enroll_left==1)


df$diff_food_elig_enroll_any = df$food_elig_any-df$food_enroll_any



# Housing vouchers (section 8 Housing Choice ouchers, Rental Assistance)	
# <50% median income 
# Benefit and overhead $1163 PMPM (95% CI: $667, $1659)
# % of those eligible who enroll	25% (95% CI: 10%, 40%)
set.seed(i)
df$sec8_elig = (df$inc<svyquantile(~inc,NHANES_all,c(0.5),ci=F)$inc[1])&(df$house=='Low housing security')
df$sec8_enroll = (df$sec8_elig==1)*rbinom(dim(df)[1],1,0.25)
df$diff_sec8_elig_enroll = df$sec8_elig - df$sec8_enroll

set.seed(i)
df$sec8_cost = (df$sec8_enroll==1)*rnorm(dim(df)[1], mean = 1163, sd = (1659-1163)/1.96)
df$sec8_cost[df$sec8_cost<0]=0

# HUD housing (Public Housing Operating Fund, Public Housing Capital Fund)	
# <50% median income, >65 years old, or having a disability	
# Benefits and overhead $430 (95% CI: $407, $457)
# % of those eligible who enroll	24% (95% CI: 23%, 25%)
set.seed(i)
df$hud_elig =((df$age>=65) | (df$prepshop==1) | (df$inc<svyquantile(~inc,NHANES_all,c(0.5),ci=F)$inc[1]))&(df$house=='Low housing security')
df$hud_enroll = (df$hud_elig==1)*rbinom(dim(df)[1],1,0.24)
df$diff_hud_elig_enroll = df$hud_elig - df$hud_enroll

set.seed(i)
df$hud_cost = (df$hud_enroll==1)*rnorm(dim(df)[1], mean = 430, sd = (457-430)/1.96)
df$hud_cost[df$hud_cost<0]=0

# federally funded versus not, accounting
df$house_elig_paid = (df$sec8_elig==1)|
  (df$hud_elig==1)

df$house_enroll_paid = (df$sec8_enroll==1)|
  (df$hud_enroll==1)

df$diff_house_elig_enroll_paid = df$house_elig_paid-df$house_enroll_paid


# Housing first subsidy 	
# Housing insecurity	
# Benefit and overhead $673 PMPM (95% CI: $661, $685)
# % of those eligible who enroll	90% (95% CI: 86%, 93%)
set.seed(1)
df$hf_left =(df$house=='Low housing security')*(df$house_enroll_paid==0)
df$hf_enroll_left = (df$hf_left==1)*rbinom(dim(df)[1],1,0.90)
df$diff_hf_elig_enroll_left = df$hf_left - df$hf_enroll_left

set.seed(i)
df$hf_cost_left = (df$hf_enroll_left==1)*rnorm(dim(df)[1], mean = 673, sd = (673-661)/1.96)
df$hf_cost_left[df$hf_cost_left<0]=0


df$house_elig_any = (df$sec8_elig==1)|
  (df$hud_elig==1)|
  (df$hf_left==1)

df$house_enroll_any = (df$sec8_enroll==1)|
  (df$hud_enroll==1)|
  (df$hf_enroll_left==1)

df$diff_house_elig_enroll_any = df$house_elig_any-df$house_enroll_any


df$house_cost_paid = df$sec8_cost + df$hud_cost
df$house_cost_unpaid = df$hf_cost_left
df$house_cost = df$house_cost_paid + df$house_cost_unpaid

df$diff_house_elig_enroll_any = (df$house_elig_paid|df$hf_left)-(df$house_enroll_paid|df$hf_enroll_left)




#Non-emergency medical transport	
#Medicare Advantage (48% of Medicare) or Medicaid	
# Benefit and overhead $104 PMPM (95% CI: $7, $599)
#% of those eligible who enroll	7% (95% Ci: 1%, 30%)
set.seed(i)
df$mcare_adv = rbinom(dim(df)[1],1,0.48)
set.seed(i)
df$nemt_elig = (((df$ins=='Medicare')&(df$mcare_adv==1))|(df$ins=='Medicaid'))&(df$trans=='Low transportation security')
set.seed(i)
df$nemt_enroll =  (df$nemt_elig==1)*rbinom(dim(df)[1],1,0.07)
df$diff_nemt_elig_enroll = df$nemt_elig - df$nemt_enroll

set.seed(i)
df$nemt_cost = (df$nemt_enroll==1)*rnorm(dim(df)[1], mean = 104, sd = (104-7)/1.96)
df$nemt_cost[df$nemt_cost<0]=0

set.seed(i)
df$nemt_left = (df$trans=='Low transportation security') & (df$nemt_enroll==0)
set.seed(i)
df$nemt_enroll_left = (df$nemt_left==1)*rbinom(dim(df)[1],1,0.07)
df$diff_nemt_elig_enroll_left = df$nemt_left - df$nemt_enroll_left

set.seed(i)
df$nemt_cost_left = (df$nemt_enroll_left==1)*rnorm(dim(df)[1], mean = 104, sd = (104-7)/1.96)
df$nemt_cost_left[df$nemt_cost_left<0]=0

df$diff_nemt_elig_enroll_paid = df$nemt_elig-df$nemt_enroll

df$nemt_cost_paid = df$nemt_cost
df$nemt_cost_unpaid = df$nemt_cost_left
df$nemt_cost_any = df$nemt_cost_paid + df$nemt_cost_unpaid

df$nemt_elig_any = ((df$nemt_elig) | (df$nemt_left))
df$nemt_enroll_any = (df$nemt_enroll|df$nemt_enroll_left)

df$diff_nemt_elig_enroll_any = (df$nemt_elig|df$nemt_left)-(df$nemt_enroll|df$nemt_enroll_left)


# CHW care coordination	
# Chronic conditions or a hospitalization in last year	
# Benefit and overhead $194 PMPM (95% CI: $64, $686)
# % of those eligible who enroll	38% (95% CI: 36%, 40%)
df$cc_elig = ((df$chrdz==1)|(df$hosp==1))
set.seed(i)
df$chw_waiver = (df$ins=='Medicaid')*rbinom(dim(df)[1],1,0.42) #42% coverage for CHW programs across state Medicaid plans, https://www.macpac.gov/wp-content/uploads/2022/04/Medicaid-coverage-of-community-health-worker-services-1.pdf
df$chw_elig = (df$chw_waiver==1)*((df$chrdz==1)|(df$hosp==1))
set.seed(1)
df$chw_enroll = (df$chw_elig==1)*rbinom(dim(df)[1],1,0.38)
df$diff_chw_elig_enroll = df$chw_elig - df$chw_enroll

set.seed(i)
df$chw_cost = (df$chw_enroll==1)*rnorm(dim(df)[1], mean = 194, sd = (194-64)/1.96)
df$chw_cost[df$chw_cost<0]=0


set.seed(i)
df$chw_left = ((df$chw_waiver==0)|(df$chw_waiver==1 & df$chw_enroll==0))*((df$chrdz==1)|(df$hosp==1))
set.seed(i)
df$chw_enroll_left = (df$chw_left==1)*rbinom(dim(df)[1],1,0.38)
df$diff_chw_elig_enroll_left = df$chw_left - df$chw_enroll_left

set.seed(i)
df$chw_cost_left = (df$chw_enroll_left==1)*rnorm(dim(df)[1], mean = 194, sd = (194-64)/1.96)
df$chw_cost_left[df$chw_cost_left<0]=0

df$diff_chw_elig_enroll_paid = df$chw_elig-df$chw_enroll

df$chw_cost_paid = df$chw_cost
df$chw_cost_unpaid = df$chw_cost_left
df$chw_cost_any = df$chw_cost_paid + df$chw_cost_unpaid

df$chw_elig_any = (df$chw_elig|df$chw_left)
df$chw_enroll_any = (df$chw_enroll|df$chw_enroll_left)
df$diff_chw_elig_enroll_any = (df$chw_elig|df$chw_left)-(df$chw_enroll|df$chw_enroll_left)


df$tot_cost_unpaid = df$screening_cost + df$food_cost_unpaid + df$house_cost_unpaid + df$nemt_cost_unpaid + df$chw_cost_unpaid
df$tot_cost_paid = df$food_cost_paid + df$house_cost_paid + df$nemt_cost_paid + df$chw_cost_paid
df$tot_cost = df$tot_cost_unpaid + df$tot_cost_paid

# final imputation and dataset construction

set.seed(i)
completedf = mice(df,1)
completedDf <- complete(completedf,1)

final_df = svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, 
                     nest = TRUE, survey.lonely.psu = "adjust", data = completedDf)




# demographics and social needs prevalence table
final_df %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), include = c("male","white","black","hisp", "educ","ins","prac","adi_quant","foodins","house","trans","cc_elig"))

final_df %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "prac",include = c("male","white","black","hisp","educ","ins","prac","adi_quant","foodins","house","trans","cc_elig"))

final_df %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "adi_quant",include = c("male","white","black","hisp","educ","ins","prac","adi_quant","foodins","house","trans","cc_elig"))





##### eligibility #####
# eligibility table

# food programs
# overall
survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), include = c("snap_elig","wic_elig","lunch_elig","bank_elig","medmeal_elig","food_elig_paid","del_left","medmeal_left","foodvou_left","food_elig_any")) %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

# by practice
survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "prac",  include = c("snap_elig","wic_elig","lunch_elig","bank_elig","medmeal_elig","food_elig_paid","del_left","medmeal_left","foodvou_left","food_elig_any")) %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

# by ADI quartile
survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "adi_quant",  include = c("snap_elig","wic_elig","lunch_elig","bank_elig","medmeal_elig","food_elig_paid","del_left","medmeal_left","foodvou_left","food_elig_any")) %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 


# housing programs
survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), include = c("sec8_elig","hud_elig","house_elig_paid","hf_left","house_elig_any")) %>%
  modify_footnote(update = everything() ~ NA) 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "prac",  include = c("sec8_elig","hud_elig","house_elig_paid","hf_left","house_elig_any")) %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "adi_quant",  include = c("sec8_elig","hud_elig","house_elig_paid","hf_left","house_elig_any")) %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 


# transport programs
survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), include = c("nemt_elig","nemt_left","nemt_elig_any")) %>%
  modify_footnote(update = everything() ~ NA) 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "prac",  include = c("nemt_elig","nemt_left","nemt_elig_any")) %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "adi_quant",  include = c("nemt_elig","nemt_left","nemt_elig_any")) %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 


# care coordination programs

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), include = c("chw_elig","chw_left","chw_elig_any")) %>%
  modify_footnote(update = everything() ~ NA) 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "prac",  include = c("chw_elig","chw_left","chw_elig_any")) %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "adi_quant",  include = c("chw_elig","chw_left","chw_elig_any")) %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 



# confidence intervals, by program, by prac and by ADI quart

# food programs
svyciprop(~I(snap_elig == 1), subset(final_df, foodins==1))
svyciprop(~I(wic_elig == 1), subset(final_df, foodins==1))
svyciprop(~I(lunch_elig == 1), subset(final_df, foodins==1))
svyciprop(~I(bank_elig == 1), subset(final_df, foodins==1))
svyciprop(~I(medmeal_elig == 1), subset(final_df, foodins==1))
svyciprop(~I(food_elig_paid == 1), subset(final_df, foodins==1))
svyciprop(~I(del_left == 1), subset(final_df, foodins==1))
svyciprop(~I(medmeal_left == 1), subset(final_df, foodins==1))
svyciprop(~I(foodvou_left == 1), subset(final_df, foodins==1))
svyciprop(~I(food_elig_any == 1), subset(final_df, foodins==1))


svyby(~snap_elig, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~snap_elig, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~wic_elig, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~wic_elig, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~lunch_elig, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~lunch_elig, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~bank_elig, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~bank_elig, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~medmeal_elig, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~medmeal_elig, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~food_elig_paid, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~food_elig_paid, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~del_left, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~del_left, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~medmeal_left, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~medmeal_left, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~foodvou_left, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~foodvou_left, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~food_elig_any, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~food_elig_any, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")


# housing programs
svyciprop(~I(sec8_elig == 1), subset(final_df, house =='Low housing security'))
svyciprop(~I(hud_elig == 1), subset(final_df, house =='Low housing security'))
svyciprop(~I(house_elig_paid == 1), subset(final_df, house =='Low housing security'))
svyciprop(~I(hf_left == 1), subset(final_df, house =='Low housing security'))
svyciprop(~I(house_elig_any == 1), subset(final_df, house =='Low housing security'))

svyby(~sec8_elig, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop,  level=0.95, method = "mean")
svyby(~sec8_elig, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")

svyby(~hud_elig, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")
svyby(~hud_elig, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")

svyby(~house_elig_paid, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")
svyby(~house_elig_paid, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")

svyby(~hf_left, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")
svyby(~hf_left, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")

svyby(~house_elig_any, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")
svyby(~house_elig_any, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")


# transportation program
svyciprop(~I(nemt_elig == 1), subset(final_df, trans =='Low transportation security'))
svyciprop(~I(nemt_left == 1), subset(final_df, trans =='Low transportation security'))
svyciprop(~I(nemt_elig_any == 1), subset(final_df, trans =='Low transportation security'))


svyby(~nemt_elig, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')), svyciprop,  level=0.95, method = "mean")
svyby(~nemt_elig, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')), svyciprop, level=0.95, method = "mean")

svyby(~nemt_left, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')), svyciprop, level=0.95, method = "mean")
svyby(~nemt_left, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')), svyciprop, level=0.95, method = "mean")

svyby(~nemt_elig_any, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')), svyciprop, level=0.95, method = "mean")
svyby(~nemt_elig_any, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')), svyciprop, level=0.95, method = "mean")


# care coordination program
svyciprop(~I(chw_elig == 1), subset(final_df, cc_elig==1))
svyciprop(~I(chw_left == 1), subset(final_df, cc_elig==1))
svyciprop(~I(chw_elig_any == 1), subset(final_df, cc_elig==1))


svyby(~chw_elig, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)), svyciprop,  level=0.95, method = "mean")
svyby(~chw_elig, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)), svyciprop, level=0.95, method = "mean")

svyby(~chw_left, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)), svyciprop, level=0.95, method = "mean")
svyby(~chw_left, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)), svyciprop, level=0.95, method = "mean")

svyby(~chw_elig_any, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)), svyciprop, level=0.95, method = "mean")
svyby(~chw_elig_any, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)), svyciprop, level=0.95, method = "mean")





##### enrollment #####


# enrollment table

# food programs
survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), include = c("snap_enroll","wic_enroll","lunch_enroll","bank_enroll","medmeal_enroll","food_enroll_paid","del_enroll_left","medmeal_enroll_left","foodvou_enroll_left","food_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA)

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "prac",  include = c("snap_enroll","wic_enroll","lunch_enroll","bank_enroll","medmeal_enroll","food_enroll_paid","del_enroll_left","medmeal_enroll_left","foodvou_enroll_left","food_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "adi_quant",  include = c("snap_enroll","wic_enroll","lunch_enroll","bank_enroll","medmeal_enroll","food_enroll_paid","del_enroll_left","medmeal_enroll_left","foodvou_enroll_left","food_enroll_any")) %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

# housing programs
survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), include = c("sec8_enroll","hud_enroll","house_enroll_paid","hf_enroll_left","house_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA) 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "prac",  include = c("sec8_enroll","hud_enroll","house_enroll_paid","hf_enroll_left","house_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "adi_quant",  include = c("sec8_enroll","hud_enroll","house_enroll_paid","hf_enroll_left","house_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

# transportation programs
survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), include = c("nemt_enroll","nemt_enroll_left", "nemt_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA)

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "prac",  include = c("nemt_enroll","nemt_enroll_left", "nemt_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "adi_quant",  include = c("nemt_enroll","nemt_enroll_left", "nemt_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

# care coordination programs
survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), include = c("chw_enroll","chw_enroll_left", "chw_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA) 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "prac",  include = c("chw_enroll","chw_enroll_left", "chw_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "adi_quant",  include = c("chw_enroll","chw_enroll_left", "chw_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 




# confidence intervals, by program, by prac and by ADI quart

# food programs

svyciprop(~I(snap_enroll == 1), subset(final_df, foodins==1))
svyciprop(~I(wic_enroll == 1), subset(final_df, foodins==1))
svyciprop(~I(lunch_enroll == 1), subset(final_df, foodins==1))
svyciprop(~I(bank_enroll == 1), subset(final_df, foodins==1))
svyciprop(~I(medmeal_enroll == 1), subset(final_df, foodins==1))
svyciprop(~I(food_enroll_paid == 1), subset(final_df, foodins==1))
svyciprop(~I(del_enroll_left == 1), subset(final_df, foodins==1))
svyciprop(~I(medmeal_enroll_left == 1), subset(final_df, foodins==1))
svyciprop(~I(foodvou_enroll_left == 1), subset(final_df, foodins==1))
svyciprop(~I(food_enroll_any == 1), subset(final_df, foodins==1))


svyby(~snap_enroll, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~snap_enroll, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~wic_enroll, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~wic_enroll, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~lunch_enroll, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~lunch_enroll, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~bank_enroll, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~bank_enroll, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~medmeal_enroll, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~medmeal_enroll, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~food_enroll_paid, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~food_enroll_paid, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~del_enroll_left, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~del_enroll_left, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~medmeal_enroll_left, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~medmeal_enroll_left, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~foodvou_enroll_left, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~foodvou_enroll_left, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~food_enroll_any, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~food_enroll_any, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")


# housing programs

svyciprop(~I(sec8_enroll == 1), subset(final_df, house =='Low housing security'))
svyciprop(~I(hud_enroll == 1), subset(final_df, house =='Low housing security'))
svyciprop(~I(house_enroll_paid == 1), subset(final_df, house =='Low housing security'))
svyciprop(~I(hf_enroll_left == 1), subset(final_df, house =='Low housing security'))
svyciprop(~I(house_enroll_any == 1), subset(final_df, house =='Low housing security'))

svyby(~sec8_enroll, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop,  level=0.95, method = "mean")
svyby(~sec8_enroll, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")

svyby(~hud_enroll, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")
svyby(~hud_enroll, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")

svyby(~house_enroll_paid, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")
svyby(~house_enroll_paid, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")

svyby(~hf_enroll_left, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")
svyby(~hf_enroll_left, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")

svyby(~house_enroll_any, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")
svyby(~house_enroll_any, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")


# transportation programs

svyciprop(~I(nemt_enroll == 1), subset(final_df, trans =='Low transportation security'))
svyciprop(~I(nemt_enroll_left == 1), subset(final_df, trans =='Low transportation security'))
svyciprop(~I(nemt_enroll_any == 1), subset(final_df, trans =='Low transportation security'))


svyby(~nemt_enroll, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')), svyciprop,  level=0.95, method = "mean")
svyby(~nemt_enroll, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')), svyciprop, level=0.95, method = "mean")

svyby(~nemt_enroll_left, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')), svyciprop, level=0.95, method = "mean")
svyby(~nemt_enroll_left, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')), svyciprop, level=0.95, method = "mean")

svyby(~nemt_enroll_any, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')), svyciprop, level=0.95, method = "mean")
svyby(~nemt_enroll_any, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')), svyciprop, level=0.95, method = "mean")


# care coordination programs

svyciprop(~I(chw_enroll == 1), subset(final_df, cc_elig==1))
svyciprop(~I(chw_enroll_left == 1), subset(final_df, cc_elig==1))
svyciprop(~I(chw_enroll_any == 1), subset(final_df, cc_elig==1))


svyby(~chw_enroll, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)), svyciprop,  level=0.95, method = "mean")
svyby(~chw_enroll, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)), svyciprop, level=0.95, method = "mean")

svyby(~chw_enroll_left, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)), svyciprop, level=0.95, method = "mean")
svyby(~chw_enroll_left, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)), svyciprop, level=0.95, method = "mean")

svyby(~chw_enroll_any, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)), svyciprop,  level=0.95, method = "mean")
svyby(~chw_enroll_any, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)), svyciprop, level=0.95, method = "mean")






##### difference: eligibility minus enrollment #####

# elig versus enrollment difference table

# food programs

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), include = c("diff_snap_elig_enroll","diff_wic_elig_enroll","diff_lunch_elig_enroll", "diff_bank_elig_enroll", "diff_medmeal_elig_enroll","diff_food_elig_enroll_paid","diff_del_elig_enroll_left","diff_medmeal_elig_enroll_left","diff_foodvou_elig_enroll_left","diff_food_elig_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA) 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "prac", include = c("diff_snap_elig_enroll","diff_wic_elig_enroll","diff_lunch_elig_enroll", "diff_bank_elig_enroll", "diff_medmeal_elig_enroll","diff_food_elig_enroll_paid","diff_del_elig_enroll_left","diff_medmeal_elig_enroll_left","diff_foodvou_elig_enroll_left","diff_food_elig_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "adi_quant", include = c("diff_snap_elig_enroll","diff_wic_elig_enroll","diff_lunch_elig_enroll", "diff_bank_elig_enroll", "diff_medmeal_elig_enroll","diff_food_elig_enroll_paid","diff_del_elig_enroll_left","diff_medmeal_elig_enroll_left","diff_foodvou_elig_enroll_left","diff_food_elig_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 


# housing programs

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), include = c("diff_sec8_elig_enroll","diff_hud_elig_enroll","diff_house_elig_enroll_paid","diff_hf_elig_enroll_left","diff_house_elig_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA) 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "prac",  include = c("diff_sec8_elig_enroll","diff_hud_elig_enroll","diff_house_elig_enroll_paid","diff_hf_elig_enroll_left","diff_house_elig_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "adi_quant",  include = c("diff_sec8_elig_enroll","diff_hud_elig_enroll","diff_house_elig_enroll_paid","diff_hf_elig_enroll_left","diff_house_elig_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 


# transportation programs

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), include = c("diff_nemt_elig_enroll","diff_nemt_elig_enroll_left", "diff_nemt_elig_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA)

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "prac",  include = c("diff_nemt_elig_enroll","diff_nemt_elig_enroll_left", "diff_nemt_elig_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "adi_quant",  include = c("diff_nemt_elig_enroll","diff_nemt_elig_enroll_left", "diff_nemt_elig_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

# care coordination programs

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), include = c("diff_chw_elig_enroll","diff_chw_elig_enroll_left","diff_chw_elig_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA) 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "prac",  include = c("diff_chw_elig_enroll","diff_chw_elig_enroll_left","diff_chw_elig_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)) %>%
  tbl_svysummary(digits = list( ~ c(0, 1)), by = "adi_quant",  include = c("diff_chw_elig_enroll","diff_chw_elig_enroll_left","diff_chw_elig_enroll_any"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 




# confidence intervals, by program, by prac and by ADI quart

# food programs

svyciprop(~I(diff_snap_elig_enroll  == 1), subset(final_df, foodins==1))
svyciprop(~I(diff_wic_elig_enroll  == 1), subset(final_df, foodins==1))
svyciprop(~I(diff_lunch_elig_enroll  == 1), subset(final_df, foodins==1))
svyciprop(~I(diff_bank_elig_enroll  == 1), subset(final_df, foodins==1))
svyciprop(~I(diff_medmeal_elig_enroll  == 1), subset(final_df, foodins==1))
svyciprop(~I(diff_food_elig_enroll_paid == 1), subset(final_df, foodins==1))
svyciprop(~I(diff_del_elig_enroll_left == 1), subset(final_df, foodins==1))
svyciprop(~I(diff_medmeal_elig_enroll_left == 1), subset(final_df, foodins==1))
svyciprop(~I(diff_foodvou_elig_enroll_left == 1), subset(final_df, foodins==1))
svyciprop(~I(diff_food_elig_enroll_any == 1), subset(final_df, foodins==1))


svyby(~diff_snap_elig_enroll , ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~diff_snap_elig_enroll , ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~diff_wic_elig_enroll , ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~diff_wic_elig_enroll , ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~diff_lunch_elig_enroll , ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~diff_lunch_elig_enroll , ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~diff_bank_elig_enroll , ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~diff_bank_elig_enroll , ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~diff_medmeal_elig_enroll , ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~diff_medmeal_elig_enroll , ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~diff_food_elig_enroll_paid, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~diff_food_elig_enroll_paid, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~diff_del_elig_enroll_left, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~diff_del_elig_enroll_left, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~diff_medmeal_elig_enroll_left, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~diff_medmeal_elig_enroll_left, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~diff_foodvou_elig_enroll_left, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~diff_foodvou_elig_enroll_left, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")

svyby(~diff_food_elig_enroll_any, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")
svyby(~diff_food_elig_enroll_any, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, foodins==1)), svyciprop, level=0.95, method = "mean")



# housing programs

svyciprop(~I(diff_sec8_elig_enroll == 1), subset(final_df, house =='Low housing security'))
svyciprop(~I(diff_hud_elig_enroll == 1), subset(final_df, house =='Low housing security'))
svyciprop(~I(diff_house_elig_enroll_paid == 1), subset(final_df, house =='Low housing security'))
svyciprop(~I(diff_hf_elig_enroll_left == 1), subset(final_df, house =='Low housing security'))
svyciprop(~I(diff_house_elig_enroll_any == 1), subset(final_df, house =='Low housing security'))

svyby(~diff_sec8_elig_enroll, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop,  level=0.95, method = "mean")
svyby(~diff_sec8_elig_enroll, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")

svyby(~diff_hud_elig_enroll, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")
svyby(~diff_hud_elig_enroll, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")

svyby(~diff_house_elig_enroll_paid, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")
svyby(~diff_house_elig_enroll_paid, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")

svyby(~diff_hf_elig_enroll_left, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")
svyby(~diff_hf_elig_enroll_left, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")

svyby(~diff_house_elig_enroll_any, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")
svyby(~diff_house_elig_enroll_any, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, house =='Low housing security')), svyciprop, level=0.95, method = "mean")



# transportation programs

svyciprop(~I(diff_nemt_elig_enroll == 1), subset(final_df, trans =='Low transportation security'))
svyciprop(~I(diff_nemt_elig_enroll_left == 1), subset(final_df, trans =='Low transportation security'))
svyciprop(~I(diff_nemt_elig_enroll_any == 1), subset(final_df, trans =='Low transportation security'))


svyby(~diff_nemt_elig_enroll, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')), svyciprop,  level=0.95, method = "mean")
svyby(~diff_nemt_elig_enroll, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')), svyciprop, level=0.95, method = "mean")

svyby(~diff_nemt_elig_enroll_left, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')), svyciprop, level=0.95, method = "mean")
svyby(~diff_nemt_elig_enroll_left, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')), svyciprop, level=0.95, method = "mean")

svyby(~diff_nemt_elig_enroll_any, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')), svyciprop,  level=0.95, method = "mean")
svyby(~diff_nemt_elig_enroll_any, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, trans =='Low transportation security')), svyciprop, level=0.95, method = "mean")


# care coordination programs

svyciprop(~I(diff_chw_elig_enroll == 1), subset(final_df, cc_elig==1))
svyciprop(~I(diff_chw_elig_enroll_left == 1), subset(final_df, cc_elig==1))
svyciprop(~I(diff_chw_elig_enroll_any == 1), subset(final_df, cc_elig==1))


svyby(~diff_chw_elig_enroll, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)), svyciprop,  level=0.95, method = "mean")
svyby(~diff_chw_elig_enroll, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)), svyciprop, level=0.95, method = "mean")

svyby(~diff_chw_elig_enroll_left, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)), svyciprop, level=0.95, method = "mean")
svyby(~diff_chw_elig_enroll_left, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)), svyciprop, level=0.95, method = "mean")

svyby(~diff_chw_elig_enroll_any, ~prac, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)), svyciprop,  level=0.95, method = "mean")
svyby(~diff_chw_elig_enroll_any, ~adi_quant, design=survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = subset(completedDf, cc_elig==1)), svyciprop, level=0.95, method = "mean")




##### cost #####



# cost table


# total costs, federally-funded and not
survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = completedDf) %>%
  tbl_svysummary(statistic = list(all_continuous() ~ "{mean} ({mean.std.error})"), digits = list( ~ c(2, 2)), include = c("tot_cost_paid", "tot_cost_unpaid", "tot_cost"))  %>%
  modify_footnote(update = everything() ~ NA)

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = completedDf) %>%
  tbl_svysummary(statistic = list(all_continuous() ~ "{mean} ({mean.std.error})"), digits = list( ~ c(2, 2)), by = "prac",  include = c("tot_cost_paid", "tot_cost_unpaid", "tot_cost"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = completedDf) %>%
  tbl_svysummary(statistic = list(all_continuous() ~ "{mean} ({mean.std.error})"), digits = list( ~ c(2, 2)), by = "adi_quant",  include = c("tot_cost_paid", "tot_cost_unpaid", "tot_cost")) %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 


# screening
survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = completedDf) %>%
  tbl_svysummary(statistic = list(all_continuous() ~ "{mean} ({mean.std.error})"), digits = list( ~ c(2, 2)), include = c("screening_cost"))  %>%
  modify_footnote(update = everything() ~ NA)

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = completedDf) %>%
  tbl_svysummary(statistic = list(all_continuous() ~ "{mean} ({mean.std.error})"), digits = list( ~ c(2, 2)),  by = "prac",  include = c("screening_cost"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = completedDf) %>%
  tbl_svysummary(statistic = list(all_continuous() ~ "{mean} ({mean.std.error})"), digits = list( ~ c(2, 2)),  by = "adi_quant",  include = c("screening_cost")) %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 


# food programs
survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = completedDf) %>%
  tbl_svysummary(statistic = list(all_continuous() ~ "{mean} ({mean.std.error})"), digits = list( ~ c(2, 2)),  include = c("snap_cost","wic_cost","lunch_cost","bank_cost","medmeal_cost","food_cost_paid","del_cost_left","medmeal_cost_left","foodvou_cost_left","food_cost"))  %>%
  modify_footnote(update = everything() ~ NA)

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = completedDf) %>%
  tbl_svysummary(statistic = list(all_continuous() ~ "{mean} ({mean.std.error})"), digits = list( ~ c(2, 2)),  by = "prac",  include = c("snap_cost","wic_cost","lunch_cost","bank_cost","medmeal_cost","food_cost_paid","del_cost_left","medmeal_cost_left","foodvou_cost_left","food_cost"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = completedDf) %>%
  tbl_svysummary(statistic = list(all_continuous() ~ "{mean} ({mean.std.error})"), digits = list( ~ c(2, 2)),  by = "adi_quant",  include = c("snap_cost","wic_cost","lunch_cost","bank_cost","medmeal_cost","food_cost_paid","del_cost_left","medmeal_cost_left","foodvou_cost_left","food_cost")) %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

# housing programs
survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data =  completedDf) %>%
  tbl_svysummary(statistic = list(all_continuous() ~ "{mean} ({mean.std.error})"), digits = list( ~ c(2, 2)),  include = c("sec8_cost","hud_cost","house_cost_paid","hf_cost_left","house_cost"))  %>%
  modify_footnote(update = everything() ~ NA) 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data =  completedDf) %>%
  tbl_svysummary(statistic = list(all_continuous() ~ "{mean} ({mean.std.error})"), digits = list( ~ c(2, 2)),  by = "prac",  include = c("sec8_cost","hud_cost","house_cost_paid","hf_cost_left","house_cost"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data =  completedDf) %>%
  tbl_svysummary(statistic = list(all_continuous() ~ "{mean} ({mean.std.error})"), digits = list( ~ c(2, 2)),  by = "adi_quant",  include = c("sec8_cost","hud_cost","house_cost_paid","hf_cost_left","house_cost"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

# transportation programs
survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data =  completedDf) %>%
  tbl_svysummary(statistic = list(all_continuous() ~ "{mean} ({mean.std.error})"), digits = list( ~ c(2, 2)),  include = c("nemt_cost","nemt_cost_left","nemt_cost_any"))  %>%
  modify_footnote(update = everything() ~ NA)

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data =  completedDf) %>%
  tbl_svysummary(statistic = list(all_continuous() ~ "{mean} ({mean.std.error})"), digits = list( ~ c(2, 2)),  by = "prac",  include = c("nemt_cost","nemt_cost_left","nemt_cost_any"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data =  completedDf) %>%
  tbl_svysummary(statistic = list(all_continuous() ~ "{mean} ({mean.std.error})"), digits = list( ~ c(2, 2)),  by = "adi_quant",  include = c("nemt_cost","nemt_cost_left","nemt_cost_any"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

# care coordination programs
survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = completedDf) %>%
  tbl_svysummary(statistic = list(all_continuous() ~ "{mean} ({mean.std.error})"), digits = list( ~ c(2, 2)),  include = c("chw_cost","chw_cost_left","chw_cost_any"))  %>%
  modify_footnote(update = everything() ~ NA) 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = completedDf) %>%
  tbl_svysummary(statistic = list(all_continuous() ~ "{mean} ({mean.std.error})"), digits = list( ~ c(2, 2)),  by = "prac",  include = c("chw_cost","chw_cost_left","chw_cost_any"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 

survey::svydesign(id = ~SDMVPSU, weights = ~WTMEC4YR, strata = ~SDMVSTRA, nest = TRUE, survey.lonely.psu = "adjust", data = completedDf) %>%
  tbl_svysummary(statistic = list(all_continuous() ~ "{mean} ({mean.std.error})"), digits = list( ~ c(2, 2)),  by = "adi_quant",  include = c("chw_cost","chw_cost_left","chw_cost_any"))  %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_header(all_stat_cols() ~ "{n} ") 


