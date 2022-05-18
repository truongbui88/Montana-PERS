rm(list = ls())
library("readxl")
library(tidyverse)
library(dplyr)
library(zoo)

FileName <- 'MT PERS BModel - NDPERS Version.xlsx'
YearStart <- 2022
Age <- 20:120
YOS <- 0:100
RetirementAge <- 20:120
Years <- 2001:(120-20+YearStart)    #change starting year to 2001 for RP-2000 mort rate calculations
HireType <- c("New Hire","Legacy")
#Updated from 2010 to 2011

#Assigning individual  Variables
model_inputs <- read_excel(FileName, sheet = 'Main')

for(i in 1:nrow(model_inputs)){
  if(!is.na(model_inputs[i,2])){
    assign(as.character(model_inputs[i,2]),as.double(model_inputs[i,3]))
  }
}
Type <- "New Hire"
#Import key data tables
SurvivalRates <- read_excel(FileName, sheet = 'RP_2000')
ScaleBB <- read_excel(FileName, sheet = "Scale BB")
# MaleMP <- read_excel(FileName, sheet = 'MP-2020_Male')
# FemaleMP <- read_excel(FileName, sheet = 'MP-2020_Female')
SalaryGrowth <- read_excel(FileName, sheet = "Salary Growth")
SalaryEntry <- read_excel(FileName, sheet = "Salary and Headcount")
# TerminationRateAfter5 <- read_excel(FileName, sheet = 'Termination Rate after 5')#Updated*
TerminationRate <- read_excel(FileName, sheet = 'Termination Rate')#Updated*
RetirementRates <- read_excel(FileName, sheet = 'Retirement Rates')#Updated*
#
################################################################################################################################################################
#
#Functions
#
# LinearInterpolation <- function(Data){
#   StartValue <- Data[1]
#   StartIndex <- 2
#   count <- 2
#   while(count <= length(Data)){
#     if(!is.na(Data[count])){
#       EndValue <- Data[count]
#       EndIndex <- count - 1
#       #print(EndIndex)
#       if(EndIndex > StartIndex){
#         for(i in StartIndex:EndIndex){
#           Data[i] <- Data[i-1] - (StartValue - EndValue)/(EndIndex - StartIndex + 2)
#         }
#       }
#       StartValue <- Data[count]
#       StartIndex <- count + 1
#     } else if((count == length(Data))) {
#       EndIndex <- count
#       
#       if(EndIndex > StartIndex){
#         for(i in StartIndex:EndIndex){
#           Data[i] <- Data[i-1] - (StartValue - EndValue)/(EndIndex - StartIndex + 2)
#         }
#       }
#     }
#     count <- count + 1
#   }
#   
#   return(Data)
# }
#

#Function to get retirement type (Regular, Early, or None)
RetirementType <- function(Age, YOS, HireType){
  Type = ifelse(HireType == 'New Hire',
                ifelse((Age >= NormalRetAgeI & YOS >= NormalYOSI) |
                         (Age >= NormalRetAgeII), 'Regular',
                       ifelse((Age >= EarlyRetAge & YOS >= EarlyRetYOS),'Early','None')),
                
                ifelse((Age >= NormalRetAgeI_Legacy & YOS >= NormalYOSI_Legacy) |
                         (Age >= NormalRetAgeII_Legacy) |
                         (YOS >= NormalYOSII_Legacy), 'Regular',
                       ifelse((Age >= EarlyRetAge_Legacy & YOS >= EarlyRetYOS_Legacy) | 
                                (Age < EarlyRetAgeII_Legacy & YOS >= EarlyRetYOSII_Legacy),'Early','None')))
  return(Type)
}
  


#Function to determine if eligible for retirement (either normal or early)
IsRetirementEligible <- function(Age, YOS, HireType){
    Check = ifelse(RetirementType(Age, YOS, HireType) == "Regular" |
                     RetirementType(Age, YOS, HireType) == "Early", TRUE, FALSE)
  return(Check)
}
#

#
cumFV <- function(interest, cashflow){
  cumvalue <- double(length = length(cashflow))
  for (i in 2 :length(cumvalue)) {
    cumvalue[i] <- cumvalue[i - 1]*(1 + interest) + cashflow[i - 1]
  }
  return(cumvalue)
}
#
################################################################################################################################################################
#
#Mortality Rates and function
#These rates dont change so they're outside the function
#Transform base mortality rates and mortality improvement rates
# MaleMP <- MaleMP %>% 
#   pivot_longer(-Age, names_to = "Years", values_to = "MP_male") %>% 
#   mutate(Years = as.numeric(Years))
# 
# MaleMP_ultimate <- MaleMP %>% 
#   filter(Years == max(Years)) %>% 
#   rename(MP_ultimate_male = MP_male) %>% 
#   select(-Years)
# 
# FemaleMP <- FemaleMP %>% 
#   pivot_longer(-Age, names_to = "Years", values_to = "MP_female") %>% 
#   mutate(Years = as.numeric(Years))
# 
# FemaleMP_ultimate <- FemaleMP %>% 
#   filter(Years == max(Years)) %>% 
#   rename(MP_ultimate_female = MP_female) %>% 
#   select(-Years)


##Mortality calculations
MortalityTable <- expand_grid(19:120, Years) %>%    #Manually set starting age at 19 here to accommodate the 1-year set back for male mortality
  rename(Age = `19:120`) %>% 
  left_join(SurvivalRates, by = "Age") %>% 
  left_join(ScaleBB, by = "Age") %>% 
  # mutate(MaleMP_final = ifelse(Years > max(MaleMP$Years), MP_ultimate_male, MP_male),
  #        FemaleMP_final = ifelse(Years > max(FemaleMP$Years),  MP_ultimate_female, MP_female),
  # entry_age = Age - (Years - YearStart),    #we don't need entry_age as mort rates for Montana PERS don't change over time
  # YOS = Age - entry_age                     #we don't need YOS as the combined rates will be used (no need to differentiate between active vs. retiree mortality rates)
  # ) %>% 
  group_by(Age) %>%
  #MPcumprod is the cumulative product of (1 - BB rates), starting from 2001. 
  #to cumsum (1 - BB rates) over 2001+ & then multiply by mort base rates (RP-2000)
  mutate(MPcumprod_male = cumprod(1 - Male_BB),
         MPcumprod_female = cumprod(1 - Female_BB),
         mort_male = Male_Comb_Health_Mort * MPcumprod_male,
         mort_female = Female_Comb_Health_Mort * MPcumprod_female) %>% 
  filter(Years == 2020) %>%  #use 2020 rates only, as specified in the 2021 val report
  ungroup() %>% 
  mutate(mort_male = lag(mort_male),   #male mortality is set back 1 year
         mort_male = replace(mort_male, Age == 120, 1),   #restore end value (at age 120) back to 1 to cancel the lag effect
         mort = (mort_male + mort_female)/2)
  
#filter out the necessary variables
MortalityTable <- MortalityTable %>% 
  filter(Age >= 20) %>% 
  select(Age, mort_male, mort_female, mort)
#
################################################################################################################################################################
#
#Separation Rates
#Linear interpolation of Termination Rates and Retirement Rates
# TerminationRateAfter5$TermAfterVest <- LinearInterpolation(TerminationRateAfter5$TermAfterVest)
# RetirementRates$Disabled_RetRate <- LinearInterpolation(RetirementRates$Disabled_RetRate)


#Separation Rates
SeparationRates <- expand_grid(Age, YOS, HireType) %>% 
  mutate(entry_age = Age - YOS) %>% 
  filter(entry_age %in% SalaryEntry$entry_age) %>% 
  arrange(HireType, entry_age, Age) %>% 
  left_join(TerminationRate, by = "YOS") %>%
  left_join(RetirementRates, by = "Age") 

#If you're retirement eligible, use the retirement rates, or else use the regular termination rates
SeparationRates <- SeparationRates %>% 
  mutate(retirement_check = IsRetirementEligible(Age,YOS,HireType),
         SepRate = ifelse(retirement_check == T, 
                          ifelse(YOS >= 30 | (Age >= 60 & YOS >= 25), Regular_RetRate, Less30_RetRate),
                          ifelse(Age >= 50 & YOS >= 5, 0, Term)))  %>%
  group_by(HireType, entry_age) %>% 
  mutate(RemainingProb = cumprod(1 - lag(SepRate, default = 0)),
         SepProb = lag(RemainingProb, default = 1) - RemainingProb) %>% 
  ungroup()

#Filter out unecessary values
SeparationRates <- SeparationRates %>% select(Age, YOS, HireType, RemainingProb, SepProb)
#
################################################################################################################################################################
#
#Salary Data
#colnames(SalaryGrowth)[2] <- "YOS"
#Create a long-form table of Age and YOS and merge with salary data
SalaryData <- expand_grid(Age, YOS, HireType) %>% 
  mutate(entry_age = Age - YOS) %>%    #Add entry age
  filter(entry_age %in% SalaryEntry$entry_age) %>% 
  arrange(HireType, entry_age, Age) %>% 
  left_join(SalaryEntry, by = "entry_age") %>% 
  left_join(SalaryGrowth, by = c("YOS"))



#################
#################
#################
#################
##### Normal Cost & Benefit Accrual Function
benefit_cal <- function(
  output = "NC",              
  EE_tier = Type,            #Either "New Hire" or "Legacy" (not used in the Shiny dashboard)
  DB_ARR = ARR,
  DB_EE = DB_EE_cont, 
  DC_EE = DC_EE_cont,
  DC_ER = DC_ER_cont,
  DB_mult10 = BenMult1,      #Benefit multiplier for < 10 YOS (new hires only) 
  DB_mult10_30 = BenMult2,   #Benefit multiplier for 10-30 YOS (new hires only)
  DB_mult30 = BenMult3,      #Benefit multiplier for 30 or more YOS (new hires only)
  DB_COLA = COLA_new,        #COLA for new hires only
  ea = HiringAge,            #Only use the numbers from SalaryEntry$entry_age
  DCreturn = DC_return) {
  
  
  SalaryData <- SalaryData %>% 
    group_by(HireType, entry_age) %>% 
    mutate(Salary = Starting_Salary*cumprod(1+lag(Total_Pay_Increase,default = 0)),
           FAS_years = ifelse(HireType == "New Hire", FinAvgSalaryYears, FinAvgSalaryYears_Legacy),
           #Salary = pmin(Salary_gross, salary_cap),
           # IRSSalaryCap = pmin(Salary,IRSCompLimit),
           FinalAvgSalary = rollmean(lag(Salary), k = FAS_years, fill = NA, align = "right"),
           EEContrib = DB_EE*Salary,
           DBEEBalance = cumFV(Interest, EEContrib),
           CumulativeWage = cumFV(DB_ARR, Salary)) %>% 
    ungroup()
  #
  ################################################################################################################################################################
  #
  #Annuity Factor and Reduced Factor
  AnnFactorData <- expand_grid(Age, HireType) %>%      #don't need entry_age as Montana PERS mort rates are fixed (no generational improvement)
    left_join(MortalityTable %>% select(Age, mort), by = "Age") %>% 
    arrange(HireType, Age) %>% 
    group_by(HireType) %>%
    mutate(COLA = ifelse(HireType == "Legacy", COLA_legacy, DB_COLA),
           surv = cumprod(1 - lag(mort, default = 0)),
           surv_DR = surv/(1+DB_ARR)^(Age - Age[1]),
           surv_DR_COLA = surv_DR * (1+COLA)^(Age - Age[1]),
           AF = rev(cumsum(rev(surv_DR_COLA)))/surv_DR_COLA)
  # AFNormalRetAge = ifelse(HireType == 'New Hire', AF[Age==NormalRetAgeI],AF[Age==NormalRetAgeI_Legacy]),
  # SurvProbNormalRetAge = ifelse(HireType == 'New Hire', surv_DR[Age==NormalRetAgeI],surv_DR[Age==NormalRetAgeI_Legacy])) %>% 
  
  
  # AnnFactorData <- AnnuityF(data = MortalityTable,
  #                           ColaType = "Compound")
  
  #Reduced Factor
  ReducedFactor <- expand_grid(Age, YOS, HireType) %>% 
    arrange(HireType, YOS) %>% 
    mutate(RetType = RetirementType(Age, YOS, HireType),
           norm_retire = ifelse(RetType == "Regular", 1, 0)) %>% 
    left_join(AnnFactorData %>% select(Age, HireType, surv_DR, AF), by = c("Age", "HireType")) %>% 
    group_by(HireType, YOS) %>% 
    mutate(AgeNormRet = max(Age) - sum(norm_retire) + 1,    #This is the earliest age of normal retirement for a given YOS
           RF = ifelse(RetType == "Early", AF[Age == AgeNormRet] * surv_DR[Age == AgeNormRet] / surv_DR / AF,   #Benefit for early retirement  = actuarially reduced amount of the normal benefit the employee would receive at the earliest age of normal retirement. 
                       ifelse(RetType == "None", 0, 1))) %>% 
    rename(RetirementAge = Age) %>% 
    ungroup()
  
  #Check wide format of the Reduced Factor table
  # ReducedFactor_wide <- ReducedFactor %>% 
  #   filter(YOS >= 5,
  #          RetirementAge >= 45,
  #          HireType == "Legacy") %>% 
  #   select(YOS, RetirementAge, RF) %>% 
  #   pivot_wider(names_from = RetirementAge,
  #               values_from = RF)
  
  #
  ################################################################################################################################################################
  #Benefits and Present Value
  
  BenefitsTable <- expand_grid(Age, YOS, RetirementAge, HireType) %>% 
    mutate(entry_age = Age - YOS) %>% 
    filter(entry_age %in% SalaryEntry$entry_age) %>% 
    arrange(HireType, entry_age, Age, RetirementAge) %>% 
    left_join(SalaryData, by = c("Age", "YOS", "HireType","entry_age")) %>% 
    left_join(ReducedFactor %>% select(RetirementAge, YOS, HireType, RF), by = c("RetirementAge", "YOS", "HireType")) %>%
    left_join(AnnFactorData %>% select(Age, HireType, surv_DR, AF), by = c("RetirementAge" = "Age", "HireType")) %>%
    
    #Rename surv_DR and AF to make clear that these variables are at retirement
    rename(surv_DR_ret = surv_DR, AF_Ret = AF) %>% 
    
    #Rejoin the table to get the surv_DR for the termination age
    left_join(AnnFactorData %>% select(Age, HireType, surv_DR), by = c("Age", "HireType")) %>% 
    mutate(
      BenMult = ifelse(HireType == "Legacy",
                       case_when(
                         YOS < 25 ~ BenMult2,
                         TRUE ~ BenMult3
                       ),
                       case_when(
                         YOS < 10 ~ DB_mult10,
                         YOS >= 10 & YOS < 30 ~ DB_mult10_30,
                         TRUE ~ DB_mult30
                       )),
      ReducedFactMult = RF * BenMult, 
      AnnFactorAdj = AF_Ret * surv_DR_ret / surv_DR,
      PensionBenefit = ReducedFactMult * FinalAvgSalary * YOS,
      PresentValue = ifelse(Age > RetirementAge, 0, PensionBenefit*AnnFactorAdj))
  
  #)
  
  #For a given combination of entry age and termination age, the member is assumed to choose the retirement age that maximizes the PV of future retirement benefits. That value is the "optimum benefit". 
  OptimumBenefit <- BenefitsTable %>% 
    group_by(HireType, entry_age, Age) %>% 
    summarise(MaxBenefit = max(PresentValue)) %>%
    mutate(MaxBenefit = ifelse(is.na(MaxBenefit), 0, MaxBenefit)) %>% 
    ungroup()
  
  ####### Benefit Accrual & Normal Cost #######
  #### Real Pension Wealth = Pension Wealth adjusted for inflation
  #### Actuarial PV of Pension Wealth = Pension Wealth 
  #Combine optimal benefit with employee balance and calculate the PV of future benefits and salaries 
  #####################################
  SalaryData <- SalaryData %>% 
    left_join(OptimumBenefit, by = c("HireType", "Age", "entry_age")) %>% 
    left_join(SeparationRates, by = c("HireType", "Age", "YOS")) %>%
    mutate(PenWealth = pmax(DBEEBalance,MaxBenefit),        #Members are assumed to elect the option with the greatest PV between a refund with interest and a deferred benefit
           RealPenWealth = PenWealth/(1 + assum_infl)^YOS,
           PVPenWealth = PenWealth/(1 + DB_ARR)^YOS * SepProb,
           PVCumWage = CumulativeWage/(1 + DB_ARR)^YOS * SepProb)
  
  
  #Calculate normal cost rate for each entry age in each hire type
  NormalCost <- SalaryData %>% 
    group_by(HireType, entry_age) %>% 
    summarise(normal_cost = sum(PVPenWealth)/sum(PVCumWage)) %>% 
    ungroup()
  
  #View(NormalCost)
  
  #Calculate the aggregate normal cost for a tier (legacy or new hire)
  NC_aggregate <- NormalCost %>% 
    left_join(SalaryEntry %>% select(entry_age, Starting_Salary, count_start), by = "entry_age") %>% 
    group_by(HireType) %>% 
    summarise(NC = sum(normal_cost * Starting_Salary * count_start)/sum(Starting_Salary * count_start)) %>% 
    filter(HireType == EE_tier) %>% 
    pull(NC)
  
  # test <- SalaryData %>% 
  #   filter(entry_age == 22)
  # write.csv(test, "test.csv")
  
  ################################
  
  ####### DC Account Balance 
  SalaryData2 <- SalaryData %>% 
    filter(entry_age == ea, HireType == EE_tier) %>% 
    select(Age, YOS, entry_age, HireType, Starting_Salary, Total_Pay_Increase, Salary, RemainingProb) %>% 
    mutate(DC_EEContrib = Salary * DC_EE,
           DC_ERContrib = Salary * DC_ER,
           DC_Contrib = DC_EEContrib + DC_ERContrib,
           DC_balance = cumFV(DCreturn, DC_Contrib),
           RealDC_balance = DC_balance/(1 + assum_infl)^YOS) %>% 
    left_join(SalaryData %>% select(Age, YOS, entry_age, HireType, RealPenWealth), by = c("Age", "YOS", "entry_age", "HireType")) %>% 
    mutate(RealHybridWealth = RealDC_balance + RealPenWealth) %>% 
    filter(Age <= 70)
  
  if (output == "NC") {
    return(NC_aggregate)
  } else if (output == "attrition") {
    return(SalaryData2 %>% 
             select(Age, RemainingProb))
  } else if (output == "DB") {
    return(SalaryData2 %>% 
             select(Age, RealPenWealth, RemainingProb))
  } else if (output == "DC") {
    return(SalaryData2 %>% 
             select(Age, RealDC_balance, RemainingProb))
  } else {
    return(SalaryData2 %>% 
             select(Age, RealHybridWealth, RemainingProb))
  }
}


#### Test benefit function
# NC <- benefit_cal()
# NC <- benefit_cal(EE_tier = "Legacy")
# NC2 <- benefit_cal(DB_ARR = 0.0665, DB_mult10 = 0.02, DB_mult10_30 = 0.02, DB_mult30 = 0.03)
# DB_4 <- benefit_cal(output = "DB", DB_ARR = 0.04, ea = 22)
# DB_7 <- benefit_cal(output = "DB", DB_ARR = 0.07, ea = 22)
# DB_current <- benefit_cal(output = "DB", ea = 37) %>% select(-RemainingProb)
# DC <- benefit_cal(output = "DC", ea = 37) %>% select(-RemainingProb)

# attri <- benefit_cal(output = "attrition", ea = 27)  
#   
# name_change <- c("DB_4", "DB_7", "DB_current", "DC")
# test <- DB_4 %>% 
#   left_join(DB_7, by = "Age") %>% 
#   left_join(DB_current, by = "Age") %>% 
#   left_join(DC, by = "Age") %>% 
#   rename_at(2:5, ~ name_change) %>% 
#   pivot_longer(cols = 2:5, 
#                names_to = "type",
#                values_to = "wealth")
# 
# ggplot(test, aes(x = Age, y = wealth, col = type)) +
#   geom_line()
# 
# ggplot(attri, aes(x = Age, y = RemainingProb)) +
#   geom_line()
#######################
