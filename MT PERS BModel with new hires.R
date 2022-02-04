rm(list = ls())
library("readxl")
library(tidyverse)
library(dplyr)
library(zoo)
#
#Read Input files
FileName <- 'MT PERS BModel Inputs with new hires.xlsx'
model_inputs <- read_excel(FileName, sheet = 'Main')
SalaryGrowthData <- read_excel(FileName, sheet = 'Salary Growth')
TermAfterVest <- read_excel(FileName, sheet = 'Termination Rate after Vesting')
TermBeforeVest <- read_excel(FileName, sheet = 'Termination Rate before Vesting')
RetirementRates <- read_excel(FileName, sheet = 'Retirement Rates')
MaleMP <- read_excel(FileName, sheet = 'MP-2020_Male')
FemaleMP <- read_excel(FileName, sheet = 'MP-2020_Female')
RP2000 <- read_excel(FileName, sheet = 'RP_2000')
#
YearStart <- 2022
HiringAge <- 20
Age <- 20:120
YOS <- 0:100
RetirementAge <- 20:120
Years <- 2011:2122 #(why 2121? Because 120 - 20 + 2021 = 2121)
#
#Assigning individual  Variables
for(i in 1:nrow(model_inputs)){
  if(!is.na(model_inputs[i,2])){
    assign(as.character(model_inputs[i,2]),as.double(model_inputs[i,3]))
  }
}
#
################################################################################################################################################################
#
#Functions
#
LinearInterpolation <- function(Data){
  StartValue <- Data[1]
  StartIndex <- 2
  count <- 2
  while(count <= length(Data)){
    if(!is.na(Data[count])){
      EndValue <- Data[count]
      EndIndex <- count - 1
      #print(EndIndex)
      if(EndIndex > StartIndex){
        for(i in StartIndex:EndIndex){
          Data[i] <- Data[i-1] - (StartValue - EndValue)/(EndIndex - StartIndex + 2)
        }
      }
      StartValue <- Data[count]
      StartIndex <- count + 1
    } else if((count == length(Data))) {
      EndIndex <- count
      
      if(EndIndex > StartIndex){
        for(i in StartIndex:EndIndex){
          Data[i] <- Data[i-1] - (StartValue - EndValue)/(EndIndex - StartIndex + 2)
        }
      }
    }
    count <- count + 1
  }
  
  return(Data)
}
#
IsRetirementEligible <- function(Age, YOS, HireType){
  if(HireType == 'Regular'){
    Check = ifelse((Age >= NormalRetAgeI & YOS >= NormalYOSI) |
                     (Age >= NormalRetAgeII), TRUE, FALSE)
  } else if (HireType == 'Legacy'){
    Check = ifelse((Age >= NormalRetAgeI_Legacy & YOS >= NormalYOSI_Legacy) |
                     (Age >= NormalRetAgeII_Legacy), TRUE, FALSE)
  }
  return(Check)
}
#
RetirementType <- function(Age, YOS, HireType){
  if(HireType == 'Regular'){
    Type = ifelse(IsRetirementEligible(Age,YOS,HireType), 'Regular',
                  ifelse((Age >= EarlyRetAge & YOS >= EarlyRetYOS),'Early','None'))
  } else if (HireType == 'Legacy'){
    Type = ifelse(IsRetirementEligible(Age,YOS,HireType), 'Regular',
                  ifelse((Age >= EarlyRetAge_Legacy & YOS >= EarlyRetYOS_Legacy) | 
                           (Age <= EarlyRetAgeII_Legacy & YOS >= EarlyRetYOSII_Legacy),'Early','None'))
  }
  
  return(Type)
}
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
#Linear interpolation of termination rates
TermAfterVest$TermAfterVest <- LinearInterpolation(TermAfterVest$TermAfterVest)
#
#Function for vested balance
GetVestedBalanceData <- function(HiringAge,StartingSalary, HireType){
  #MP Scales
  #Transform base mortality rates and mortality improvement rates
  MaleMP <- MaleMP %>% 
    pivot_longer(-Age, names_to = "Years", values_to = "MP_male") %>% 
    mutate(Years = as.numeric(Years), YOS = Age - HiringAge) %>%
    filter(YOS >= 0)
  
  MaleMP_ultimate <- MaleMP %>% 
    filter(Years == max(Years)) %>% 
    rename(MP_ultimate_male = MP_male) %>% 
    select(-Years)
  
  FemaleMP <- FemaleMP %>% 
    pivot_longer(-Age, names_to = "Years", values_to = "MP_female") %>% 
    mutate(Years = as.numeric(Years), YOS = Age - HiringAge) %>%
    filter(YOS >= 0)
  
  FemaleMP_ultimate <- FemaleMP %>% 
    filter(Years == max(Years)) %>% 
    rename(MP_ultimate_female = MP_female) %>% 
    select(-Years)
  
  MortalityTable <- expand_grid(Age, Years)
  colnames(MortalityTable) <- c('Age','Years')
  MortalityTable<- MortalityTable %>%
    left_join(RP2000, by = "Age") %>% 
    left_join(MaleMP_ultimate, by = c("Age")) %>% 
    left_join(FemaleMP_ultimate, by = c("Age","YOS")) %>% 
    group_by(Age) %>%
    #MPcumprod is the cumulative product of (1 - MP rates), starting from 2011. We use it later so make life easy and calculate now
    mutate(MPcumprod_male = cumprod(1 - MP_ultimate_male),
           #Started mort. table from 2011 (instead of 2010) 
           #to cumsum over 2011+ & then multiply by 2010 MP-2019
           #removed /(1 - MaleMP_final[Years == 2010])
           MPcumprod_female = cumprod(1 - MP_ultimate_female),
           mort_male = ifelse(IsRetirementEligible(Age, YOS, HireType) == TRUE, Male_Healthy_Mort,
                              Male_Reg_Mort) * MPcumprod_male,
           mort_female = ifelse(IsRetirementEligible(Age, YOS, HireType) == TRUE, Female_Healthy_Mort,
                                Female_Reg_Mort) * MPcumprod_female,
           mort_combined = (mort_male + mort_female)/2,
           CheckData = IsRetirementEligible(Age, YOS, HireType)) %>% 
    #Recalcualting average
    filter((Age - YOS) == HiringAge, (Years - YearStart) == HiringAge) %>%
    select(Age,YOS,mort_male,mort_female, mort_combined) %>%
    ungroup()
  #
  #Change the sequence for the Age and YOS depending on the hiring age
  Age <- seq(HiringAge,120)
  YOS <- seq(0,(95-(HiringAge - 25)))
  #Pay Increases need to have the same length as Age when 
  #the hiring age changes
  TotalSalaryGrowth <- SalaryGrowthData$Total_Pay_Increase[1:length(Age)]
  
  #Salary increases and other
  SalaryData <- tibble(Age,YOS) %>%
    mutate(Salary = StartingSalary*cumprod(1+lag(TotalSalaryGrowth,default = 0)),
           FinalAvgSalary = ifelse(YOS >= Vesting, rollmean(lag(Salary), k = FinAvgSalaryYears, fill = 0, align = "right"), 0),
           DB_EE_Contrib = DB_EE_Contrib_Rate*Salary, DB_ER_Contrib = DB_ER_Contrib_Rate*Salary,
           DC_EE_Contrib = DC_EE_Contrib_Rate*Salary, DC_ER_Contrib = DC_ER_Contrib_Rate*Salary,
           DBEEBalance = cumFV(Interest,DB_EE_Contrib),
           DBERBalance = ifelse(YOS < Vesting, 0,cumFV(Interest,DB_ER_Contrib)),
           DCEEBalance = cumFV(DC_return,DC_EE_Contrib),
           DCERBalance = cumFV(DC_return,DC_ER_Contrib),
           CumWage = cumFV(ARR,Salary)) %>%
    ungroup()
  
  #Survival Probability and Annuity Factor
  AnnFactorData <- MortalityTable %>% select(Age,mort_combined) %>%
    mutate(Prob = cumprod(1 - lag(mort_combined, default = 0)),
           DiscProb = Prob / (1+ARR)^(Age - HiringAge),
           surv_DR_COLA = DiscProb * (1+COLA)^(Age - HiringAge),
           AF = rev(cumsum(rev(surv_DR_COLA))) / surv_DR_COLA) %>% 
    ungroup()
  
  #Recalibrate these values - legacy vs regular
  AFNormalRetAge <- ifelse(RetirementType(Age,YOS, HireType) == 'Regular',AnnFactorData$AF[AnnFactorData$Age == NormalRetAgeI],
                           AnnFactorData$AF[AnnFactorData$Age == NormalRetAgeI_Legacy])
  
  SurvProbNormalRetAge <- ifelse(RetirementType(Age,YOS, HireType) == 'Regular',AnnFactorData$Prob[AnnFactorData$Age == NormalRetAgeI],
                           AnnFactorData$Prob[AnnFactorData$Age == NormalRetAgeI_Legacy])
  
  NormalRetAge_Final <- ifelse(RetirementType(Age,YOS, HireType) == 'Regular', NormalRetAgeI,NormalRetAgeI_Legacy)
  ReducedFactor <- expand_grid(Age, YOS) %>% 
    group_by(YOS) %>%
    left_join(AnnFactorData,by = "Age") %>% 
    mutate(RF = ifelse(RetirementType(Age,YOS, HireType) == 'Regular', 1,
                       ifelse(RetirementType(Age,YOS, HireType) == 'Early', AFNormalRetAge / (1+ARR)^(NormalRetAge_Final - Age)*SurvProbNormalRetAge / Prob / AF, 0)),
           Ret_Type = RetirementType(Age,YOS, HireType)) %>% 
    rename(RetirementAge = Age) %>% 
    ungroup() 
  
  RetAges <- HiringAge:120
  BenefitsTable <- expand_grid(HiringAge:120,RetAges)
  colnames(BenefitsTable) <- c('Age','RetirementAge')
  BenefitsTable <- left_join(BenefitsTable,SalaryData, by = "Age") %>% 
    left_join(ReducedFactor %>% select(RetirementAge, YOS, RF, Ret_Type), by = c("RetirementAge", "YOS")) %>%
    left_join(AnnFactorData %>% select(Age, surv_DR_COLA, AF), by = c("RetirementAge" = "Age")) %>%
    #Rename surv_DR and AF to make clear that these variables are at retirement
    rename(surv_DR_ret = surv_DR_COLA, AF_Ret = AF) %>% 
    #Rejoin the table to get the surv_DR for the termination age
    left_join(AnnFactorData %>% select(Age, surv_DR_COLA), by = c("Age")) %>% 
    mutate(GradedMult = BenMult1*pmin(YOS,10) + BenMult2*pmax(pmin(YOS,30)-10,0) + BenMult3*pmax(YOS-30,0),
           
           #Legacy Multiplier for early and new hires
           GradedMult_Legacy = ifelse(RetirementType(Age,YOS,'Legacy') == 'Regular',
                                      BenMult2*pmin(YOS,25) + BenMult3*pmax(YOS-25,0),
                                      #Reduction for early retirement for legacy
                                      ifelse(RetirementType(Age,YOS,'Legacy') == 'Early',
                                             BenMult2*pmin(YOS,25) + BenMult3*pmax(YOS-25,0) -
                                             0.05*pmin(EarlyRetAgeII_Legacy - EarlyRetAge_Legacy,5) -
                                             0.03*pmax(EarlyRetAgeII_Legacy - EarlyRetAge_Legacy - 5,0),0)),
           ReducedFactMult = ifelse(RetirementType(Age,YOS,HireType) == 'Regular',
                                    RF*GradedMult,RF*GradedMult_Legacy),
                            
           AnnFactorAdj = AF_Ret * surv_DR_ret / surv_DR_COLA,
           PensionBenefit = ReducedFactMult*FinalAvgSalary*YOS,
           PresentValue = ifelse(Age > RetirementAge, 0, PensionBenefit*AnnFactorAdj)) %>% 
    ungroup() 
  
  #The max benefit is done outside the table because it will be merged with Salary data
  OptimumBenefit <- BenefitsTable %>% group_by(Age) %>% summarise(MaxBenefit = max(PresentValue))
  SalaryData <- left_join(SalaryData,OptimumBenefit) 
  SalaryData <- left_join(SalaryData,TermAfterVest,by = 'Age') %>%
    mutate(PenWealth = pmax(DBERBalance+DBEEBalance,MaxBenefit), 
           RealPenWealth = PenWealth/(1 + assum_infl)^YOS,
           PVPenWealth = PenWealth/(1 + ARR)^YOS,
           PVCumWage = CumWage/(1 + assum_infl)^YOS,
           Ratio = PVPenWealth/PVCumWage,
           DCEEBalance_Infl = DCEEBalance/(1+assum_infl)^YOS,
           DCERBalance_Infl = DCERBalance/(1+assum_infl)^YOS) %>% 
    ungroup() 
  
  return(SalaryData)
}
#
################################################################################################################################################################
#
SalaryHeadcountData <- read_excel(FileName, sheet = 'Salary and Headcount')
GetNormalCostFinal <- function(SalaryHeadcountData){
  #This part requires a for loop since GetNormalCost cant be vectorized.
  for(i in 1:nrow(SalaryHeadcountData)){
    VestedBalanceData <- GetVestedBalanceData(SalaryHeadcountData$entry_age[i], 
                                              SalaryHeadcountData$Starting_Salary[i],
                                              'Legacy')
    #Calc and return Normal Cost
    SalaryHeadcountData$NormalCost[i] <- sum(VestedBalanceData$TermAfterVest*VestedBalanceData$PVPenWealth) / 
      sum(VestedBalanceData$TermAfterVest*VestedBalanceData$PVCumWage)
    
  }
  
  #Calc the weighted average Normal Cost
  NormalCostFinal <- sum(SalaryHeadcountData$Average_Salary*SalaryHeadcountData$Headcount_Total*SalaryHeadcountData$NormalCost) /
    sum(SalaryHeadcountData$Average_Salary*SalaryHeadcountData$Headcount_Total)
  
  return(NormalCostFinal)
}
GetNormalCostFinal(SalaryHeadcountData)