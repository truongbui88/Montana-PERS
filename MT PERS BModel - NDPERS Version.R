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
  


#Function to determine if eligible for retirement (either regular or early)
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

SalaryData <- SalaryData %>% 
  group_by(HireType, entry_age) %>% 
  mutate(Salary = Starting_Salary*cumprod(1+lag(Total_Pay_Increase,default = 0)),
         FAS_years = ifelse(HireType == "New Hire", FinAvgSalaryYears, FinAvgSalaryYears_Legacy),
         #Salary = pmin(Salary_gross, salary_cap),
         # IRSSalaryCap = pmin(Salary,IRSCompLimit),
         FinalAvgSalary = rollmean(lag(Salary), k = FAS_years, fill = NA, align = "right"),
         EEContrib = DB_EE_cont*Salary,
         DBEEBalance = cumFV(Interest, EEContrib),
         CumulativeWage = cumFV(ARR, Salary)) %>% 
  ungroup()
#
################################################################################################################################################################
#
#Annuity Factor and Reduced Factor
AnnFactorData <- expand_grid(Age, HireType) %>%      #don't need entry_age as Montana PERS mort rates are fixed (no generational improvement)
  left_join(MortalityTable %>% select(Age, mort), by = "Age") %>% 
  arrange(HireType, Age) %>% 
  group_by(HireType) %>%
  mutate(COLA = ifelse(HireType == "Legacy", COLA_legacy, COLA_new),
         surv = cumprod(1 - lag(mort, default = 0)),
         surv_DR = surv/(1+ARR)^(Age - Age[1]),
         surv_DR_COLA = surv_DR * (1+COLA)^(Age - Age[1]),
         AF = rev(cumsum(rev(surv_DR_COLA)))/surv_DR_COLA)

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
                       YOS < 10 ~ BenMult1,
                       YOS >= 10 & YOS < 30 ~ BenMult2,
                       TRUE ~ BenMult3
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
         PVPenWealth = PenWealth/(1 + ARR)^YOS * SepProb,
         PVCumWage = CumulativeWage/(1 + ARR)^YOS * SepProb)


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
  filter(HireType == Type) %>% 
  pull(NC)

#Calculate the aggregate normal cost
NC_aggregate

# test <- SalaryData %>% 
#   filter(entry_age == 22)
# write.csv(test, "test.csv")

################################

####### DC Account Balance 
SalaryData2 <- SalaryData %>% 
  filter(entry_age == HiringAge, HireType == Type) %>% 
  select(Age, YOS, entry_age, HireType, Starting_Salary, Total_Pay_Increase, Salary, RemainingProb) %>% 
  mutate(DC_EEContrib = Salary * DC_EE_cont,
         DC_ERContrib = Salary * DC_ER_cont,
         DC_Contrib = DC_EEContrib + DC_ERContrib,
         DC_balance = cumFV(DC_return, DC_Contrib),
         RealDC_balance = DC_balance/(1 + assum_infl)^YOS) %>% 
  left_join(SalaryData %>% select(Age, YOS, entry_age, HireType, RealPenWealth), by = c("Age", "YOS", "entry_age", "HireType")) %>% 
  mutate(RealHybridWealth = RealDC_balance + RealPenWealth)


#View(SalaryData)
## Graphing PWealth accrual [ALL ENTRY AGES]
ggplot(SalaryData %>% filter(HireType == Type), aes(Age,RealPenWealth/1000, group = entry_age, col = as.factor(entry_age)))+
  geom_line(size = 1)+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 80, by = 10),labels = function(x) paste0(x), 
                     name = "Age (Entry age at 27)", expand = c(0,0)) + 
  scale_y_continuous(breaks = seq(0, 5000, by = 100),labels = function(x) paste0("$",x), 
                     name = "Present Value of Pension Wealth ($Thousands)", expand = c(0,0)) 

##Graphing DC vs DB wealth 
ggplot(SalaryData2 %>% filter(Age <= 75), aes(x = Age)) +
  geom_line(aes(y = RealPenWealth), col = "red") +
  geom_line(aes(y = RealDC_balance), col = "blue")

##################################

######### Graphing SINGLE ENTRY AGE + RETENTION

# palette_reason <- list(Orange="#FF6633",
#                        LightOrange="#FF9900",
#                        DarkGrey="#333333", 
#                        LightGrey= "#CCCCCC", 
#                        SpaceGrey ="#A69FA1",
#                        DarkBlue="#0066CC", 
#                        GreyBlue= "#6699CC", 
#                        Yellow= "#FFCC33",
#                        LightBlue = "#66B2FF", 
#                        SatBlue = "#3366CC", 
#                        Green = "#669900",LightGreen = "#00CC66", Red = "#CC0000",LightRed="#FF0000")
# 
# 
# colnames(SalaryData2)[13] <- "PVPenWealth"
# e.age <- unique(SalaryData2$entry_age)
# SalaryData2 <- data.frame(SalaryData2)
# SalaryData2$entry_age <- as.numeric(SalaryData2$entry_age)
# # #View(SalaryData2)
# # 
# SalaryData2 <- SalaryData2 %>% filter(entry_age == 27)
# SalaryData2 <- SalaryData2 %>% filter(Age < 81)
# SalaryData2$PVPenWealth <- as.numeric(SalaryData2$PVPenWealth)
# y_max <- max(SalaryData2$PVPenWealth)
# 
# 
# ####
# pwealth <- ggplot(SalaryData2, aes(Age,PVPenWealth/1000))+
#   geom_line(aes(group = 1,
#                 text = paste0("Age: ", Age,
#                               "<br>DB Pension Wealth: $",round(PVPenWealth/1000,1), " Thousands")),size = 1.25, color = palette_reason$SatBlue)+
#   geom_line(aes(Age, RealDC_balance/1000,
#                 group = 2,
#                 text = paste0("Age: ", Age,
#                               "<br>DC Wealth: $", round(RealDC_balance/1000,1), " Thousands")), size = 1.25, color = palette_reason$Orange)+
#   geom_line(aes(Age, RemainingProb* (y_max/1000),
#                 group = 3,
#                 text = paste0("Age: ", Age,
#                               "<br>Members Remaining: ", round(RemainingProb*100,1), "%")), size = 1.25, color = palette_reason$LightBlue, linetype = "dashed")+
#   scale_x_continuous(breaks = seq(0, 80, by = 10),labels = function(x) paste0(x),
#                      name = paste0("Age (Entry age at 22 )"), expand = c(0,0)) +
#   
#   scale_y_continuous(breaks = seq(0, 5000, by = 100),limits = c(0, y_max/1000*1.1), labels = function(x) paste0("$",x),
#                      sec.axis = sec_axis(~./(y_max/100), breaks = scales::pretty_breaks(n = 10), name = "Percent of Members Remaining",
#                                          labels = function(b) paste0(round(b, 0), "%")), 
#                      name = "Present Value of Pension Wealth ($Thousands)", expand = c(0,0)) +
#   theme_bw()+
#   theme(   #panel.grid.major = element_blank(),
#     #panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
#     plot.margin = margin(0.5, 0.5,0.5,0.5, "cm"),
#     axis.text.y = element_text(size=11, color = "black"),
#     axis.text.y.right = element_text(size=11, color = "black"),
#     axis.text.y.left = element_text(size=11, color = "black"),
#     axis.text.x = element_text(size=11, color = "black"),
#     legend.title = element_text(size = 9, colour = "black", face = "bold"))
# 
# library(plotly)
# ggplotly(pwealth, tooltip = c("text"))

#######################
