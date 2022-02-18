rm(list = ls())
library("readxl")
library(tidyverse)
library(dplyr)
library(zoo)

FileName <- 'MT PERS BModel - NDPERS Version.xlsx'
YearStart <- 2021
Age <- 20:120
YOS <- 0:100
RetirementAge <- 20:120
Years <- 2011:2121    #(why 2121? Because 120 - 20 + 2021 = 2121)
#Updated from 2010 to 2011

#Assigning individual  Variables
model_inputs <- read_excel(FileName, sheet = 'Main')

for(i in 1:nrow(model_inputs)){
  if(!is.na(model_inputs[i,2])){
    assign(as.character(model_inputs[i,2]),as.double(model_inputs[i,3]))
  }
}

#Import key data tables
SurvivalRates <- read_excel(FileName, sheet = 'RP_2000')
MaleMP <- read_excel(FileName, sheet = 'MP-2020_Male')
FemaleMP <- read_excel(FileName, sheet = 'MP-2020_Female')
SalaryGrowth <- read_excel(FileName, sheet = "Salary Growth")
SalaryEntry <- read_excel(FileName, sheet = "Salary and Headcount")
TerminationRateAfter5 <- read_excel(FileName, sheet = 'Termination Rate after 5')#Updated*
TerminationRateBefore5 <- read_excel(FileName, sheet = 'Termination Rate before 5')#Updated*
RetirementRates <- read_excel(FileName, sheet = 'Retirement Rates')#Updated*
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
  if(HireType == 'New Hire'){
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
  if(HireType == 'New Hire'){
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
#Mortality Rates and function
#These rates dont change so they're outside the function
#Transform base mortality rates and mortality improvement rates
MaleMP <- MaleMP %>% 
  pivot_longer(-Age, names_to = "Years", values_to = "MP_male") %>% 
  mutate(Years = as.numeric(Years))

MaleMP_ultimate <- MaleMP %>% 
  filter(Years == max(Years)) %>% 
  rename(MP_ultimate_male = MP_male) %>% 
  select(-Years)

FemaleMP <- FemaleMP %>% 
  pivot_longer(-Age, names_to = "Years", values_to = "MP_female") %>% 
  mutate(Years = as.numeric(Years))

FemaleMP_ultimate <- FemaleMP %>% 
  filter(Years == max(Years)) %>% 
  rename(MP_ultimate_female = MP_female) %>% 
  select(-Years)


##Mortality calculations
#Expand grid for ages 20-120 and years 2010 to 2121 (why 2121? Because 120 - 20 + 2021 = 2121)
MortalityTable <- expand_grid(Age, Years,c('New Hire','Legacy'))
colnames(MortalityTable) <- c('Age','Years','HireType')
SurvivalRates <- SurvivalRates %>% mutate_all(as.numeric) 

mortality <- function(data = MortalityTable,
                      SurvivalRates = SurvivalRates,
                      MaleMP = MaleMP,
                      FemaleMP = FemaleMP,
                      MaleMP_ultimate = MaleMP_ultimate,
                      FemaleMP_ultimate = FemaleMP_ultimate
){
  
  MortalityTable <- data %>% 
    left_join(SurvivalRates, by = "Age") %>% 
    left_join(MaleMP, by = c("Age", "Years")) %>% 
    left_join(FemaleMP, by = c("Age", "Years")) %>% 
    left_join(MaleMP_ultimate, by = "Age") %>% 
    left_join(FemaleMP_ultimate, by = "Age") %>% 
    mutate(MaleMP_final = ifelse(Years > max(MaleMP$Years), MP_ultimate_male, MP_male),
           FemaleMP_final = ifelse(Years > max(FemaleMP$Years),  MP_ultimate_female, MP_female),
           entry_age = Age - (Years - YearStart),
           YOS = Age - entry_age) %>% 
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
           mort = (mort_male + mort_female)/2,
           CheckData = IsRetirementEligible(Age, YOS, HireType)) %>% 
    filter(Years >= 2021, entry_age >= 20) %>% 
    ungroup()
  
  MortalityTable
  
}

##### Mortality Function #####
MortalityTable <- mortality(data = MortalityTable,
                            SurvivalRates = SurvivalRates,
                            MaleMP = MaleMP,
                            FemaleMP = FemaleMP,
                            MaleMP_ultimate = MaleMP_ultimate,
                            FemaleMP_ultimate = FemaleMP_ultimate)

#filter out the necessary variables
MortalityTable <- MortalityTable %>% select(Age, Years, HireType, entry_age, mort) %>% 
  arrange(entry_age) 
#
################################################################################################################################################################
#
#Separation Rates
#Linear interpolation of Termination Rates and Retirement Rates
TerminationRateAfter5$TermAfterVest <- LinearInterpolation(TerminationRateAfter5$TermAfterVest)
RetirementRates$Disabled_RetRate <- LinearInterpolation(RetirementRates$Disabled_RetRate)


#Separation Rates
SeparationRates <- expand_grid(Age, YOS,c('New Hire','Legacy'))
colnames(SeparationRates) <- c('Age','YOS','HireType')
SeparationRates <- SeparationRates %>% 
  mutate(entry_age = Age - YOS) %>% 
  filter(entry_age %in% SalaryEntry$entry_age) %>% 
  arrange(entry_age, Age) %>% 
  left_join(TerminationRateAfter5, by = "Age") %>%
  left_join(TerminationRateBefore5, by = "YOS") %>% # Joining by YOS & AGE
  left_join(RetirementRates, by = "Age") 

#If you're retirement eligible, use the retirement rates, then checks YOS < 5 and use the regular termination rates
SeparationRates <- SeparationRates %>% 
  mutate(retirement_type = RetirementType(Age,YOS,HireType),
         
         SepRate = ifelse(retirement_type == "Regular", Regular_RetRate,
                          ifelse(retirement_type == "Reduced", Less30_RetRate,
                                 ifelse(YOS < 5, TermBeforeVest, TermAfterVest))))  %>%
  group_by(entry_age) %>% 
  mutate(RemainingProb = cumprod(1 - lag(SepRate, default = 0)),
         SepProb = lag(RemainingProb, default = 1) - RemainingProb) %>% 
  ungroup()

#Filter out unecessary values
SeparationRates <- SeparationRates %>% select(Age, YOS, RemainingProb, SepProb)
#
################################################################################################################################################################
#
#Salary Data
#colnames(SalaryGrowth)[2] <- "YOS"
#Create a long-form table of Age and YOS and merge with salary data
SalaryData <- expand_grid(Age, YOS, c('New Hire','Legacy')) 
colnames(SalaryData) <- c('Age','YOS','HireType')
SalaryData <- SalaryData %>% 
  mutate(entry_age = Age - YOS) %>%    #Add entry age
  filter(entry_age %in% SalaryEntry$entry_age) %>% 
  arrange(entry_age) %>% 
  left_join(SalaryEntry, by = "entry_age") %>% 
  left_join(SalaryGrowth, by = c("YOS"))

SalaryData <- SalaryData %>% 
  group_by(entry_age) %>% 
  mutate(Salary = Starting_Salary*cumprod(1+lag(Total_Pay_Increase,default = 0)),
         #Salary = pmin(Salary_gross, salary_cap),
         # IRSSalaryCap = pmin(Salary,IRSCompLimit),
         FinalAvgSalary = rollmean(lag(Salary), k = FinAvgSalaryYears, fill = NA, align = "right"),
         EEContrib = DB_EE_Contrib_Rate*Salary,
         DBEEBalance = cumFV(Interest, EEContrib),
         CumulativeWage = cumFV(ARR, Salary)) %>% 
  ungroup()
#
################################################################################################################################################################
#
#Annuity Factor and Reduced Factor
AnnuityF <- function(data = MortalityTable,
                     ColaType = "Simple"){
  
  AnnFactorData <- data %>% 
    select(Age, entry_age, HireType, mort) %>%
    group_by(entry_age) %>% 
    mutate(surv = cumprod(1 - lag(mort, default = 0)),
           surv_DR = surv/(1+ARR)^(Age - entry_age),
           surv_DR_COLA = surv_DR * ifelse(ColaType == "Simple", 1+(COLA * (Age - entry_age)), (1+COLA)^(Age - entry_age)),
           AF = rev(cumsum(rev(surv_DR_COLA)))/surv_DR_COLA,
           AFNormalRetAge = ifelse(HireType == 'New Hire', AF[Age==NormalRetAgeI],AF[Age==NormalRetAgeI_Legacy]),
           SurvProbNormalRetAge = ifelse(HireType == 'New Hire', surv_DR[Age==NormalRetAgeI],surv_DR[Age==NormalRetAgeI_Legacy])) %>% 
    ungroup()
  
  AnnFactorData
  
}

AnnFactorData <- AnnuityF(data = MortalityTable,
                          ColaType = "Compound")

#Reduced Factor
ReducedFactor <- expand_grid(Age, YOS, c('New Hire','Legacy')) 
colnames(ReducedFactor) <- c('Age','YOS','HireType')
ReducedFactor <- ReducedFactor %>% 
  group_by(YOS) %>%
  left_join(AnnFactorData,by = c("Age","HireType")) %>% 
  mutate(NormalRetAge_Final = ifelse(HireType == 'New Hire', NormalRetAgeI,NormalRetAgeI_Legacy),
         RF = ifelse(RetirementType(Age,YOS, HireType) == 'Regular', 1,
                     ifelse(RetirementType(Age,YOS, HireType) == 'Early', AFNormalRetAge / (1+ARR)^(NormalRetAge_Final - Age)*SurvProbNormalRetAge / surv_DR / AF, 0)),
         Ret_Type = RetirementType(Age,YOS, HireType)) %>% 
  rename(RetirementAge = Age) %>% 
  ungroup() 
#
################################################################################################################################################################
#
BenefitsTable <- expand_grid(Age, YOS, RetirementAge, c('New Hire','Legacy')) 
colnames(BenefitsTable) <- c('Age','YOS','RetirementAge', 'HireType')
BenefitsTable <- BenefitsTable %>% 
  mutate(entry_age = Age - YOS) %>% 
  filter(entry_age %in% SalaryEntry$entry_age) %>% 
  arrange(entry_age, Age, RetirementAge) %>% 
  left_join(SalaryData, by = c("Age", "YOS", "HireType","entry_age")) %>% 
  left_join(ReducedFactor %>% select(RetirementAge, YOS, HireType, RF), by = c("RetirementAge", "YOS", "HireType")) %>%
  left_join(AnnFactorData %>% select(Age, entry_age, surv_DR, AF, HireType), by = c("RetirementAge" = "Age", "entry_age", "HireType")) %>%
  
  #Rename surv_DR and AF to make clear that these variables are at retirement
  rename(surv_DR_ret = surv_DR, AF_Ret = AF) %>% 
  
  #Rejoin the table to get the surv_DR for the termination age
  left_join(AnnFactorData %>% select(Age, entry_age, surv_DR, HireType), by = c("Age", "entry_age", "HireType")) %>% 
  mutate(#Legacy Multiplier for early and new hires
    GradedMult_Legacy = ifelse(RetirementType(Age,YOS,'Legacy') == 'Regular',
                               BenMult2*pmin(YOS,25) + BenMult3*pmax(YOS-25,0),
                               #Reduction for early retirement for legacy
                               ifelse(RetirementType(Age,YOS,'Legacy') == 'Early',
                                      BenMult2*pmin(YOS,25) + BenMult3*pmax(YOS-25,0) -
                                        0.05*pmin(EarlyRetAgeII_Legacy - Age,5) -
                                        0.03*pmax(EarlyRetAgeII_Legacy - Age - 5,0),0)),
    GradedMult = BenMult1*pmin(YOS,10) + BenMult2*pmax(pmin(YOS,30)-10,0) + BenMult3*pmax(YOS-30,0),
    
    ReducedFactMult = ifelse(HireType == 'New Hire',RF*GradedMult,GradedMult_Legacy), 
    AnnFactorAdj = AF_Ret * surv_DR_ret / surv_DR,
    PensionBenefit = ReducedFactMult * FinalAvgSalary,
    PresentValue = ifelse(Age > RetirementAge, 0, PensionBenefit*AnnFactorAdj))

#)

#For a given combination of entry age and termination age, the member is assumed to choose the retirement age that maximizes the PV of future retirement benefits. That value is the "optimum benefit". 
OptimumBenefit <- BenefitsTable %>% 
  group_by(entry_age, Age) %>% 
  summarise(MaxBenefit = max(PresentValue)) %>%
  mutate(MaxBenefit = ifelse(is.na(MaxBenefit), 0, MaxBenefit)) %>% 
  ungroup()

####### Benefit Accrual & Normal Cost #######
#### Real Pension Wealth = Pension Wealth adjusted for inflation
#### Actuarial PV of Pension Wealth = Pension Wealth 
#Combine optimal benefit with employee balance and calculate the PV of future benefits and salaries 
#####################################
SalaryData <- SalaryData %>% 
  left_join(OptimumBenefit, by = c("Age", "entry_age")) %>% 
  left_join(SeparationRates, by = c("Age", "YOS")) %>%
  mutate(PenWealth = pmax(DBEEBalance,MaxBenefit),        #Members are assumed to elect the option with the greatest PV between a refund with interest and a deferred benefit
         RealPenWealth = PenWealth/(1 + assum_infl)^YOS,
         PVPenWealth = PenWealth/(1 + ARR)^YOS * SepProb,
         PVCumWage = CumulativeWage/(1 + ARR)^YOS * SepProb)


#Calculate normal cost rate for each entry age
NormalCost <- SalaryData %>% 
  group_by(entry_age) %>% 
  summarise(normal_cost = sum(PVPenWealth)/sum(PVCumWage)) %>% 
  ungroup()

#View(NormalCost)

#Calculate the aggregate normal cost
NC_aggregate <- sum(NormalCost$normal_cost * SalaryEntry$start_sal * SalaryEntry$count_start)/
  sum(SalaryEntry$start_sal * SalaryEntry$count_start)

#Calculate the aggregate normal cost
NC_aggregate
################################

####### DC Account Balance 
SalaryData2 <- SalaryData %>% 
  filter(entry_age == HiringAge) %>% 
  select(Age, YOS, entry_age, start_sal, Total_Pay_Increase, Salary, RemainingProb) %>% 
  mutate(DC_EEContrib = Salary * DC_EE_cont,
         DC_ERContrib = Salary * DC_ER_cont,
         DC_Contrib = DC_EEContrib + DC_ERContrib,
         DC_balance = cumFV(DC_return, DC_Contrib),
         RealDC_balance = DC_balance/(1 + assum_infl)^YOS) %>% 
  left_join(SalaryData %>% select(Age, YOS, RealPenWealth), by = c("Age", "YOS")) %>% 
  mutate(RealHybridWealth = RealDC_balance + RealPenWealth)


#View(SalaryData)
## Graphing PWealth accrual [ALL ENTRY AGES]

ggplot(SalaryData, aes(Age,RealPenWealth/1000, group = entry_age, col = as.factor(entry_age)))+
  geom_line(size = 1)+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 80, by = 10),labels = function(x) paste0(x), 
                     name = "Age (Entry age at 27)", expand = c(0,0)) + 
  scale_y_continuous(breaks = seq(0, 5000, by = 100),labels = function(x) paste0("$",x), 
                     name = "Present Value of Pension Wealth ($Thousands)", expand = c(0,0)) 
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
