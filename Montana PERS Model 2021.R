library("readxl")
library(ggplot2)
library(tidyverse)
setwd(getwd())
rm(list = ls())
#
#User can change these values
StartYear <- 2015
StartProjectionYear <- 2022
EndProjectionYear <- 2050
FileName <- 'Model Inputs 2021.xlsx'
#
#Reading Input File
user_inputs_numeric <- read_excel(FileName, sheet = 'Numeric Inputs')
user_inputs_character <- read_excel(FileName, sheet = 'Character Inputs')
Historical_Data <- read_excel(FileName, sheet = 'Historical Data')
Scenario_Data <- read_excel(FileName, sheet = 'Inv_Returns')
BenefitPayments <- read_excel(FileName, sheet = 'Benefit Payments')
#
##################################################################################################################################################################
#
#Functions for later use
#Function for Present Value for Amortization
PresentValue = function(rate, nper, pmt) {
  PV = pmt * (1 - (1 + rate) ^ (-nper)) / rate * (1 + rate)
  return(PV)
}

NPV = function(rate, cashflows) {
  for(i in 1:length(cashflows)){
    if(i == 1){
      NPV <- cashflows[i]/((1+rate)^(i))
    } else {
      NPV <- NPV + cashflows[i]/((1+rate)^(i))
    }
  }
  
  return(NPV)
}

#Function for calculating amo payments
#pmt0 = basic amo payment calculation, assuming payment beginning of period 
PMT0 <- function(r, nper, pv) {
  if (r == 0) {
    a <- pv/nper
  } else {
    a <- pv*r*(1+r)^(nper-1)/((1+r)^nper-1)  
  }
  
  if(nper == 0){
    a <- 0
  }
  
  return(a)
}

#pmt = amo payment function with growth rate and timing added; t = 1 for end of period payment, 0.5 for half period, and 0 for beginning of period. 
PMT <- function(r, g = 0, nper, pv, t = 1) {
  a <- PMT0((1+r)/(1+g) - 1, nper, pv*(1+r)^t)
  return(a)
}

#Bisection Method
#GetNPER(NewDR[i],AmoBaseInc,UAL_AVA[i-1],0.5,VarStat_AmoPayment[i])
#PresentValue(((1+NewDR[i])/(1+AmoBaseInc))-1,AmoYearsInput_Current[ProjectionCount+1,2],1) / ((1+NewDR[i])^0.5)
GetNPER <- function(r,g,pv,t,pmt){
  PV <- pv*(1+r)^t
  R <- (1+r)/(1+g)
  TempValue <- PV*(R+1)/(R*pmt)
  #print(PV)
  #print(pmt)
  #print(TempValue)
  #print(R)
  NPER <- log(1/abs(1-TempValue),R)
  print(NPER)
  return(NPER)
  
  
}
#
##################################################################################################################################################################
#
#Reading Values from Input input file and assigning values
#Assigning numeric inputs
for(i in 1:nrow(user_inputs_numeric)){
  if(!is.na(user_inputs_numeric[i,2])){
    assign(as.character(user_inputs_numeric[i,2]),as.double(user_inputs_numeric[i,3]))
  }
}
#
#Assigning character inputs
for(i in 1:nrow(user_inputs_character)){
  if(!is.na(user_inputs_character[i,2])){
    assign(as.character(user_inputs_character[i,2]),as.character(user_inputs_character[i,3]))
  }
}

#Create an empty Matrix for the Projection Years
EmptyMatrix <- matrix(0,(EndProjectionYear - StartProjectionYear + 1), 1)
for(j in 1:length(colnames(Historical_Data))){
  TempMatrix <- rbind(as.matrix(Historical_Data[,j]), EmptyMatrix)
  assign(as.character(colnames(Historical_Data)[j]), TempMatrix)
}
#Assign values for Projection Years
FYE <- StartYear:EndProjectionYear
#Get Start Index, since historical data has 3 rows, we want to start at 4
StartIndex <- StartProjectionYear - StartYear + 1
HistoricalIndex <- StartProjectionYear - StartYear

#Initialize the Inflation Adjusted Variables to use later
UAL_AVA_InflAdj <- UAL_AVA
colnames(UAL_AVA_InflAdj) <- 'UAL_AVA_InflAdj'
UAL_MVA_InflAdj <- UAL_MVA
colnames(UAL_MVA_InflAdj) <- 'UAL_MVA_InflAdj'

#Assign values for simulation
if(SimType == 'Assumed'){
  SimReturn <- SimReturnAssumed
} else if(AnalysisType == 'Conservative'){
  SimReturn <- SimReturnConservative
}

#Initialize Amortization and Outstnading Base
RowColCount <- (EndProjectionYear - StartProjectionYear + 1)
OutstandingBase <- matrix(0,RowColCount, RowColCount + 1)
Amortization <- matrix(0,RowColCount, RowColCount + 1)
AmoYearsInput_Current <- read_excel(FileName, sheet = 'Amortization_CurrentDebt')
AmoYearsInput_New <- read_excel(FileName, sheet = 'Amortization_NewDebt')
#
##################################################################################################################################################################
#
#Offset Matrix. Used later for Amortization calculation
OffsetYears <- matrix(0,RowColCount, RowColCount)
for(i in 1:nrow(OffsetYears)){
  RowCount <- i
  ColCount <- 1
  #This is to create the "zig-zag" pattern of amo years. you also want it to become 0 if the amo years is greater than the payments
  #Meaning if its 10 years, then after 10 years, the amo payment is 0
  while(RowCount <= nrow(OffsetYears) && (ColCount <= as.double(AmoYearsInput_New[i,2]))){
    OffsetYears[RowCount,ColCount] <- as.double(AmoYearsInput_New[i,2])
    RowCount <- RowCount + 1
    ColCount <- ColCount + 1
  }
}

for(i in 1:nrow(OffsetYears)-1){
  for(j in 1:i+1){
    if(OffsetYears[i+1,j] > 0) {
      OffsetYears[i+1,j] <- max(OffsetYears[i+1,j] - j + 1, 1) 
    }
  }
}

#Initialize first row of Amortization and Outstanding Base
OutstandingBase[1,1] <- UAL_AVA[HistoricalIndex]
rate <- ((1 + NewDR[HistoricalIndex]) / (1 + AmoBaseInc)) - 1
period <- max(OffsetYears[1,1], 1)
pmt <- 1
Amortization[1,1] <- OutstandingBase[1,1] / (PresentValue(rate,period,pmt) / ((1+NewDR[HistoricalIndex])^0.5))
#
##################################################################################################################################################################
#
#This is payroll upto acrrued liability. these values do not change regardless of scenario or other stress testing
#In order to optimize, they are placed outside the function
for(i in StartIndex:length(FYE)){
  #Payroll
  TotalPayroll[i] <- TotalPayroll[i-1]*(1 + Payroll_growth) 
  if(i == StartIndex){
    PayrollLegacy_Pct[i] <- 0.95
    PayrollNewTier[i] <- 0.05
  } else {
    PayrollLegacy_Pct[i] <- PayrollLegacy_Pct[i-1]*0.9
    PayrollNewTier[i] <- 1 - PayrollLegacy_Pct[i]
  }
  NewHirePayroll[i] <- PayrollNewTier[i]*TotalPayroll[i]
  PayrollLegacy[i] <- PayrollLegacy_Pct[i]*TotalPayroll[i]
  #
  #Discount Rate
  OriginalDR[i] <- dis_r
  NewDR[i] <- dis_r_proj
  #
  #Benefit Payments, Admin Expenses
  BenPayments[i] <- as.double(BenefitPayments$Baseline_BP[i])
  Refunds[i] <- as.double(BenefitPayments$Refunds_Admin[i])
  AdminExp[i] <- -1*Admin_Exp_Pct*TotalPayroll[i]
  #
  #Accrued Liability, MOY NC - Original DR
  MOYNCExistOrigDR[i] <- PayrollLegacy[i]*NC_CurrentHires_Pct_1
  if(NewHirePlan == 'DB'){
    MOYNCNewHiresOrigDR[i] <- NewHirePayroll[i]*NC_NewHires_Pct_1
  } else if(NewHirePlan == 'Hybrid'){
    MOYNCNewHiresOrigDR[i] <- NewHirePayroll[i]*NC_NewHires_Pct_1*(1-PctNotUsed_NewHires)
  } else {
    MOYNCNewHiresOrigDR[i] <- 0
  }
  AccrLiabOrigDR[i] <- AccrLiabOrigDR[i-1]*(1+OriginalDR[i]) + (MOYNCExistOrigDR[i]+MOYNCNewHiresOrigDR[i])*(1+OriginalDR[i])^0.5 + (BenPayments[i]+Refunds[i])*(1+OriginalDR[i])^0.5
  #
  #Accrued Liability, MOY NC - New DR
  DRDifference <- 100*(OriginalDR[i] - NewDR[i])
  MOYNCExistNewDR[i] <- MOYNCExistOrigDR[i]*((1+(NCSensDR/100))^(DRDifference))
  MOYNCNewHiresNewDR[i] <- MOYNCNewHiresOrigDR[i]*((1+(NCSensDR/100))^(DRDifference))
  AccrLiabNewDR[i] <- AccrLiabOrigDR[i]*((1+(LiabSensDR/100))^(DRDifference))*((1+(Convexity/100))^((DRDifference)^2/2))
  #
  #NC, ER, EE
  TotalNC_Pct[i] <- (MOYNCExistNewDR[i]+MOYNCNewHiresNewDR[i]) / TotalPayroll[i]
  NC_Legacy_Pct[i] <- MOYNCExistNewDR[i] / PayrollLegacy[i]
  NC_NewHires_Pct[i] <- MOYNCNewHiresNewDR[i] / NewHirePayroll[i]
  EE_NC_Legacy_Pct[i] <- EEContrib_CurrentHires
  
  if(NewHirePlan == 'DC'){
    EE_NC_NewHires_Pct[i] <- 0
  } else {
    EE_NC_NewHires_Pct[i] <- EEContrib_NewHires
  }
  
  ER_NC_Legacy_Pct[i] <- NC_Legacy_Pct[i] - EE_NC_Legacy_Pct[i]
  ER_NC_NewHires_Pct[i] <- NC_NewHires_Pct[i] - EE_NC_NewHires_Pct[i]
  Total_EE[i] <- ((ER_NC_Legacy_Pct[i]*PayrollLegacy[i]) + (ER_NC_NewHires_Pct[i]*NewHirePayroll[i])) / TotalPayroll[i]
  
  if(FYE[i] < 2025){
    Stat_ER[i] <- Stat_ER[i-1] + 0.001
  } else {
    Stat_ER[i] <- Stat_ER[i-1]
  }
  Suppl_Contrib[i] <- Suppl_Contrib[i-1]*1.01
  EffStat_ER[i] <- (Stat_ER[i]*TotalPayroll[i] + Suppl_Contrib[i]) / TotalPayroll[i]
  
  EE_NC_CurrentHires[i] <- EE_NC_Legacy_Pct[i]*PayrollLegacy[i]
  
  if(NewHirePlan == 'Hybrid'){
    EE_NC_NewHires[i] <- EE_NC_NewHires_Pct[i]*NewHirePayroll[i]*(1-PctNotUsed_NewHires) 
  } else {
    EE_NC_NewHires[i] <- EE_NC_NewHires_Pct[i]*NewHirePayroll[i]
  }
}
#
##################################################################################################################################################################
#Running the model from NC onwards. this gets iterated for different scenarios
LvDollarorPercent <- 'Lv%'
RunModel <- function(AnalysisType, SimReturn, SimVolatility, ER_Policy, ScenType, SupplContrib, LvDollarorPercent){
  #Scenario Index for referencing later based on investment return data
  ScenarioIndex <- which(colnames(Scenario_Data) == as.character(ScenType))
  
  #Default value is Lv% for Amo Base
  #If its Level Perecent, then set to 0
  if(LvDollarorPercent == 'Lv$'){
    AmoBaseInc <- 0
    #Recalculate first value for Lv$
    OutstandingBase[1,1] <- UAL_AVA[HistoricalIndex]
    rate <- ((1 + NewDR[HistoricalIndex]) / (1 + AmoBaseInc)) - 1
    period <- max(OffsetYears[1,1], 1)
    pmt <- 1
    Amortization[1,1] <- OutstandingBase[1,1] / (PresentValue(rate,period,pmt) / ((1+NewDR[HistoricalIndex])^0.5))
  }
  
  #intialize this value at 0 for Total ER Contributions
  Total_ER[StartIndex-1] <- 0
  #length(FYE
  for(i in StartIndex:length(FYE)){
    #ProjectionCount is used because amortization and return scenarios do not start at the same time as start index
    #Because start index includes historical data and thus might be 3 by the time ProjectionCount is 1
    ProjectionCount <- i - StartIndex + 1
    
    CurrentHireFixStat <- NC_Legacy_Pct[i] + Admin_Exp_Pct - EE_NC_Legacy_Pct[i]
    NewHireFixStat <- NC_NewHires_Pct[i] + Admin_Exp_Pct - EE_NC_Legacy_Pct[i]
    DBHybrid <- 1 - PctNotUsed_NewHires
    if(NewHirePlan == 'DB'){
      FixedStat_AmoPayment[i] <- EffStat_ER[i]*TotalPayroll[i] - (CurrentHireFixStat*PayrollLegacy[i]) - (NewHireFixStat*NewHirePayroll[i])
    } else if(NewHirePlan == 'DC'){
      if(ERAmo_Type == 'DB-only Payroll'){
        FixedStat_AmoPayment[i] <- EffStat_ER[i]*PayrollLegacy[i] - (CurrentHireFixStat*PayrollLegacy[i])
      } else {
        FixedStat_AmoPayment[i] <- EffStat_ER[i]*TotalPayroll[i] - (CurrentHireFixStat*PayrollLegacy[i])
      }
    } else if(NewHirePlan == 'Hybrid'){
      if(ERAmo_Type == 'DB-only Payroll'){
        FixedStat_AmoPayment[i] <- EffStat_ER[i]*(PayrollLegacy[i]+DBHybrid*NewHirePayroll[i]) - (CurrentHireFixStat*PayrollLegacy[i]) - (NewHireFixStat*DBHybrid*NewHirePayroll[i])
      } else {
        FixedStat_AmoPayment[i] <- EffStat_ER[i]*TotalPayroll[i] - (CurrentHireFixStat*PayrollLegacy[i]) - (NewHireFixStat*DBHybrid*NewHirePayroll[i])
      }
    }
    #print(i)
    #print(FundPeriod[i])
    if(i == StartIndex){
      VarStat_AmoPayment[i] <- FixedStat_AmoPayment[i]
    } else {
      #print(FundPeriod[i-1])
      if((FundPeriod[i-1] <= AmoYearsInput_Current[ProjectionCount+1,2]) && (UAL_AVA[i-1] > 0)){
        VarStat_AmoPayment[i] <- VarStat_AmoPayment[i-1]*(1+AmoBaseInc)
      } else{
        if(UAL_AVA[i-1] <= 0){
          VarStat_AmoPayment[i] <- 0
        } else {
          VarStat_AmoPayment[i] <- UAL_AVA[i-1] / AmoFactor[i-1]
        }
      }
    }
    
    #FundPeriod[i] <- GetNPER(NewDR[i],AmoBaseInc,UAL_AVA[i-1],0.5,VarStat_AmoPayment[i])
    #Placeholder. will update later
    FundPeriod[i] <- 29
    #print(FundPeriod[i])
    AmoFactor[i] <- as.matrix(PresentValue(((1+NewDR[i])/(1+AmoBaseInc))-1,AmoYearsInput_Current[ProjectionCount+1,2],1) / ((1+NewDR[i])^0.5))
    
    if(ER_Policy == 'Statutory Rate'){
      Amo_Policy[i] <- FixedStat_AmoPayment[i]
    } else if(ER_Policy == 'Variable Statutory'){
      Amo_Policy[i] <- VarStat_AmoPayment[i]
    } else if(ER_Policy == 'ADC'){
      Amo_Policy[i] <- sum(Amortization[ProjectionCount,])
    }
    AmoRate_Pct[i] <- Amo_Policy[i] / TotalPayroll[i]
    
    if((ContrFreeze == 'Freeze') && (FYE[i] < 2026)){
      ER_NC_CurrentHires[i] <- ER_NC_CurrentHires[i-1]
      ER_NC_NewHires[i] <- ER_NC_NewHires[i-1]
      ER_Amo_CurrentHires[i] <- max(-ER_NC_CurrentHires[i], ER_Amo_CurrentHires[i-1])
      ER_Amo_NewHires[i] <- max(-ER_NC_NewHires[i], ER_Amo_NewHires[i-1])
    } else {
      ER_NC_CurrentHires[i] <- (ER_NC_Legacy_Pct[i]+Admin_Exp_Pct)*PayrollLegacy[i]
      if(NewHirePlan == 'DC'){
        ER_NC_NewHires[i] <- 0
        if(ERAmo_Type == 'DB-only Payroll'){
          ER_Amo_CurrentHires[i] <- max(-ER_NC_CurrentHires[i], AmoRate_Pct[i]*TotalPayroll[i])
          ER_Amo_NewHires[i] <- max(-ER_NC_NewHires[i], 0)
        }
      } else if(NewHirePlan == 'Hybrid'){
        ER_NC_NewHires[i] <- DBHybrid*ER_NC_NewHires_Pct[i]*NewHirePayroll[i]
        if(ERAmo_Type == 'DB-only Payroll'){
          ER_Amo_CurrentHires[i] <- max(-ER_NC_CurrentHires[i], AmoRate_Pct[i]*(PayrollLegacy[i] + NewHirePayroll[i]*PctNotUsed_NewHires))
          ER_Amo_NewHires[i] <- max(-ER_NC_NewHires[i], AmoRate_Pct[i]*NewHirePayroll[i]*(1-PctNotUsed_NewHires))
        }
      } else {
        ER_NC_NewHires[i] <- ER_NC_NewHires_Pct[i]*NewHirePayroll[i]
        ER_Amo_CurrentHires[i] <- max(-ER_NC_CurrentHires[i], AmoRate_Pct[i]*PayrollLegacy[i])
        ER_Amo_NewHires[i] <- max(-ER_NC_NewHires[i], AmoRate_Pct[i]*NewHirePayroll[i])
      }
    }
    #
    #Return data based on deterministic or stochastic
    if((AnalysisType == 'Stochastic') && (i >= StartIndex)){
      ROA_MVA[i] <- rnorm(1,SimReturn,SimVolatility)
    } else if(AnalysisType == 'Deterministic'){
      ROA_MVA[i] <- as.double(Scenario_Data[i,ScenarioIndex]) 
    }
    #
    #Solvency Contribution, Net CF, Expected MVA
    CashFlows <- BenPayments[i] + AdminExp[i] +  EE_NC_CurrentHires[i] + EE_NC_NewHires[i] + ER_NC_CurrentHires[i] + ER_NC_NewHires[i] + ER_Amo_CurrentHires[i] + ER_Amo_NewHires[i]
    Solv_Contrib[i] <- as.double(max(-(MVA[i-1]*(1+ROA_MVA[i]) + CashFlows*(1+ROA_MVA[i])^0.5) / (1+ROA_MVA[i])^0.5,0))
    
    NetCF[i] <- CashFlows + Solv_Contrib[i]
    ExpInvInc[i] <- (MVA[i-1]*NewDR[i-1]) + (NetCF[i]*NewDR[i-1]*0.5)
    ExpectedMVA[i] <- MVA[i-1] + NetCF[i] + ExpInvInc[i]
    MVA[i] <- MVA[i-1]*(1+ROA_MVA[i]) + NetCF[i]*(1+ROA_MVA[i])^0.5
    #
    #Gain Loss, Defered Losses
    GainLoss[i] <- MVA[i] - ExpectedMVA[i] 
    DeferedCurYear[i] <- GainLoss[i]*(0.8/1)
    Year1GL[i] <- DeferedCurYear[i-1]*(0.6/0.8)
    Year2GL[i] <- Year1GL[i-1]*(0.4/0.6)
    Year3GL[i] <- Year2GL[i-1]*(0.2/0.4)
    TotalDefered[i] <- Year1GL[i] + Year2GL[i] + Year3GL[i] + DeferedCurYear[i]
    #
    #AVA, MVA, UA, FR
    AVA[i] <- MVA[i] - TotalDefered[i]
    AVA[i] <- min(AVA[i],AVA_upperbound*MVA[i])
    AVA[i] <- max(AVA[i],AVA_lowerbound*MVA[i])
    UAL_AVA[i] <- AccrLiabNewDR[i] - AVA[i]
    UAL_MVA[i] <- AccrLiabNewDR[i] - MVA[i]
    FR_AVA[i] <- AVA[i] / AccrLiabNewDR[i]
    FR_MVA[i] <- MVA[i] / AccrLiabNewDR[i]
    UAL_AVA_InflAdj[i] <- UAL_AVA[i] / ((1 + asum_infl)^(FYE[i] - NC_StaryYear))
    UAL_MVA_InflAdj[i] <- UAL_MVA[i] / ((1 + asum_infl)^(FYE[i] - NC_StaryYear))
    #
    #Employer Contribution
    Total_Contrib_DB[i] <- ER_NC_CurrentHires[i] + ER_NC_NewHires[i] + ER_Amo_CurrentHires[i] + ER_Amo_NewHires[i] + Solv_Contrib[i]
    if(NewHirePlan == "Hybrid"){
      Total_Contrib_DC[i] <- Hybrid_Contrib*NewHiresDBHybridPayroll[i]
    } else if (NewHirePlan == "DC"){
      Total_Contrib_DC[i] <- DC_Contrib*NewHiresDCPayroll[i]
    }
    Total_Contrib[i] <- max(Total_Contrib_DB[i] + Total_Contrib_DC[i],0)
    
    ER_InflAdj[i] <- Total_Contrib[i] / ((1 + asum_infl)^(FYE[i] - NC_StaryYear))
    ER_Percentage[i] <- Total_Contrib[i] / TotalPayroll[i]
    #Total ER starts after first year so set the first year to 0.
    if(i < (StartIndex+1)){
      Total_ER[i] <- 0
    } else {
      Total_ER[i] <- Total_ER[i-1] + ER_InflAdj[i] 
    }
    AllInCost[i] <- Total_ER[i] + UAL_MVA_InflAdj[i]
    #
    #Amortization
    #Current Hires
    
    if(ProjectionCount < nrow(Amortization)){
      OutstandingBase[ProjectionCount+1,2:(ProjectionCount + 1)] <- OutstandingBase[ProjectionCount,1:ProjectionCount]*(1 + NewDR[i-1]) - (Amortization[ProjectionCount,1:ProjectionCount]*(1 + NewDR[i-1])^0.5)
      OutstandingBase[ProjectionCount+1,1] <- UAL_AVA[i] - sum(OutstandingBase[ProjectionCount+1,2:ncol(OutstandingBase)])
      
      #Amo Layers
      Amortization[ProjectionCount+1,1:(ProjectionCount + 1)] <- PMT(pv = OutstandingBase[ProjectionCount+1,1:(ProjectionCount + 1)], 
                                                                     r = NewDR[i-1], g = AmoBaseInc, t = 0.5,
                                                                     nper = pmax(OffsetYears[ProjectionCount+1,1:(ProjectionCount + 1)],1))
    }
  }
  
  Output <- cbind(FYE,TotalPayroll,PayrollLegacy,NewHirePayroll,PayrollLegacy_Pct,PayrollNewTier,OriginalDR,NewDR,
                  AccrLiabOrigDR,MOYNCExistOrigDR,MOYNCNewHiresOrigDR,AccrLiabNewDR,MOYNCExistNewDR,MOYNCNewHiresNewDR,
                  AVA,MVA,ROA_MVA,UAL_AVA,UAL_MVA,UAL_MVA_InflAdj, UAL_AVA_InflAdj, FR_AVA,FR_MVA,TotalNC_Pct,NC_Legacy_Pct,
                  NC_NewHires_Pct,EE_NC_Legacy_Pct,EE_NC_NewHires_Pct,ER_NC_Legacy_Pct,ER_NC_NewHires_Pct,AmoRate_Pct,Total_Contrib_Pct,
                  Total_EE,BenPayments,Refunds,AdminExp,EE_NC_CurrentHires,EE_NC_NewHires,ER_NC_CurrentHires,ER_NC_NewHires,ER_Amo_CurrentHires,
                  ER_Amo_NewHires,Additional_ER,Solv_Contrib,Total_Contrib_DB,Total_Contrib_DC,Total_Contrib,AllInCost,ER_InflAdj,
                  Total_ER,ER_Percentage,NetCF,ExpInvInc,ExpectedMVA,GainLoss,DeferedCurYear,Year1GL,Year2GL,Year3GL,TotalDefered,
                  FundPeriod,Amo_Policy,VarStat_AmoPayment,FixedStat_AmoPayment,EffStat_ER,Stat_ER,Suppl_Contrib,AmoFactor)
  
  return(Output)
}
#
# ##################################################################################################################################################################
#
#Scenarios
Scenario_Returns <- as.data.frame(FYE)
Scenario_UAL <- as.data.frame(FYE)
Scenario_FR <- as.data.frame(FYE)
Scenario_ER_Percentage <- as.data.frame(FYE)
Scenario_ER_InflAdj <- as.data.frame(FYE)
Scenario_Total_ER <- as.data.frame(FYE)
Scenario_AllIn_ER <- as.data.frame(FYE)

#There are 3 types of scenarios here - Recessions, Supplemental and Lv$%
#We are trying to run all of them outside of a function because we need the data for UAL, FR, etc.
#If we run them in a function, we can only generate one output
ScenarioRuns <- 'Return Scenarios'
#Initialize Max Length, this will be used at the end
MaxLength <- 0
if(ScenarioRuns == 'Return Scenarios'){
  Scenarios <- c('Assumption','Model','Recession','Recurring Recession')
  for (i in 1:length(Scenarios)){
    NewData <- as.data.frame(RunModel('Deterministic',SimReturn, SimVolatility, 'ADC', Scenarios[i], c(0), 'Lv%'))
    Scenario_Returns <- cbind(Scenario_Returns,NewData$ROA_MVA)
    Scenario_UAL <- cbind(Scenario_UAL,NewData$UAL_MVA_InflAdj)
    Scenario_FR <- cbind(Scenario_FR,NewData$FR_MVA)
    Scenario_ER_Percentage <- cbind(Scenario_ER_Percentage,NewData$ER_Percentage)
    Scenario_ER_InflAdj <- cbind(Scenario_ER_InflAdj,NewData$ER_InflAdj)
    Scenario_Total_ER <- cbind(Scenario_Total_ER,NewData$Total_ER)
    Scenario_AllIn_ER <- cbind(Scenario_AllIn_ER,NewData$AllInCost)
  }
  #Scenario names should be the same as Scenarios but for some cases like supplemental, lv$, etc. it will be different
  ScenarioNames <- Scenarios
  
} else if(ScenarioRuns == 'Lv$ vs %'){
  Scenarios <- c('Assumption','Lv%','Assumption','Lv$','Recurring Recession','Lv%','Recurring Recession','Lv$')
  MaxLength <- length(Scenarios)/2
  for (i in 1:MaxLength){
    NewData <- as.data.frame(RunModel('Deterministic',SimReturn, SimVolatility, 'ADC', Scenarios[i*2 - 1], c(0), Scenarios[i*2]))
    Scenario_Returns <- cbind(Scenario_Returns,NewData$ROA_MVA)
    Scenario_UAL <- cbind(Scenario_UAL,NewData$UAL_MVA_InflAdj)
    Scenario_FR <- cbind(Scenario_FR,NewData$FR_MVA)
    Scenario_ER_Percentage <- cbind(Scenario_ER_Percentage,NewData$ER_Percentage)
    Scenario_ER_InflAdj <- cbind(Scenario_ER_InflAdj,NewData$ER_InflAdj)
    Scenario_Total_ER <- cbind(Scenario_Total_ER,NewData$Total_ER)
    Scenario_AllIn_ER <- cbind(Scenario_AllIn_ER,NewData$AllInCost)
  }
  #Scenario names should be the same as Scenarios but for some cases like supplemental, lv$, etc. it will be different
  ScenarioNames <- c('Assumption Lv%', 'Assumption Lv$', 'Recurring Recession Lv%', 'Recurring Recession Lv$')
}

#MaxLength should in theory be the lenght of the scenarios but because of Lv$%, it may not be
#Hence we have to do the max function
if(ScenarioRuns != 'Lv$ vs %'){
  MaxLength <- length(Scenarios)
}

for(i in 1:MaxLength){
  #Start from StartIndex because thats when the projection is
  #Total ER is already inflation adjusted
  TotalERScenario <- sum(Scenario_Total_ER[nrow(Scenario_Total_ER),i+1])/1000
  #inflation adjusted UAL
  EndingUAL <- Scenario_UAL[nrow(Scenario_UAL),i+1]/1000
  AllInER <- Scenario_AllIn_ER[nrow(Scenario_AllIn_ER),i+1]/1000
  
  if(i == 1){
    ERCostTable <- c(TotalERScenario,EndingUAL, AllInER)
  } else {
    ERCostTable <- rbind(ERCostTable, c(TotalERScenario,EndingUAL, AllInER))
  }
}
colnames(ERCostTable) <- c('Total ER Contributions','Ending UAL','All in ER Cost')
rownames(ERCostTable) <- ScenarioNames

colnames(Scenario_Returns) <- c('FYE',ScenarioNames)
colnames(Scenario_UAL) <- c('FYE',ScenarioNames)
colnames(Scenario_FR) <- c('FYE',ScenarioNames)
colnames(Scenario_ER_Percentage) <- c('FYE',ScenarioNames)
colnames(Scenario_ER_InflAdj) <- c('FYE',ScenarioNames)

ScenarioPlot <- function(Data, YAxisLabel){
  ggplot(Data, aes(x = FYE)) +
    geom_line(aes(y = Data[,2]), color = "#FF6633", size = 2) +
    geom_line(aes(y = Data[,3]), color = "#FFCC33", size = 2) +
    geom_line(aes(y = Data[,4]), color = "#0066CC", size = 2) +
    geom_line(aes(y = Data[,5]), color = "#CC0000", size = 2) +
    theme(legend.position = "top") +
    #ggplot2::theme(legend.text = 'Test', legend.title = 'test') +
    #scale_linetype_manual(values = c('dashed','dashed','dashed','dashed')) +
    labs(y = YAxisLabel, x = 'Year') + ggtitle(YAxisLabel)
  #scale_linetype_manual(labels = '')
}
ScenarioPlot(Scenario_UAL, 'Unfunded Liabilities (MVA)')
#
##################################################################################################################################################################
#
#Simulations
# start_time <- Sys.time()
# #Set seed insures a consistency when simulations are run multiple times
# set.seed((1234))
# NumberofSimulations <- 1000
# #initialize the return simulations based on years and # of simulations
# Returns_Sims <- matrix(1:length(FYE),nrow = length(FYE), ncol = NumberofSimulations + 1)
# UAL_Sims <- matrix(1:length(FYE),nrow = length(FYE), ncol = NumberofSimulations + 1)
# FR_Sims <- matrix(1:length(FYE),nrow = length(FYE), ncol = NumberofSimulations + 1)
# ER_Sims <- matrix(1:length(FYE),nrow = length(FYE), ncol = NumberofSimulations + 1)
#
# #Run the simulations
# for (i in 1:NumberofSimulations){
#   NewData <- as.data.frame(RunModel('Stochastic', SimReturnAssumed, SimVolatility, 'ADC', '', c(0), 'Lv%'))
#   Returns_Sims[,i+1] <- NewData$ROA_MVA
#   UAL_Sims[,i+1] <- NewData$UAL_MVA_InflAdj
#   FR_Sims[,i+1] <- NewData$FR_MVA
#   ER_Sims[,i+1] <- NewData$ER_Percentage
# }
#
# Simulations_Returns <- cbind(FYE,FYE,FYE)
# Simulations_UAL <- cbind(FYE,FYE,FYE)
# Simulations_FR <- cbind(FYE,FYE,FYE)
# Simulations_ER <- cbind(FYE,FYE,FYE)
#
# #Get the 25th, 50th, 75th percentile
# for(i in 1:length(FYE)){
#   Simulations_Returns[i,] <- t(quantile(Returns_Sims[i,2:ncol(Returns_Sims)],c(0.25,0.5,0.75)))
#   Simulations_UAL[i,] <- t(quantile(UAL_Sims[i,2:ncol(UAL_Sims)],c(0.25,0.5,0.75)))
#   Simulations_FR[i,] <- t(as.data.frame(quantile(FR_Sims[i,2:ncol(FR_Sims)],c(0.25,0.5,0.75))))
#   Simulations_ER[i,] <- t(quantile(ER_Sims[i,2:ncol(ER_Sims)],c(0.25,0.5,0.75)))
# }
#
# #plot the graphs
# SimulationPlot <- function(Data, FYE){
#   Data <- (as.data.frame(Data))
#   Data <- cbind(FYE, Data)
#   colnames(Data) <- c('FYE','25th Percentile', '50th Percentile', '75th Percentile')
#   ggplot(Data, aes(x = Data[,1])) +
#     geom_line(aes(y = Data[,2]), color = "#FF6633", size = 2) +
#     geom_line(aes(y = Data[,3]), color = "#FFCC33", size = 2) +
#     geom_line(aes(y = Data[,4]), color = "#0066CC", size = 2)
# }
# SimulationPlot(Simulations_ER, FYE)
#
# end_time <- Sys.time()
# print(end_time - start_time)
