library("readxl")
library(ggplot2)
library(tidyverse)
setwd(getwd())
rm(list = ls())
#
#User can change these values
StartYear <- 2015
StartProjectionYear <- 2022
EndProjectionYear <- 2052
FileName <- 'Funding Model Inputs 2021.xlsx'
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
  PV = (1 + rate)*pmt * (1 - (1 + rate) ^ (-nper)) / rate
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

#Get period function
GetNPER <- function(r,g,pv,t,pmt){
  PV <- pv*(1+r)^t
  R <- (1+r)/(1+g) - 1
  TempValue <- PV*R/(pmt*(1+R))
  NPER <- -log(1-TempValue,(1+R))
  if(is.infinite(NPER)) {NPER <- 100}
  if(is.nan(NPER)) {NPER <- 100}
  return(NPER)
}
#
##################################################################################################################################################################
#
#Replace NAs
Historical_Data[is.na(Historical_Data)] <- 0
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

#Assign values for simulation
if(SimType == 'Assumed'){
  SimReturn <- SimReturnAssumed
} else if(AnalysisType == 'Conservative'){
  SimReturn <- SimReturnConservative
}

#Initialize Amortization and Outstnading Base
RowColCount <- (EndProjectionYear - StartProjectionYear + 1)
OutstandingBase_CurrentHires <- matrix(0,RowColCount, RowColCount + 1)
Amortization_CurrentHires <- matrix(0,RowColCount, RowColCount + 1)
OutstandingBase_NewHires <- matrix(0,RowColCount, RowColCount + 1)
Amortization_NewHires <- matrix(0,RowColCount, RowColCount + 1)
AmoYearsInput_CurrentHires <- read_excel(FileName, sheet = 'Amortization_CurrentDebt')
AmoYearsInput_NewHires <- read_excel(FileName, sheet = 'Amortization_NewDebt')
#
##################################################################################################################################################################
#
#Offset Matrix. Used later for Amortization calculation
#Current Hires
OffsetYears_CurrentHires <- matrix(0,RowColCount, RowColCount)
for(i in 1:nrow(OffsetYears_CurrentHires)){
  RowCount <- i
  ColCount <- 1
  #This is to create the "zig-zag" pattern of amo years. you also want it to become 0 if the amo years is greater than the payments
  #Meaning if its 10 years, then after 10 years, the amo payment is 0
  while(RowCount <= nrow(OffsetYears_CurrentHires) && (ColCount <= as.double(AmoYearsInput_CurrentHires[i,2]))){
    OffsetYears_CurrentHires[RowCount,ColCount] <- as.double(AmoYearsInput_CurrentHires[i,2])
    RowCount <- RowCount + 1
    ColCount <- ColCount + 1
  }
}

for(i in 1:nrow(OffsetYears_CurrentHires)-1){
  for(j in 1:i+1){
    if(OffsetYears_CurrentHires[i+1,j] > 0) {
      OffsetYears_CurrentHires[i+1,j] <- max(OffsetYears_CurrentHires[i+1,j] - j + 1, 1) 
    }
  }
}

#New Hires
OffsetYears_NewHires <- matrix(0,RowColCount, RowColCount)
for(i in 1:nrow(OffsetYears_NewHires)){
  RowCount <- i
  ColCount <- 1
  #This is to create the "zig-zag" pattern of amo years. you also want it to become 0 if the amo years is greater than the payments
  #Meaning if its 10 years, then after 10 years, the amo payment is 0
  while(RowCount <= nrow(OffsetYears_NewHires) && (ColCount <= as.double(AmoYearsInput_NewHires[i,2]))){
    OffsetYears_NewHires[RowCount,ColCount] <- as.double(AmoYearsInput_NewHires[i,2])
    RowCount <- RowCount + 1
    ColCount <- ColCount + 1
  }
}

for(i in 1:nrow(OffsetYears_NewHires)-1){
  for(j in 1:i+1){
    if(OffsetYears_NewHires[i+1,j] > 0) {
      OffsetYears_NewHires[i+1,j] <- max(OffsetYears_NewHires[i+1,j] - j + 1, 1) 
    }
  }
}

#Initialize first row of Amortization and Outstanding Base
OutstandingBase_CurrentHires[1,1] <- UAL_AVA_CurrentHires[HistoricalIndex]
Amortization_CurrentHires[1,1] <- PMT(pv = OutstandingBase_CurrentHires[1,1], 
                                      r = NewDR[HistoricalIndex], g = AmoBaseInc, t = 0.5,
                                      nper = pmax(OffsetYears_CurrentHires[1,1],1))

OutstandingBase_NewHires[1,1] <- UAL_AVA_NewHires[HistoricalIndex]
Amortization_NewHires[1,1] <- PMT(pv = OutstandingBase_NewHires[1,1], 
                                  r = NewDR[HistoricalIndex], g = AmoBaseInc, t = 0.5,
                                  nper = pmax(OffsetYears_NewHires[1,1],1))
#
##################################################################################################################################################################
#
LvDollarorPercent <- 'Lv%'
ExternalContrib <- c(0)
RunModel <- function(Analysis_Type = AnalysisType, 
                     Sim_Return = SimReturn, 
                     Sim_Volatility = SimVolatility, 
                     ER_Policy = ERPolicy,
                     Scen_Type = ScenType,
                     CostSharing_NC = CostSharingNC,
                     CostSharing_Amo = CostSharingAmo,
                     AmoBase_Inc = AmoBaseInc,
                     Lv_DollarorPercent = LvDollarorPercent,
                     External_Contrib = ExternalContrib){
  
  if(Lv_DollarorPercent == 'Lv$'){
    AmoBase_Inc <- 0
    
    #Initialize first row of Amortization and Outstanding Base
    OutstandingBase_CurrentHires[1,1] <- UAL_AVA_CurrentHires[HistoricalIndex]
    Amortization_CurrentHires[1,1] <- PMT(pv = OutstandingBase_CurrentHires[1,1], 
                                          r = NewDR[HistoricalIndex], g = AmoBase_Inc, t = 0.5,
                                          nper = pmax(OffsetYears_CurrentHires[1,1],1))
    
    OutstandingBase_NewHires[1,1] <- UAL_AVA_NewHires[HistoricalIndex]
    Amortization_NewHires[1,1] <- PMT(pv = OutstandingBase_NewHires[1,1], 
                                      r = NewDR[HistoricalIndex], g = AmoBase_Inc, t = 0.5,
                                      nper = pmax(OffsetYears_NewHires[1,1],1))
  }
  #Scenario Index for referencing later based on investment return data
  ScenarioIndex <- which(colnames(Scenario_Data) == as.character(Scen_Type))
  
  #intialize this value at 0 for Total ER Contributions
  Total_ER[StartIndex-1] <- 0
  for(i in StartIndex:length(FYE)){
    #Because start index includes historical data and thus might be 3 by the time ProjectionCount is 1
    ProjectionCount <- i - StartIndex + 1
    
    #Payroll
    TotalPayroll[i] <- TotalPayroll[i-1]*(1 + Payroll_growth) 
    if(i == StartIndex){
      PayrollLegacy_Pct[i] <- 0.95
      PayrollNewTier[i] <- 0.05
    } else if (FYE[i] <= 2043) {
      PayrollLegacy_Pct[i] <- PayrollLegacy_Pct[i-1]*0.9
      PayrollNewTier[i] <- 1 - PayrollLegacy_Pct[i]
    } else {
      PayrollLegacy_Pct[i] <- PayrollLegacy_Pct[i-1]*0.8
      PayrollNewTier[i] <- 1 - PayrollLegacy_Pct[i]
    }
    NewHirePayrollDB[i] <- PayrollNewTier[i]*TotalPayroll[i]*(1 - DC_NewHires)
    NewHirePayrollDC[i] <- PayrollNewTier[i]*TotalPayroll[i]*DC_NewHires
    PayrollLegacy[i] <- PayrollLegacy_Pct[i]*TotalPayroll[i]
    #
    #Discount Rate
    OriginalDR[i] <- dis_r
    NewDR[i] <- dis_r_proj
    #
    #Benefit Payments, Admin Expenses
    BenPayments[i] <- BenPayments[i-1]*(1+BenPayment_Growth)
    BenPayments_NewHires[i] <- -1*BenefitPayments$NewHireBP_Pct[i]*NewHirePayrollDB[i]
    BenPayments_CurrentHires[i] <- BenPayments[i] - BenPayments_NewHires[i]
    
    Refunds[i] <- 0
    AdminExp_CurrentHires[i] <- -1*Admin_Exp_Pct*PayrollLegacy[i]
    AdminExp_NewHires[i] <- -1*Admin_Exp_Pct*NewHirePayrollDB[i]
    #
    #Accrued Liability, MOY NC - Original DR
    MOYNCExistOrigDR[i] <- PayrollLegacy[i]*NC_CurrentHires_Pct_1
    MOYNCNewHiresOrigDR[i] <- NewHirePayrollDB[i]*NC_NewHires_Pct_1
    AccrLiabOrigDR_CurrentHires[i] <- AccrLiabOrigDR_CurrentHires[i-1]*(1+OriginalDR[i]) + (MOYNCExistOrigDR[i] + BenPayments_CurrentHires[i])*(1+OriginalDR[i])^0.5
    AccrLiabOrigDR_NewHires[i] <- AccrLiabOrigDR_NewHires[i-1]*(1+OriginalDR[i]) + (MOYNCNewHiresOrigDR[i] + BenPayments_NewHires[i])*(1+OriginalDR[i])^0.5
    AccrLiabOrigDR_Total[i] <- AccrLiabOrigDR_CurrentHires[i] + AccrLiabOrigDR_NewHires[i]
    #
    #Accrued Liability, MOY NC - New DR
    DRDifference <- 100*(OriginalDR[i] - NewDR[i])
    MOYNCExistNewDR[i] <- MOYNCExistOrigDR[i]*((1+(NCSensDR/100))^(DRDifference))
    MOYNCNewHiresNewDR[i] <- MOYNCNewHiresOrigDR[i]*((1+(NCSensDR/100))^(DRDifference))
    AccrLiabNewDR_CurrentHires[i] <- AccrLiabOrigDR_CurrentHires[i]*((1+(LiabSensDR/100))^(DRDifference))*((1+(Convexity/100))^((DRDifference)^2/2))
    AccrLiabNewDR_NewHires[i] <- AccrLiabOrigDR_NewHires[i]*((1+(LiabSensDR/100))^(DRDifference))*((1+(Convexity/100))^((DRDifference)^2/2))
    AccrLiabNewDR_Total[i] <- AccrLiabOrigDR_Total[i]*((1+(LiabSensDR/100))^(DRDifference))*((1+(Convexity/100))^((DRDifference)^2/2))
    
    #NC, Reduce Rate contribution policy
    TotalNC_Pct[i-1] <- (MOYNCExistNewDR[i] + MOYNCNewHiresNewDR[i]) / TotalPayroll[i]
    NC_Legacy_Pct[i-1] <- MOYNCExistNewDR[i] / PayrollLegacy[i]
    
    if(NewHirePayrollDB[i] > 0){
      NC_NewHires_Pct[i-1] <- MOYNCNewHiresNewDR[i] / NewHirePayrollDB[i]
    } else {
      NC_NewHires_Pct[i-1] <- 0
    }
    
    Suppl_Contrib[i] <- Suppl_Contrib[i-1]*1.01
    EffStat_ER_Red[i] <- (RedRatFundPeriod_ER*TotalPayroll[i] + Suppl_Contrib[i]) / TotalPayroll[i]
    FixedStat_AmoPayment_Red[i] <- EffStat_ER_Red[i]*TotalPayroll[i] - 
      (NC_Legacy_Pct[i-1] + Admin_Exp_Pct - RedRatFundPeriod_EE)*PayrollLegacy[i] -
      (NC_NewHires_Pct[i-1] + Admin_Exp_Pct - RedRatFundPeriod_EE)*NewHirePayrollDB[i] -
      DC_Contrib*NewHirePayrollDC[i]
    FundPeriod_Red[i] <- GetNPER(NewDR[i],AmoBase_Inc,UAL_AVA[i-1],0.5,FixedStat_AmoPayment_Red[i])
    #
    #ER, EE, Amo Rates
    if(FundPeriod_Red[i] <= RedRatFundPeriod){
      EE_NC_Legacy_Pct[i-1] <- RedRatFundPeriod_EE
    } else {
      EE_NC_Legacy_Pct[i-1] <- EEContrib_CurrentHires
    }
    
    if(CostSharing_NC == 'Yes'){
      EE_NC_NewHires_Pct[i-1] <- NC_NewHires_Pct[i-1]/2
    } else {
      if(FundPeriod_Red[i] <= RedRatFundPeriod){
        EE_NC_NewHires_Pct[i-1] <- RedRatFundPeriod_EE 
      } else {
        EE_NC_NewHires_Pct[i-1] <- EEContrib_NewHires
      }
    }
    
    ER_NC_Legacy_Pct[i-1] <- NC_Legacy_Pct[i-1] - EE_NC_Legacy_Pct[i-1]
    ER_NC_NewHires_Pct[i-1] <- NC_NewHires_Pct[i-1] - EE_NC_NewHires_Pct[i-1]
    #
    #Regular Funding Period
    AmoFactor[i] <- as.matrix(PresentValue(((1+NewDR[i])/(1+AmoBase_Inc))-1,AmoYearsInput_CurrentHires[ProjectionCount+1,2],1) / ((1+NewDR[i])^0.5))
    if(FYE[i] < 2025){
      Stat_ER[i] <- Stat_ER[i-1] + 0.001
    } else {
      Stat_ER[i] <- Stat_ER[i-1]
    }
    EffStat_ER[i] <- (Stat_ER[i]*TotalPayroll[i] + Suppl_Contrib[i]) / TotalPayroll[i]
    FixedStat_AmoPayment[i] <- EffStat_ER[i]*TotalPayroll[i] - 
      (ER_NC_Legacy_Pct[i-1] + Admin_Exp_Pct)*PayrollLegacy[i] -
      (ER_NC_NewHires_Pct[i-1] + Admin_Exp_Pct)*NewHirePayrollDB[i] -
      DC_Contrib*NewHirePayrollDC[i]
    
    if((round(FundPeriod[i-1],2) > AmoPeriod_CurrentHires) && (UAL_AVA[i-1] > 0) && (FYE[i] > 2022)){
      ADC_Cond[i] <- "Yes"
    } else {
      ADC_Cond[i] <- "No"
    }
    
    if(i == StartIndex){
      VarStat_AmoPayment[i] <- FixedStat_AmoPayment[i]
    } else if(UAL_AVA[i-1] <= 0){
      VarStat_AmoPayment[i] <- 0
    } else if(FundPeriod_Red[i] <= RedRatFundPeriod){
      VarStat_AmoPayment[i] <- FixedStat_AmoPayment_Red[i]
    } else if(ADC_Cond[i] == "Yes"){
      VarStat_AmoPayment[i] <- UAL_AVA[i-1] / AmoFactor[i-1]
      #This condition is to say if there was any instance of yes for ADC Cond
    } else if(!is_empty(which(ADC_Cond[StartIndex:i-1] == "Yes"))){
      VarStat_AmoPayment[i] <- VarStat_AmoPayment[i-1]*(1 + AmoBase_Inc)
    } else {
      VarStat_AmoPayment[i] <- FixedStat_AmoPayment[i]
    }
    
    FundPeriod[i] <- GetNPER(NewDR[i],AmoBase_Inc,UAL_AVA[i-1],0.5,VarStat_AmoPayment[i])
    #
    #Amo Rates
    AmoRate_CurrentHires[i-1] <- sum(Amortization_CurrentHires[ProjectionCount,]) / TotalPayroll[i]
    AmoRate_NewHires[i-1] <- sum(Amortization_NewHires[ProjectionCount,]) / (NewHirePayrollDB[i] + NewHirePayrollDC[i])
    
    if(CostSharing_Amo == "Yes"){
      EE_AmoRate_NewHires[i] <- AmoRate_NewHires[i] / 2
    } else {
      EE_AmoRate_NewHires[i] <- 0
    }
    
    if(ER_Policy == "Statutory Rate"){
      StatAmoRate[i-1] <- FixedStat_AmoPayment[i] / TotalPayroll[i]
    } else {
      StatAmoRate[i-1] <- VarStat_AmoPayment[i] / TotalPayroll[i]
    }
    #
    #Cashflows, NC, Amo. Solv, etc.
    EE_NC_CurrentHires[i] <- EE_NC_Legacy_Pct[i-1]*PayrollLegacy[i]
    EE_NC_NewHires[i] <- EE_NC_NewHires_Pct[i-1]*NewHirePayrollDB[i]
    EE_Amo_NewHires[i] <- EE_AmoRate_NewHires[i-1]*NewHirePayrollDB[i]
    ER_NC_CurrentHires[i] <- ER_NC_Legacy_Pct[i-1]*PayrollLegacy[i] - AdminExp_CurrentHires[i]
    ER_NC_NewHires[i] <- ER_NC_NewHires_Pct[i-1]*NewHirePayrollDB[i] - AdminExp_NewHires[i]
    
    if(ER_Policy == "ADC"){
      ER_Amo_CurrentHires[i] <- max(AmoRate_CurrentHires[i-1]*TotalPayroll[i],-ER_NC_CurrentHires[i])
      ER_Amo_NewHires[i] <- max(AmoRate_NewHires[i-1]*(NewHirePayrollDB[i] + NewHirePayrollDC[i]),-ER_NC_NewHires[i])
    } else {
      ER_Amo_CurrentHires[i] <- max(StatAmoRate[i-1]*PayrollLegacy[i],-ER_NC_CurrentHires[i])
      ER_Amo_NewHires[i] <- max(StatAmoRate[i-1]*(NewHirePayrollDB[i] + NewHirePayrollDC[i])-EE_Amo_NewHires[i],-ER_NC_NewHires[i])
    }
    #
    #Solvency Contribution and Return data
    if((Analysis_Type == 'Stochastic') && (i >= StartIndex)){
      ROA_MVA[i] <- rnorm(1,Sim_Return,Sim_Volatility)
    } else if(Analysis_Type == 'Deterministic'){
      ROA_MVA[i] <- as.double(Scenario_Data[i,ScenarioIndex]) 
    }
    
    Total_Contrib_DC[i] <- DC_Contrib*Ratio_DCVesting*NewHirePayrollDC[i]
    DC_Forfeit[i] <- DC_Contrib*(1 - Ratio_DCVesting)*NewHirePayrollDC[i]
    CashFlows_Total <- BenPayments_CurrentHires[i] + BenPayments_NewHires[i] + Refunds[i] + AdminExp_CurrentHires[i] +
      AdminExp_NewHires[i] + EE_NC_CurrentHires[i] + EE_NC_NewHires[i] + EE_Amo_NewHires[i] +
      ER_NC_CurrentHires[i] + ER_NC_NewHires[i] + ER_Amo_CurrentHires[i] + ER_Amo_NewHires[i] + DC_Forfeit[i]
    
    if(FYE[i] == 2022){
      CashFlows_Total <- External_Contrib + CashFlows_Total
    }
    Solv_Contrib[i] <- as.double(max(-(MVA[i-1]*(1+ROA_MVA[i]) + CashFlows_Total*(1+ROA_MVA[i])^0.5) / (1+ROA_MVA[i])^0.5,0))
    Solv_Contrib_CurrentHires[i] <- Solv_Contrib[i] * (AccrLiabNewDR_CurrentHires[i] / AccrLiabNewDR_Total[i])
    Solv_Contrib_NewHires[i] <- Solv_Contrib[i] * (AccrLiabNewDR_NewHires[i] / AccrLiabNewDR_Total[i])
    #
    #Net CF, Expected MVA, Gain Loss, Defered Losses
    NetCF_CurrentHires[i] <- BenPayments_CurrentHires[i] + AdminExp_CurrentHires[i] + EE_NC_CurrentHires[i] +
      ER_NC_CurrentHires[i] + ER_Amo_CurrentHires[i] + Solv_Contrib_CurrentHires[i] + DC_Forfeit[i]
    
    if(FYE[i] == 2022){
      NetCF_CurrentHires[i] <- External_Contrib + NetCF_CurrentHires[i]
    }
    
    ExpInvInc_CurrentHires[i] <- (MVA_CurrentHires[i-1]*NewDR[i-1]) + (NetCF_CurrentHires[i]*NewDR[i-1]*0.5)
    ExpectedMVA_CurrentHires[i] <- MVA_CurrentHires[i-1] + NetCF_CurrentHires[i] + ExpInvInc_CurrentHires[i]
    MVA_CurrentHires[i] <- MVA_CurrentHires[i-1]*(1+ROA_MVA[i]) + (NetCF_CurrentHires[i])*(1+ROA_MVA[i])^0.5 
    GainLoss_CurrentHires[i] <- MVA_CurrentHires[i] - ExpectedMVA_CurrentHires[i] 
    Year1GL_CurrentHires[i] <- GainLoss_CurrentHires[i]*0.25
    Year2GL_CurrentHires[i] <- Year1GL_CurrentHires[i-1]
    Year3GL_CurrentHires[i] <- Year2GL_CurrentHires[i-1]
    Year4GL_CurrentHires[i] <- Year3GL_CurrentHires[i-1]
    TotalDefered_CurrentHires[i] <- Year1GL_CurrentHires[i] + Year2GL_CurrentHires[i] + Year3GL_CurrentHires[i] + Year4GL_CurrentHires[i]
    
    NetCF_NewHires[i] <- BenPayments_NewHires[i] + AdminExp_NewHires[i] + EE_NC_NewHires[i] + EE_Amo_NewHires[i] +
      ER_NC_NewHires[i] + ER_Amo_NewHires[i] + Solv_Contrib_NewHires[i]
    
    ExpInvInc_NewHires[i] <- (MVA_NewHires[i-1]*NewDR[i-1]) + (NetCF_NewHires[i]*NewDR[i-1]*0.5)
    ExpectedMVA_NewHires[i] <- MVA_NewHires[i-1] + NetCF_NewHires[i] + ExpInvInc_NewHires[i]
    MVA_NewHires[i] <- MVA_NewHires[i-1]*(1+ROA_MVA[i]) + (NetCF_NewHires[i])*(1+ROA_MVA[i])^0.5 
    GainLoss_NewHires[i] <- MVA_NewHires[i] - ExpectedMVA_NewHires[i] 
    Year1GL_NewHires[i] <- GainLoss_NewHires[i]*0.25
    Year2GL_NewHires[i] <- Year1GL_NewHires[i-1]
    Year3GL_NewHires[i] <- Year2GL_NewHires[i-1]
    Year4GL_NewHires[i] <- Year3GL_NewHires[i-1]
    TotalDefered_NewHires[i] <- Year1GL_NewHires[i] + Year2GL_NewHires[i] + Year3GL_NewHires[i] + Year4GL_NewHires[i]
    #
    #AVA, MVA, UA, FR
    AVA_CurrentHires[i] <- AVA_CurrentHires[i-1] + NetCF_CurrentHires[i] + ExpInvInc_CurrentHires[i] + TotalDefered_CurrentHires[i]
    #AVA_CurrentHires[i] <- MVA_CurrentHires[i] - TotalDefered_CurrentHires[i]
    AVA_CurrentHires[i] <- max(AVA_CurrentHires[i],AVA_lowerbound*MVA_CurrentHires[i])
    AVA_CurrentHires[i] <- min(AVA_CurrentHires[i],AVA_upperbound*MVA_CurrentHires[i])
    UAL_AVA_CurrentHires[i] <- AccrLiabNewDR_CurrentHires[i] - AVA_CurrentHires[i]
    UAL_MVA_CurrentHires[i] <- AccrLiabNewDR_CurrentHires[i] - MVA_CurrentHires[i]
    
    AVA_NewHires[i] <- AVA_NewHires[i-1] + NetCF_NewHires[i] + ExpInvInc_NewHires[i] + TotalDefered_NewHires[i]
    #AVA_NewHires[i] <- MVA_NewHires[i] - TotalDefered_NewHires[i]
    AVA_NewHires[i] <- max(AVA_NewHires[i],AVA_lowerbound*MVA_NewHires[i])
    AVA_NewHires[i] <- min(AVA_NewHires[i],AVA_upperbound*MVA_NewHires[i])
    UAL_AVA_NewHires[i] <- AccrLiabNewDR_NewHires[i] - AVA_NewHires[i]
    UAL_MVA_NewHires[i] <- AccrLiabNewDR_NewHires[i] - MVA_NewHires[i]
    
    AVA[i] <- AVA_CurrentHires[i] + AVA_NewHires[i]
    MVA[i] <- MVA_CurrentHires[i] + MVA_NewHires[i]
    FR_AVA[i] <- AVA[i] / AccrLiabNewDR_Total[i]
    FR_MVA[i] <- MVA[i] / AccrLiabNewDR_Total[i]
    UAL_AVA[i] <- AccrLiabNewDR_Total[i] - AVA[i]
    UAL_MVA[i] <- AccrLiabNewDR_Total[i] - MVA[i]
    UAL_AVA_InflAdj[i] <- UAL_AVA[i] / ((1 + asum_infl)^(FYE[i] - NC_StaryYear))
    UAL_MVA_InflAdj[i] <- UAL_MVA[i] / ((1 + asum_infl)^(FYE[i] - NC_StaryYear))
    #
    Total_Contrib_DB[i] <- ER_NC_CurrentHires[i] + ER_NC_NewHires[i] + ER_Amo_CurrentHires[i] + 
      ER_Amo_NewHires[i] + Solv_Contrib[i]
    if(FYE[i] == 2022){
      Total_Contrib_DB[i] <- External_Contrib + Total_Contrib_DB[i]
    }
    Total_Contrib[i] <- max(Total_Contrib_DB[i] + Total_Contrib_DC[i],0)
    
    ER_InflAdj[i] <- Total_Contrib[i] / ((1 + asum_infl)^(FYE[i] - NC_StaryYear))
    ER_Percentage[i] <- Total_Contrib[i] / TotalPayroll[i]
    #Total ER starts after first year so set the first year to 0.
    if(i < StartIndex){
      Total_ER[i] <- 0
    } else {
      Total_ER[i] <- Total_ER[i-1] + ER_InflAdj[i] 
    }
    AllInCost[i] <- Total_ER[i] + UAL_MVA_InflAdj[i]
    #
    #Amortization
    #Current Hires
    if(ProjectionCount < nrow(Amortization_CurrentHires)){
      OutstandingBase_CurrentHires[ProjectionCount+1,2:(ProjectionCount + 1)] <- OutstandingBase_CurrentHires[ProjectionCount,1:ProjectionCount]*(1 + NewDR[i-1]) - (Amortization_CurrentHires[ProjectionCount,1:ProjectionCount]*(1 + NewDR[i-1])^0.5)
      OutstandingBase_CurrentHires[ProjectionCount+1,1] <- UAL_AVA_CurrentHires[i] - sum(OutstandingBase_CurrentHires[ProjectionCount+1,2:ncol(OutstandingBase_CurrentHires)])
      
      #Amo Layers
      Amortization_CurrentHires[ProjectionCount+1,1:(ProjectionCount + 1)] <- PMT(pv = OutstandingBase_CurrentHires[ProjectionCount+1,1:(ProjectionCount + 1)], 
                                                                                  r = NewDR[i-1], g = AmoBase_Inc, t = 0.5,
                                                                                  nper = pmax(OffsetYears_CurrentHires[ProjectionCount+1,1:(ProjectionCount + 1)],1))
    }
    
    #New Hires
    if(ProjectionCount < nrow(Amortization_NewHires)){
      OutstandingBase_NewHires[ProjectionCount+1,2:(ProjectionCount + 1)] <- OutstandingBase_NewHires[ProjectionCount,1:ProjectionCount]*(1 + NewDR[i-1]) - (Amortization_NewHires[ProjectionCount,1:ProjectionCount]*(1 + NewDR[i-1])^0.5)
      OutstandingBase_NewHires[ProjectionCount+1,1] <- UAL_AVA_NewHires[i] - sum(OutstandingBase_NewHires[ProjectionCount+1,2:ncol(OutstandingBase_NewHires)])
      
      #Amo Layers
      Amortization_NewHires[ProjectionCount+1,1:(ProjectionCount + 1)] <- PMT(pv = OutstandingBase_NewHires[ProjectionCount+1,1:(ProjectionCount + 1)], 
                                                                              r = NewDR[i-1], g = AmoBase_Inc, t = 0.5,
                                                                              nper = pmax(OffsetYears_NewHires[ProjectionCount+1,1:(ProjectionCount + 1)],1))
    }
    #
  }  
  
  #Join all the outputs together
  #Initialize Output as the first column FYE
  Output <- FYE
  for(i in 2:length(Historical_Data)){
    Output <- cbind(Output,get(colnames(Historical_Data)[i]))
  }
  return(as.data.frame(Output))
}
#
# ##################################################################################################################################################################
#
# #Scenarios
# Scenario_Returns <- as.data.frame(FYE)
# Scenario_UAL <- as.data.frame(FYE)
# Scenario_FR <- as.data.frame(FYE)
# Scenario_ER_Percentage <- as.data.frame(FYE)
# Scenario_ER_InflAdj <- as.data.frame(FYE)
# Scenario_Total_ER <- as.data.frame(FYE)
# Scenario_AllIn_ER <- as.data.frame(FYE)
# 
# #There are 3 types of scenarios here - Recessions, Supplemental and Lv$%
# #We are trying to run all of them outside of a function because we need the data for UAL, FR, etc.
# #If we run them in a function, we can only generate one output
# ScenarioRuns <- 'Return Scenarios'
# #Initialize Max Length, this will be used at the end
# MaxLength <- 0
# if(ScenarioRuns == 'Return Scenarios'){
#   Scenarios <- c('Assumption','Model','Recession','Recurring Recession')
#   for (i in 1:length(Scenarios)){
#     NewData <- as.data.frame(RunModel('Deterministic',SimReturn, SimVolatility, 'Variable Statutory', Scenarios[i], c(0), 'Lv%'))
#     Scenario_Returns <- cbind(Scenario_Returns,NewData$ROA_MVA)
#     Scenario_UAL <- cbind(Scenario_UAL,NewData$UAL_MVA_InflAdj)
#     Scenario_FR <- cbind(Scenario_FR,NewData$FR_MVA)
#     Scenario_ER_Percentage <- cbind(Scenario_ER_Percentage,NewData$ER_Percentage)
#     Scenario_ER_InflAdj <- cbind(Scenario_ER_InflAdj,NewData$ER_InflAdj)
#     Scenario_Total_ER <- cbind(Scenario_Total_ER,NewData$Total_ER)
#     Scenario_AllIn_ER <- cbind(Scenario_AllIn_ER,NewData$AllInCost)
#   }
#   #Scenario names should be the same as Scenarios but for some cases like supplemental, lv$, etc. it will be different
#   ScenarioNames <- Scenarios
#   
# } else if(ScenarioRuns == 'Supplemental Contribution'){
#   Scenarios <- c('Assumption','Recurring Recession','Assumption','Recurring Recession')
#   ExternalContrib <- c(0,0,1000,1000)
#   for (i in 1:length(Scenarios)){
#     NewData <- as.data.frame(RunModel('Deterministic',SimReturn, SimVolatility, 'ADC', Scenarios[i], ExternalContrib[i], 'Lv%'))
#     Scenario_Returns <- cbind(Scenario_Returns,NewData$ROA_MVA)
#     Scenario_UAL <- cbind(Scenario_UAL,NewData$UAL_MVA_InflAdj)
#     Scenario_FR <- cbind(Scenario_FR,NewData$FR_MVA)
#     Scenario_ER_Percentage <- cbind(Scenario_ER_Percentage,NewData$ER_Percentage)
#     Scenario_ER_InflAdj <- cbind(Scenario_ER_InflAdj,NewData$ER_InflAdj)
#     Scenario_Total_ER <- cbind(Scenario_Total_ER,NewData$Total_ER)
#     Scenario_AllIn_ER <- cbind(Scenario_AllIn_ER,NewData$AllInCost)
#   }
#   #Scenario names should be the same as Scenarios but for some cases like supplemental, lv$, etc. it will be different
#   ScenarioNames <- c('Assumption - No Supplemental', 'Recurring Recession - No Supplemental', 'Assumption - $500 Million', 'Recurring Recession - $500 Million')
#   
# } else if(ScenarioRuns == 'Lv$ vs %'){
#   Scenarios <- c('Assumption','Lv%','Assumption','Lv$','Recurring Recession','Lv%','Recurring Recession','Lv$')
#   MaxLength <- length(Scenarios)/2
#   for (i in 1:MaxLength){
#     NewData <- as.data.frame(RunModel('Deterministic',SimReturn, SimVolatility, 'Statutory Rate', Scenarios[i*2 - 1], c(0), Scenarios[i*2]))
#     Scenario_Returns <- cbind(Scenario_Returns,NewData$ROA_MVA)
#     Scenario_UAL <- cbind(Scenario_UAL,NewData$UAL_MVA_InflAdj)
#     Scenario_FR <- cbind(Scenario_FR,NewData$FR_MVA)
#     Scenario_ER_Percentage <- cbind(Scenario_ER_Percentage,NewData$ER_Percentage)
#     Scenario_ER_InflAdj <- cbind(Scenario_ER_InflAdj,NewData$ER_InflAdj)
#     Scenario_Total_ER <- cbind(Scenario_Total_ER,NewData$Total_ER)
#     Scenario_AllIn_ER <- cbind(Scenario_AllIn_ER,NewData$AllInCost)
#   }
#   #Scenario names should be the same as Scenarios but for some cases like supplemental, lv$, etc. it will be different
#   ScenarioNames <- c('Assumption Lv%', 'Assumption Lv$', 'Recurring Recession Lv%', 'Recurring Recession Lv$')
# }
# 
# #MaxLength should in theory be the lenght of the scenarios but because of Lv$%, it may not be
# #Hence we have to do the max function
# if(ScenarioRuns != 'Lv$ vs %'){
#   MaxLength <- length(Scenarios)
# }
# 
# for(i in 1:MaxLength){
#   #Start from StartIndex because thats when the projection is
#   #Total ER is already inflation adjusted
#   TotalERScenario <- sum(Scenario_Total_ER[nrow(Scenario_Total_ER),i+1])/1000
#   #inflation adjusted UAL
#   EndingUAL <- Scenario_UAL[nrow(Scenario_UAL),i+1]/1000
#   AllInER <- Scenario_AllIn_ER[nrow(Scenario_AllIn_ER),i+1]/1000
#   
#   if(i == 1){
#     ERCostTable <- c(TotalERScenario,EndingUAL, AllInER)
#   } else {
#     ERCostTable <- rbind(ERCostTable, c(TotalERScenario,EndingUAL, AllInER))
#   }
# }
# colnames(ERCostTable) <- c('Total ER Contributions','Ending UAL','All in ER Cost')
# rownames(ERCostTable) <- ScenarioNames
# 
# colnames(Scenario_Returns) <- c('FYE',ScenarioNames)
# colnames(Scenario_UAL) <- c('FYE',ScenarioNames)
# colnames(Scenario_FR) <- c('FYE',ScenarioNames)
# colnames(Scenario_ER_Percentage) <- c('FYE',ScenarioNames)
# colnames(Scenario_ER_InflAdj) <- c('FYE',ScenarioNames)
# 
# write_excel_csv(Scenario_ER_Percentage,'ER_Pct.csv')
# write_excel_csv(Scenario_ER_InflAdj,'ER_Infl.csv')
# write_excel_csv(Scenario_FR,'FR.csv')
# write_excel_csv(Scenario_UAL,'UAL.csv')
# 
# ScenarioPlot <- function(Data, YAxisLabel){
#   ggplot(Data, aes(x = FYE)) +
#     geom_line(aes(y = Data[,2]), color = "#FF6633", size = 2) +
#     geom_line(aes(y = Data[,3]), color = "#FFCC33", size = 2) +
#     geom_line(aes(y = Data[,4]), color = "#0066CC", size = 2) +
#     geom_line(aes(y = Data[,5]), color = "#CC0000", size = 2) +
#     theme(legend.position = "top") +
#     #ggplot2::theme(legend.text = 'Test', legend.title = 'test') +
#     #scale_linetype_manual(values = c('dashed','dashed','dashed','dashed')) +
#     labs(y = YAxisLabel, x = 'Year') + ggtitle(YAxisLabel)
#   #scale_linetype_manual(labels = '')
# }
# ScenarioPlot(Scenario_ER_Percentage, 'Unfunded Liabilities (MVA)')
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