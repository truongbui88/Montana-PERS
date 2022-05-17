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
  
  # if(nper == 0){
  #   a <- 0
  # }
  
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



#Initialize Amortization and Outstnading Base
RowColCount <- (EndProjectionYear - StartProjectionYear + 1)

#
##################################################################################################################################################################

RunModel <- function(NewHireDC_choice = DC_NewHires,                     #Percentage of new hires electing the DC plan. This should be in % unit.
                     DC_Policy = DCPolicy,                               #DC contribution policy. Three choices: "Current Policy", "No DC Rollback", "Constant Rate". Default choice: "Current Policy".
                     DC_ContRate = DC_Contrib,                           #DC contribution rate for the "Constant Rate" option
                     DR_CurrentHires = dis_r_current,                    #Discount rate for current hires. Min = 4%. Max = 9%. 0.25% step
                     DR_NewHires = dis_r_new,                            #Discount rate for new hires. Min = 4%. Max = 9%. 0.25% step
                     ReturnType = AnalysisType,                           #"Deterministic" or "Stochastic" type of simulated returns.
                     DeSimType = ScenType,                                #Deterministic return scenarios. 
                     ModelReturn = model_return,                          #Manually set constant return under the deterministic "Model" scenario
                     StoSimType = SimType,                                #"Assumed" or "Conservative" (for stochastic analysis)
                     FundingPolicy = ERPolicy,                            #"Variable Statutory", "Fixed Statutory", or "ADC" funding policy.
                     CostShare_AmoNew = CostSharingAmo,                   #"No" or "Yes". "No" means no Amo cost sharing between the employer and new hires.
                     CostShare_NCNew = CostSharingNC,                     #"No" or "Yes". "No" means no Normal cost sharing between the employer and new hires.
                     CurrentDebt_period = NoYearsADC_CurrentDebt,         #Amortization period (in years) for current unfunded liability. 
                     NewDebtCurrentHire_period = NoYearsADC_NewDebtCurrentHire,       #Amortization period (in years) for new unfunded liability created under current hire plan
                     NewDebtNewHire_period = NoYearsADC_NewDebtNewHire,               #Amortization period (in years) for new unfunded liability created under new hire plan  
                     AmoMethod_current = AmoMethod_CurrentHire,                       #"Level %" or "Level dollar" amortization method for unfunded liability created under current hire plan
                     AmoMethod_new = AmoMethod_NewHire,                               #"Level %" or "Level dollar" amortization method for unfunded liability created under new hire plan
                     OneTimeInfusion = CashInfusion,                                  #One time cash infusion in 2022.
                     Max_ERContrib = MaxERContrib,                                    #Maximum ER Contribution Rate (ER Contribution Cap). Default is 100%, meaning effectively no cap.
                     BenMult = BenMult_new){  
  
  
  ##Amo period tables
  currentlayer <- seq(CurrentDebt_period, 1)
  futurelayer_currenthire <- seq(NewDebtCurrentHire_period, 1)
  futurelayer_futurehire <- seq(NewDebtNewHire_period, 1)
  n <- max(length(currentlayer), length(futurelayer_currenthire))
  length(currentlayer) <- n
  length(futurelayer_currenthire) <- n
  
  #Amo period table for current hires plan
  OffsetYears_CurrentHires <- rbind(currentlayer, matrix(futurelayer_currenthire, 
                                                         nrow = RowColCount,
                                                         ncol = length(currentlayer),
                                                         byrow = T))
  
  rownames(OffsetYears_CurrentHires) <- NULL         #Remove row names
  
  for (i in 1:ncol(OffsetYears_CurrentHires)) {      #Put the amo periods on diagonal rows
    OffsetYears_CurrentHires[,i] <- lag(OffsetYears_CurrentHires[,i], n = i - 1)
  }
  
  OffsetYears_CurrentHires[is.na(OffsetYears_CurrentHires)] <- 0    #Turn all NAs in the table to 0s
  
  #Amo period table for future hires plan
  OffsetYears_NewHires <- matrix(futurelayer_futurehire, 
                                 nrow = RowColCount + 1,
                                 ncol = length(futurelayer_futurehire),
                                 byrow = T)
  
  for (i in 1:ncol(OffsetYears_NewHires)) {      #Put the amo periods on diagonal rows
    OffsetYears_NewHires[,i] <- lag(OffsetYears_NewHires[,i], n = i - 1)
  }
  
  OffsetYears_NewHires[is.na(OffsetYears_NewHires)] <- 0    #Turn all NAs in the table to 0s
  
  ##Amo base and payment tables
  #Default value is Lv% for Amo Base 
  #If its Level $, then set to 0
  if(AmoMethod_current == "Level $"){
    AmoBaseInc_CurrentHire <- 0
  }
  
  if(AmoMethod_new == "Level $"){
    AmoBaseInc_NewHire <- 0
  }
  
  #Amo base & payment - current hires initial setup
  OutstandingBase_CurrentHires <- matrix(0,RowColCount + 1, length(currentlayer) + 1)
  Amortization_CurrentHires <- matrix(0,RowColCount + 1, length(currentlayer))
  #Initialize the first UAAL layer and amo payment (current hires)
  OutstandingBase_CurrentHires[1,1] <- UAL_AVA_CurrentHires[HistoricalIndex]
  Amortization_CurrentHires[1,1] <- PMT(pv = OutstandingBase_CurrentHires[1,1], 
                                        r = CurrentHires_DR[HistoricalIndex], 
                                        g = AmoBaseInc_CurrentHire, 
                                        t = 0.5,
                                        nper = OffsetYears_CurrentHires[1,1])
  
  #Amo base & payment - future hires initial setup
  OutstandingBase_NewHires <- matrix(0,RowColCount + 1, length(futurelayer_futurehire) + 1)
  Amortization_NewHires <- matrix(0,RowColCount + 1, length(futurelayer_futurehire))
  
  #Set return values for "Model" and "Assumption" deterministic scenarios
  Scenario_Data$Model[StartIndex:nrow(Scenario_Data)] <- ModelReturn
  Scenario_Data$Assumption[StartIndex:nrow(Scenario_Data)] <- DR_CurrentHires    #Change return values in "Assumption" scenario to match the discount rate input for current hires
  
  #Scenario Index for referencing later based on investment return data
  ScenarioIndex <- which(colnames(Scenario_Data) == as.character(DeSimType))
  
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
    
    CurrentHirePayroll[i] <- PayrollLegacy_Pct[i] * TotalPayroll[i]
    CurrentHirePayrollDB[i] <- CurrentHirePayroll[i] * CurrentHirePayrollDB[StartIndex-1] / CurrentHirePayroll[StartIndex-1]   #Assume that the DB current hire payroll / DC current hire payroll ratio remains constant over time
    CurrentHirePayrollDC[i] <- CurrentHirePayroll[i] * CurrentHirePayrollDC[StartIndex-1] / CurrentHirePayroll[StartIndex-1]
    
    NewHirePayroll[i] <- PayrollNewTier[i] * TotalPayroll[i]
    NewHirePayrollDB[i] <- NewHirePayroll[i] * (1 - NewHireDC_choice)
    NewHirePayrollDC[i] <- NewHirePayroll[i] * NewHireDC_choice
    
    #
    #Discount Rate
    CurrentHires_DR[i] <- DR_CurrentHires
    NewHires_DR[i] <- DR_NewHires
    #
    #Benefit Payments, Admin Expenses
    BenPayments_BaseTotal[i] <- BenPayments_BaseTotal[i-1]*(1+BenPayment_Growth)
    BenPayments_BaseNew[i] <- -1*BenefitPayments$NewHireBP_Pct[i]*NewHirePayroll[i]
    BenPayments_NewHires[i] <- -1*BenefitPayments$NewHireBP_Pct[i]*NewHirePayrollDB[i]*BenMult/BenMult_current      #Revise this later with NC ratio
    BenPayments_CurrentHires[i] <- BenPayments_BaseTotal[i] - BenPayments_BaseNew[i]
    
    Refunds[i] <- 0
    AdminExp_CurrentHires[i] <- -1*Admin_Exp_Pct*CurrentHirePayrollDB[i]
    AdminExp_NewHires[i] <- -1*Admin_Exp_Pct*NewHirePayrollDB[i]
    #
    ##Accrued Liability and Normal Cost calculations (projection + DR adjustment combined in one place)
    #Normal Cost
    DRDifference_CurrentNC <- 100*(CurrentHires_DR[HistoricalIndex] - CurrentHires_DR[i])
    DRDifference_NewNC <- 100*(NewHires_DR[HistoricalIndex] - NewHires_DR[i])
    MOYNCExist[i] <- CurrentHirePayrollDB[i]*NC_CurrentHires_Pct_1*(1 + NCSensDR/100)^DRDifference_CurrentNC     #Revise this later with NC model 
    MOYNCNewHires[i] <- NewHirePayrollDB[i]*NC_NewHires_Pct_1*(1 + NCSensDR/100)^DRDifference_NewNC       #Revise this later with NC model
    
    #Accrued Liability
    DRDifference_CurrentAAL <- 100*(CurrentHires_DR[i-1] - CurrentHires_DR[i])
    DRDifference_NewAAL <- 100*(NewHires_DR[i-1] - NewHires_DR[i])
    AccrLiab_CurrentHires[i] <- (AccrLiab_CurrentHires[i-1]*(1 + CurrentHires_DR[i]) + (MOYNCExist[i] + BenPayments_CurrentHires[i])*(1 + CurrentHires_DR[i])^0.5) * ((1+LiabSensDR/100)^DRDifference_CurrentAAL) * ((1+Convexity/100)^(DRDifference_CurrentAAL^2/2))
    AccrLiab_NewHires[i] <- (AccrLiab_NewHires[i-1]*(1 + NewHires_DR[i]) + (MOYNCNewHires[i] + BenPayments_NewHires[i])*(1 + NewHires_DR[i])^0.5) * ((1+LiabSensDR/100)^DRDifference_NewAAL) * ((1+Convexity/100)^(DRDifference_NewAAL^2/2))
    AccrLiab_Total[i] <- AccrLiab_CurrentHires[i] + AccrLiab_NewHires[i]
    #
    
    #NC, Reduced Rate contribution policy
    # TotalNC_Pct[i-1] <- (MOYNCExistNewDR[i] + MOYNCNewHiresNewDR[i]) / TotalPayroll[i]
    NC_Legacy_Pct[i-1] <- MOYNCExist[i] / CurrentHirePayrollDB[i]
    
    if(NewHirePayrollDB[i] > 0){
      NC_NewHires_Pct[i-1] <- MOYNCNewHires[i] / NewHirePayrollDB[i]
    } else {
      NC_NewHires_Pct[i-1] <- 0
    }
    
    Suppl_Contrib[i] <- Suppl_Contrib[i-1]*1.01
    EffStat_ER_Red[i] <- (RedRatFundPeriod_ER*(CurrentHirePayrollDB[i] + NewHirePayrollDB[i]) + Suppl_Contrib[i]) / (CurrentHirePayrollDB[i] + NewHirePayrollDB[i])
    FixedStat_AmoPayment_Red[i] <- EffStat_ER_Red[i]*(CurrentHirePayrollDB[i] + NewHirePayrollDB[i]) - 
      (NC_Legacy_Pct[i-1] + Admin_Exp_Pct - RedRatFundPeriod_EE)*CurrentHirePayrollDB[i] -
      (NC_NewHires_Pct[i-1] + Admin_Exp_Pct - RedRatFundPeriod_EE)*NewHirePayrollDB[i]
    
    FundPeriod_Red[i] <- GetNPER(CurrentHires_DR[i],
                                 AmoBaseInc_CurrentHire,
                                 UAL_AVA[i-1],
                                 0.5,
                                 FixedStat_AmoPayment_Red[i])
    #
    ##ER, EE, Amo Rates
    #Employee normal cost (current hires) 
    if(FundPeriod_Red[i] <= RedRatFundPeriod){
      EE_NC_Legacy_Pct[i-1] <- RedRatFundPeriod_EE
    } else {
      EE_NC_Legacy_Pct[i-1] <- EEContrib_CurrentHires
    }
    
    #Employee normal cost (new hires + cost sharing condition)
    if(CostShare_NCNew == 'Yes'){
      EE_NC_NewHires_Pct[i-1] <- NC_NewHires_Pct[i-1]/2
    } else if (FundPeriod_Red[i] <= RedRatFundPeriod){
      EE_NC_NewHires_Pct[i-1] <- RedRatFundPeriod_EE 
    } else {
      EE_NC_NewHires_Pct[i-1] <- EEContrib_NewHires
    }
    
    #Employer normal cost
    ER_NC_Legacy_Pct[i-1] <- NC_Legacy_Pct[i-1] - EE_NC_Legacy_Pct[i-1]
    ER_NC_NewHires_Pct[i-1] <- NC_NewHires_Pct[i-1] - EE_NC_NewHires_Pct[i-1]
    
    #Amo factor to calculate the variable statutory contribution
    AmoFactor[i] <- as.matrix(PresentValue((1+CurrentHires_DR[i])/(1+AmoBaseInc_CurrentHire)-1,CurrentDebt_period,1) / ((1+CurrentHires_DR[i])^0.5))
    if(FYE[i] < 2025){
      Stat_ER[i] <- Stat_ER[i-1] + 0.001
    } else {
      Stat_ER[i] <- Stat_ER[i-1]
    }
    
    #Fixed Statutory Employer Contribution
    EffStat_ER[i] <- (Stat_ER[i]*(CurrentHirePayrollDB[i] + NewHirePayrollDB[i]) + Suppl_Contrib[i]) / (CurrentHirePayrollDB[i] + NewHirePayrollDB[i])
    
    FixedStat_AmoPayment[i] <- EffStat_ER[i]*(CurrentHirePayrollDB[i] + NewHirePayrollDB[i]) - 
      (ER_NC_Legacy_Pct[i-1] + Admin_Exp_Pct)*CurrentHirePayrollDB[i] -
      (ER_NC_NewHires_Pct[i-1] + Admin_Exp_Pct)*NewHirePayrollDB[i] 
    
    #Variable Statutory Employer Contribution
    if((round(FundPeriod[i-1],2) > CurrentDebt_period) && (UAL_AVA[i-1] > 0) && (FYE[i] > 2022)){
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
      #This condition is to say if there was any instance of yes for ADC Cond before current year
    } else if(!is_empty(which(ADC_Cond[StartIndex:i-1] == "Yes"))){
      VarStat_AmoPayment[i] <- VarStat_AmoPayment[i-1]*(1 + AmoBaseInc_CurrentHire)
    } else {
      VarStat_AmoPayment[i] <- FixedStat_AmoPayment[i]
    }
    
    #Funding period for variable statutory contribution
    FundPeriod[i] <- GetNPER(CurrentHires_DR[i],AmoBaseInc_CurrentHire,UAL_AVA[i-1],0.5,VarStat_AmoPayment[i])
    #
    #Amo Rates
    AmoRate_CurrentHires[i-1] <- sum(Amortization_CurrentHires[ProjectionCount,]) / TotalPayroll[i]
    AmoRate_NewHires[i-1] <- sum(Amortization_NewHires[ProjectionCount,]) / NewHirePayroll[i]
    
    if(CostShare_AmoNew == "Yes"){
      EE_AmoRate_NewHires[i-1] <- AmoRate_NewHires[i-1] / 2
    } else {
      EE_AmoRate_NewHires[i-1] <- 0
    }
    
    if(FundingPolicy == "Fixed Statutory"){
      StatAmoRate[i-1] <- FixedStat_AmoPayment[i] / (CurrentHirePayrollDB[i] + NewHirePayrollDB[i])
    } else {
      StatAmoRate[i-1] <- VarStat_AmoPayment[i] / (CurrentHirePayrollDB[i] + NewHirePayrollDB[i])
    }
    #
    
    #DC Contribution
    if(DC_Policy == "Current Policy"){
      ER_DC_Pct[i] <- if(FundPeriod_Red[i] <= RedRatFundPeriod) {RedRatFundPeriod_ER} else {Stat_ER[i]}   #If the DB amortization period drops below 25 years and remains below 25 following the rollback of additional ER & EE contributions, then reduce the DC ER contribution rate to 6.9%.
    } else if(DC_Policy == "No DC Rollback"){
      ER_DC_Pct[i] <- Stat_ER[i]
    } else {
      ER_DC_Pct[i] <- DC_ContRate
    }
    
    ERContrib_DC[i] <- ER_DC_Pct[i] * Ratio_DCVesting * (CurrentHirePayrollDC[i] + NewHirePayrollDC[i])
    
    #Cashflows, NC, Amo. Solv, etc.
    EE_NC_CurrentHires[i] <- EE_NC_Legacy_Pct[i-1]*CurrentHirePayrollDB[i]
    EE_NC_NewHires[i] <- EE_NC_NewHires_Pct[i-1]*NewHirePayrollDB[i]
    EE_Amo_NewHires[i] <- EE_AmoRate_NewHires[i-1]*NewHirePayrollDB[i]
    ER_NC_CurrentHires[i] <- ER_NC_Legacy_Pct[i-1]*CurrentHirePayrollDB[i] - AdminExp_CurrentHires[i]
    ER_NC_NewHires[i] <- ER_NC_NewHires_Pct[i-1]*NewHirePayrollDB[i] - AdminExp_NewHires[i]
    
    if(FundingPolicy == "ADC"){
      ER_Amo_CurrentHires[i] <- max(AmoRate_CurrentHires[i-1]*TotalPayroll[i],-ER_NC_CurrentHires[i])
      ER_Amo_NewHires[i] <- max(AmoRate_NewHires[i-1]*NewHirePayroll[i] - EE_Amo_NewHires[i],-ER_NC_NewHires[i])
    } else {
      ER_Amo_CurrentHires[i] <- max(StatAmoRate[i-1]*CurrentHirePayrollDB[i],-ER_NC_CurrentHires[i])
      ER_Amo_NewHires[i] <- max(StatAmoRate[i-1]*NewHirePayrollDB[i],-ER_NC_NewHires[i])
    }
    
    ER_Amo_CurrentHires[i] <- min(ER_Amo_CurrentHires[i], Max_ERContrib * TotalPayroll[i] - ER_NC_CurrentHires[i] - ER_NC_NewHires[i] - ER_Amo_NewHires[i] - ERContrib_DC[i]) #Subject the employer contribution to a cap 
    
    
    if(FYE[i] == 2022){
      ERCashInfusion <- OneTimeInfusion
    } else {
      ERCashInfusion <- 0
    }
    
    Additional_ER[i] <- ERCashInfusion
    
    #R#eturn data
    #Assign values for simulation
    if(StoSimType == 'Assumed'){
      SimReturn <- SimReturnAssumed
    } else if(StoSimType == 'Conservative'){
      SimReturn <- SimReturnConservative
    }
    
    #Return data based on deterministic or stochastic
    if((ReturnType == 'Stochastic') && (i >= StartIndex)){
      ROA_MVA[i] <- rnorm(1,SimReturn,SimVolatility)
    } else if(ReturnType == 'Deterministic'){
      ROA_MVA[i] <- as.double(Scenario_Data[i,ScenarioIndex]) 
    }
    
    #Solvency Contribution
    DC_Forfeit[i] <- DC_ContRate * (1 - Ratio_DCVesting) * (CurrentHirePayrollDC[i] + NewHirePayrollDC[i])
    CashFlows_Total <- BenPayments_CurrentHires[i] + BenPayments_NewHires[i] + Refunds[i] + AdminExp_CurrentHires[i] +
      AdminExp_NewHires[i] + EE_NC_CurrentHires[i] + EE_NC_NewHires[i] + EE_Amo_NewHires[i] +
      ER_NC_CurrentHires[i] + ER_NC_NewHires[i] + ER_Amo_CurrentHires[i] + ER_Amo_NewHires[i] + Additional_ER[i] + DC_Forfeit[i]
    
    Solv_Contrib[i] <- as.double(max(-(MVA[i-1]*(1+ROA_MVA[i]) + CashFlows_Total*(1+ROA_MVA[i])^0.5) / (1+ROA_MVA[i])^0.5,0))
    Solv_Contrib_CurrentHires[i] <- Solv_Contrib[i] * (AccrLiab_CurrentHires[i] / AccrLiab_Total[i])
    Solv_Contrib_NewHires[i] <- Solv_Contrib[i] * (AccrLiab_NewHires[i] / AccrLiab_Total[i])
    #
    #Net CF, Expected MVA, Gain Loss, Defered Losses for current hires
    NetCF_CurrentHires[i] <- BenPayments_CurrentHires[i] + AdminExp_CurrentHires[i] + EE_NC_CurrentHires[i] +
      ER_NC_CurrentHires[i] + ER_Amo_CurrentHires[i] + Additional_ER[i] + Solv_Contrib_CurrentHires[i] + DC_Forfeit[i]
    
    ExpInvInc_CurrentHires[i] <- (MVA_CurrentHires[i-1]*CurrentHires_DR[i]) + (NetCF_CurrentHires[i]*CurrentHires_DR[i]*0.5)
    ExpectedMVA_CurrentHires[i] <- MVA_CurrentHires[i-1] + NetCF_CurrentHires[i] + ExpInvInc_CurrentHires[i]
    MVA_CurrentHires[i] <- MVA_CurrentHires[i-1]*(1+ROA_MVA[i]) + (NetCF_CurrentHires[i])*(1+ROA_MVA[i])^0.5 
    GainLoss_CurrentHires[i] <- MVA_CurrentHires[i] - ExpectedMVA_CurrentHires[i] 
    Year1GL_CurrentHires[i] <- GainLoss_CurrentHires[i]*0.25
    Year2GL_CurrentHires[i] <- Year1GL_CurrentHires[i-1]
    Year3GL_CurrentHires[i] <- Year2GL_CurrentHires[i-1]
    Year4GL_CurrentHires[i] <- Year3GL_CurrentHires[i-1]
    TotalDefered_CurrentHires[i] <- Year1GL_CurrentHires[i] + Year2GL_CurrentHires[i] + Year3GL_CurrentHires[i] + Year4GL_CurrentHires[i]
    
    #Net CF, Expected MVA, Gain Loss, Defered Losses for new hires
    NetCF_NewHires[i] <- BenPayments_NewHires[i] + AdminExp_NewHires[i] + EE_NC_NewHires[i] + EE_Amo_NewHires[i] +
      ER_NC_NewHires[i] + ER_Amo_NewHires[i] + Solv_Contrib_NewHires[i]
    
    ExpInvInc_NewHires[i] <- (MVA_NewHires[i-1]*NewHires_DR[i]) + (NetCF_NewHires[i]*NewHires_DR[i]*0.5)
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
    UAL_AVA_CurrentHires[i] <- AccrLiab_CurrentHires[i] - AVA_CurrentHires[i]
    UAL_MVA_CurrentHires[i] <- AccrLiab_CurrentHires[i] - MVA_CurrentHires[i]
    
    AVA_NewHires[i] <- AVA_NewHires[i-1] + NetCF_NewHires[i] + ExpInvInc_NewHires[i] + TotalDefered_NewHires[i]
    #AVA_NewHires[i] <- MVA_NewHires[i] - TotalDefered_NewHires[i]
    AVA_NewHires[i] <- max(AVA_NewHires[i],AVA_lowerbound*MVA_NewHires[i])
    AVA_NewHires[i] <- min(AVA_NewHires[i],AVA_upperbound*MVA_NewHires[i])
    UAL_AVA_NewHires[i] <- AccrLiab_NewHires[i] - AVA_NewHires[i]
    UAL_MVA_NewHires[i] <- AccrLiab_NewHires[i] - MVA_NewHires[i]
    
    AVA[i] <- AVA_CurrentHires[i] + AVA_NewHires[i]
    MVA[i] <- MVA_CurrentHires[i] + MVA_NewHires[i]
    FR_AVA[i] <- AVA[i] / AccrLiab_Total[i]
    FR_MVA[i] <- MVA[i] / AccrLiab_Total[i]
    UAL_AVA[i] <- AccrLiab_Total[i] - AVA[i]
    UAL_MVA[i] <- AccrLiab_Total[i] - MVA[i]
    UAL_AVA_InflAdj[i] <- UAL_AVA[i] / ((1 + asum_infl)^(FYE[i] - NC_StaryYear))
    UAL_MVA_InflAdj[i] <- UAL_MVA[i] / ((1 + asum_infl)^(FYE[i] - NC_StaryYear))
    
    
    #Employer Contribution
    Total_ERContrib_DB[i] <- ER_NC_CurrentHires[i] + ER_NC_NewHires[i] + ER_Amo_CurrentHires[i] + 
      ER_Amo_NewHires[i] + Additional_ER[i] + Solv_Contrib[i]
    
    Total_ERContrib[i] <- max(Total_ERContrib_DB[i] + ERContrib_DC[i],0)
    
    ER_InflAdj[i] <- Total_ERContrib[i] / ((1 + asum_infl)^(FYE[i] - NC_StaryYear))
    ER_Percentage[i] <- Total_ERContrib[i] / TotalPayroll[i]
    
    #All-in Employer Cost
    #The total (cumulative) employer contribution in the first year of projection must be set to equal the ER_InflAdj
    if(i == StartIndex){
      Total_ER[i] <- ER_InflAdj[i] 
    } else {
      Total_ER[i] <- Total_ER[i-1] + ER_InflAdj[i] 
    }
    
    AllInCost[i] <- Total_ER[i] + UAL_MVA_InflAdj[i]
    
  
    ##Amortization
    #Current Hires
    if(ProjectionCount < nrow(Amortization_CurrentHires)){
      OutstandingBase_CurrentHires[ProjectionCount+1,2:ncol(OutstandingBase_CurrentHires)] <- OutstandingBase_CurrentHires[ProjectionCount,1:(ncol(OutstandingBase_CurrentHires)-1)]*(1 + CurrentHires_DR[i]) - (Amortization_CurrentHires[ProjectionCount,1:ncol(Amortization_CurrentHires)]*(1 + CurrentHires_DR[i])^0.5)
      OutstandingBase_CurrentHires[ProjectionCount+1,1] <- UAL_AVA_CurrentHires[i] - sum(OutstandingBase_CurrentHires[ProjectionCount+1,2:ncol(OutstandingBase_CurrentHires)])
      
      #Amo Layers
      Amortization_CurrentHires[ProjectionCount+1,1:ncol(Amortization_CurrentHires)] <- PMT(pv = OutstandingBase_CurrentHires[ProjectionCount+1,1:(ncol(OutstandingBase_CurrentHires)-1)], 
                                                                                            r = CurrentHires_DR[i], 
                                                                                            g = AmoBaseInc_CurrentHire, 
                                                                                            t = 0.5,
                                                                                            nper = pmax(OffsetYears_CurrentHires[ProjectionCount+1,1:ncol(OffsetYears_CurrentHires)],1))
    }
    
    #New Hires
    if(ProjectionCount < nrow(Amortization_NewHires)){
      OutstandingBase_NewHires[ProjectionCount+1,2:ncol(OutstandingBase_NewHires)] <- OutstandingBase_NewHires[ProjectionCount,1:(ncol(OutstandingBase_NewHires)-1)]*(1 + NewHires_DR[i]) - (Amortization_NewHires[ProjectionCount,1:ncol(Amortization_NewHires)]*(1 + NewHires_DR[i])^0.5)
      OutstandingBase_NewHires[ProjectionCount+1,1] <- UAL_AVA_NewHires[i] - sum(OutstandingBase_NewHires[ProjectionCount+1,2:ncol(OutstandingBase_NewHires)])
      
      #Amo Layers
      Amortization_NewHires[ProjectionCount+1,1:ncol(Amortization_NewHires)] <- PMT(pv = OutstandingBase_NewHires[ProjectionCount+1,1:(ncol(OutstandingBase_NewHires)-1)], 
                                                                                    r = NewHires_DR[i], 
                                                                                    g = AmoBaseInc_NewHire, 
                                                                                    t = 0.5,
                                                                                    nper = pmax(OffsetYears_NewHires[ProjectionCount+1,1:ncol(OffsetYears_NewHires)],1))
    }
    #
  }  
  
  #Join all the outputs together
  Output <- data.frame(sapply(colnames(Historical_Data), get, envir = sys.frame(sys.parent(0))))  #Get all the vectors that match the column names in Historical_Data and put them in the output data frame
  Output <- data.frame(lapply(Output, function(x) ifelse(!is.na(as.numeric(x)), as.numeric(x), x)))  #The code above converted all columns into character due to the ADC_Cond column. This line fixes that by converting all columns (except ADC_Cond) back to numeric.
  return(Output)
}
#
# ##################################################################################################################################################################
#Test

# library(plotly)
# 
# test2 <- RunModel(NewHireDC_choice = 0.75, DC_Policy = "No DC Rollback", FundingPolicy = "ADC")
# 
# ggplotly(qplot(data = test2, x = FYE, y = ER_Percentage, geom = "line", ylim = c(0,NA)))
# ggplotly(qplot(data = test2, x = FYE, y = FR_MVA, geom = "line", ylim = c(0,NA)))
# ggplotly(qplot(data = test2, x = FYE, y = AllInCost, geom = "line", ylim = c(0,NA)))
# ggplotly(qplot(data = test2, x = FYE, y = ER_DC_Pct, geom = "line", ylim = c(0,NA)))
# 
# 
# test2$ER_Percentage
# write.csv(test2, "test2.csv")


# a <- as.character(expression(Output, AccrLiab_Total, MVA, AVA, UAL_AVA_InflAdj, FR_AVA, FR_MVA, ER_Percentage, Total_ERContrib, AllInCost))
# Stat_baseline <- as.data.frame(RunModel()) %>% select(all_of(a))
# Stat_two_recession <- as.data.frame(RunModel(DeSimType = "Recurring Recession")) %>% select(all_of(a))
# ADC_baseline <- as.data.frame(RunModel(FundingPolicy = "ADC")) %>% select(all_of(a))
# ADC_two_recession <- as.data.frame(RunModel(FundingPolicy = "ADC", DeSimType = "Recurring Recession")) %>% select(all_of(a))
# ADC_two_recession_costshare <- as.data.frame(RunModel(FundingPolicy = "ADC", DeSimType = "Recurring Recession", CostShare_AmoNew = "Yes", CostShare_NCNew = "Yes")) %>% select(all_of(a))
# ADC_two_recess_costshare_40DC <- as.data.frame(RunModel(FundingPolicy = "ADC", DeSimType = "Recurring Recession", CostShare_AmoNew = "Yes", CostShare_NCNew = "Yes", NewHireDC_choice = 0.4)) %>% select(all_of(a))
# Stat_cash <- as.data.frame(RunModel(OneTimeInfusion = 300)) %>% select(all_of(a))
# ADC_cash <- as.data.frame(RunModel(OneTimeInfusion = 300, FundingPolicy = "ADC")) %>% select(all_of(a))
# ADC_DR_change <- as.data.frame(RunModel(FundingPolicy = "ADC", DR_CurrentHires = 0.06, DR_NewHires = 0.06)) %>% select(all_of(a))
# ADC_amo_change <- as.data.frame(RunModel(FundingPolicy = "ADC", 
#                                          CurrentDebt_period = 25, 
#                                          NewDebtCurrentHire_period = 20, 
#                                          NewDebtNewHire_period = 10, 
#                                          DeSimType = "Recurring Recession")) %>% select(all_of(a))
# 
# 
# 
# result <- list(Stat_baseline = Stat_baseline,
#                Stat_two_recession = Stat_two_recession,
#                ADC_baseline = ADC_baseline,
#                ADC_two_recession = ADC_two_recession,
#                ADC_two_recession_costshare = ADC_two_recession_costshare,
#                ADC_two_recess_costshare_40DC = ADC_two_recess_costshare_40DC,
#                Stat_cash = Stat_cash,
#                ADC_cash = ADC_cash,
#                ADC_DR_change = ADC_DR_change,
#                ADC_amo_change = ADC_amo_change
#                )
# 
# 
# library(openxlsx)
# write.xlsx(result, "result.xlsx", overwrite = T)

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