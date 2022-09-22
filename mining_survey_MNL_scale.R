rm(list = ls())

library(apollo)
install.packages("apollo")

apollo_initialise()

apollo_control = list(
  modelName  = "MNL_mining_all",
  modelDescr = "MNL_model with separate relative scale for non-use choices",
  indivID    = "ID",
  mixing     = FALSE,
  nCores     = 3,
  outputDirectory = "output Mining MNL scaled"
)

pacman::p_load(
  tidyverse,    # data management packages 
  readxl        # handle Excel data
)


database = read_excel("Mining_survey_NEW_RSK.xlsx")
databaseOslo <- subset(database, database$count ==3)
databaseFinnm <- subset(database, database$count == 20)

dataN <- merge(databaseOslo, databaseFinnm, all=T)

attach(database)

#this is where we ususally would use apollo_choiceAnalysis but I didn't

apollo_beta=c(asc_SQ = 0,
              asc_SQ_shift_split = 0,
              b_SB = 0,
              b_SB_shift_split = 0,
              b_SAL = 0,
              b_SAL_shift_split = 0,
              b_JOB = 0,
              b_JOB_shift_split = 0,
              b_COST = 0,
              b_COST_shift_split = 0,
              scale_split_T1 =1,
              scale_split_T2 = 1)

apollo_beta=c(asc_SQ = 0,
              asc_SQ_shift_count = 0,
              b_SB = 0,
              b_SB_shift_count = 0,
              b_SAL = 0,
              b_SAL_shift_count = 0,
              b_JOB = 0,
              b_JOB_shift_count = 0,
              b_COST = 0,
              b_COST_shift_count = 0,
              scale_count_oslo =1,
              scale_count_Finnm = 1)


apollo_fixed = c("scale_count_oslo") #fix the scale for one of the versions

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P and List of utilities V
  P = list()
  
  ### Create alternative specific constants and coefficients using interactions with demographics
  #asc_SQ_value = asc_SQ + asc_SQ_shift_split*split
  #b_SB_value  = b_SB +  b_SB_shift_split*split
  #b_SAL_value  = b_SAL + b_SAL_shift_split*split
  #b_JOB_value  = b_JOB + b_JOB_shift_split*split
  
  #V = list() 
  #V[["alternative1"]]  =  asc_SQ_value +  b_SB_value * SB1 + b_JOB_value * JOB1 + b_SAL_value * SAL1
  #V[["alternative2"]]  =                         b_SB_value * SB2 + b_JOB_value * JOB2 + b_SAL_value * SAL2 + b_COST * COST2
  #V[["alternative3"]]  =                         b_SB_value * SB3 + b_JOB_value * JOB3 + b_SAL_value * SAL3 + b_COST * COST3
  

    # model in preference space with separate scale for T1 and T2
  #V = list()
  
  #V[["alternative1"]]  =  (scale_split_T2*(1-split_dum)+scale_split_T1*(split_dum))*((split_dum)*(asc_SQ + b_COST*COST1 + b_SB*SB1 + b_SAL*SAL1 +b_JOB * JOB1)
   #                                                                       +(1-split_dum)*((asc_SQ+asc_SQ_shift_split)+(b_COST+b_COST_shift_split)*COST1+(b_SB+b_SB_shift_split)*SB1 + (b_SAL+b_SAL_shift_split)*SAL1 + (b_JOB+b_JOB_shift_split)*JOB1))
                                                                          
                                                                       
                                                                                                                              
  #V[["alternative2"]]  =  (scale_split_T2*(1-split_dum)+scale_split_T1*(split_dum))*((split_dum)*(b_COST*COST2 + b_SB*SB2 + b_SAL*SAL2 +b_JOB * JOB2)
   #                                                                        +(1-split_dum)*((b_COST+b_COST_shift_split)*COST2+(b_SB+b_SB_shift_split)*SB2 + (b_SAL+b_SAL_shift_split)*SAL2 + (b_JOB+b_JOB_shift_split)*JOB2))
  
  #V[["alternative3"]]  =  (scale_split_T2*(1-split_dum)+scale_split_T1*(split_dum))*((split_dum)*(b_COST*COST3 + b_SB*SB3 + b_SAL*SAL3 +b_JOB * JOB3)
   #                                                                        +(1-split_dum)*((b_COST+b_COST_shift_split)*COST3+(b_SB+b_SB_shift_split)*SB3 + (b_SAL+b_SAL_shift_split)*SAL3 + (b_JOB+b_JOB_shift_split)*JOB3))
  
  
  
  V = list()
  
  V[["alternative1"]]  =  (scale_count_oslo*(oslo_dum)+scale_count_Finnm*(finnm_dum))*((oslo_dum)*(asc_SQ + b_COST*COST1 + b_SB*SB1 + b_SAL*SAL1 +b_JOB * JOB1)+
                                                                   (finnm_dum)*((asc_SQ+asc_SQ_shift_count)+(b_COST+b_COST_shift_count)*COST1+(b_SB+b_SB_shift_count)*SB1 + (b_SAL+b_SAL_shift_count)*SAL1 + (b_JOB+b_JOB_shift_count)*JOB1))
  
  V[["alternative2"]]  =  (scale_count_oslo*(oslo_dum)+scale_count_Finnm*(finnm_dum))*((oslo_dum)*(b_COST*COST2 + b_SB*SB2 + b_SAL*SAL2 +b_JOB * JOB2)+
                                                                  (finnm_dum)*((b_COST+b_COST_shift_count)*COST2+(b_SB+b_SB_shift_count)*SB2+(b_SAL+b_SAL_shift_count)*SAL2 + (b_JOB+b_JOB_shift_count)*JOB2))
  
  V[["alternative3"]]  = (scale_count_oslo*(oslo_dum)+scale_count_Finnm*(finnm_dum))*((oslo_dum)*(b_COST*COST3 + b_SB*SB3 + b_SAL*SAL3 +b_JOB * JOB3)+
                                                                  (finnm_dum)*((b_COST+b_COST_shift_count)*COST3+(b_SB+b_SB_shift_count)*SB3 + (b_SAL+b_SAL_shift_count)*SAL3 + (b_JOB+b_JOB_shift_count)*JOB3))
  
  
  
    ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alternative1=1, alternative2=2, alternative3=3), 
    avail         = list(alternative1=1, alternative2=1, alternative3=1), #not required
    choiceVar     = choice,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model)

apollo_saveOutput(model) #format model to file



#delta method for WTP

deltaMethod_settings=list(expression=c(WTP_SB = "b_SB/b_COST",
                                       WTP_SAL = "b_SAL/b_COST",
                                       WTP_JOB = "b_JOB/b_COST"))
apollo_deltaMethod(model, deltaMethod_settings)
