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


database = read_excel("Mining_survey_new.xlsx")

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

apollo_fixed = c("scale_split_T1") #fix the scale for one of the versions

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
  V = list()
  
  V[["alternative1"]]  =  (scale_split_T2*(1-split)+scale_split_T1*split)*((split)*(asc_SQ + b_COST*COST1 + b_SB*SB1 + b_SAL*SAL1 +b_JOB * JOB1)
                                                                          +(1-split)*((asc_SQ+asc_SQ_shift_split)+(b_COST+b_COST_shift_split)*COST1+(b_SB+b_SB_shift_split)*SB1 + (b_SAL+b_SAL_shift_split)*SAL1 + (b_JOB+b_JOB_shift_split)*JOB1))
                                                                           
                                                                           
                                                                                                                              
  V[["alternative2"]]  =  (scale_split_T2*(1-split)+scale_split_T1*split)*((split)*(asc_SQ + b_COST*COST2 + b_SB*SB2 + b_SAL*SAL2 +b_JOB * JOB2)
                                                                           +(1-split)*((asc_SQ+asc_SQ_shift_split)+(b_COST+b_COST_shift_split)*COST2+(b_SB+b_SB_shift_split)*SB2 + (b_SAL+b_SAL_shift_split)*SAL2 + (b_JOB+b_JOB_shift_split)*JOB2))
  
  V[["alternative3"]]  =  (scale_split_T2*(1-split)+scale_split_T1*split)*((split)*(asc_SQ + b_COST*COST3 + b_SB*SB3 + b_SAL*SAL3 +b_JOB * JOB3)
                                                                           +(1-split)*((asc_SQ+asc_SQ_shift_split)+(b_COST+b_COST_shift_split)*COST3+(b_SB+b_SB_shift_split)*SB3 + (b_SAL+b_SAL_shift_split)*SAL3 + (b_JOB+b_JOB_shift_split)*JOB3))
  
  
  
  #V = list()
  
  #V[['alt1']]  =  (scale_base*(Certain)+scale_treat*(1-Certain))*((Certain)*(asc_B + cost *Cost1 + torsk*KT1 + laks*VL1+bunn*HB1 + land*KL1)+
   #                                                                 (1-Certain)*((asc_B+asc_B_D)+(cost+cost_D)*Cost1 + (torsk+torsk_D)*KT1+(laks +laks_D)*VL1+ (bunn+bunn_D)*HB1 + (land+land_D)*KL1))
  
  #V[['alt2']]  =  (scale_base*(Certain)+scale_treat*(1-Certain))*((Certain)*(cost *Cost2 + torsk*KT2 + laks*VL2+bunn*HB2 + land*KL2)+
   #                                                                 (1-Certain)*((cost+cost_D)*Cost2 + (torsk+torsk_D)*KT2+(laks +laks_D)*VL2+ (bunn+bunn_D)*HB2 + (land+land_D)*KL2))
  
  #V[['alt3']]  =  (scale_base*(Certain)+scale_treat*(1-Certain))*((Certain)*(cost *Cost3 + torsk*KT3 + laks*VL1+bunn*HB3 + land*KL3)+
   #                                                                 (1-Certain)*((cost+cost_D)*Cost3 + (torsk+torsk_D)*KT3+(laks +laks_D)*VL3+ (bunn+bunn_D)*HB3 + (land+land_D)*KL3))
  
  
  
  
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
