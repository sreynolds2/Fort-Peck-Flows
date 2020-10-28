

setwd("./GitHub/Fort-Peck-Flows")

source("./R/1_global.r")
source("./R/2_functions.r")

inputs_old<- create_inputs()
inputs_new<- create_inputs(a_mat_min=14, a_mat_max=27, a_mat_h=18,    
                           mat_var_k=TRUE,      #Control the variance in terms of k (measure of maturation rate) or directly?
                           k_mat=1,       #Measure of maturation rate 
                           three_sigma=5, #Age width
                           direct_maturation=FALSE)

barplot(cumsum(inputs_old$mat$m_i[9:27]))
barplot(cumsum(inputs_new$mat$m_i[9:27]))



abund$RRF_2020<- round(inputs_new$psi[1:23]*inputs_new$probF*abund$N_2020)
abund$matF_2020<- round(inputs_new$mat$m_i[1:23]*inputs_new$probF*abund$N_2020)
sum(abund$N_2020)*0.5
sum(abund$matF_2020)
abund$imm_2020<- abund$N_2020-abund$mat_2020 
