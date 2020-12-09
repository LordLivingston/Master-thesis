# The 2-country model of section 3.3

#####################################
#initialize parameters and framework#
#####################################

iterations<-1000000
library(gmodels)
#initialize the costs of country A and B per generation unit
Cost = matrix(c(0.1, 1.1, 2.2, 0.1, 1.0, 2.0),nrow=2, ncol=3, byrow = TRUE) 
dimnames(Cost) = list(c("A", "B"),c("c_r", "c_b", "c_p"))
Cost=data.frame(Cost)


#initialize exogenous parameters
Parameters<-matrix(c(10,10,250,250,10,10,500,500,100), nrow=1)
dimnames(Parameters) = list(c("Parameters"),c("lambda_A","lambda_B","alpha_A","alpha_B","N_A","N_B","Q_Pmax_A","Q_Pmax_B","Q_K"))
Parameters<-data.frame(Parameters)


#initialize data frame to collect results
Collect <- data.frame("D_A"        = numeric(1000000),        "D_B" = numeric(1000000),
                      "Q_R_A"      = numeric(1000000),      "Q_R_B" = numeric(1000000),
                      "Q_B_A"      = numeric(1000000),      "Q_B_B" = numeric(1000000),
                      "Q_P_A"      = numeric(1000000),      "Q_P_B" = numeric(1000000),
                      "Q_tot_A"    = numeric(1000000),    "Q_tot_B" = numeric(1000000),
                      "P_A"        = numeric(1000000),        "P_B" = numeric(1000000),
                      "Blackout_A" = logical(1000000), "Blackout_B" = logical(1000000),
                      "Intermed_A" = logical(1000000), "Intermed_B" = logical(1000000),
                      "W_c_A"      = numeric(1000000),      "W_c_B" = numeric(1000000),
                      "W_p_A"      = numeric(1000000),      "W_p_B" = numeric(1000000),
                      "W_A"        = numeric(1000000),        "W_B" = numeric(1000000),
                      "Import_A"   = numeric(1000000),   "Import_B" = numeric(1000000),
                      "V_A"        = numeric(1000000),        "V_B" = numeric(1000000),
                      "pv_A"       = numeric(1000000),       "pv_B" = numeric(1000000),
                      "bm.W_c_A"   = numeric(1000000),   "bm.W_c_B" = numeric(1000000),
                      "bm.W_p_A"   = numeric(1000000),   "bm.W_p_B" = numeric(1000000),
                      "bm.W_A"     = numeric(1000000),     "bm.W_B" = numeric(1000000),
                      "bm.P_A"     = numeric(1000000),     "bm.P_B" = numeric(1000000),
                      "bm.Blackout_A"     = numeric(1000000),     "bm.Blackout_B" = numeric(1000000),
                      "Delta_A"    = numeric(1000000),    "Delta_B" = numeric(1000000),
                      "Delta1_A"   = numeric(1000000),    "Delta1_B" = numeric(1000000),
                      "costcheckP_A" = numeric(1000000), "costcheckP_B" = numeric(1000000),
                      "costcheckB_A" = numeric(1000000), "costcheckB_B" = numeric(1000000),
                      "ShortfallA"   = numeric(1000000),  "ShortfallB"  = numeric(1000000))

#set random number generator
set.seed(12345)

D <-c(replicate(iterations*2,rnorm(1,2700,300)))
Q_R<-c(replicate(iterations*2,runif(1,0,400)))
Collect$D_A         = D[1:1000000]
Collect$D_B         = D[1000001:2000000]
Collect$Q_R_A       = Q_R[1:1000000]
Collect$Q_R_B       = Q_R[1000001:2000000]
Collect$Blackout_A <- Collect$D_A > Collect$Q_R_A+Parameters$N_A*Parameters$alpha_A+Parameters$Q_Pmax_A
Collect$Blackout_B <- Collect$D_B > Collect$Q_R_B+Parameters$N_B*Parameters$alpha_B+Parameters$Q_Pmax_B
Collect$Intermed_A <- Collect$D_A > Collect$Q_R_A & Collect$D_A < Collect$Q_R_A+Parameters$N_A*Parameters$alpha_A+Parameters$Q_Pmax_A
Collect$Intermed_B <- Collect$D_B > Collect$Q_R_B & Collect$D_B < Collect$Q_R_B+Parameters$N_B*Parameters$alpha_B+Parameters$Q_Pmax_B

table(Collect$Blackout_A)
table(Collect$Intermed_A)
table(Collect$Blackout_B)
table(Collect$Intermed_B)

#######################################################################
# In this section, we determine the outcome based on the random draw. #
# We enter the data separately per case: first, we only enter the     #
# quantities and prices resulting in the case of a blackout.          #
# Lower-case letters denote temporary vectors that are written into   #
# the collect matrix.                                                 #
#######################################################################

# The results for blackout case are defined in section 3: All energy is dispatched domestically, 
# the total sum of energy is equal to total renewable, base-load and peak-load generation.
# The price is equal to VoLL. As a result, consumer welfare is always zero in this case.
# The zeros assigned in the "else"-case are placeholders and will be overwritten in the respective case.
# The variable exPeakA is only true if some, but not all peak-load capacity is dispatched domestically. In this case,
# the difference between the maximum capacity and the dispatch is available for export. 
# This logical variable helps compute imports. 

Collect$Q_tot_A <- ifelse(Collect$Blackout_A > 0, Collect$Q_R_A+Parameters$N_A*Parameters$alpha_A+Parameters$Q_Pmax_A, 0) 
Collect$Q_B_A   <- ifelse(Collect$Blackout_A > 0, Parameters$N_A*Parameters$alpha_A, 0)
Collect$Q_P_A   <- ifelse(Collect$Blackout_A > 0, Parameters$Q_Pmax_A, 0) 
Collect$P_A     <- ifelse(Collect$Blackout_A > 0, Parameters$lambda_A, 0) 
Collect$W_c_A   <- ifelse(Collect$Blackout_A > 0, 0,0)
Collect$W_p_A   <- ifelse(Collect$Blackout_A > 0, (Collect$P_A-Cost$c_r[1])*Collect$Q_R_A+(Collect$P_A-Cost$c_b[1])*Collect$Q_B_A+(Collect$P_A-Cost$c_p[1])*Collect$Q_P_A, 0)
Collect$W_A     <- ifelse(Collect$Blackout_A > 0, Collect$Q_tot_A*Parameters$lambda_A-(Collect$Q_B_A*Cost$c_b[1]+Collect$Q_P_A*Cost$c_p[1]+Collect$Q_R_A*Cost$c_r[1]),0)
Collect$V_A     <- ifelse(Collect$Blackout_A > 0, 0, 0)
Collect$pv_A    <- ifelse(Collect$Blackout_A > 0, 42, 0)
Collect$exPeakA <- ifelse(Collect$Blackout_A > 0, 0, 0)

Collect$Q_tot_B <- ifelse(Collect$Blackout_B > 0, Collect$Q_R_B+Parameters$N_B*Parameters$alpha_B+Parameters$Q_Pmax_B, 0) 
Collect$Q_B_B   <- ifelse(Collect$Blackout_B > 0, Parameters$N_B*Parameters$alpha_B, 0)
Collect$Q_P_B   <- ifelse(Collect$Blackout_B > 0, Parameters$Q_Pmax_B, 0) 
Collect$P_B     <- ifelse(Collect$Blackout_B > 0, Parameters$lambda_B, 0) 
Collect$W_c_B   <- ifelse(Collect$Blackout_B > 0, 0,0)
Collect$W_p_B   <- ifelse(Collect$Blackout_B > 0, (Collect$P_B-Cost$c_r[2])*Collect$Q_R_B+(Collect$P_B-Cost$c_b[2])*Collect$Q_B_B+(Collect$P_B-Cost$c_p[2])*Collect$Q_P_B, 0)
Collect$W_B     <- ifelse(Collect$Blackout_B > 0, Collect$Q_tot_B*Parameters$lambda_B-(Collect$Q_B_B*Cost$c_b[2]+Collect$Q_P_B*Cost$c_p[2]+Collect$Q_R_B*Cost$c_r[2]),0)
Collect$V_B     <- ifelse(Collect$Blackout_B > 0, 0, 0)
Collect$pv_B    <- ifelse(Collect$Blackout_B > 0, 42, 0)
Collect$exPeakB <- ifelse(Collect$Blackout_B > 0, 0, 0)

# The other corner-case is that renewable energy generation is higher than demand. Given above parameters, the probability for this is approximately zero.
# Consequently, this case does not appear in this draw. The following code is included to execute the program correctly in case of modifications to the seed or parameters.

Collect$Q_tot_A     <- ifelse(Collect$Blackout_A == 0 & Collect$Intermed_A==0, Collect$Q_R_A, Collect$Q_tot_A) 
Collect$Q_B_A       <- ifelse(Collect$Blackout_A == 0 & Collect$Intermed_A==0, 0, Collect$Q_B_A) 
Collect$Q_P_A       <- ifelse(Collect$Blackout_A == 0 & Collect$Intermed_A==0, 0, Collect$Q_P_A) 
Collect$P_A         <- ifelse(Collect$Blackout_A == 0 & Collect$Intermed_A==0, Cost$c_r[1], Collect$P_A) 
Collect$W_c_A       <- ifelse(Collect$Blackout_A == 0 & Collect$Intermed_A==0, Collect$D_A * (Parameters$lambda_A-Collect$P_A), Collect$W_c_A) 
Collect$W_p_A       <- ifelse(Collect$Blackout_A == 0 & Collect$Intermed_A==0, Collect$Q_R_A * (Collect$P_A-Cost$c_r[1]), Collect$W_p_A) 
Collect$W_A         <- ifelse(Collect$Blackout_A == 0 & Collect$Intermed_A==0, Collect$D_A*Parameters$lambda_A-(Collect$Q_R_A*Cost$c_r[1]), Collect$W_A) 
Collect$V_A         <- ifelse(Collect$Blackout_A == 0 & Collect$Intermed_A==0, Collect$Q_R_A - Collect$D_A, Collect$V_A) 
Collect$pv_A        <- ifelse(Collect$Blackout_A == 0 & Collect$Intermed_A==0, Collect$P_A, Collect$pv_A) 
Collect$exPeakA     <- ifelse(Collect$Blackout_A == 0 & Collect$Intermed_A==0, FALSE, Collect$exPeakA) 

Collect$Q_tot_B     <- ifelse(Collect$Blackout_B == 0 & Collect$Intermed_B==0, Collect$Q_R_B, Collect$Q_tot_B) 
Collect$Q_B_B       <- ifelse(Collect$Blackout_B == 0 & Collect$Intermed_B==0, 0, Collect$Q_B_B) 
Collect$Q_P_B       <- ifelse(Collect$Blackout_B == 0 & Collect$Intermed_B==0, 0, Collect$Q_P_B) 
Collect$P_B         <- ifelse(Collect$Blackout_B == 0 & Collect$Intermed_B==0, Cost$c_r[2], Collect$P_B) 
Collect$W_c_B       <- ifelse(Collect$Blackout_B == 0 & Collect$Intermed_B==0, Collect$D_B * (Parameters$lambda_B-Collect$P_B), Collect$W_c_B) 
Collect$W_p_B       <- ifelse(Collect$Blackout_B == 0 & Collect$Intermed_B==0, Collect$Q_R_B * (Collect$P_B-Cost$c_r[2]), Collect$W_p_B) 
Collect$W_B         <- ifelse(Collect$Blackout_B == 0 & Collect$Intermed_B==0, Collect$D_B*Parameters$lambda_B-(Collect$Q_R_B*Cost$c_r[2]), Collect$W_B) 
Collect$V_B         <- ifelse(Collect$Blackout_B == 0 & Collect$Intermed_B==0, Collect$Q_R_B - Collect$D_B, Collect$V_B) 
Collect$pv_B        <- ifelse(Collect$Blackout_B == 0 & Collect$Intermed_B==0, Collect$P_B, Collect$pv_B) 
Collect$exPeakB     <- ifelse(Collect$Blackout_B == 0 & Collect$Intermed_B==0, FALSE, Collect$exPeakB)

#The intermediate cases for country A

#number of baseload units that can be used at 100%. 
# First: Compute net energy demand divided by capacity
# Second: Replace values greater than number of generation units by maximum
Collect$NumB_A<-(Collect$D_A-Collect$Q_R_A)%/%Parameters$alpha_A
Collect$NumB_A<-ifelse(Collect$NumB_A>Parameters$N_A,Parameters$N_A,Collect$NumB_A)
#Remainder
Collect$Delta_A<-(Collect$D_A-Collect$Q_R_A)-Collect$NumB_A*Parameters$alpha_A
#cost of covering delta with peak-load
Collect$costcheckP_A<-Collect$Delta_A*Cost$c_p[1]
#cost of covering delta with one more baseload
Collect$costcheckB_A<-Cost$c_b[1]*Parameters$alpha_A
# Auxiliary variable Delta1 checks that peak-load production will not surpass capacity
Collect$Delta1_A <- Collect$Delta_A
Collect$Delta1_A <- ifelse(Collect$Delta1_A>Parameters$Q_Pmax_A, Parameters$Q_Pmax_A, Collect$Delta1_A)

# Handle intermediate cases where one additional base-load unit can be dispatched and it is cost-efficient to do so
Collect$Q_P_A      <- ifelse(Collect$Intermed_A==1 & Collect$NumB_A < Parameters$N_A & Collect$costcheckB_A < Collect$costcheckP_A, 0,                                                                                          Collect$Q_P_A)
Collect$Q_B_A      <- ifelse(Collect$Intermed_A==1 & Collect$NumB_A < Parameters$N_A & Collect$costcheckB_A < Collect$costcheckP_A, (Collect$NumB_A+1)*Parameters$alpha_A,                                                      Collect$Q_B_A)  
Collect$Q_tot_A    <- ifelse(Collect$Intermed_A==1 & Collect$NumB_A < Parameters$N_A & Collect$costcheckB_A < Collect$costcheckP_A, Collect$Q_R_A+Collect$Q_B_A,                                                                Collect$Q_tot_A)  
Collect$Blackout_A <- ifelse(Collect$Intermed_A==1 & Collect$NumB_A < Parameters$N_A & Collect$costcheckB_A < Collect$costcheckP_A, FALSE,                                                                                      Collect$Blackout_A)  
Collect$P_A        <- ifelse(Collect$Intermed_A==1 & Collect$NumB_A < Parameters$N_A & Collect$costcheckB_A < Collect$costcheckP_A, Cost$c_b[1],                                                                                Collect$P_A)  
Collect$W_c_A      <- ifelse(Collect$Intermed_A==1 & Collect$NumB_A < Parameters$N_A & Collect$costcheckB_A < Collect$costcheckP_A, Collect$D_A*(Parameters$lambda_A-Collect$P_A),                                              Collect$W_c_A)  
Collect$W_p_A      <- ifelse(Collect$Intermed_A==1 & Collect$NumB_A < Parameters$N_A & Collect$costcheckB_A < Collect$costcheckP_A, Collect$D_A*Collect$P_A-(Cost$c_r[1]*Collect$Q_R_A+Cost$c_b[1]*Collect$Q_B_A),              Collect$W_p_A)  
Collect$W_A        <- ifelse(Collect$Intermed_A==1 & Collect$NumB_A < Parameters$N_A & Collect$costcheckB_A < Collect$costcheckP_A, Collect$D_A*Parameters$lambda_A-(Collect$Q_B_A*Cost$c_b[1]+Collect$Q_R_A*Cost$c_r[1]),      Collect$W_A)  
Collect$V_A        <- ifelse(Collect$Intermed_A==1 & Collect$NumB_A < Parameters$N_A & Collect$costcheckB_A < Collect$costcheckP_A, Collect$Q_tot_A-Collect$D_A,                                                                Collect$V_A)  
Collect$pv_A       <- ifelse(Collect$Intermed_A==1 & Collect$NumB_A < Parameters$N_A & Collect$costcheckB_A < Collect$costcheckP_A, Cost$c_b[1],                                                                                Collect$pv_A)  
Collect$exPeakA    <- ifelse(Collect$Intermed_A==1 & Collect$NumB_A < Parameters$N_A & Collect$costcheckB_A < Collect$costcheckP_A, FALSE,                                                                                      Collect$exPeakA)  

# Handle intermediate cases where no more base-load units are available
Collect$Q_P_A      <- ifelse(Collect$Intermed_A==1 & (Collect$NumB_A == Parameters$N_A ), Collect$Delta1_A,                                                                                                         Collect$Q_P_A)
Collect$Q_B_A      <- ifelse(Collect$Intermed_A==1 & (Collect$NumB_A == Parameters$N_A ), Collect$NumB_A*Parameters$alpha_A,                                                                                       Collect$Q_B_A)  
Collect$Q_tot_A    <- ifelse(Collect$Intermed_A==1 & (Collect$NumB_A == Parameters$N_A ), Collect$Q_R_A+Collect$Q_B_A+Collect$Q_P_A,                                                                               Collect$Q_tot_A)  
Collect$Blackout_A <- ifelse(Collect$Intermed_A==1 & (Collect$NumB_A == Parameters$N_A ), FALSE,                                                                                                                   Collect$Blackout_A)  
Collect$P_A        <- ifelse(Collect$Intermed_A==1 & (Collect$NumB_A == Parameters$N_A ), Cost$c_p[1],                                                                                                             Collect$P_A)  
Collect$W_c_A      <- ifelse(Collect$Intermed_A==1 & (Collect$NumB_A == Parameters$N_A ), Collect$D_A*(Parameters$lambda_A-Collect$P_A),                                                                           Collect$W_c_A)  
Collect$W_p_A      <- ifelse(Collect$Intermed_A==1 & (Collect$NumB_A == Parameters$N_A ), Collect$D_A*Collect$P_A-(Cost$c_r[1]*Collect$Q_R_A+Cost$c_b[1]*Collect$Q_B_A+Cost$c_p[1]*Collect$Q_P_A),                 Collect$W_p_A)  
Collect$W_A        <- ifelse(Collect$Intermed_A==1 & (Collect$NumB_A == Parameters$N_A ), Collect$D_A*Parameters$lambda_A-(Collect$Q_B_A*Cost$c_b[1]+Collect$Q_P_A*Cost$c_p[1]+Collect$Q_R_A*Cost$c_r[1]),         Collect$W_A)  
Collect$V_A        <- ifelse(Collect$Intermed_A==1 & (Collect$NumB_A == Parameters$N_A ), Collect$Q_R_A+Collect$Q_B_A+Parameters$Q_Pmax_A-Collect$D_A,                                                             Collect$V_A)  
Collect$pv_A       <- ifelse(Collect$Intermed_A==1 & (Collect$NumB_A == Parameters$N_A ), Cost$c_p[1],                                                                                                             Collect$pv_A)  
Collect$exPeakA    <- ifelse(Collect$Intermed_A==1 & (Collect$NumB_A == Parameters$N_A ), TRUE,                                                                                                                    Collect$exPeakA)  

# Handle intermediate cases where peak-load electricity is cheaper: If Delta_A can be covered by peak-load, dispatch peak-load. If Delta exceeds peak-load capacity, dispatch base-load (at least more base-load. must be available, else we would be in the blackout case. One additional base-load plant must suffice, else a higher value of NumB_A would have been chosen above.)
Collect$Q_P_A      <- ifelse(Collect$Intermed_A==1 & ( Collect$costcheckB_A > Collect$costcheckP_A) & Collect$Delta1_A==Collect$Delta_A, Collect$Delta1_A,                                                                     Collect$Q_P_A)
Collect$Q_P_A      <- ifelse(Collect$Intermed_A==1 & ( Collect$costcheckB_A > Collect$costcheckP_A) & Collect$Delta1_A<Collect$Delta_A, 0,                                                                                     Collect$Q_P_A)

Collect$Q_B_A      <- ifelse(Collect$Intermed_A==1 & ( Collect$costcheckB_A > Collect$costcheckP_A) & Collect$Delta1_A==Collect$Delta_A, Collect$NumB_A*Parameters$alpha_A,                                                                                       Collect$Q_B_A)  
Collect$Q_B_A      <- ifelse(Collect$Intermed_A==1 & ( Collect$costcheckB_A > Collect$costcheckP_A) & Collect$Delta1_A<Collect$Delta_A, (Collect$NumB_A+1)*Parameters$alpha_A,                                                                                       Collect$Q_B_A)  

Collect$Q_tot_A    <- ifelse(Collect$Intermed_A==1 & ( Collect$costcheckB_A > Collect$costcheckP_A), Collect$Q_R_A+Collect$Q_B_A+Collect$Q_P_A,                                                                               Collect$Q_tot_A)  
Collect$Blackout_A <- ifelse(Collect$Intermed_A==1 & ( Collect$costcheckB_A > Collect$costcheckP_A), FALSE,                                                                                                                   Collect$Blackout_A)  
Collect$P_A        <- ifelse(Collect$Intermed_A==1 & ( Collect$costcheckB_A > Collect$costcheckP_A), Cost$c_p[1],                                                                                                             Collect$P_A)  
Collect$W_c_A      <- ifelse(Collect$Intermed_A==1 & ( Collect$costcheckB_A > Collect$costcheckP_A), Collect$D_A*(Parameters$lambda_A-Collect$P_A),                                                                           Collect$W_c_A)  
Collect$W_p_A      <- ifelse(Collect$Intermed_A==1 & ( Collect$costcheckB_A > Collect$costcheckP_A), Collect$D_A*Collect$P_A-(Cost$c_r[1]*Collect$Q_R_A+Cost$c_b[1]*Collect$Q_B_A+Cost$c_p[1]*Collect$Q_P_A),                 Collect$W_p_A)  
Collect$W_A        <- ifelse(Collect$Intermed_A==1 & ( Collect$costcheckB_A > Collect$costcheckP_A), Collect$D_A*Parameters$lambda_A-(Collect$Q_B_A*Cost$c_b[1]+Collect$Q_P_A*Cost$c_p[1]+Collect$Q_R_A*Cost$c_r[1]),         Collect$W_A)  

Collect$V_A        <- ifelse(Collect$Intermed_A==1 & ( Collect$costcheckB_A > Collect$costcheckP_A) & Collect$Delta1_A==Collect$Delta_A, Collect$Q_R_A+Collect$Q_B_A+Parameters$Q_Pmax_A-Collect$D_A,                         Collect$V_A)  
Collect$V_A        <- ifelse(Collect$Intermed_A==1 & ( Collect$costcheckB_A > Collect$costcheckP_A) & Collect$Delta1_A<Collect$Delta_A, Collect$Q_tot_A-Collect$D_A,                         Collect$V_A)  

Collect$pv_A       <- ifelse(Collect$Intermed_A==1 & ( Collect$costcheckB_A > Collect$costcheckP_A) & Collect$Delta1_A==Collect$Delta_A, Cost$c_p[1],                                                                                                             Collect$pv_A)  
Collect$pv_A       <- ifelse(Collect$Intermed_A==1 & ( Collect$costcheckB_A > Collect$costcheckP_A) & Collect$Delta1_A<Collect$Delta_A, Cost$c_b[1],                                                                                                             Collect$pv_A)  

Collect$exPeakA    <- ifelse(Collect$Intermed_A==1 & ( Collect$costcheckB_A > Collect$costcheckP_A) & Collect$Delta1_A==Collect$Delta_A, TRUE,                                                                                Collect$exPeakA)  

#The in-between cases for country B
#number of baseload units that can be used at 100%. 
# First: Compute net energy demand divided by capacity
# Second: Replace values greater than number of generation units by maximum
Collect$NumB_B <- (Collect$D_B-Collect$Q_R_B)%/%Parameters$alpha_B
Collect$NumB_B <- ifelse(Collect$NumB_B>Parameters$N_B,Parameters$N_B,Collect$NumB_B)
#Remainder
Collect$Delta_B<-(Collect$D_B-Collect$Q_R_B)-Collect$NumB_B*Parameters$alpha_B
#cost of covering delta with peak-load
Collect$costcheckP_B<-Collect$Delta_B*Cost$c_p[2]
#cost of covering delta with one more baseload
Collect$costcheckB_B<-Cost$c_b[2]*Parameters$alpha_B
# Auxiliary variable Delta1 checks that peak-load production will not surpass capacity
Collect$Delta1_B <- Collect$Delta_B
Collect$Delta1_B <- ifelse(Collect$Delta1_B>Parameters$Q_Pmax_B, Parameters$Q_Pmax_B, Collect$Delta1_B)

# Handle intermediate cases where one additional base-load unit can be dispatched and it is cost-efficient to do so
Collect$Q_P_B      <- ifelse(Collect$Intermed_B==1 & Collect$NumB_B < Parameters$N_B & Collect$costcheckB_B < Collect$costcheckP_B, 0,                                                                                     Collect$Q_P_B)
Collect$Q_B_B      <- ifelse(Collect$Intermed_B==1 & Collect$NumB_B < Parameters$N_B & Collect$costcheckB_B < Collect$costcheckP_B, (Collect$NumB_B+1)*Parameters$alpha_B,                                                 Collect$Q_B_B)  
Collect$Q_tot_B    <- ifelse(Collect$Intermed_B==1 & Collect$NumB_B < Parameters$N_B & Collect$costcheckB_B < Collect$costcheckP_B, Collect$Q_R_B+Collect$Q_B_B,                                                           Collect$Q_tot_B)  
Collect$Blackout_B <- ifelse(Collect$Intermed_B==1 & Collect$NumB_B < Parameters$N_B & Collect$costcheckB_B < Collect$costcheckP_B, FALSE,                                                                                 Collect$Blackout_B)  
Collect$P_B        <- ifelse(Collect$Intermed_B==1 & Collect$NumB_B < Parameters$N_B & Collect$costcheckB_B < Collect$costcheckP_B, Cost$c_b[2],                                                                           Collect$P_B)  
Collect$W_c_B      <- ifelse(Collect$Intermed_B==1 & Collect$NumB_B < Parameters$N_B & Collect$costcheckB_B < Collect$costcheckP_B, Collect$D_B*(Parameters$lambda_B-Collect$P_B),                                         Collect$W_c_B)  
Collect$W_p_B      <- ifelse(Collect$Intermed_B==1 & Collect$NumB_B < Parameters$N_B & Collect$costcheckB_B < Collect$costcheckP_B, Collect$D_B*Collect$P_B-(Cost$c_r[2]*Collect$Q_R_B+Cost$c_b[2]*Collect$Q_B_B),         Collect$W_p_B)  
Collect$W_B        <- ifelse(Collect$Intermed_B==1 & Collect$NumB_B < Parameters$N_B & Collect$costcheckB_B < Collect$costcheckP_B, Collect$D_B*Parameters$lambda_B-(Collect$Q_B_B*Cost$c_b[2]+Collect$Q_R_B*Cost$c_r[2]), Collect$W_B)  
Collect$V_B        <- ifelse(Collect$Intermed_B==1 & Collect$NumB_B < Parameters$N_B & Collect$costcheckB_B < Collect$costcheckP_B, Collect$Q_tot_B-Collect$D_B,                                                           Collect$V_B)  
Collect$pv_B       <- ifelse(Collect$Intermed_B==1 & Collect$NumB_B < Parameters$N_B & Collect$costcheckB_B < Collect$costcheckP_B, Cost$c_b[2],                                                                           Collect$pv_B)  
Collect$exPeakB    <- ifelse(Collect$Intermed_B==1 & Collect$NumB_B < Parameters$N_B & Collect$costcheckB_B < Collect$costcheckP_B, FALSE,                                                                                 Collect$exPeakB)  

# Handle intermediate cases where using peak-load energy is either cost-efficient or no more base-load units are available
Collect$Q_P_B      <- ifelse(Collect$Intermed_B==1 & (Collect$NumB_B == Parameters$N_B), Collect$Delta1_B,                                                                                                         Collect$Q_P_B)
Collect$Q_B_B      <- ifelse(Collect$Intermed_B==1 & (Collect$NumB_B == Parameters$N_B), Collect$NumB_B*Parameters$alpha_B,                                                                                       Collect$Q_B_B)  
Collect$Q_tot_B    <- ifelse(Collect$Intermed_B==1 & (Collect$NumB_B == Parameters$N_B), Collect$Q_R_B+Collect$Q_B_B+Collect$Q_P_B,                                                                               Collect$Q_tot_B)  
Collect$Blackout_B <- ifelse(Collect$Intermed_B==1 & (Collect$NumB_B == Parameters$N_B), FALSE,                                                                                                                   Collect$Blackout_B)  
Collect$P_B        <- ifelse(Collect$Intermed_B==1 & (Collect$NumB_B == Parameters$N_B), Cost$c_p[2],                                                                                                             Collect$P_B)  
Collect$W_c_B      <- ifelse(Collect$Intermed_B==1 & (Collect$NumB_B == Parameters$N_B), Collect$D_B*(Parameters$lambda_B-Collect$P_B),                                                                           Collect$W_c_B)  
Collect$W_p_B      <- ifelse(Collect$Intermed_B==1 & (Collect$NumB_B == Parameters$N_B), Collect$D_B*Collect$P_B-(Cost$c_r[2]*Collect$Q_R_B+Cost$c_b[2]*Collect$Q_B_B+Cost$c_p[2]*Collect$Q_P_B),                 Collect$W_p_B)  
Collect$W_B        <- ifelse(Collect$Intermed_B==1 & (Collect$NumB_B == Parameters$N_B), Collect$D_B*Parameters$lambda_B-(Collect$Q_B_B*Cost$c_b[2]+Collect$Q_P_B*Cost$c_p[2]+Collect$Q_R_B*Cost$c_r[2]),         Collect$W_B)  
Collect$V_B        <- ifelse(Collect$Intermed_B==1 & (Collect$NumB_B == Parameters$N_B), Collect$Q_R_B+Collect$Q_B_B+Parameters$Q_Pmax_B-Collect$D_B,                                                             Collect$V_B)  
Collect$pv_B       <- ifelse(Collect$Intermed_B==1 & (Collect$NumB_B == Parameters$N_B), Cost$c_p[2],                                                                                                             Collect$pv_B)  
Collect$exPeakB    <- ifelse(Collect$Intermed_B==1 & (Collect$NumB_B == Parameters$N_B), TRUE,                                                                                                                    Collect$exPeakB)  

# Handle intermediate cases where peak-load electricity is cheaper: If Delta_B can be covered by peak-load, dispatch peak-load. If Delta exceeds peak-load capacity, dispatch base-load (at least more base-load. must be available, else we would be in the blackout case. One additional base-load plant must suffice, else a higher value of NumB_B would have been chosen above.)
Collect$Q_P_B      <- ifelse(Collect$Intermed_B==1 & ( Collect$costcheckB_B > Collect$costcheckP_B) & Collect$Delta1_B==Collect$Delta_B, Collect$Delta1_B,                                                                     Collect$Q_P_B)
Collect$Q_P_B      <- ifelse(Collect$Intermed_B==1 & ( Collect$costcheckB_B > Collect$costcheckP_B) & Collect$Delta1_B<Collect$Delta_B, 0,                                                                                     Collect$Q_P_B)

Collect$Q_B_B      <- ifelse(Collect$Intermed_B==1 & ( Collect$costcheckB_B > Collect$costcheckP_B) & Collect$Delta1_B==Collect$Delta_B, Collect$NumB_B*Parameters$alpha_B,                                                                                       Collect$Q_B_B)  
Collect$Q_B_B      <- ifelse(Collect$Intermed_B==1 & ( Collect$costcheckB_B > Collect$costcheckP_B) & Collect$Delta1_B<Collect$Delta_B, (Collect$NumB_B+1)*Parameters$alpha_B,                                                                                       Collect$Q_B_B)  

Collect$Q_tot_B    <- ifelse(Collect$Intermed_B==1 & ( Collect$costcheckB_B > Collect$costcheckP_B), Collect$Q_R_B+Collect$Q_B_B+Collect$Q_P_B,                                                                               Collect$Q_tot_B)  
Collect$Blackout_B <- ifelse(Collect$Intermed_B==1 & ( Collect$costcheckB_B > Collect$costcheckP_B), FALSE,                                                                                                                   Collect$Blackout_B)  
Collect$P_B        <- ifelse(Collect$Intermed_B==1 & ( Collect$costcheckB_B > Collect$costcheckP_B), Cost$c_p[2],                                                                                                             Collect$P_B)  
Collect$W_c_B      <- ifelse(Collect$Intermed_B==1 & ( Collect$costcheckB_B > Collect$costcheckP_B), Collect$D_B*(Parameters$lambda_B-Collect$P_B),                                                                           Collect$W_c_B)  
Collect$W_p_B      <- ifelse(Collect$Intermed_B==1 & ( Collect$costcheckB_B > Collect$costcheckP_B), Collect$D_B*Collect$P_B-(Cost$c_r[2]*Collect$Q_R_B+Cost$c_b[2]*Collect$Q_B_B+Cost$c_p[2]*Collect$Q_P_B),                 Collect$W_p_B)  
Collect$W_B        <- ifelse(Collect$Intermed_B==1 & ( Collect$costcheckB_B > Collect$costcheckP_B), Collect$D_B*Parameters$lambda_B-(Collect$Q_B_B*Cost$c_b[2]+Collect$Q_P_B*Cost$c_p[2]+Collect$Q_R_B*Cost$c_r[2]),         Collect$W_B)  

Collect$V_B        <- ifelse(Collect$Intermed_B==1 & ( Collect$costcheckB_B > Collect$costcheckP_B) & Collect$Delta1_B==Collect$Delta_B, Collect$Q_R_B+Collect$Q_B_B+Parameters$Q_Pmax_B-Collect$D_B,                         Collect$V_B)  
Collect$V_B        <- ifelse(Collect$Intermed_B==1 & ( Collect$costcheckB_B > Collect$costcheckP_B) & Collect$Delta1_B<Collect$Delta_B, Collect$Q_tot_B-Collect$D_B,                         Collect$V_B)  

Collect$pv_B       <- ifelse(Collect$Intermed_B==1 & ( Collect$costcheckB_B > Collect$costcheckP_B) & Collect$Delta1_B==Collect$Delta_B, Cost$c_p[2],                                                                                                             Collect$pv_B)  
Collect$pv_B       <- ifelse(Collect$Intermed_B==1 & ( Collect$costcheckB_B > Collect$costcheckP_B) & Collect$Delta1_B<Collect$Delta_B, Cost$c_b[2],                                                                                                             Collect$pv_B)  

Collect$exPeakA    <- ifelse(Collect$Intermed_B==1 & ( Collect$costcheckB_B > Collect$costcheckP_B) & Collect$Delta1_B==Collect$Delta_B, TRUE,                                                                                Collect$exPeakA)  

# Set benchmark ("bm.") values to compare with the outcome after trade.
Collect$bm.W_c_A<-Collect$W_c_A
Collect$bm.W_c_B<-Collect$W_c_B
Collect$bm.W_p_A<-Collect$W_p_A
Collect$bm.W_p_B<-Collect$W_p_B
Collect$bm.W_A<-Collect$W_A
Collect$bm.W_B<-Collect$W_B
Collect$bm.P_A<-Collect$P_A
Collect$bm.P_B<-Collect$P_B
Collect$bm.Blackout_A<-Collect$Blackout_A
Collect$bm.Blackout_B<-Collect$Blackout_B
# After computing production and available trade energy, run a second round
# For each country, compute the amount of energy that it would like to import

###########Case 1a) Country A has a blackout, wants to import
Collect$ShortfallA <- Collect$D_A-Collect$Q_tot_A
Collect$ShortfallA <- ifelse(Collect$ShortfallA<0, 0, Collect$ShortfallA)

Collect$Import_A <- Collect$ShortfallA
Collect$Import_A <- ifelse(Collect$Import_A > Collect$V_B,  Collect$V_B, Collect$Import_A)
Collect$Import_A <- ifelse(Collect$Import_A > Parameters$Q_K,  Parameters$Q_K, Collect$Import_A)

#Update the price and resulting welfare if energy is traded
#If we assume that peak-load capacity is larger than transmission volume, this pricing is correct
#Otherwise, we would have to check that imported energy doesn't crowd out peak-load energy
Collect$Q_tot_A <- Collect$Q_tot_A+Collect$Import_A

Collect$P_A <- ifelse(Collect$Import_A==Collect$ShortfallA & Collect$Blackout_A == 1, Collect$pv_B, Collect$P_A)
Collect$P_A <- ifelse(Collect$Import_A==Collect$ShortfallA & Collect$Blackout_A == 1 & Collect$pv_B < Cost$c_p[1], Cost$c_p[1], Collect$P_A)

Collect$Blackout_A <- ifelse(Collect$Import_A == Collect$ShortfallA & Collect$Blackout_A == 1, FALSE, Collect$Blackout_A)

#there are two possible sources of change in consumer welfare here: first, an increase in traded amount through import, second, a possibly lower price through importing
Collect$Q_P_B <- ifelse(Collect$bm.Blackout_A==1&Collect$bm.P_A>Collect$pv_B, Collect$Q_P_B+Collect$Import_A*Collect$exPeakB, Collect$Q_P_B)
Collect$W_c_A <- ifelse(Collect$bm.Blackout_A==1&Collect$bm.P_A>Collect$pv_B, Collect$Q_tot_A*(Parameters$lambda_A-Collect$P_A), Collect$W_c_A)
Collect$W_p_A <- ifelse(Collect$bm.Blackout_A==1&Collect$bm.P_A>Collect$pv_B,(Collect$P_A-Cost$c_r[1])*Collect$Q_R_A+(Collect$P_A-Cost$c_b[1])*Collect$Q_B_A+(Collect$P_A-Cost$c_p[1])*Collect$Q_P_A, Collect$W_p_A)
Collect$W_p_B <- ifelse(Collect$Import_A>0, Collect$W_p_B+(Collect$P_A-(Collect$exPeakB*Collect$pv_B))*Collect$Import_A, Collect$W_p_B) #producers gain the proceeds from exporting, but if they produce peak-load energy for export, they have to pay the costs
Collect$W_A = Collect$W_c_A+Collect$W_p_A
Collect$W_B = Collect$W_c_B+Collect$W_p_B
  
###########Case 1b) Country B has a blackout, wants to import
Collect$ShortfallB <- Collect$D_B-Collect$Q_tot_B
Collect$ShortfallB <- ifelse(Collect$ShortfallB<0, 0, Collect$ShortfallB)

Collect$Import_B <- Collect$ShortfallB
Collect$Import_B <- ifelse(Collect$Import_B > Collect$V_A,  Collect$V_A, Collect$Import_B)
Collect$Import_B <- ifelse(Collect$Import_B > Parameters$Q_K, Parameters$Q_K, Collect$Import_B)

#Update the price and resulting welfare if energy is traded
#If we assume that peak-load capacity is larger than transmission volume, this pricing is correct
#Otherwise, we would have to check that imported energy doesn't crowd out peak-load energy
Collect$Q_tot_B <- Collect$Q_tot_B+Collect$Import_B

Collect$P_B <- ifelse(Collect$Import_B==Collect$ShortfallB & Collect$Blackout_B == 1, Collect$pv_A, Collect$P_B)
Collect$P_B <- ifelse(Collect$Import_B==Collect$ShortfallB & Collect$Blackout_B == 1 & Collect$pv_A < Cost$c_p[2], Cost$c_p[2], Collect$P_B)

Collect$Blackout_B <- ifelse(Collect$Import_B == Collect$ShortfallB & Collect$Blackout_B == 1, FALSE, Collect$Blackout_B)

#there are two possible sources of change in consumer welfare here: first, an increase in traded amount through import, second, a possibly lower price through importing
Collect$Q_P_A <- ifelse(Collect$bm.Blackout_B==1&Collect$bm.P_B>Collect$pv_A, Collect$Q_P_A+Collect$Import_B*Collect$exPeakA, Collect$Q_P_A)
Collect$W_c_B <- ifelse(Collect$bm.Blackout_B==1&Collect$bm.P_B>Collect$pv_A, Collect$Q_tot_B*(Parameters$lambda_B-Collect$P_B), Collect$W_c_B)
Collect$W_p_B <- ifelse(Collect$bm.Blackout_B==1&Collect$bm.P_B>Collect$pv_A,(Collect$P_B-Cost$c_r[2])*Collect$Q_R_B+(Collect$P_B-Cost$c_b[2])*Collect$Q_B_B+(Collect$P_B-Cost$c_p[2])*Collect$Q_P_B, Collect$W_p_B)
Collect$W_p_A <- ifelse(Collect$Import_B>0, Collect$W_p_A+(Collect$P_B-(Collect$exPeakA*Collect$pv_A))*Collect$Import_B, Collect$W_p_A) #producers gain the proceeds from exporting, but if they produce peak-load energy for export, they have to pay the costs
Collect$W_B = Collect$W_c_B+Collect$W_p_B
Collect$W_A = Collect$W_c_A+Collect$W_p_A

### Case 2a), Country A is the in-between case, considers importing only if cheap energy is offered
Collect$Import_A <- ifelse(Collect$Intermed_A == 1 & Collect$pv_B < Collect$P_A & Collect$Q_P_A > 0, Collect$V_B, Collect$Import_A)
Collect$Import_A <- ifelse(Collect$Intermed_A == 1 & Collect$Import_A > Parameters$Q_K, Parameters$Q_K, Collect$Import_A)
Collect$Import_A <- ifelse(Collect$Intermed_A == 1 & Collect$Import_A > Collect$Q_P_A, Collect$Q_P_A, Collect$Import_A)

Collect$Q_P_A <- ifelse(Collect$Intermed_A == 1 & Collect$Q_R_A+Collect$Q_B_A+Collect$Import_A > Collect$D_A, 0, Collect$Q_P_A - Collect$Import_A )

Collect$P_A <- ifelse(Collect$Intermed_A == 1 & Collect$Import_A > 0 & Collect$Q_R_A+Collect$Q_B_A+Collect$Import_A > Collect$D_A, Collect$pv_B, Collect$P_A)
Collect$P_A <- ifelse(Collect$Intermed_A == 1 & Collect$Import_A > 0 & Collect$Q_R_A+Collect$Q_B_A+Collect$Import_A > Collect$D_A & Collect$pv_B < Cost$c_b[1] & Collect$Q_B_A > 0, Cost$c_b[1], Collect$P_A)
  
Collect$Q_P_B <- ifelse(Collect$Intermed_B==1 & ( Collect$costcheckB_B > Collect$costcheckP_B) & Collect$Delta1_B==Collect$Delta_B, Collect$Q_P_B+Collect$Import_A*Collect$exPeakB, Collect$Q_P_B)
  
Collect$W_c_A <- ifelse(Collect$Intermed_A == 1 & Collect$Import_A >0, Collect$D_A*(Parameters$lambda_A-Collect$P_A), Collect$W_c_A)
Collect$W_p_A <- ifelse(Collect$Intermed_A == 1 & Collect$Import_A >0, (Collect$P_A-Cost$c_r[1])*Collect$Q_R_A+(Collect$P_A-Cost$c_b[1])*Collect$Q_B_A+(Collect$P_A-Cost$c_p[1])*Collect$Q_P_A , Collect$W_p_A)
Collect$W_p_B <- ifelse(Collect$Intermed_A == 1 & Collect$Import_A >0, Collect$W_p_B+(Collect$P_A-(Collect$exPeakB*Collect$pv_B))*Collect$Import_A, Collect$W_p_B)
Collect$W_B=Collect$W_c_B+Collect$W_p_B
Collect$W_A=Collect$W_c_A+Collect$W_p_A

  ### Case 2b, Country B is the in-between case, considers importing only if cheap energy is offered
  #The in-between cases for country B
    ##The last two conditions make sure that imported energy never leads to using peak-load energy when originally only base-load was used
Collect$Import_B <- ifelse(Collect$Intermed_B == 1 & Collect$pv_A < Collect$P_B & Collect$Q_P_B > 0, Collect$V_A, Collect$Import_B)
Collect$Import_B <- ifelse(Collect$Intermed_B == 1 & Collect$Import_B > Parameters$Q_K, Parameters$Q_K, Collect$Import_B)
Collect$Import_B <- ifelse(Collect$Intermed_B == 1 & Collect$Import_B > Collect$Q_P_B, Collect$Q_P_B, Collect$Import_B)

Collect$Q_P_B <- ifelse(Collect$Intermed_B == 1 & Collect$Q_R_B+Collect$Q_B_B+Collect$Import_B > Collect$D_B, 0, Collect$Q_P_B - Collect$Import_B)

Collect$P_B <- ifelse(Collect$Intermed_B == 1 & Collect$Import_B > 0 & Collect$Q_R_B+Collect$Q_B_B+Collect$Import_B > Collect$D_B, Collect$pv_A, Collect$P_B)
Collect$P_B <- ifelse(Collect$Intermed_B == 1 & Collect$Import_B > 0 & Collect$Q_R_B+Collect$Q_B_B+Collect$Import_B > Collect$D_B & Collect$pv_A < Cost$c_b[2] & Collect$Q_B_B > 0, Cost$c_b[2], Collect$P_B)

Collect$Q_P_A <- ifelse(Collect$Intermed_A==1 & ( Collect$costcheckB_A > Collect$costcheckP_A) & Collect$Delta1_A==Collect$Delta_A, Collect$Q_P_A+Collect$Import_B*Collect$exPeakA, Collect$Q_P_A)

Collect$W_c_B <- ifelse(Collect$Intermed_B == 1 & Collect$Import_B >0, Collect$D_B*(Parameters$lambda_B-Collect$P_B), Collect$W_c_B)
Collect$W_p_B <- ifelse(Collect$Intermed_B == 1 & Collect$Import_B >0, (Collect$P_B-Cost$c_r[2])*Collect$Q_R_B+(Collect$P_B-Cost$c_b[2])*Collect$Q_B_B+(Collect$P_B-Cost$c_p[2])*Collect$Q_P_B , Collect$W_p_B)
Collect$W_p_A <- ifelse(Collect$Intermed_B == 1 & Collect$Import_B >0, Collect$W_p_A+(Collect$P_B-(Collect$exPeakA*Collect$pv_A))*Collect$Import_B, Collect$W_p_A)
Collect$W_A=Collect$W_c_A+Collect$W_p_A
Collect$W_B=Collect$W_c_B+Collect$W_p_B
Collect$Q_tot_A <- Collect$Q_R_A + Collect$Q_B_A + Collect$Q_P_A + Collect$Import_A
Collect$Q_tot_B <- Collect$Q_R_B + Collect$Q_B_B + Collect$Q_P_B + Collect$Import_B
###############################
#The output here corresponds to tables 2 - 4
summary(Collect)
CrossTable(Collect$P_A, Collect$P_B)
