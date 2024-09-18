
#install.packages('lhs')
library('lhs')

set.seed(111)

####IDU Network Transmission Parameters
#E -
#  Injections Per Month
#  0.05
#  Epi; Min = 0; Max = 1

####Heterosexual Network Transmission Parameters
#E -
#  Partners Per Person Per Month
#  1.1
#  Epi; Min=0.8; Max = 1.5

#E/G -
#  Assortativity
#  0.2000
#  Epi+Genetic; Min = 0; Max = 1

#E -
#  hiv_status
#  0.7000
#  Epi; Min = 0; Max = 1

####MSM Network Transmission Parameters
#E -
#  Partners Per Person Per Month
#  1.3
#  Epi; Min=0.8; Max = 1.5

#E/G -
#  Assortativity
#  0.2000
#  Epi+Genetic; Min = 0; Max = 1

#E -
#  hiv_status
#  0.7000
#  Epi; Min = 0; Max = 1

#E -
#  HIV Monthly Transmission Probability Between Partners Multiplier
#  1.0000
#  Epi; Min = 1; Max =2


####MSMW Network Transmission Parameters
#E/G -
#  Percentage of MSM who are MSMW
#  0
#  Epi+Genetic; Min = 0; Max = 1

###############################
###############################

# Epi only
#
# -Generate 10 random starting values for O
# -Generate 10 random starting values for the Y
# -Create inputs by crossing the 10 O and 10 Y
#
# -Fix value of O, we select the best Y
# -Run each parameter set 20 times
# -Marginalize over the results
#
# Epi + Genetic
#
# -Generate inputs by pairing 10 O and 10 Y

###############################
###############################

A_O <- randomLHS(10, 3)
#A_O[,3] <- qunif(A_O[,3], min = 0.0, max = 0.2)
B_O <- A_O

A_Y <- randomLHS(10, 6)
B_Y <- A_Y
B_Y[,2] <- qunif(A_Y[,2], min = 0.8, max = 1.5)
B_Y[,4] <- qunif(A_Y[,4], min = 0.8, max = 1.5)
B_Y[,6] <- qunif(A_Y[,6], min = 1, max = 2)



###############################
###############################

B_O
B_Y


