#Clear the workspace and identify how many time periods (T) you wish your model to run (once you write the commands, press 'Source')

rm(list=ls(all=TRUE))
T<-83

#STEP 1: For each endogenous variable, create a vector that has a length equal to the time periods. (Once you have written the commands, press 'Source'.)

#Endogenous variables
Y<- vector(length=T)
E<- vector(length=T)
E_F<- vector(length=T)
EMIS_IN<- vector(length=T)
EMIS_L<- vector(length=T)
EMIS<- vector(length=T)
g_EMIS_L<- vector(length=T)
CO2_CUM<- vector(length=T)
T_AT<- vector(length=T)

#STEP 2: Give values to the parameters (use the values reported in the table in Section 3).

#Parameters
for (i in 1:T) {
  if (i == 1) {
    for (iterations in 1:10){
      g_Y<-0.029
      epsilon <- E[i]/Y[i] #EJ/trillion US$
      theta<-0.15
      omega<-EMIS_IN[i]/E_F[i] #Gt/EJ
      seq<- 0.002186
      zeta_9<-0.014
      t_1<- 0.5
      t_2<- 1.1
      phi<-1.72/(3.667*1000)
      
      #STEP 3: Give values to your initial variables (use the values reported in the table in Section 3).
      
      #Initial values
      Y[i] <- 85.9 #trillion US$
      E[i]<-590 #EJ
      E_F[i]<-(1-theta)*E[i] #EJ
      EMIS_IN[i]<-36.6 #Gt
      EMIS_L[i] <-5.5 #Gt
      g_EMIS_L[i]<-0.016
      EMIS[i]<- EMIS_IN[i]+EMIS_L[i] #Gt
      CO2_CUM[i]<-2210 # GtCO2
      T_AT[i] <-1.14 #0C
    }
  }
  
  #STEP 4: Write down the equations and run the model. (Once you have written the commands, press 'Source'.)
  
  #Equations
  else {
    
    for (iterations in 1:10){
      
      Y[i] <- Y[i-1]*(1+g_Y) #Eq. (1)
      E[i] <- epsilon*Y[i] #Eq. (2)
      E_F[i] <- (1-theta)*E[i] #Eq. (3)
      EMIS_IN[i] <- omega*(1-seq)*E_F[i] #Eq. (4)
      EMIS_L[i]<- EMIS_L[i-1]*(1-g_EMIS_L[i]) #Eq. (5)
      g_EMIS_L[i]<-g_EMIS_L[i-1]*(1-zeta_9) #Eq. (6)
      EMIS[i] <- EMIS_IN[i]+EMIS_L[i] #Eq. (7)
      CO2_CUM[i]<-CO2_CUM[i-1]+EMIS[i] #Eq. (8)
      T_AT[i]<-T_AT[i-1]+t_1*(t_2*phi*CO2_CUM[i-1]-T_AT[i-1]) #Eq. (9)
    }
  }
}

#STEP 5: Create a table to report the following variables: Y, E_F, EMIS_IN, EMIS, CO2_CUM, T_AT. Create also 3 graphs for the variables Y, EMIS and T_AT. (Once you have written the commands, press 'Source'.)

#Table
matrixname<-paste("Table")
assign (matrixname, (round(cbind(Y, E_F, EMIS_IN, EMIS, CO2_CUM, T_AT), digits=4)))

plot(Table[1:83,c("Y")], type="l", xlab= "Year", ylab="Output (US$ trillion)", xaxt="n")
axis(side=1, at=c(1, 13, 23, 33, 43, 53, 63, 73, 83) , labels=c("2018", "2030","2040","2050","2060", "2070", "2080", "2090", "2100"))

plot(Table[1:83,c("EMIS")], type="l", xlab= "Year", ylab=expression("CO" [2]*" emissions (Gt)"),xaxt="n")
axis(side=1, at=c(1, 13, 23, 33, 43, 53, 63, 73, 83) , labels=c("2018", "2030","2040","2050","2060", "2070", "2080", "2090", "2100"))

plot(Table[1:83,c("T_AT")], type="l", xlab= "Year", ylab=expression("Temperature ("^{o}*"C above pre-industrial)"), xaxt="n")
axis(side=1, at=c(1, 13, 23, 33, 43, 53, 63, 73, 83) , labels=c("2018", "2030","2040","2050","2060", "2070", "2080", "2090", "2100"))





