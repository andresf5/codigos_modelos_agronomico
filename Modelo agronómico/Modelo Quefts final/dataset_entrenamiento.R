
vector <- c()
library(Rquefts)

intancias <- 1:15000

#intancias <- seq(300, 450, 10)

for (i in intancias) {
  
  
  #temps <- sample(seq(18.4, 21.7, 0.1), 1)
  temps <-21
  phs <- sample(seq(4.7, 5.9, 0.1), 1)
  #phs <- 6.7
  #phs <- i
  

  
  socs<- sample(9:35, 1)
  #socs<- 1.7
  #socs<- i
  
  k_exs <- sample(seq(0.1, 1.8, 0.2), 1)
  #k_exs <- 10
  #k_exs <- i
  
  #P_olsens <- sample(seq(0, 26, 0.1), 1)
  
  P_bray <- sample(seq(2, 11.5, 0.1), 1)
  #p_bray <- 1.1
  #p_bray <- i
  
  #P_totals <- sample(100:1200, 1)
  P_totals <- NA
  
  
  #N_trees <- 5000
  N_trees <- sample(seq(2500, 5000, 10), 1)
  
  I <- (130.73 + (0.052647*N_trees)) - (0.000002359*(N_trees^2))
  
  leaf_biom <- (856.714*N_trees)/1000
  
  stem_biom <- (922.61*N_trees)/1000
  
  store_biom <- (I *12.5)
  
  N_Yzero <-  leaf_biom + stem_biom
  
  #init_data <- list(PH=phs, TEMP=temps, SOC= socs, KEX= k_exs, POLSEN= P_olsens, PTOTAL= P_totals);
  

  
  
  init_data <- list(PH=phs, TEMP=temps, SOC= socs, KEX= k_exs, PBRAY= p_bray);
  
  s <- nutSupply1(pH=phs, SOC=socs, Kex=k_exs, Pbray=p_bray, Ptotal=P_totals, NT= N_trees, newYzero =  N_Yzero)
  
  
  supply<- list(N_base_supply=s[1], P_base_supply=s[2], K_base_supply=s[3]) 
  
  soil <- quefts_soil()
  
  soil2 = list(soil[1], soil[2])
  
  crop <- quefts_crop(name="Coffee")
  
  
  Nit <- sample(seq(150, 700, 10), 1)
  Phos <- sample(seq(10, 150, 10), 1)
  kk <- sample(seq(50, 350, 10), 1)
  
  #Nit <- 70
  #Nit <- 150-700
  #Phos <- 22
  #Phos <- i 10-150
  #kk <- 31
  #kk <- i 150-500
  fertilizer <- list(N=Nit, P=Phos, K=kk, No=0, Po=0, Ko=0)
  
  soil$N_base_supply=round(s[1], 1)
  
  soil$P_base_supply=round(s[2], 1)
  soil$K_base_supply=round(s[3], 1)
  soil$NrTrees=s[4]
  soil$New_Yzero=s[5]
  
  
  soil$N_recovery=0.7  #0.4-0.8
  soil$P_recovery=0.17  #0.1-0.3
  soil$K_recovery=0.7  #0.7-0.9
  
  soil$N_recovery_o=0.42
  soil$P_recovery_o=0.087
  soil$K_recovery_o=0.7
  
  #valores para NYratA-- KYratD
  soil$N_ac= 11
  soil$N_d= (soil$N_ac * 3)
  soil$P_ac= 74
  soil$P_d= (soil$P_ac * 3)
  soil$K_ac= 22
  soil$K_d= (soil$K_ac * 3)
  

  
  #biomass
  
  
  biomass <- list(leaf_att= leaf_biom, stem_att= stem_biom, store_att= store_biom, SeasonLength=240)
  
  
  q <- quefts(soil, crop, fertilizer, biomass)
  #str(q)
  output <- run(q)
  output2 <- round(output, 1)
  
  #en la siguiente linea esta la estructura para generar el dataset CON las variables acumuladas y diluidas
  #newrow <-c(init_data, soil[1:7], soil[12:17], crop[0:13], s[5], fertilizer, biomass, output2)
  
  #aqui estan SIN las variables acumuladas y diluidas
  newrow <-c(init_data, soil[1:7], crop[0:13], s[5], fertilizer, biomass, output2)
  vector <- rbind(vector, c(newrow))
  
}


write.csv(vector,"D:/Univesidad/Trabajo de grado/Rquefts/Sensibilidad/Sensibilidad_NutSupply\\real_ds_test.csv", row.names = FALSE)

#write.csv(vector, timestamp()"*.csv", row.names = FALSE)

