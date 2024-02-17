# This R code is provided by Imma, and shared by Eleanor, this is the R codes that make Vcmax and Jmax (from Aci curve) for Ghana
## Load relevant libraries
library(chron)
library(stats)
library(grDevices)
library(svMisc)
library(svMisc)
library(stringr)
# change the path for an apropriate one - where the talbe with results and the pdf file with graphs will be saved
setwd("C:\\Users\\PC-\\Desktop\\PHOTOSYNTHESIS DATA\\Brazil and Ghana working\\experiment")



# Creating a table that will receive the results of the model fitting.


output.names     <- matrix (c("File_Name","Mean.Tleaf","Max.Tleaf","Asat", "Amax","Amin", 
                              "gs.at.Asat","gs.index","Mean.gs","Min.gs","Max.gs","gs.slope.open","gs.Slope.close","Min.PAR", 'Ciatasat',
                              "Ci.min", "Ci.max","CO2R.min", "CO2R.max","mean.RH","Start.time"," Curve.Duration","Flow", "mean.Press", "Match","Max.VPD",
                              "Vcmax_ACi", "Jmax_ACi", "TPU_ACi", "Rd_ACi", 
                              "gm_MEAD_1","Vcmax_ACC_MEAD_1", "Jmax_ACC_MEAD_1", "TPU_ACC_MEAD_1", "Rd_ACC_MEAD_1", 
                              "gm_MEAD_2","Vcmax_ACC_MEAD_2", "Jmax_ACC_MEAD_2", "TPU_ACC_MEAD_2", "Rd_ACC_MEAD_2",
                              "gm_BFGS_3","Vcmax_ACC_BFGS_3", "Jmax_ACC_BFGS_3", "TPU_ACC_BFGS_3", "Rd_ACC_BFGS_3",
                              "gm_BFGS_4","Vcmax_ACC_BFGS_4", "Jmax_ACC_BFGS_4", "TPU_ACC_BFGS_4", "Rd_ACC_BFGS_4", 
                              "Vcmax_ACi_25C", "Jmax_ACi_25C", "TPU_ACi_25C", "Rd_ACi_25C",
                              "gm_MEAD_1_25C","Vcmax_ACC_MEAD_1_25C", "Jmax_ACC_MEAD_1_25C", "TPU_ACC_MEAD_1_25C", "Rd_ACC_MEAD_1_25C",
                              "gm_MEAD_2_25C","Vcmax_ACC_MEAD_2_25C", "Jmax_ACC_MEAD_2_25C", "TPU_ACC_MEAD_2_25C", "Rd_ACC_MEAD_2_25C",
                              "gm_BFGS_3_25C","Vcmax_ACC_BFGS_3_25C", "Jmax_ACC_BFGS_3_25C", "TPU_ACC_BFGS_3_25C", "Rd_ACC_BFGS_3_25C",
                              "gm_BFGS_4_25C","Vcmax_ACC_BFGS_4_25C", "Jmax_ACC_BFGS_4_25C", "TPU_ACC_BFGS_4_25C", "Rd_ACC_BFGS_4_25C"),nrow=1)


colnames (output.names)	<- output.names

# give a name for the table to be generated
# use the same name at the end of the script (line 391)
arquivo <-"Results_Baca_mess.txt"

write.table (output.names, arquivo, append=TRUE, sep="\t", row.names=FALSE, col.names=FALSE)

###things that don't cahnge
O2  		<- 21 		                       # Estimated Oxigen concentration at chloroplast - (kPa)
R       <- 0.008314472                   # Gas constant
c_Kc   	    	<- 35.9774				  			 # Sharkey et al, 2007
delta_c_Kc 		<- 80.99								   # Sharkey et al, 2007
c_Ko   	    	<- 12.3772					   			        # Sharkey et al, 2007
delta_c_Ko 		<- 23.72								            # Sharkey et al, 2007
c_gstar  	    <- 11.187								                # Sharkey et al, 2007
delta_c_gstar <- 24.46								          # Sharkey et al, 2007		
curv <- 0.7
Ambient.CO2 <- 380
K0 <-  0.388
Kc   		    <- 40.4		         # Michaelis-Menten constant for CO2 (Pa)
delta_Kc 		<- 59.36*1000 	# (J mol-1)
Ko   		    <- 24.8		         # Michaelis-Menten constant for CO2 (kPa)
delta_Ko 		<- 35.94*1000	 # (J mol-1)
gstar  		<- 3.7		       # CO2 compensation point (Pa)
delta_gstar 	<- 23.4*1000	# (J mol-1)
Cseq <- seq(0,200,0.5)
## The model with internal conductance term - A-Cc.
ffitt   <- function(x){
  gm    <- x[1]  	# variable 1
  Vcmax <- x[2]	  # variable 2
  Jmax  <- x[3]	  # variable 3
  TPU   <- x[4]		# variable 4
  Rd    <- x[5]		# variable 5
  Cc <- Cipa - (NPhoto / gm) 
  J <- (Ie + Jmax - sqrt((Ie + Jmax)^2 - 4 * curv * Ie * Jmax))/(2*curv)
  aj <-    J * (Cc - gstar_Tleaf) / (4* Cc + 8 * gstar_Tleaf)-Rd 	                                       
  av <-    Vcmax * (Cc - gstar_Tleaf) / (Cc + Km)-Rd       		
  atpu <- ifelse (Cipa == max(Cipa),  3 * TPU - Rd, NPhoto+0.00001)			
  a <- pmin(av,aj)
  adiff <- sum((NPhoto - a)^2)								 
  bdiff <- sum((NPhoto[Cipa == max(Cipa)] - atpu[Cipa == max(Cipa)])^2)
  diffsum <- adiff + bdiff + sqrt(gm^2)
}

ffitti  <- function(x){
  Vcmax <- x[1]    # variable 1
  Jmax  <- x[2]	  # variable 2
  TPU   <- x[3]		# variable 3
  Rd    <- x[4]		# variable 4
  
  J <- (Ie + Jmax - sqrt((Ie + Jmax)^2 - 4 * curv * Ie * Jmax))/(2*curv)
  aj <- J * (Cipa - gstari_Tleaf) / (4*Cipa + 8 * gstari_Tleaf)-Rd # electron transport rate
  av <- Vcmax * (Cipa - gstari_Tleaf) / (Kmi + Cipa)-Rd				  # carboxylation rate
  atpu <- ifelse (Cipa == max(Cipa),3 * TPU -Rd , NPhoto+0.00001)	# triose phosphate utilization
  
  
  a <- pmin(av,aj)
  adiff <- sum((NPhoto - a)^2)				 # least-square
  bdiff <- sum((NPhoto[Cipa == max(Cipa)] - atpu[Cipa == max(Cipa)])^2)
  diffsum <- adiff + bdiff
}

#change directory for the one that contains the licor files
curvas1<-dir("C:\\Users\\PC-\\Desktop\\PHOTOSYNTHESIS DATA\\Brazil and Ghana working\\Brazil and Ghana ACi curves with deleted text", full.names=TRUE)



pdf(file="Bra_Mess_fits1.pdf",width=10,height=7)

for (i in curvas1) {
  
  
  Curve <- na.omit(read.table(nome <- i, header = T,fill=T)) #,sep=",")           
  
 
 Tleafk  <- 273.15 + Curve$Tleaf          # Leaf temp in Kelvins
Kc_Tleaf	  	<- exp(c_Kc-delta_c_Kc/(R*Tleafk))	# Sharkey et al, 2007
Ko_Tleaf	    <- exp(c_Ko-delta_c_Ko/(R*Tleafk))  # Sharkey et al, 2007
gstar_Tleaf		<- exp(c_gstar-delta_c_gstar/(R*Tleafk))	# Sharkey et al, 2007
Km			<- Kc_Tleaf*(1+O2/Ko_Tleaf)				            	# Sharkey et al, 2007
  
# converts Ci from ppm to Pa
Cipa <- Curve$Ci * Curve$Press * 0.001
  # leaf absorptance (abs = 0.85, f = 0.15 ===> 0.361 = abs * (1-f/2), f is the correction for spectral quality of the light and curvature factor for Jmax
  Ie <- Curve$PARi * 0.361
  K2 <- K0 * ((Curve$Tleaf+273)/273)^(1.8*Curve$Press/100)
  NPhoto<-Curve$Photo+(K2/(100*Curve$Area)*(Ambient.CO2-Curve$CO2S))
  Kci_Tleaf		<- Kc * exp((Curve$Tleaf+273.15-298.15)*delta_Kc/(298.15*8.314*(Curve$Tleaf+273.15)))
  Koi_Tleaf		<- Ko * exp((Curve$Tleaf+273.15-298.15)*delta_Ko/(298.15*8.314*(Curve$Tleaf+273.15)))
  gstari_Tleaf		<- gstar * exp((Curve$Tleaf+273.15-298.15)*delta_gstar/(298.15*8.314*(Curve$Tleaf+273.15)))
  Kmi			<- Kci_Tleaf*(1+O2/Koi_Tleaf)
  
  
  
   ####  Alternative starting values
  start.values <- c(max(Curve$Photo) * 1.7 , max(Curve$Photo) * 2.9, max(Curve$Photo) * 0.25, max(Curve$Photo) * 0.02)
  first <- optim(start.values,ffitti, method = "BFGS",control = list(maxit=2000))
  start.values1 <- c(.5,first$par)	
  second <- optim(start.values1,ffitt, method = "BFGS",control = list(maxit=2000))
  start.values2 <- c(1,first$par)   
  third  <- optim(start.values2,ffitt, method = "BFGS",control = list(maxit=2000))
  
  Cc2 <- ifelse(NPhoto>0,(Cipa - (NPhoto / second$par[1]) + sqrt((Cipa - (NPhoto / second$par[1]))^2))/2	+0.0001,Cipa)
  Cc3 <- ifelse(NPhoto>0,(Cipa - (NPhoto / third$par[1]) + sqrt((Cipa - (NPhoto /  third$par[1]))^2))/2	+0.0001,Cipa)	
  
  av1 <- first$par[1] * (Cseq - mean(gstari_Tleaf))/(mean(Kmi) + Cseq ) - first$par[4] # creates values of av based on calculated parameters (Vcmax and Rd)
  J1 <- mean((Ie + first$par[2] - sqrt((Ie + first$par[2])^2 - 4 * curv * Ie * first$par[2]))/(2*curv) )
  aj1 <- J1/4 * (Cseq - mean(gstari_Tleaf))/(Cseq + 2*mean(gstari_Tleaf)) - first$par[4] # creates values of af based on calculated parameters (Jmax and Rd)
  atpu1 <- first$par[3] * 3 - first$par[4] + (Cseq /10^10) # creates values of atpu based on calculated parameters (TPU and Rd)
  amin1 <- pmin(av1,aj1,atpu1)
  
  av1b <- first$par[1] * (Cipa - mean(gstar_Tleaf))/(mean(Km) + Cipa ) - first$par[4]
  aj1b <- J1 / 4 * (Cipa - mean(gstar_Tleaf))/(Cipa + 2*mean(gstar_Tleaf)) - first$par[4]
  atpu1b <- first$par[3] * 3 - first$par[4]+ (Cipa /10^10)
  amin1b <- pmin(av1b,aj1b,atpu1b)
  
  
  av2 <- second$par[2] * (Cseq - mean(gstar_Tleaf))/(mean(Km) + Cseq ) - second$par[5]
  J2 <- mean((Ie + second$par[3] - sqrt((Ie + second$par[3])^2 - 4 * curv * Ie * second$par[3]))/(2*curv))
  aj2 <- J2 / 4 * (Cseq - mean(gstar_Tleaf))/(Cseq + 2*mean(gstar_Tleaf)) - second$par[5]
  atpu2 <- second$par[4] * 3 - second$par[5]+ (Cseq /10^10)
  amin2 <- pmin(av2,aj2,atpu2)
  
  av2b <- second$par[2] * (Cc2 - mean(gstar_Tleaf))/(mean(Km) + Cc2 ) - second$par[5]
  aj2b <- J2 / 4 * (Cc2 - mean(gstar_Tleaf))/(Cc2 + 2*mean(gstar_Tleaf)) - second$par[5]
  atpu2b <- second$par[4] * 3 - second$par[5]+ (Cc2 /10^10)
  amin2b <- pmin(av2b,aj2b,atpu2b)
  
  
  av3 <- third$par[2] * (Cseq - mean(gstar_Tleaf))/(mean(Km) + Cseq ) - third$par[5]
  J3 <- mean((Ie + third$par[3] - sqrt((Ie + third$par[3])^2 - 4 * curv * Ie * third$par[3]))/(2*curv) )
  aj3 <- J3 / 4 * (Cseq - mean(gstar_Tleaf))/(Cseq + 2*mean(gstar_Tleaf)) - third$par[5]
  atpu3 <- third $par[4] * 3 - third $par[5]+ (Cseq /10^10)
  amin3 <- pmin(av3,aj3,atpu3)
  
  av3b <- third$par[2] * (Cc3 - mean(gstar_Tleaf))/(mean(Km) + Cc3 ) - third$par[5]
  aj3b <- J3 / 4 * (Cc3 - mean(gstar_Tleaf))/(Cc3 + 2*mean(gstar_Tleaf)) - third$par[5]
  atpu3b <- third$par[4] * 3 - third$par[5]+ (Cc3 /10^10)
  amin3b <- pmin(av3b,aj3b,atpu3b)
  
  Limitationa<- (J2 / 4 * (200 - mean(gstar_Tleaf))/(200 + 2*mean(gstar_Tleaf)) - second$par[5])-(second$par[4] * 3 - second$par[5]+ (200 /10^10))
  Limitationb<- (J3 / 4 * (200 - mean(gstar_Tleaf))/(200 + 2*mean(gstar_Tleaf)) -  third$par[5])-( third$par[4] * 3 -  third$par[5]+ (200 /10^10))
  Transa<-(8*mean(gstar_Tleaf)-mean(Km)*second$par[3]/second$par[2])/(second$par[3]/second$par[2]-4)
  Transb<-(8*mean(gstar_Tleaf)-mean(Km)* third$par[3]/ third$par[2])/( third$par[3]/ third$par[2]-4)
  
  
  #Fitting the curves using the "Nelder-Mead" algorithm for the optim function, just for double-checking
  
  fourth <- optim(start.values1,ffitt, method = "Nelder-Mead",control = list(maxit=2000))
  fifth  <- optim(start.values2,ffitt, method = "Nelder-Mead",control = list(maxit=2000))
  
  Cc4 <- ifelse(NPhoto>0,(Cipa - (NPhoto / fourth$par[1]) + sqrt((Cipa - (NPhoto / fourth$par[1]))^2))/2  +0.0001,Cipa)
  Cc5 <- ifelse(NPhoto>0,(Cipa - (NPhoto /  fifth$par[1]) + sqrt((Cipa - (NPhoto /  fifth$par[1]))^2))/2  +0.0001,Cipa)	
  
  av4 <- fourth$par[2] * (Cseq - mean(gstar_Tleaf))/(mean(Km) + Cseq ) - fourth$par[5]
  J4 <- mean((Ie + fourth$par[3] - sqrt((Ie + fourth$par[3])^2 - 4 * curv * Ie * fourth$par[3]))/(2*curv))
  aJ4 <- J4 / 4 * (Cseq - mean(gstar_Tleaf))/(Cseq + 2*mean(gstar_Tleaf)) - fourth$par[5]
  atpu4 <- fourth$par[4] * 3 - fourth$par[5]+ (Cseq /10^10)
  amin4 <- pmin(av4,aJ4,atpu4)
  
  av4b <- fourth$par[2] * (Cc4 - mean(gstar_Tleaf))/(mean(Km) + Cc4 ) - fourth$par[5]
  aJ4b <- J4 / 4 * (Cc4 - mean(gstar_Tleaf))/(Cc4 + 2*mean(gstar_Tleaf)) - fourth$par[5]
  atpu4b <- fourth$par[4] * 3 - fourth$par[5]+ (Cc4 /10^10)
  amin4b <- pmin(av4b,aJ4b,atpu4b)
  
  
  av5 <- fifth$par[2] * (Cseq - mean(gstar_Tleaf))/(mean(Km) + Cseq ) - fifth$par[5]
  J5 <- mean((Ie + fifth$par[3] - sqrt((Ie + fifth$par[3])^2 - 4 * curv * Ie * fifth$par[3]))/(2*curv) )
  aJ5 <- J5 / 4 * (Cseq - mean(gstar_Tleaf))/(Cseq + 2*mean(gstar_Tleaf)) - fifth$par[5]
  atpu5 <- fifth $par[4] * 3 - fifth $par[5]+ (Cseq /10^10)
  amin5 <- pmin(av5,aJ5,atpu5)
  
  av5b <- fifth$par[2] * (Cc5 - mean(gstar_Tleaf))/(mean(Km) + Cc5 ) - fifth$par[5]
  aJ5b <- J5 / 4 * (Cc5 - mean(gstar_Tleaf))/(Cc5 + 2*mean(gstar_Tleaf)) - fifth$par[5]
  atpu5b <- fifth$par[4] * 3 - fifth$par[5]+ (Cc5 /10^10)
  amin5b <- pmin(av5b,aJ5b,atpu5b)
  
 ################### making the figures
  
  inter <- (max(Curve$Photo)-min(Curve$Photo))/9
  f1 <-(min(Curve$Photo))
  f2 <-(min(Curve$Photo)+ inter)
  f3 <-(min(Curve$Photo)+ 2 * inter)
  f4 <-(min(Curve$Photo)+ 3 * inter)
  f5 <-(min(Curve$Photo)+ 4 * inter)
  f6 <-(min(Curve$Photo)+ 5 * inter)
  f7 <-(min(Curve$Photo)+ 6 * inter)
  f8 <-(min(Curve$Photo)+ 7 * inter)
  
  par(mfrow=c(2,3))
  plot (Cipa,NPhoto,col ="blue",pch=19,xlim=c(0,200), cex = 1.5, ylim=c(min(Curve$Photo)-0.5,max(Curve$Photo)+max(Curve$Photo)/8), ylab="A", xlab="Ci, Pa")
  title(cex.main = 1,main = c(basename(i), "A-Ci curve - BFGS", paste("Start = ",as.character(start.values)[1],as.character(start.values)[2],as.character(start.values)[3],as.character(start.values)[4],sep=",")))
  lines (Cseq[av1<45], av1[av1<45],   col = "red",   lwd = "4")
  lines (Cseq, aj1,   col = "orange", lwd = "4")
  lines (Cseq, atpu1, col = "plum", lwd = "4")
  lines (c(27,27),c(-5,45), col = "black", lty = 3)
  lines (c(45,45),c(-5,45), col = "black", lty = 3)
  lines (Cseq, amin1,   col = "black", 	lwd = "4")
  lines (Cseq, amin1,   col = "white", 	lwd = "1")
  text (190,f5,paste("Vcmax =",round(first$par[1],digits=1)),pos=2,col = "red")
  text (190,f4,paste("Jmax =" ,round(first$par[2],digits=1)),pos=2,col = "orange")
  text (190,f3,paste("TPU ="  ,round(first$par[3],digits=1)),pos=2,col = "purple")
  text (190,f2,paste("Rd ="   ,round(first$par[4],digits=2)),pos=2)
  text (190,f1,paste("error =",round(first$value[1],digits=2)),pos=2)
  text ( 50,f2,paste("gs ="   ,round(mean(Curve$Cond),digits=2)),pos=4)
  text ( 50,f1,paste("Tleaf =",round(mean(Curve$Tleaf),digits=1)),pos=4)
  points(Cipa[av1b==amin1b],NPhoto[av1b==amin1b],col ="red",pch=19, cex = 2)
  points(Cipa[aj1b==amin1b],NPhoto[aj1b==amin1b],col ="orange",pch=19, cex = 2)
  points(Cipa[atpu1b==amin1b],NPhoto[atpu1b==amin1b],col ="purple",pch=19, cex = 2)
  points(Cipa[Cipa > 27 & Cipa < 45],NPhoto[Cipa > 27 & Cipa < 45],col ="grey",pch=19, cex = 2.5)
  
  
  plot (Cc4,NPhoto,col ="blue",pch=19, cex = 1.5,xlim=c(0,200), ylim=c(min(Curve$Photo)-0.5,max(Curve$Photo)+max(Curve$Photo)/8), ylab="A", xlab="Cc, Pa")
  title(cex.main = 1,main = c("A-Cc curve", "Low start gm - Neader-Mead"))
  lines (Cseq[av4<45], av4[av4<45],   col = "red",   lwd = "4")
  lines (Cseq, aJ4,   col = "orange", lwd = "4")
  lines (Cseq, atpu4, col = "plum", lwd = "4")
  lines (Cseq, amin4,   col = "black",   lwd = "4")
  lines (Cseq, amin4,   col = "white", 	lwd = "1")
  text (190,f5,paste("Vcmax =",round(fourth$par[2],  digits=1)),pos=2,col = "red")
  text (190,f4,paste("Jmax =" ,round(fourth$par[3],  digits=1)),pos=2,col = "orange")
  text (190,f3,paste("TPU ="  ,round(fourth$par[4],  digits=1)),pos=2,col = "purple")
  text (190,f2,paste("Rd ="   ,round(fourth$par[5],  digits=2)),pos=2)
  text (190,f1,paste("error =",round(fourth$value[1],digits=2)),pos=2)
  text (190,f6,paste("gm ="   ,round(fourth$par[1],  digits=2)),pos=2)
  points(Cc4[av4b==amin4b],NPhoto[av4b==amin4b],col ="red",pch=19, cex = 2)
  points(Cc4[aJ4b==amin4b],NPhoto[aJ4b==amin4b],col ="orange",pch=19, cex = 2)
  points(Cc4[atpu4b==amin4b],NPhoto[atpu4b==amin4b],col ="purple",pch=19, cex = 2)
  
  
  plot (Cc5,NPhoto,col ="blue",pch=19, cex = 1.5,xlim=c(0,200), ylim=c(min(Curve$Photo)-0.5,max(Curve$Photo)+max(Curve$Photo)/8), ylab="A", xlab="Cc, Pa")
  title(cex.main = 1,main =  c("A-Cc curve", "High start gm - Neader-Mead"))
  lines (Cseq[av5<45], av5[av5<45],   col = "red", 	lwd = "4")
  lines (Cseq, aJ5,   col = "orange", lwd = "4")
  lines (Cseq, atpu5, col = "plum", lwd = "4")
  lines (Cseq, amin5,   col = "black", 	lwd = "4")
  lines (Cseq, amin5,   col = "white", 	lwd = "1")
  text (190,f5,paste("Vcmax =",round(fifth$par[2],  digits=1)),pos=2,col = "red")
  text (190,f4,paste("Jmax =" ,round(fifth$par[3],  digits=1)),pos=2,col = "orange")
  text (190,f3,paste("TPU ="  ,round(fifth$par[4],  digits=1)),pos=2,col = "purple")
  text (190,f2,paste("Rd ="   ,round(fifth$par[5],  digits=2)),pos=2)
  text (190,f1,paste("error =",round(fifth$value[1],digits=2)),pos=2)
  text (190,f6,paste("gm ="   ,round(fifth$par[1],  digits=2)),pos=2)
  points(Cc5[av5b==amin5b],NPhoto[av5b==amin5b],col ="red",pch=19, cex = 2)
  points(Cc5[aJ5b==amin5b],NPhoto[aJ5b==amin5b],col ="orange",pch=19, cex = 2)
  points(Cc5[atpu5b==amin5b],NPhoto[atpu5b==amin5b],col ="purple",pch=19, cex = 2)
  
  
  plot (Curve$FTime,Curve$Cond,col ="gray",pch=19, cex = 1.5, ylab="Cond", xlab="Time, s", type = "b",ylim=c(0,.75))
  points(Curve$FTime[Curve$CO2R<350],Curve$Cond[Curve$CO2R<350],col ="red",pch=19, cex = 2)  
  points(Curve$FTime[Curve$CO2R>450],Curve$Cond[Curve$CO2R>450],col ="blue",pch=19, cex = 2) 
  if (length(Curve$CO2R [Curve$CO2R < 350])>2) abline(try(lm(Curve$Cond[Curve$CO2R < 350]~Curve$FTime[Curve$CO2R < 350])),col="red")
  if (length(Curve$CO2R [Curve$CO2R > 450])>2) abline(try(lm(Curve$Cond[Curve$CO2R > 450]~Curve$FTime[Curve$CO2R > 450])),col="blue")
  abline(0.05,0,lty=3)
  
  plot (Cc2,NPhoto,col ="blue",pch=19, cex = 1.5,xlim=c(0,200), ylim=c(min(Curve$Photo)-0.5,max(Curve$Photo)+max(Curve$Photo)/8), ylab="A", xlab="Cc, Pa")
  title(cex.main = 1,main = c("A-Cc curve", "Low start gm - BFGS"))
  lines (Cseq[av2<45], av2[av2<45],   col = "red",   lwd = "4")
  lines (Cseq, aj2,   col = "orange", lwd = "4")
  lines (Cseq, atpu2, col = "plum", lwd = "4")
  lines (Cseq, amin2,   col = "black", 	lwd = "4")
  lines (Cseq, amin2,   col = "white", 	lwd = "1")
  text (190,f5,paste("Vcmax =",round(second$par[2],  digits=1)),pos=2,col = "red")
  text (190,f4,paste("Jmax =" ,round(second$par[3],  digits=1)),pos=2,col = "orange")
  text (190,f3,paste("TPU ="  ,round(second$par[4],  digits=1)),pos=2,col = "purple")
  text (190,f2,paste("Rd ="   ,round(second$par[5],  digits=2)),pos=2)
  text (190,f1,paste("error =",round(second$value[1],digits=2)),pos=2)
  text (190,f6,paste("gm ="   ,round(second$par[1],  digits=2)),pos=2)
  points(Cc2[av2b==amin2b],NPhoto[av2b==amin2b],col ="red",pch=19, cex = 2)
  points(Cc2[aj2b==amin2b],NPhoto[aj2b==amin2b],col ="orange",pch=19, cex = 2)
  points(Cc2[atpu2b==amin2b],NPhoto[atpu2b==amin2b],col ="purple",pch=19, cex = 2)
  
  
  plot (Cc3,NPhoto,col ="blue",pch=19, cex = 1.5,xlim=c(0,200), ylim=c(min(Curve$Photo)-0.5,max(Curve$Photo)+max(Curve$Photo)/8), ylab="A", xlab="Cc, Pa")
  title(cex.main = 1,main =  c("A-Cc curve", "High start gm - BFGS"))
  lines (Cseq[av3<45], av3[av3<45],   col = "red", 	lwd = "4")
  lines (Cseq, aj3,   col = "orange", lwd = "4")
  lines (Cseq, atpu3, col = "plum", lwd = "4")
  lines (Cseq, amin3,   col = "black", 	lwd = "4")
  lines (Cseq, amin3,   col = "white", 	lwd = "1")
  text (190,f5,paste("Vcmax =",round(third$par[2],  digits=1)),pos=2,col = "red")
  text (190,f4,paste("Jmax =" ,round(third$par[3],  digits=1)),pos=2,col = "orange")
  text (190,f3,paste("TPU ="  ,round(third$par[4],  digits=1)),pos=2,col = "purple")
  text (190,f2,paste("Rd ="   ,round(third$par[5],  digits=2)),pos=2)
  text (190,f1,paste("error =",round(third$value[1],digits=2)),pos=2)
  text (190,f6,paste("gm ="   ,round(third$par[1],  digits=2)),pos=2)
  points(Cc3[av3b==amin3b],NPhoto[av3b==amin3b],col ="red",pch=19, cex = 2)
  points(Cc3[aj3b==amin3b],NPhoto[aj3b==amin3b],col ="orange",pch=19, cex = 2)
  points(Cc3[atpu3b==amin3b],NPhoto[atpu3b==amin3b],col ="purple",pch=19, cex = 2)
  
    
  HHMMSS2<-times(Curve$HHMMSS)
  
  gsindex<-(max(Curve$Cond)-min(Curve$Cond))/max(Curve$Cond)
  
  match<-paste(length(unique(as.factor(Curve$CsMch))),"of",length(Curve$CsMch))
  
  # Temperature conversion coefficients taken from Sharkey et al, 2007
  Vcmax_ACI_25C <- first$par[1]/(exp(26.355-(65.33/(R*mean(Tleafk)))))
  Jmax_ACI_25C  <- first$par[2]/(exp(17.71 -(43.9 /(R*mean(Tleafk)))))
  TPU_ACI_25C   <- first$par[3]/((exp(21.46-53.1/(R*mean(Tleafk))))/(1+exp((0.65*mean(Tleafk)-201.8)/(R*mean(Tleafk)))))
  Rd_ACI_25C    <- first$par[4]/(exp(18.715-(46.39/(R*mean(Tleafk)))))
  
  gm_ACC_25C_3    <- second$par[1]/((exp(20.01-49.6/(R*mean(Tleafk))))/(1+exp(( 1.4*mean(Tleafk)-437.4)/(R*mean(Tleafk)))))
  Vcmax_ACC_25C_3 <- second$par[2]/(exp(26.355-(65.33/(R*mean(Tleafk)))))
  Jmax_ACC_25C_3  <- second$par[3]/(exp(17.71 -(43.9 /(R*mean(Tleafk)))))
  TPU_ACC_25C_3   <- second$par[4]/((exp(21.46-53.1/(R*mean(Tleafk))))/(1+exp((0.65*mean(Tleafk)-201.8)/(R*mean(Tleafk)))))
  Rd_ACC_25C_3    <- second$par[5]/(exp(18.715-(46.39/(R*mean(Tleafk)))))
  
  gm_ACC_25C_1    <- fourth$par[1]/((exp(20.01-49.6/(R*mean(Tleafk))))/(1+exp(( 1.4*mean(Tleafk)-437.4)/(R*mean(Tleafk)))))
  Vcmax_ACC_25C_1 <- fourth$par[2]/(exp(26.355-(65.33/(R*mean(Tleafk)))))
  Jmax_ACC_25C_1  <- fourth$par[3]/(exp(17.71 -(43.9 /(R*mean(Tleafk)))))
  TPU_ACC_25C_1   <- fourth$par[4]/((exp(21.46-53.1/(R*mean(Tleafk))))/(1+exp((0.65*mean(Tleafk)-201.8)/(R*mean(Tleafk)))))
  Rd_ACC_25C_1    <- fourth$par[5]/(exp(18.715-(46.39/(R*mean(Tleafk)))))
  
  gm_ACC_25C_4    <- third$par[1]/((exp(20.01-49.6/(R*mean(Tleafk))))/(1+exp(( 1.4*mean(Tleafk)-437.4)/(R*mean(Tleafk)))))
  Vcmax_ACC_25C_4 <- third$par[2]/(exp(26.355-(65.33/(R*mean(Tleafk)))))
  Jmax_ACC_25C_4  <- third$par[3]/(exp(17.71 -(43.9 /(R*mean(Tleafk)))))
  TPU_ACC_25C_4   <- third$par[4]/((exp(21.46-53.1/(R*mean(Tleafk))))/(1+exp((0.65*mean(Tleafk)-201.8)/(R*mean(Tleafk)))))
  Rd_ACC_25C_4    <- third$par[5]/(exp(18.715-(46.39/(R*mean(Tleafk)))))
  
  gm_ACC_25C_2    <- fifth$par[1]/((exp(20.01-49.6/(R*mean(Tleafk))))/(1+exp(( 1.4*mean(Tleafk)-437.4)/(R*mean(Tleafk)))))
  Vcmax_ACC_25C_2 <- fifth$par[2]/(exp(26.355-(65.33/(R*mean(Tleafk)))))
  Jmax_ACC_25C_2  <- fifth$par[3]/(exp(17.71 -(43.9 /(R*mean(Tleafk)))))
  TPU_ACC_25C_2   <- fifth$par[4]/((exp(21.46-53.1/(R*mean(Tleafk))))/(1+exp((0.65*mean(Tleafk)-201.8)/(R*mean(Tleafk)))))
  Rd_ACC_25C_2    <- fifth$par[5]/(exp(18.715-(46.39/(R*mean(Tleafk)))))
  



  output <- matrix (c (basename(i),mean(Curve$Tleaf),max(Curve$Tleaf),max(Curve$Photo[Curve$CO2R>370 & Curve$CO2R<430]),
                       Curve$Photo[Curve$CO2R==max(Curve$CO2R)],min(Curve$Photo),
                       max(Curve$Cond[Curve$CO2R>350 & Curve$CO2R<450]),gsindex,mean(Curve$Cond),range(Curve$Cond),
                       try(lm(Curve$Cond[Curve$CO2R < 350]~Curve$FTime[Curve$CO2R < 350])$coeff[2]),
                       try(lm(Curve$Cond[Curve$CO2R > 450]~Curve$FTime[Curve$CO2R > 450])$coeff[2]),min(Curve$PARi),max(Curve$Ci[Curve$CO2R>350 & Curve$CO2R<450]),
                       range(Curve$Ci),range(Curve$CO2S),mean(Curve$RH_S),as.character(HHMMSS2[1]),as.character(max(HHMMSS2)-min(HHMMSS2)),min(Curve$Flow),mean(Curve$Press),match,max(Curve$VpdL),
                       first$par, fourth$par,fifth$par,second$par,third$par, 
                       Vcmax_ACI_25C ,Jmax_ACI_25C, TPU_ACI_25C,  Rd_ACI_25C, 
                       gm_ACC_25C_1, Vcmax_ACC_25C_1, Jmax_ACC_25C_1, TPU_ACC_25C_1, Rd_ACC_25C_1, 
                       gm_ACC_25C_2,Vcmax_ACC_25C_2,Jmax_ACC_25C_2,TPU_ACC_25C_2,Rd_ACC_25C_2,
                       gm_ACC_25C_3, Vcmax_ACC_25C_3, Jmax_ACC_25C_3, TPU_ACC_25C_3, Rd_ACC_25C_3, 
                       gm_ACC_25C_4,Vcmax_ACC_25C_4,Jmax_ACC_25C_4,TPU_ACC_25C_4,Rd_ACC_25C_4), nrow=1)
  # use the same name given at the beggining of the script
  write.table (output, arquivo, append=TRUE, sep="\t", row.names=FALSE, col.names=FALSE)
  
}
dev.off() 

