year <- c(1938, 1939, 1940, 1941, 1942, 1943, 1944, 1945, 1946, 1947)

medicineS <- c(18,16,7 ,12,24 ,16,22 ,12,22,28)
medicineT <- c(22,23,17 ,25,50,21,32,14,34,37)

artsS <- c(16,13,11,12,8,11,4,4,0,13)
artsT <- c(30,22,25,14,12,20,10,12,0,23)

scienceS <-c(9,9,12,12,20,16,25,32,4,25)
scienceT <-c(14,12,19,12,20,16,31,38,5,31)

engineeringS <-c(10,7,12,8,5,1,16,19,0,25)
engineeringT <-c(16,11,15,9,7,2,22,25,0,35)


artsSw <- c(14,11,15,15,8,13,18,18,1,13)
artsTw <- c(19,16,18,21,9,13,22,22,1,16)


scienceSw <-c(1,4,6,3,4,8,5,16,1,10)
scienceTw <- c(1,4,7,3,4,9,5,17,1,10)


men.df <- data.frame(ArtsS = artsS, ArtsT = artsT, ScienceS = scienceS, ScienceT = scienceT, EngineeringS = engineeringS, EngineeringT = engineeringT)
women.df <- data.frame(ArtsS = artsSw, ArtsT = artsTw, ScienceS = scienceSw, ScienceT = scienceTw)

men.total <- rowSums(men.df)
women.total <- rowSums(women.df)


medicineS_proportions <- medicineS/men.total
medicineT_proportions <- medicineT/men.total

artsS_proportions <- artsS/men.total
artsT_proportions <- artsT/men.total

scienceS_proportions <- scienceS/men.total
scienceT_proportions <- scienceT/men.total

engineeringS_proportions <- engineeringS/men.total
engineeringT_proportions <- engineeringT/men.total




artsSw_proportions <- artsSw/women.total
artsTw_proportions <- artsTw/women.total


scienceSw_proportions <- scienceSw/women.total
scienceTw_proportions <- scienceTw/women.total


proportions.men <- data.frame(ArtsS = artsS_proportions, ArtsT = artsT_proportions, ScienceS = scienceS_proportions, ScienceT = scienceT_proportions, EngineeringS = engineeringS_proportions, EngineeringT = engineeringT_proportions)
proportions.women <- data.frame(ArtsS = artsSw_proportions, ArtsT = artsTw_proportions, ScienceS = scienceSw_proportions, ScienceT = scienceTw_proportions)


rownames(proportions.men) <- year
rownames(proportions.women) <- year


proportions.men
proportions.women




total.all <- cbind(men.df, women.df)
rownames(total.all) <- year

data_frame <- total.all

# looping over the rows of data frame
for (i in 1:nrow(data_frame)){
  
  # looping over the columns of data frame
  for (j in 1:ncol(data_frame)){
    
    # computing sum of row i 
    row_sum <- sum(data_frame[i,])
    
    # calculating row proportion of the cell 
    # value
    data_frame[i,j] <- data_frame[i,j]/row_sum
  }
}

# printing modified data frame
print ("Modified data frame")
print (data_frame)


medicineS <- c(18,16,7 ,12,24 ,16,22 ,12,22,28)
medicineT <- c(22,23,17 ,25,50,21,32,14,34,37)

artsS <- c(16,13,11,12,8,11,4,4,0,13)
artsT <- c(30,22,25,14,12,20,10,12,0,23)

scienceS <-c(9,9,12,12,20,16,25,32,4,25)
scienceT <-c(14,12,19,12,20,16,31,38,5,31)

engineeringS <-c(10,7,12,8,5,1,16,19,0,25)
engineeringT <-c(16,11,15,9,7,2,22,25,0,35)


artsSw <- c(14,11,15,15,8,13,18,18,1,13)
artsTw <- c(19,16,18,21,9,13,22,22,1,16)


scienceSw <-c(1,4,6,3,4,8,5,16,1,10)
scienceTw <- c(1,4,7,3,4,9,5,17,1,10)



all.survived <- data.frame(MedicineMen <- medicineS,
                           ArtsMen <- artsS,
                           ScienceMen <- scienceS,
                           EngineeringMen <- engineeringS,
                           ArtsWomen <- artsSw,
                           ScienceWomen <- scienceSw)


all.survived$sums <- rowSums(total.all)



all.all <- data.frame(medicineS ,
                      medicineT,
                      
                      artsS ,
                      artsT,
                      
                      scienceS, 
                      scienceT ,
                      
                      engineeringS,
                      engineeringT,
                      
                      artsSw,
                      artsTw,
                      
                      
                      scienceSw,
                      scienceTw
                      
)


all.survived <- data.frame(medicineS,
                           artsS,
                           scienceS,
                           engineeringS,
                           artsSw,
                           scienceSw
)


all.dead <- data.frame(medicineT,
                           artsT,
                           scienceT,
                           engineeringT,
                           artsTw,
                           scienceTw
)




men.survived <- all.survived[,1:4]
women.survived <-all.survived[,5:6]

rownames(all.survived) <- year
rownames(men.survived)<-year
rownames(women.survived)<-year

all.survived$total <- rowSums(all.all)
men.survived$total <- rowSums(all.all)
women.survived$total <- rowSums(all.all)

all.survived.proportions <- all.survived
for(i in 1:(ncol(all.survived)-1)){
  all.survived.proportions[,i] = all.survived[,i]/all.survived$total
}





#proportions

sum(all.t.1$X1938[1:length(all.t.1$X1938)-1])/rowSums(all.dead)[1]
sum(all.t.1$X1939[1:length(all.t.1$X1939)-1])/rowSums(all.dead)[2]
sum(all.t.1$X1940[1:length(all.t.1$X1940)-1])/rowSums(all.dead)[3]
sum(all.t.1$X1941[1:length(all.t.1$X1941)-1])/rowSums(all.dead)[4]
sum(all.t.1$X1942[1:length(all.t.1$X1942)-1])/rowSums(all.dead)[5]
sum(all.t.1$X1943[1:length(all.t.1$X1943)-1])/rowSums(all.dead)[6]
sum(all.t.1$X1944[1:length(all.t.1$X1944)-1])/rowSums(all.dead)[7]
sum(all.t.1$X1945[1:length(all.t.1$X1945)-1])/rowSums(all.dead)[8]
sum(all.t.1$X1946[1:length(all.t.1$X1946)-1])/rowSums(all.dead)[9]
sum(all.t.1$X1947[1:length(all.t.1$X1947)-1])/rowSums(all.dead)[10]
#A) Roughly same



sum(all.survived$medicineS)/sum(all.dead$medicineT)
sum(all.survived$artsS)/sum(all.dead$artsT)
sum(all.survived$scienceS)/sum(all.dead$scienceT)
sum(all.survived$engineeringS)/sum(all.dead$engineeringT)
#B NO




sum(all.survived$artsSw)/sum(all.dead$artsTw)
sum(all.survived$scienceSw)/sum(all.dead$scienceTw)
#C NO

sum(all.survived$scienceSw)/sum(all.dead$scienceTw)-sum(all.survived$scienceS)/sum(all.dead$scienceT)
sum(all.survived$artsSw)/sum(all.dead$artsTw)-sum(all.survived$artsS)/sum(all.dead$artsT)

#D NO
