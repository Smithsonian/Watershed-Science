#################################################
#                                               #
#                   Code by Joshua Thompson     #
#   O__  ----          thompsonj@si.edu         #    
#  c/ /'_ ---           logger files            # 
# (*) \(*) --           main stations           #
#                                               #
################################################# 
#Edited by Carey Pelc 2020, 8/11/2023 Consolidated all long term sites to one script.

# set working directory
setwd("S:/Nutrient/Loggers/Weirs/File_Processing/Loggers 2023/2023_1002")

fileNames<-list.files(pattern="CR10")

for (fileName in fileNames){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("campbell_out")){
    data1 <- read.table(fileName, header=FALSE, sep = ',')
    data1$V4 <- sprintf("%04.f", as.numeric(data1$V4))
    data1$V19 <- with(data1, paste(data1$V2, data1$V3, data1$V4))
    data1$V19 <- strptime(data1$V19, "%Y %j %H%M")	
    campbell_out <- data.frame(data1$V1, data1$V19, data1$V5, data1$V6, data1$V7)
    names(campbell_out) <- c("outcode","datetime","station","stageheight","flow_cfs") 
  }
  
  # if the merged dataset does exist, append to it
  if (exists("campbell_out")){
    data1 <- read.table(fileName, header=FALSE, sep = ',')
    data1$V4 <- sprintf("%04.f", as.numeric(data1$V4))
    data1$V19 <- with(data1, paste(data1$V2, data1$V3, data1$V4))
    data1$V19 <- strptime(data1$V19, "%Y %j %H%M")	
    temp_campbell_out <- data.frame(data1$V1, data1$V19, data1$V5, data1$V6, data1$V7)
    names(temp_campbell_out) <- c("outcode","datetime","station","stageheight","flow_cfs") 
    campbell_out<-rbind(campbell_out, temp_campbell_out)
    rm(temp_campbell_out)
  }
  
}

station_101   <- subset(campbell_out, station == 101.0)
station_102   <- subset(campbell_out, station == 102.0)
station_109   <- subset(campbell_out, station == 109)
station_110   <- subset(campbell_out, station == 110)
#station_101_5 <- subset(campbell_out, station == 101.5)
#station_103 <- subset(campbell_out, station == 103)
#station_108   <- subset(campbell_out, station == 108)


#*ACTION NEEDED* Change folder path to correct folder and change file name to correct date for .csvs.
write.csv(station_110,  "S:/Nutrient/Loggers/Weirs/File_Processing/Loggers 2023/2023_1002/W110_2023_1002.csv", row.names = FALSE)				# export data to excel file (will need to adjust for file name)
write.table(station_110, "S:/Nutrient/Loggers/Weirs/File_Processing/Station_110/station_110.csv", append = TRUE, sep = ",", col.names = FALSE)

write.csv(station_109,"S:/Nutrient/Loggers/Weirs/File_Processing/Loggers 2023/2023_1002/station_109.csv", append = TRUE, sep = ",", col.names = FALSE)
write.table(station_109, "S:/Nutrient/Loggers/Weirs/File_Processing/Station_109/station_109.csv", append = TRUE, sep = ",", col.names = FALSE)

write.csv(station_101,  "S:/Nutrient/Muddy Creek Restoration/Data/stage_files/101/2023/station_101_2023_1002.csv", row.names = FALSE)				# export data to excel file (will need to adjust for file name)
write.table(station_101, "S:/Nutrient/Loggers/Weirs/File_Processing/Station_101/station_101.csv", append = TRUE, sep = ",", col.names = FALSE)

write.csv(station_102,  "S:/Nutrient/Muddy Creek Restoration/Data/stage_files/102//W102_2023/station_102_2023_1002.csv", row.names = FALSE)				# export data to excel file (will need to adjust for file name)
write.table(station_102, "S:/Nutrient/Loggers/Weirs/File_Processing/Station_102/station_102.csv", append = TRUE, sep = ",", col.names = FALSE)

#write.csv(station_101_5,"S:/Nutrient/Loggers/Weirs/File_Processing/Loggers 2023/2023_0501/W101-5_2023_0501.csv", row.names = FALSE)				# export data to excel file (will need to adjust for file name)
#write.table(station_101_5, "S:/Nutrient/Loggers/Weirs/File_Processing/Station_101-5/station_101-5.csv", append = TRUE, sep = ",", col.names = FALSE)
#write.csv(station_103,"S:/Nutrient/Loggers/Weirs/File_Processing/Loggers 2023/2023_1108-15/W103_2023_0809.csv", row.names = FALSE)				# export data to excel file (will need to adjust for file name)
#write.table(station_103, "S:/Nutrient/Loggers/Weirs/File_Processing/Station_103/station_103.csv", append = TRUE, sep = ",", col.names = FALSE)
#write.csv(station_108,  "S:/Nutrient/Loggers/Weirs/File_Processing/Loggers 2021/2021_0615/W108_2021_0615.csv", row.names = FALSE)				# export data to excel file (will need to adjust for file name)
#write.table(station_108, "S:/Nutrient/Loggers/Weirs/File_Processing/Station_108/station_108.csv", append = TRUE, sep = ",", col.names = FALSE)

#*ACTION NEEDED* Change Dates for PDF Title
pdf("2023_1002MonthlyGraphs.pdf", width=9, height=6) 
par(mfrow=c(5,1),mar=c(0.5,1, 1, 1) + 0.1, 
    oma = c(4,4,0,0) + 0.1)  
plot(station_101$datetime, station_101$stageheight, pch=ifelse(station_101$outcode ==129,24,21), col="black", bg=ifelse(station_101$outcode ==129,"red","NA")
     , xlab = "",xaxt='n',xlim = c(min(station_109$datetime),max(station_109$datetime)), ylab = "")
legend("topleft","101", bty = "n")
plot(station_102$datetime, station_102$stageheight, pch=ifelse(station_102$outcode ==119,24,21), col="black", bg=ifelse(station_102$outcode ==119,"red","NA")
     , xlab = "",xaxt='n',xlim = c(min(station_109$datetime),max(station_109$datetime)), ylab = "")
legend("topleft","102", bty = "n")
plot(station_109$datetime, station_109$stageheight, pch=ifelse(station_109$outcode ==129,24,21), col="black", bg=ifelse(station_109$outcode ==129,"red","NA")
     , xlab = "", xaxt='n',ylab = "",xlim = c(min(station_109$datetime),max(station_109$datetime)))
legend("topleft","109", bty = "n")
plot(station_110$datetime, station_110$stageheight, pch=ifelse(station_110$outcode ==129,24,21), col="black", bg=ifelse(station_110$outcode ==129,"red","NA")
     , xlab = "",xaxt='n',xlim = c(min(station_109$datetime),max(station_109$datetime)), ylab = "")
legend("topleft","110", bty = "n")
#plot(station_101_5$datetime, station_101_5$stageheight, pch=ifelse(station_101_5$outcode ==123,24,21), col="black", bg=ifelse(station_101_5$outcode ==123,"red","NA")
#     , xlab = "",xaxt='n',xlim = c(min(station_109$datetime),max(station_109$datetime)), ylab = "")
#legend("topleft","101.5", bty = "n")
#plot(station_103$datetime, station_103$stageheight, pch=ifelse(station_103$outcode ==123,24,21), col="black", bg=ifelse(station_103$outcode ==123,"red","NA")
#     , xlab = "",xaxt='n',xlim = c(min(station_109$datetime),max(station_109$datetime)), ylab = "")
#legend("topleft","103", bty = "n")
#plot(station_108$datetime, station_108$stageheight, pch=ifelse(station_108$outcode ==123,24,21), col="black", bg=ifelse(station_108$outcode ==123,"red","NA")
#    , xlab = "",xaxt='n',xlim = c(min(station_110$datetime),max(station_110$datetime)), ylab = "")
#legend("topleft","108", bty = "n")
title(xlab = "Date", ylab = "Stage (ft)",
      outer = TRUE, line = 3, cex.lab = 1.4)
axis.POSIXct(1,station_109$datetime, format = '%Y-%m-%d %H:%S')
dev.off()

#*ACTION NEEDED* Change Dates for PDF Title
pdf("2023_1002Hg_MonthlyGraphs.pdf", width=9, height=6)
par(mfrow=c(5,1),mar=c(0.5,1, 1, 1) + 0.1, 
    oma = c(4,4,0,0) + 0.1)  
plot(station_101$datetime, station_101$stageheight, pch=ifelse(station_101$outcode ==119,24,21), col="black", bg=ifelse(station_101$outcode ==119,"red","NA")
     , xlab = "",xaxt='n',xlim = c(min(station_109$datetime),max(station_109$datetime)), ylab = "")
legend("topleft","101 Hg", bty = "n")
plot(station_109$datetime, station_109$stageheight, pch=ifelse(station_109$outcode ==119,24,21), col="black", bg=ifelse(station_109$outcode ==119,"red","NA")
     , xlab = "", xaxt='n',ylab = "",xlim = c(min(station_109$datetime),max(station_109$datetime)))
legend("topleft","109", bty = "n")
plot(station_110$datetime, station_110$stageheight, pch=ifelse(station_110$outcode ==119,24,21), col="black", bg=ifelse(station_110$outcode ==119,"red","NA")
     , xlab = "",xaxt='n',xlim = c(min(station_109$datetime),max(station_109$datetime)), ylab = "")
legend("topleft","110", bty = "n")
title(xlab = "Date", ylab = "Stage (ft)",
      outer = TRUE, line = 3, cex.lab = 1.4)
axis.POSIXct(1,station_109$datetime, format = '%Y-%m-%d %H:%S')
dev.off()

