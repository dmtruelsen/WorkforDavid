rm(list=ls())

library(raster)
library(sp)
library(rgdal)

setwd("/Users/Ditte/Desktop/Work")
velo_gen <- read.delim("temperature_Velocity_genetic.txt", header=T, stringsAsFactors=F)

############### Temperature #############################
setwd("/Users/Ditte/Desktop/T/")
cli_vel_temp <- raster("climate_change_velocity_T_cl1.tif")

# Change extent of file
e <- extent(c(-180,180,-90,90))
cli_vel_temp@extent <- e
cli_vel_temp

################### Change resolution into 4x4 and create a data frame with values ##################

# Mean
cli_vel_temp_agg <- aggregate(cli_vel_temp, fact=24, fun=mean) # Change resolution and get the mean of the cells
cli_vel_temp_agg_frame <- as.data.frame(as.matrix(cli_vel_temp_agg))

# Max velocity
cli_vel_temp_max <- aggregate(cli_vel_temp, fact=24, fun=max) # Change resolution and get the max of the cells
cli_vel_temp_max_frame <- as.data.frame(as.matrix(cli_vel_temp_max))

# Min velcoity
cli_vel_temp_min <- aggregate(cli_vel_temp, fact=24, fun=min) # Change resolution and get the min of the cells
cli_vel_temp_min_frame <- as.data.frame(as.matrix(cli_vel_temp_min))

# Median
cli_vel_temp_med <- aggregate(cli_vel_temp, fact=24, fun=median) # Change resolution and get the median of the cells

# Standard deviation
cli_vel_temp_sd <- aggregate(cli_vel_temp, fact=24, fun=sd) # Change resolution and get the sd of the cells
cli_vel_temp_sd_frame <- as.data.frame(as.matrix(cli_vel_temp_sd))

################ Get coordinates with associated velocities #######################

# Mean
coor_agg <- as.data.frame(rasterToPoints(cli_vel_temp_agg))

# Median
coor_median <- as.data.frame(rasterToPoints(cli_vel_temp_med))
velocity_4by4 <- cbind(coor_agg, coor_median[,3])

# Max
coor_max <- as.data.frame(rasterToPoints(cli_vel_temp_max))
velocity_4by4 <- cbind(velocity_4by4, coor_max[,3])

# Min
coor_min <- as.data.frame(rasterToPoints(cli_vel_temp_min))
velocity_4by4 <- cbind(velocity_4by4, coor_min[,3])

velocity_4by4[,7:8] <- NA # Add two empty columns for sd velocity and the indentifier
colnames(velocity_4by4) <- c("Longitude","Latitude", "Mean_velocity","Median_velocity","Max_velocity","Min_velocity","sd_velocity", "Identifier") # Add names
velocity_4by4$Identifier <- paste("x",velocity_4by4$Latitude, velocity_4by4$Longitude, sep="_") # Creates the identifier

# Standard deviation
coor_sd <- as.data.frame(rasterToPoints(cli_vel_temp_sd))
coor_sd[,4] <- NA # Adds a column to dataframe for the identifier to be able to match
colnames(coor_sd) <- c("Longitude","Latitude","sd_velcoity","Identifier")
coor_sd$Identifier <- paste("x",coor_sd$Latitude, coor_sd$Longitude, sep="_") #Identifier creation

velocity_4by4$sd_velocity <- coor_sd[match(velocity_4by4$Identifier, coor_sd$Identifier),3] # Match standard deviation coordinates with velocity dataframe

#write.table(velocity_4by4, "velocity_temp.txt", sep ="\t", row.names=F)

################### Genetic data ######################

setwd("/Users//Ditte/Desktop/Work/")

genetic_data <- read.delim("gd_4x4.txt", header=T, stringsAsFactors=F) # Load genetic data
genetic_data[,9] <- NA # Add column for identifier
colnames(genetic_data)[9] <- "Identifier"
genetic_data$Identifier <- paste("x",genetic_data$lat,genetic_data$lon, sep="_")

############## Add genetic data to velocity data frame ####################

velocity_4by4[,9:11] <- NA
colnames(velocity_4by4)[9:11] <- c("GD_mammmals","GD_amphibians","GD_average")
velocity_4by4$GD_mammmals <- genetic_data[match(velocity_4by4$Identifier, genetic_data$Identifier),6]
#velocity_4by4$GD_mammmals <- genetic_data$ma_gd[match(velocity_4by4$Identifier,genetic_data$Identifier)]
velocity_4by4$GD_amphibians <- genetic_data[match(velocity_4by4$Identifier, genetic_data$Identifier),4]
velocity_4by4$GD_average <- genetic_data[match(velocity_4by4$Identifier, genetic_data$Identifier),8]

write.table(velocity_4by4, "Velocity_genetic.txt", sep ="\t", row.names=F)

###################### Precipitation ##########################

setwd("/Users/Ditte/Desktop/P/")

cli_vel_prec <- raster("climate_change_velocity_P_cl1.tif")

# Change extent of file
e <- extent(c(-180,180,-90,90))
cli_vel_prec@extent <- e
cli_vel_prec

################### Change resolution into 4x4 and create a data frame with values ##################

# Mean
cli_vel_prec_agg <- aggregate(cli_vel_prec, fact=24, fun=mean) # Change resolution and get the mean of the cells
cli_vel_prec_agg_frame <- as.data.frame(as.matrix(cli_vel_prec_agg))

# Max velocity
cli_vel_prec_max <- aggregate(cli_vel_prec, fact=24, fun=max) # Change resolution and get the max of the cells
cli_vel_prec_max_frame <- as.data.frame(as.matrix(cli_vel_prec_max))

# Min velcoity
cli_vel_prec_min <- aggregate(cli_vel_prec, fact=24, fun=min) # Change resolution and get the min of the cells
cli_vel_prec_min_frame <- as.data.frame(as.matrix(cli_vel_prec_min))

# Median
cli_vel_prec_med <- aggregate(cli_vel_prec, fact=24, fun=median) # Change resolution and get the median of the cells

# Standard deviation
cli_vel_prec_sd <- aggregate(cli_vel_prec, fact=24, fun=sd) # Change resolution and get the sd of the cells
cli_vel_prec_sd_frame <- as.data.frame(as.matrix(cli_vel_prec_sd))

################ Get coordinates with associated velocities #######################

# Mean
coor_agg <- as.data.frame(rasterToPoints(cli_vel_prec_agg))

# Median
coor_median <- as.data.frame(rasterToPoints(cli_vel_prec_med))
velocity_4by4_prec <- cbind(coor_agg, coor_median[,3])

# Max
coor_max <- as.data.frame(rasterToPoints(cli_vel_prec_max))
velocity_4by4_prec <- cbind(velocity_4by4_prec, coor_max[,3])

# Min
coor_min <- as.data.frame(rasterToPoints(cli_vel_prec_min))
velocity_4by4_prec <- cbind(velocity_4by4_prec, coor_min[,3])

velocity_4by4_prec[,7:8] <- NA # Add two empty columns for sd velocity and the indentifier
colnames(velocity_4by4_prec) <- c("Longitude","Latitude", "Mean_velocity","Median_velocity","Max_velocity","Min_velocity","sd_velocity", "Identifier") # Add names
velocity_4by4_prec$Identifier <- paste("x",velocity_4by4_prec$Latitude, velocity_4by4_prec$Longitude, sep="_") # Creates the identifier

# Standard deviation
coor_sd <- as.data.frame(rasterToPoints(cli_vel_prec_sd))
coor_sd[,4] <- NA # Adds a column to dataframe for the identifier to be able to match
colnames(coor_sd) <- c("Longitude","Latitude","sd_velcoity","Identifier")
coor_sd$Identifier <- paste("x",coor_sd$Latitude, coor_sd$Longitude, sep="_") #Identifier creation

velocity_4by4_prec$sd_velocity <- coor_sd[match(velocity_4by4_prec$Identifier, coor_sd$Identifier),3] # Match standard deviation coordinates with velocity dataframe

#write.table(velocity_4by4, "velocity_temp.txt", sep ="\t", row.names=F)

################### Genetic data ######################

setwd("/Users//Ditte/Desktop/Work/")

genetic_data <- read.delim("gd_4x4.txt", header=T, stringsAsFactors=F) # Load genetic data
genetic_data[,9] <- NA # Add column for identifier
colnames(genetic_data)[9] <- "Identifier"
genetic_data$Identifier <- paste("x",genetic_data$lat,genetic_data$lon, sep="_")

############## Add genetic data to velocity data frame ####################

velocity_4by4_prec[,9:11] <- NA
colnames(velocity_4by4_prec)[9:11] <- c("GD_mammmals","GD_amphibians","GD_average")
velocity_4by4_prec$GD_mammmals <- genetic_data[match(velocity_4by4_prec$Identifier, genetic_data$Identifier),6]
#velocity_4by4$GD_mammmals <- genetic_data$ma_gd[match(velocity_4by4$Identifier,genetic_data$Identifier)]
velocity_4by4_prec$GD_amphibians <- genetic_data[match(velocity_4by4_prec$Identifier, genetic_data$Identifier),4]
velocity_4by4_prec$GD_average <- genetic_data[match(velocity_4by4_prec$Identifier, genetic_data$Identifier),8]

write.table(velocity_4by4_prec, "Precipitation_Velocity_genetic.txt", sep ="\t", row.names=F)

######################## Combined temperature and precipitation #############################

setwd("/Users/Ditte/Desktop/TP/")

cli_vel_tp <- raster("climate_change_velocity_TP_cl1.tif")

# Change extent of file
e <- extent(c(-180,180,-90,90))
cli_vel_tp@extent <- e
cli_vel_tp

################### Change resolution into 4x4 and create a data frame with values ##################

# Mean
cli_vel_tp_agg <- aggregate(cli_vel_tp, fact=24, fun=mean) # Change resolution and get the mean of the cells
cli_vel_tp_agg_frame <- as.data.frame(as.matrix(cli_vel_tp_agg))

# Max velocity
cli_vel_tp_max <- aggregate(cli_vel_tp, fact=24, fun=max) # Change resolution and get the max of the cells
cli_vel_tp_max_frame <- as.data.frame(as.matrix(cli_vel_tp_max))

# Min velcoity
cli_vel_tp_min <- aggregate(cli_vel_tp, fact=24, fun=min) # Change resolution and get the min of the cells
cli_vel_tp_min_frame <- as.data.frame(as.matrix(cli_vel_tp_min))

# Median
cli_vel_tp_med <- aggregate(cli_vel_tp, fact=24, fun=median) # Change resolution and get the median of the cells

# Standard deviation
cli_vel_tp_sd <- aggregate(cli_vel_tp, fact=24, fun=sd) # Change resolution and get the sd of the cells
cli_vel_tp_sd_frame <- as.data.frame(as.matrix(cli_vel_tp_sd))

################ Get coordinates with associated velocities #######################

# Mean
coor_agg <- as.data.frame(rasterToPoints(cli_vel_tp_agg))

# Median
coor_median <- as.data.frame(rasterToPoints(cli_vel_tp_med))
velocity_4by4_tp <- cbind(coor_agg, coor_median[,3])

# Max
coor_max <- as.data.frame(rasterToPoints(cli_vel_tp_max))
velocity_4by4_tp <- cbind(velocity_4by4_tp, coor_max[,3])

# Min
coor_min <- as.data.frame(rasterToPoints(cli_vel_tp_min))
velocity_4by4_tp <- cbind(velocity_4by4_tp, coor_min[,3])

velocity_4by4_tp[,7:8] <- NA # Add two empty columns for sd velocity and the indentifier
colnames(velocity_4by4_tp) <- c("Longitude","Latitude", "Mean_velocity","Median_velocity","Max_velocity","Min_velocity","sd_velocity", "Identifier") # Add names
velocity_4by4_tp$Identifier <- paste("x",velocity_4by4_tp$Latitude, velocity_4by4_tp$Longitude, sep="_") # Creates the identifier

# Standard deviation
coor_sd <- as.data.frame(rasterToPoints(cli_vel_tp_sd))
coor_sd[,4] <- NA # Adds a column to dataframe for the identifier to be able to match
colnames(coor_sd) <- c("Longitude","Latitude","sd_velcoity","Identifier")
coor_sd$Identifier <- paste("x",coor_sd$Latitude, coor_sd$Longitude, sep="_") #Identifier creation

velocity_4by4_tp$sd_velocity <- coor_sd[match(velocity_4by4_tp$Identifier, coor_sd$Identifier),3] # Match standard deviation coordinates with velocity dataframe

#write.table(velocity_4by4, "velocity_temp.txt", sep ="\t", row.names=F)

################### Genetic data ######################

setwd("/Users//Ditte/Desktop/Work/")

genetic_data <- read.delim("gd_4x4.txt", header=T, stringsAsFactors=F) # Load genetic data
genetic_data[,9] <- NA # Add column for identifier
colnames(genetic_data)[9] <- "Identifier"
genetic_data$Identifier <- paste("x",genetic_data$lat,genetic_data$lon, sep="_")

############## Add genetic data to velocity data frame ####################

velocity_4by4_tp[,9:11] <- NA
colnames(velocity_4by4_tp)[9:11] <- c("GD_mammmals","GD_amphibians","GD_average")
velocity_4by4_tp$GD_mammmals <- genetic_data[match(velocity_4by4_tp$Identifier, genetic_data$Identifier),6]
#velocity_4by4$GD_mammmals <- genetic_data$ma_gd[match(velocity_4by4$Identifier,genetic_data$Identifier)]
velocity_4by4_tp$GD_amphibians <- genetic_data[match(velocity_4by4_tp$Identifier, genetic_data$Identifier),4]
velocity_4by4_tp$GD_average <- genetic_data[match(velocity_4by4_tp$Identifier, genetic_data$Identifier),8]

write.table(velocity_4by4_tp, "Precipitation&Temperature_Velocity_genetic.txt", sep ="\t", row.names=F)

################## Boxplot percentiles #####################

velo_gen$percentile <- NA
velo_gen$mean_GD_mammal <- NA
mean_data_frame <- data.frame(matrix(ncol=2, nrow=10))
colnames(mean_data_frame) <- c("Percentile", "Mean_GD")
median_percentiles <- quantile(velo_gen$Median_velocity, probs=seq(0.1,1,0.1))
percent_category <- seq(10, 100, 10)

for (quant in seq_along(median_percentiles )){
  if(quant == 1){
    velo_gen$percentile[which(velo_gen$Median_velocity < median_percentiles[quant])] <- percent_category[quant]
  }
  if(is.element(quant, 2:10)){
    velo_gen$percentile[which(velo_gen$Median_velocity >= median_percentiles[quant - 1] & velo_gen$Median_velocity < median_percentiles[quant])] <- percent_category[quant]
  }
}

boxplot(velo_gen$GD_mammmals ~ velo_gen$percentile, xlab="Median velocity percentile", ylab="Genetic diversity", main="Mammals")
boxplot(velo_gen$GD_mammmals ~ velo_gen$percentile, outline = F, xlab="Median velocity percentile", ylab="Genetic diversity", main="Mammals - without outliers") # no outliers
boxplot(velo_gen$GD_mammmals ~ velo_gen$percentile, outline = F, names = table(velo_gen$percentile, xlab="Median velocity percentile", ylab="Genetic diversity", main="Mammals")) # counting points

##### Average GD per percentile #####

for (bar in seq_along(percent_category)){
  velo_gen$mean_GD_mammal[which(velo_gen$percentile == percent_category[bar])] <- mean(na.omit(velo_gen$GD_mammmals[which(velo_gen$percentile ==percent_category[bar])]))
  mean_data_frame$Percentile[bar] <- percent_category[bar]
  mean_data_frame$Mean_GD[bar] <- velo_gen$mean_GD_mammal[which(velo_gen$percentile == percent_category[bar])[bar]]
}

barplot(mean_data_frame$Mean_GD, names.arg = percent_category, 
        ylim = c(0,0.014), main = "Average GD for mammals per percentile", col="#ADFF2F95",xlab="Percentiles", ylab="Average GD")


################## Equally spaced velocity boxplots #########################

velo_gen$equal_vel <- NA
equal_velocities <- seq(min(velo_gen$Median_velocity), max(velo_gen$Median_velocity), length.out=11)
Velocity_category <- LETTERS[1:11]
for (vel in seq_along(equal_velocities)[-1]){
  if(vel == 2){
    velo_gen$equal_vel[which(velo_gen$Median_velocity < equal_velocities[vel])] <- Velocity_category[vel]
  }
  if(is.element(vel, 3:11)){
    velo_gen$equal_vel[which(velo_gen$Median_velocity >= equal_velocities[vel-1] & velo_gen$Median_velocity < equal_velocities[vel])] <- Velocity_category[vel]}
}
boxplot(velo_gen$GD_mammmals ~ velo_gen$equal_vel, xlab="Median velocity equally spaced", ylab="Genetic diversity", main="Mammals")
boxplot(velo_gen$GD_mammmals ~ velo_gen$equal_vel, outline = F, xlab="Median velocity equally spaced", ylab="Genetic diversity", main="Mammals - without outliers") # no outliers
boxplot(velo_gen$GD_mammmals ~ velo_gen$equal_vel, outline = F, names = table(velo_gen$equal_vel)[1:8]) # counting points

#### Average GD for equally spaced velocities ####

velo_gen$mean_GD_equal_mammal <- NA
mean_equal_frame <- data.frame(matrix(ncol=2, nrow=11))
colnames(mean_equal_frame) <- c("Velocity_category","Mean_GD")

for (equal in seq_along(equal_velocities)){
  velo_gen$mean_GD_equal_mammal[which(velo_gen$equal_vel == Velocity_category[equal])] <- mean(na.omit(velo_gen$GD_mammmals[which(velo_gen$equal_vel ==Velocity_category[equal])]))
  mean_equal_frame$Velocity_category[equal] <- Velocity_category[equal]
  mean_equal_frame$Mean_GD[equal] <- velo_gen$mean_GD_equal_mammal[which(velo_gen$equal_vel == Velocity_category[equal])[equal]]
}
barplot(mean_equal_frame$Mean_GD, names.arg = Velocity_category, main="Average GD - equally spaced velocities - Mammals", xlab= "Velocity categories", ylab="Average GD")

#### Assign latitudinal bands ####

velo_gen$lat_band <- NA
velo_gen$col_lat <- NA
lat_band <- seq(1:7)
lati_coord <- seq(90,-90,-30)
#lati_coord <- seq(90, -90, -12)
#lati_coord <- c(90,78,66,54,42,30,18,6,-6,-18,-30,-42,-54,-66,-78,-90)
lat_col <- colorRampPalette(c("pink", "darkorange"))(7)
lat_col <- c("#98F5FF90","#FA807290","#ADFF2F90","#8B3A6290","#00FF0090","#6E8B3D90","#53868B90")

for (lat in seq_along(lat_band)){
    velo_gen$lat_band[which(velo_gen$Latitude <= lati_coord[lat] & velo_gen$Latitude > lati_coord[lat+1])] <- lat_band[lat]
    velo_gen$col_lat[which(velo_gen$Latitude <= lati_coord[lat] & velo_gen$Latitude > lati_coord[lat+1])] <- lat_col[lat]
}
plot(velo_gen$Median_velocity,velo_gen$GD_mammmals, col=velo_gen$col_lat, pch=16,xlab="Median velocity", ylab="Genetic diversity for mammals", xlim = c(0.1,9)) 
legend("topright", legend =lati_coord, fill=lat_col, title="Latitudinal bands") 

boxplot(velo_gen$GD_mammmals ~ velo_gen$equal_vel, outline = F, names = table(velo_gen$equal_vel)[1:8]) # counting points

subset_latband <- split(velo_gen, velo_gen$lat_band)
corr_df <- data.frame(matrix(nrow=length(subset_latband), ncol=3))
lm_list <- list()
colnames(corr_df) <- c("Lat", "r", "p-value")
corr_df$Lat <- 1:(length(subset_latband))
for (lat in 1:(length(subset_latband))){
  temp <- subset_latband[[lat]]
  if(sum(!is.na(temp$GD_mammmals)) > 5){
  temp2 <- temp[which(!is.na(temp$GD_mammmals)),]
  corr_object <- cor.test(x =temp2$Median_velocity, y=temp2$GD_mammmals)
  lm_object <- lm(temp2$GD_mammmals ~ temp2$Median_velocity)
  corr_df[lat, c(2,3)] <- c(corr_object$estimate, corr_object$p.value)
  lm_list[[lat]] <- lm_object
  }else{
    corr_df[lat, c(2,3)] <- NA
  }
}
 
for (lm in 1:length(lm_list)){
  abline(lm_list[[lm]], col=lat_col[lm])
}

plot(temp$Median_velocity,temp$GD_mammmals, pch=16, col="purple")
abline(lm_object, col="purple")

lat_col <- c("#98F5FF90","#FA807290","#ADFF2F90","#8B3A6290","#00FF0090","#6E8B3D90","#53868B90")

temp <- subset_latband[[1]]
lm_object <- lm(temp$GD_mammmals ~ temp$Median_velocity)
abline(lm_object, col="#98F5FF")

temp2 <- subset_latband[[2]]
lm_object2 <- lm(temp2$GD_mammmals ~ temp2$Median_velocity)
abline(lm_object2, col="#FA8072")

temp3 <- subset_latband[[3]]
lm_object3 <- lm(temp3$GD_mammmals ~ temp3$Median_velocity)
abline(lm_object3, col="#ADFF2F")

temp4 <- subset_latband[[4]]
lm_object4 <- lm(temp4$GD_mammmals ~ temp4$Median_velocity)
abline(lm_object4, col="#8B3A62")

temp5 <- subset_latband[[5]]
lm_object5 <- lm(temp5$GD_mammmals ~ temp5$Median_velocity)
abline(lm_object5, col="#00FF00")
