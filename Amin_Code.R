# Tamara Amin - Programming Assesment RA - 03/23/2019
# author Tamara Amin tta@andrew.cmu.edu

####### Load libraries#######

library(modelr)
library(Amelia)
library(sf)
library(spData)
library(maps)
library(maptools)
library(rworldxtra)
library(rworldmap)
library(car)
library(ggmap)
library(GISTools)
options(na.action = na.warn)

####### Import Data #######
df <- read.csv ("MotherData.csv")
df_Location <- read.csv("Locations.csv")

####### PART 1: WIDE TO LONG #######

# Tidy Data using Melt

# Each row contains information about 20 children (from same mother)
# There are 18 birth-related measures: bidx, bord, and b0 to b16 excluding b14
# There are 20 columns for each measure (one for each child) (measure_01 to measure_20)
# Note: I know I do not need all columns for assesment but will tidy all of it as protocol

# Must create list containing character vectors of column headers for each measure

measureList=list() #initialize list with column headers

# Create list containing measure names (bidx, bord, bi ..bn)
measureNames=c("bidx", "bord") 
# Iterate through measuress b0 to b16
childSeq=seq(0,16)
for (i in childSeq) {
  if (i!=14){ # excluding b14 
    #create string "bi" to be appended onto list of measures
    bMeasure=str_c("b", i)
    measureNames=append(measureNames,bMeasure)}
}

# Create char vector corresponding to child numbers in format _01 to _20
childStr=c(paste("_0",1:9, sep=""), paste("_", 10:20, sep=""))

# Loop through measures and add numerical car vector to them
i=1  #initialize index

for (measureN in measureNames) {
  # list in format of measure_01 to measure_20
  mList=paste(measureN, childStr,sep="")
  #append to list of measures
  measureList[i] <- list(mList)
  i=i+1
}

# Melt dataframe (wide -to - long)
dfLong = melt.data.table(as.data.table(df), 
                         measure = measureList, 
                         value.name = measureNames)

# remove rows where bidx_n = NA (no nth child)
dfClean=na.omit(dfLong, cols="bidx")

#convert to tibble for easier analysis
dfCleanTibFull=as_tibble(dfClean)

# Extrcat only columns necessary for this analysis
# caseid, v001 (location join), b7 (age of death), v191 (wealth), b4 (sex of child)

dfCleanTib= select(dfCleanTibFull, v001,b7, v191, b4)

####### PART 2: INDICATOR VARIABLE #######

# Create indicator variable for infant mortality
# ie. child died at or before 12 months, where age of death is column b7

dfInd  <- mutate(dfCleanTib, death_ind = as.integer(b7<=12))

#assuming NA means that child is still alive (indicator =0)
dfInd$death_ind[is.na(dfInd$death_ind)] <- 0


####### PART 3: LINEAR REGRESSION -INFANT MORTALITY W/ WEALTH SCORE #######

# Weath measure is variable v191
# infant mortality indicator is binary -> logistic regression is preferable linear mode

### Check for Class Bias ###
# table(dfInd$death_ind) #since proportion of 0:1 unequa, need to split

### Create Training and Test samples ###
# Since no. observations>10K: create development sample w/ equal 0:1, & use rest for test data

## Create Training data ##
dfModelLn=select(dfInd,v191, death_ind )
#Check if there are any missing values
sumNaLn=sum( is.na( dfModelLn ) ) > 0

in_ones <- dfModelLn[which(dfModelLn$death_ind == 1), ]  # all 1's
in_zeros <- dfModelLn[which(dfModelLn$death_ind == 0), ]  # all 0's
set.seed(100)  # repeatability of samples

#1's for training
in_ones_train_rows <- sample(1:nrow(in_ones), 0.7*nrow(in_ones))
#0's for training
in_zeros_train_rows <- sample(1:nrow(in_zeros), 0.7*nrow(in_ones)) 
train_ones <- in_ones[in_ones_train_rows, ]  
train_zeros <- in_zeros[in_zeros_train_rows, ]
#row bind 0s and 1s
trainData <- rbind(train_ones, train_zeros) 

## Create Test data ##
test_ones <- in_ones[-in_ones_train_rows, ]
test_zeros <- in_zeros[-in_zeros_train_rows, ]
testData <- rbind(test_ones, test_zeros) # row bind 0s and 1s

##Build linear Regression Model ##
lmModel = lm(death_ind~v191, data = trainData) #Create the linear regression

## Interpret Results ##
summaryLin=summary(lmModel) 
# p value of 4.63e-13 (statistically significant, reject null hypothesis)
# Coefficient for v191 is -0.074546 (inverse relationship between wealth score & infant mortality)
# can be reasonably sure that predictor has an effect on the dependent variable
# R-squared is low (0.01838): linear Model is poor predictor due to large unexplained variance

## How well does linear model fit ##
pred <- predict(lmModel, newdata = testData) 
# Recode factors for linear model
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$death_ind
# Accuracy of linear model
meanAcc <- mean(y_pred == y_act) #48% accuracy of model is low

#  Note: this method does not account for covariance w/ other confounding variable

####### PART 4: LOGISTIC REGRESSION - INFANT MORTALITY W/ WEALTH SCORE & GENDER #######
#variable b4 corresponds to sex of child

## Create Training data ##
dfModelLog=select(dfInd,v191, death_ind, b4)
#convert b4 data from character to factor
dfModelLog$b4 <- as.factor(dfModelLog$b4)
#Check if there are any missing values
sumNaLn=sum( is.na( dfModelLog ) ) > 0 #no missing

in_ones2 <- dfModelLog[which(dfModelLog$death_ind == 1), ]  # all 1's
in_zeros2 <- dfModelLog[which(dfModelLog$death_ind == 0), ]  # all 0's
set.seed(100)  # repeatability of samples

#1's for training
in_ones_train_rows2 <- sample(1:nrow(in_ones2), 0.7*nrow(in_ones2))
#0's for training
in_zeros_train_rows2 <- sample(1:nrow(in_zeros2), 0.7*nrow(in_ones2)) 
train_ones2 <- in_ones2[in_ones_train_rows2, ]  
train_zeros2 <- in_zeros2[in_zeros_train_rows2, ]
#row bind 0s and 1s
trainData2 <- rbind(train_ones2, train_zeros2) 

## Create Test data ##

test_ones2 <- in_ones2[-in_ones_train_rows2, ]
test_zeros2 <- in_zeros2[-in_zeros_train_rows2, ]
testData2 <- rbind(test_ones2, test_zeros2) # row bind 0s and 1s

## Run Model ##
# Regression type was not specified logistic regression was used
logModel <- glm(death_ind ~.,family=binomial(link='logit'),data=trainData2)

## Interpret Results ##
summaryLog=summary(logModel) #get summary statistics
# P values show gender and wealth are statistically signficantly associated (reject NH)
# Wealth Score: Pr(>|z|) = 1.22e-12 --> Highly statisticaly significant
# Gender:Pr(>|z|) = 1.8e-4--> Satisticaly significant (lower association than wealth)
# Inverse association with for wealth (coefficient =-0.30124)
# Positive association for gender=male (0.286), male children more likely to die before 12 months

## How well does logistic model fit ##
predLog <- predict(logModel, newdata = testData2) 
# Recode factors for linear model
y_pred_num_log <- ifelse(predLog > 0.5, 1, 0)
y_pred_log <- factor(y_pred_num_log, levels=c(0, 1))
y_act_log <- testData2$death_ind
# Accuracy of linear model
meanAcc_log <- mean(y_pred_log == y_act_log) #95.9% accuracy of model is high

## Anova Analysis
anov_results_log=anova(logModel, test="Chisq") #analyze deviance
#Both wealth (p=4.7e-13)and gender (p=1.76e-4)are statistically sinificantly associated
# wealth decreases deviance by 52.307, including gender decreases deviance by additional 14.068

# Examine Residuals
plot(logModel$residuals, pch = 16, col =alpha("blue", 0.4), main= "Residuals Plot") 
# nonuniform residual plots indicates pattern in data 
#two distinct groups =  association of  infant mortality with additional binary variable

#Cooks Distance
cooksd <-cooks.distance(logModel)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
sample_size <- nrow(dfModelLog)
abline(h = 4/sample_size, col="red") 
# group of potential identified, however values for cooks distabce are less than 1

# McFadden fit -> 0.0169
mFadden=pR2(logModel) #low psuedo R^2 (model only explains ~2% of variance )


####### PART 5: VISUALIZING CLUSTER OUTCOMES #######

# group by cluster
#find average wealth score and probability of infant death for clusters
wealth_cluster <- dfInd %>% 
  group_by(v001)%>%
  mutate(child_prob=sum(death_ind)/n(), wealth_av=mean(v191))

##Create plot of wealth vs. infant death ##
ggplot(data = wealth_cluster) + 
  
## Format Data Visuzalizion based on Tuft andmethod 

  #jitter used avoid gridding, transparency for clarity
  geom_point(mapping = aes(x = child_prob, y = wealth_av),
             alpha=0.2, 
             color="blue", size=3) + 
  
  #label axes, titles, subtitles and captions(source data)
  labs(x="Average Infant Mortality Rate", y="Average Wealth Score", 
       title = "Figure 1",
       subtitle = "Relationship between Village Wealth and Infant Mortality Rate", 
       caption="Source: USAID Demographic and Health Survey VI") +
  
  ##Reduce ink/data ratio and remove noisy elements
  
  #customize theme
  theme_bw() + #minimize ink:data ratio
  
  theme( 
    legend.position="none", #legend unnecesary
    axis.line = element_line(colour = "grey"),
    plot.title = element_text(hjust = 0.5, face="bold", size=14),
    plot.caption = element_text(size=8),
    plot.subtitle = element_text(hjust = 0.5),
  )

##Save figue as PDF
ggsave("figure1.jpeg", width=8, height=5)

####### PART 6: MAPPING CLUSTERS #######
jpeg("figure2.jpeg")

#add map of Nigeria
newmap<- getMap(resolution = "high")
plot(newmap, xlim = c(1, 15), ylim = c(8, 11), asp = 1)
#add cluster points
points(df_Location$lon, df_Location$lat, pch=21, col = "red", bg="red", lwd=1)
#Format Map
north.arrow(xb=2, yb=4.5, len=0.1, lab="N",cex.lab=0.8,col='gray10')
map.scale(2, 3.8,2,"Km", 2, 1)
title(main="Figure 2", sub="Cluster Locations in Nigeria", font.sub=3)
box(which = "plot", lty = "solid")
dev.off() 


####### PART 7: TEMP VS MORTALITY #######
# Import Raster library (errors w/select function so uploaded later#
library(raster)

# Import climate file for June (month=7)
june_raster<-'wc2.0_10m_tavg_07.tif' #june file
imported_raster=raster(june_raster)

# Isolate coordinates of clusters
cluster_coord=df_Location[2:3]
# Get rasted values for coordinates
rasValue=raster::extract(imported_raster, cluster_coord)
clusterRaster=cbind(cluster_coord,rasValue) #column bind

# Join with location data w/ cluster identifier
rasterLoc=clusterRaster %>% inner_join(df_Location, by=c("lat", "lon"))
# Join wih remaining data
dfTemp=rasterLoc %>% inner_join(dfInd, by="v001")

#crate table of average temp and child mortality %
wealth_clusterTemp <- dfTemp %>% 
  group_by(v001) %>%
  summarise(child_prob=sum(death_ind)/n(), temp_av=mean(rasValue)) %>%
 #filter empty temp observations
   filter(!is.na(temp_av))


##Visualize data##

ggplot(wealth_clusterTemp, aes(x=temp_av, y=child_prob, )) + 
  geom_point(position="jitter",alpha=0.7, color="blue") +
  
  #linear regression(2 variables)
  geom_smooth(method="lm", color="orange")+ #orange/blue are colorblind friendly

#label axes, titles, subtitles and captions(source data)
labs(x="Average Temperature (C)", y="Average Mortality Rate ", 
     title = "Figure 3",
     subtitle = "Relationship between 10 min Temperature in June and Mortality Rate", 
     caption="Sources: USAID Demographic and Health Survey VI, World Clim v.2") +
  
  ##Reduce ink/data ratio and remove noisy elements
  
  #customize theme
  theme_bw() +
  
  theme( 
    legend.position="none", #legend unnecesary
    axis.line = element_line(colour = "grey"),
    plot.title = element_text(hjust = 0.5, face="bold"),
    plot.caption = element_text(size=8),
    plot.subtitle = element_text(hjust = 0.5),
  )

##Save figue as PDF
ggsave("figure3.jpeg", width=8, height=5)

