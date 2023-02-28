## All functions related 'randomForest' analyses for Manas NP(VA) Land Cover Classification 2004

# Packages needed:

library(Boruta)
library(caret)
library(raster)
library(randomForest)
library(rgdal)

# Step 1: Read the point shapefile data
# This will be partitioned into training and test data. In our case this will
# be the ground survey data that indicates "vegetation type class" as the response
# and the corresponding environmental predictors

# the Environmental predictors are:

# 1) Landsat bands: Band1, Band2, Band3, Band4, Band5, Band7     
# 2) Derived indices: NDVI, NDMI

# Ground data

MAGT2000 <- read.csv(file = "I:/OneDrive/Subham/Work Place/2022/2008/MA_2000.csv",header = T, as.is = T)

# Step 2: Create a partition of the data for Training and Testing

MAGTPar00 <- createDataPartition(MAGT2000[,1],times = 1, p = 0.60, list = FALSE)
MAGTtrain00 <- MAGT2000[MAGTPar00,]
MAGTtest00 <- MAGT2000[-MAGTPar00,]
format.data <- function(t.data)
   {
        rownames(t.data) <- t.data$Id
         t.data <- t.data[-c(1,2)]
         return(t.data)
     }

# Step 3: Check datasets and export as .csv files
# Check the columns and remove redundant columns
# Make location IDs as rownames

write.csv(MAGTtrain00,file = "MAGTtrain00.csv")
write.csv(MAGTtest00,file = "MAGTtest00.csv")

# Step 4: Create list of Rasters
# Images is the directory where the images are stored

MAGTrast00 <- list.files(path = "I:/OneDrive/Subham/Work Place/2022/2008/Band2000",pattern = "img$", full.names = TRUE)

#Step 5: Create raster stack

MAGTstack00 <- stack (MAGTrast00)

#Step 6: Create shape files using point locations in the Training and Testing Datasets and store them in the following location

directory <- "I:/OneDrive/Subham/Work Place/2022/2008"

MAGTtrainshp00 <- readOGR(dsn=directory,layer ="MAGTtrain00")
MAGTtestshp00 <- readOGR(dsn=directory,layer ="MAGTtest00")

#Step 7: Assign predictor raster values to training and test data

MAGTtrainpred00 <- as.data.frame(extract(MAGTstack00,MAGTtrainshp00))
MAGTtestpred00 <- as.data.frame(extract(MAGTstack00,MAGTtestshp00))

# Step 8: Restore rownames, as they are lost here. Also, columns get sorted in alphabetical order, but that is not a problem

rownames(MAGTtrainpred00) <- rownames(MAGTtrain00)
rownames(MAGTtestpred00) <- rownames(MAGTtest00)

# Step 9: Now make dataframes for RandomForests analysis . . . 
# This should have Forest.type as the response and the rest being predictors.

MAGTfintrain00 <- data.frame(landclass =MAGTtrain00$Landclass,MAGTtrainpred00)
MAGTfintest00 <- data.frame(landclass =MAGTtest00$Landclass,MAGTtestpred00)

# Step 10: Run the RF Model

set.seed(31)
MAGT_RF00 <- randomForest(landclass~.,data= MAGTfintrain00,ytest= MAGTfintest00[,1],xtest = MAGTfintest00[,-1], ntree= 501, importance = TRUE, na.rm=T, keep.forest = T)

# Step 11: Predict the landcover classification
MApredict00 <- predict(MAGT_RF00, object = MAGTstack00, filename = "MApred00.img", type = "response", index = 1, na.rm = T, progress ="window")
