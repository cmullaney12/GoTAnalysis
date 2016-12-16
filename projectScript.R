# I load the necessary packages for the project
# NOTE: I assume the packages have been installed already
library(RMySQL)
library(e1071)
library(randomForest)
library(caret)

# Database Connection Information
# Replace user, password, and host with your database information
# NOTE: Make sure you've run the ds_project_db_dump.sql file that was included with this project
user <- 'your username'
password <- 'your password'
host <- 'your host'

# Connect to the ds_project database 
db <- dbConnect(MySQL(), user=user, password=password, dbname='ds_project', host=host)

# Check to make sure the correct tables are in the database
tables <- dbListTables(db)
if(!("battles" %in% tables) || !("characters" %in% tables)) {
    stop("Cannot find appropriate tables! Ensure database was created properly!")
}

getEarliestAppearance <- function(row) {
    # This function returns the earliest book that a character appeared in
    # Args:
    #    row -> a row from the characters data set
    # Returns: a character value representing the name of a book in A Song of Ice and Fire
    
    # Get the bit values [0, 1] for the appropriate columns in the row
    got <- row[[9]]
    cok <- row[[10]]
    sos <- row[[11]]
    ffc <- row[[12]]
    dwd <- row[[13]]
    
    # Returns the abbreviation of the first column that had a 1 (from left to right)
    if(got == 1) {
        return("GoT")
    } else if(cok == 1) {
        return("CoK")
    } else if(sos == 1) {
        return("SoS")
    } else if(ffc == 1) {
        return("FfC")
    } else if(dwd == 1) {
        return("DwD")
    }
}

getLatestAppearance <- function(row) {
    # This function returns the latest book that a character appeared in
    # Args:
    #    row -> a row from the characters data set
    # Returns: a character value representing the name of a book in A Song of Ice and Fire
    
    # Get the bit values [0, 1] for the appropriate columns in the row
    got <- row[[9]]
    cok <- row[[10]]
    sos <- row[[11]]
    ffc <- row[[12]]
    dwd <- row[[13]]
    
    # Returns the abbreviation of the first column that had a 1 (from right to left)
    if(dwd == 1) {
        return("DwD")
    } else if(ffc == 1) {
        return("FfC")
    } else if(sos == 1) {
        return("SoS")
    } else if(cok == 1) {
        return("CoK")
    } else if(got == 1) {
        return("GoT")
    }
}

# Get the battles table from the MySQL database
battleData <- dbGetQuery(db, "select * from battles")

# Begin cleaning the battles data -------------------------------------------------------

# Remove unnecessary columns
battleData <- battleData[c(-3, -16, -17, -23, -25)]

# Remove rows with missing values for attacker_outcome
battleData <- battleData[!is.na(battleData$attacker_outcome),]

# Set new 'attackers' column equal to first attacker
battleData$attackers <- battleData$attacker_1

# Set new 'defenders' column equal to first defender
battleData$defenders <- battleData$defender_1

# Remove all attacker and defender columns
battleData <- battleData[-(5:12)]

# Turns attacker_outcome into factor (so that Naive Bayes model works)
battleData$attacker_outcome <- as.factor(battleData$attacker_outcome)
# Finish cleaning data ------------------------------------------------------------------

# Get random training indices
battleTrainingIndices <- sample(nrow(battleData), 30)

# Create training and test data sets
battleData.training <- battleData[battleTrainingIndices,]
battleData.test <- battleData[-battleTrainingIndices,]

# Create Naive Bayes model
battlesNBModel <- naiveBayes(attacker_outcome ~ attacker_king + attackers + attacker_size + defender_king + defenders + defender_size + battle_type + summer + region, data = battleData.training)

# Make predictions for test data set
battleData.test$NBPredict <- predict(battlesNBModel, battleData.test)

# Create confusion matrix for predicted values and actual values
battlesNBConfMatrix <- confusionMatrix(data=battleData.test$NBPredict, reference=battleData.test$attacker_outcome)

###########################################################################################

# Get the characters table from the MySQL database
deathData <- dbGetQuery(db, "select * from characters")

# Begin cleaning the characters data ----------------------------------------------------

# Remove the charID primary key field
deathData <- deathData[-1]

# Change Gender from 1 and 0 to male and female
deathData$Gender[deathData$Gender == 1] <- "male"
deathData$Gender[deathData$Gender == 0] <- "female"

# Create a new Column called IsDead, which is true whenever there is a value for Death Year
deathData$IsDead <- !is.na(deathData$Death_Year)

# Create EarliestBook and LatestBook columns with the previous functions I wrote
deathData$EarliestBook <- apply(deathData, 1, getEarliestAppearance)
deathData$LatestBook <- apply(deathData, 1, getLatestAppearance)

# Remove "House" from any of the allegiances
deathData$Allegiances <- sapply(deathData$Allegiances, function(alleg) {return(gsub("House ", "", alleg))})

# Turn all character values into factors so they can be used for creating models
deathData$Allegiances <- as.factor(deathData$Allegiances)
deathData$EarliestBook <- as.factor(deathData$EarliestBook)
deathData$LatestBook <- as.factor(deathData$LatestBook)
deathData$Gender <- as.factor(deathData$Gender)
deathData$IsDead <- as.factor(deathData$IsDead)

# Remove unnecessary columns
deathData <- deathData[c(-3, -4, -5, -6, -9, -10, -11, -12, -13)]
# Finish cleaning data ------------------------------------------------------------------

# Get random training indices
deathTrainingIndices <- sample(nrow(deathData), 700)

# Create training and test data sets
deathData.training <- deathData[deathTrainingIndices,]
deathData.test <- deathData[-deathTrainingIndices,]

# Create Naive Bayes model
deathsNBModel <- naiveBayes(as.factor(IsDead) ~ Allegiances + Gender + Nobility + EarliestBook + LatestBook, data = deathData.training)

# Create Random Forest model
deathsRFModel <- randomForest(as.factor(IsDead) ~ Allegiances + Gender + Nobility + EarliestBook + LatestBook, data = deathData.training)

# Make predictions for test data set
deathData.test$NBPredict <- predict(deathsNBModel, deathData.test)
deathData.test$RFPredict <- predict(deathsRFModel, deathData.test)

# Create confusion matrix for predicted values and actual values
deathsNBConfMatrix <- confusionMatrix(data=deathData.test$NBPredict, reference=deathData.test$IsDead)
deathsRFConfMatrix <- confusionMatrix(data=deathData.test$RFPredict, reference=deathData.test$IsDead)


# Disconnect from the database when done with the script
dbDisconnect(db)