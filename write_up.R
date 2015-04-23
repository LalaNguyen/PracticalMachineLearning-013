# Library we use
require(caret)
require(knitr)

# Set system path
sys_path <- "Development/R-studio/PracticalMachineLearning-013"
setwd(sys_path)
train_file <- "train.txt"
test_file <- "test.txt"
destTrainFile <- paste("data", train_file, sep = "/")
destTestFile <- paste("data", test_file, sep = "/")

# Retrieve seperated files and place it inside data folder if such folder does not exist
trainFileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testFileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv" 

if(dir.create(path = "data")){
        download.file(url = trainFileUrl, destfile = destTrainFile, method = "curl")
        download.file(url = testFileUrl, destfile = destTestFile, method = "curl")
}

# Load up file and preprocessing
train <- read.csv(file = destTrainFile, header = TRUE, sep = ",", na.strings = c("NA","#DIV/0!", ""), stringsAsFactors = FALSE)
test <- read.csv(file = destTestFile, header = TRUE, sep = ",", na.strings = c("NA","#DIV/0!", ""), stringsAsFactors = FALSE)

# Delete columns with all missing values
# 1. We find col that has total number of NA equals 0
train_select_col <- colSums(is.na(train)) == 0
test_select_col <- colSums(is.na(test)) == 0
# 2. Filter selected column.
train <- train[,train_select_col]
test <- test[,test_select_col]

train$classe <- as.factor(train$classe)
# 3. user_name, raw_timestamp_part_1, raw_timestamp_part_3
# cvtd_timestamp, new_window, num_window are not useful.
# Remove it
train <- train[,-c(1:7)]
test <- test[,-c(1:7)]

# Plot histogram to see frequency of A, B, C, D, E
# in Train 
ggplot(train, aes( x = classe)) +
        geom_histogram( aes(fill=..count..)) 

# Devide our sample data into train and test to perform
# cross validation
set.seed(2)
inTrain <- createDataPartition(train$classe, p = 0.75, list = FALSE)
sampleTrain <- train[inTrain,]
sampleTest <- train[-inTrain,]

# Train our model on sampleTrain 
modFit <- train(classe ~., method ="rf", data = sampleTrain, prox = FALSE)
# Tree visualization of our training data
print(modFit$finalModel)

# We will try to predict on in-sample-error of training dataset
sampleTrain_preds <-predict(modFit, newdata = sampleTrain)
# The accuracy of our model is remarkbly low using rpart...
train_tab <-table(sampleTrain_preds, sampleTrain$classe)
confusionMatrix(train_tab)[3]$overall[1]
# Undoubtedly, out-of-sample error of training dataset is also low
sampleTest_preds <-predict(modFit, newdata = sampleTest)
# The accuracy of our model is remarkbly low using rpart...
test_tab <-table(sampleTest_preds, sampleTest$classe)
confusionMatrix(test_tab)[3]$overall[1]

# Finally, we predict classification on new sample of testing data
preds <- predict(modFit, newdata = test)
# Filter and evaluate our hypothesis
test$classe <- preds
isC <- test$classe == "C"
res <- test[isC,c("roll_belt","pitch_forearm","magnet_dumbbell_y","roll_forearm")]
print(res, quote = TRUE, row.names = FALSE)

# Session Info
sessionInfo()

# Add final part to get scores ready for assessment
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("./submission/problem_id_",i,".txt")
                  write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

# Invoke function and produce output
pml_write_files(preds)
