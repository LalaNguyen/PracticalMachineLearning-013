# Library we use
require(caret)
require(rattle)

# Set system path
sys_path <- "Development/R-studio/PracticalMachineLearning-013"
setwd(sys_path)
train_file <- "train.txt"
test_file <- "test.txt"
destTrainFile <- paste("data", train_file, sep = "/")
destTestFile <- paste("data", test_file, sep = "/")
# Retrieve the file
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
select_col <- colSums(is.na(train)) == 0
# 2. Filter selected column.
train <- train[,select_col]
test <- test[,select_col]

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
# Train data
modFit <- train(classe ~., method ="rpart", data = train)
# Tree visualization of our training data
fancyRpartPlot(modFit$finalModel)
# Predict new data
predict(modFit, newdata = test)
# Tree visualization of our testing data
fancyRpartPlot(modFit$finalModel)
# Session Info
sessionInfo()