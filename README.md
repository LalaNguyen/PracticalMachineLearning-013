---
title: "Project-WriteUp"
author: "Lala Ng"
date: "April 22, 2015"
output: html_document
---


### I. Cleaning & Prepocessing Data

In this assignment, we will require **caret**, **rattle** and **rpart** to workout the solution. In addition, preprocessing the raw file is trivial to remove noise data 

```{r,results='markup',message=FALSE,warning=FALSE}

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
# cvtd_timestamp, new_window, num_window are not particularly # useful.
# Remove it
train <- train[,-c(1:7)]
test <- test[,-c(1:7)]
```

Finally, Data is now clean and ready to be extracted. It's often useful to take a look at initial data for me. From the histogram, **class A** frequently occurs while **class D** has the least number of occurences.

```{r,results='asis',message=FALSE,warning=FALSE}
ggplot(train, aes( x = classe)) +
        geom_histogram( aes(fill=..count..)) 
```

### II. Trainning and Testing

We are given **train set** and **test set**, predicting a **class** in test set would require a prediction model to be built first. In this task, we will build up model by using recursive partitioning for classification provided from packet **rpart** on our **classe feature** .

```{r,results='asis',message=FALSE,warning=FALSE}
# Train data
modFit <- train(classe ~., method ="rpart", data = train)
# Tree visualization of our training data
fancyRpartPlot(modFit$finalModel)
```

A quick glance at the graph reveals that if **roll_belt < 130 ** and **pitch_forearm > -34** and **magnet_dumbbell_y < 440** and **roll_forearm > 124** would place our subject into **class C**. In the predicting part, we will try to verify this hypothesis.

```{r,results='markup',message=FALSE,warning=FALSE}
# Predict new data
preds <- predict(modFit, newdata = test)
# Filter and evaluate our hypothesis
test$classe <- preds
isC <- test$classe == "C"
rel <- test[isC,c("roll_belt","pitch_forearm","magnet_dumbbell_y","roll_forearm")]
print(rel, quote = TRUE, row.names = FALSE)
```

It's nice to see that they're all fit the hypothesis's condition and thus, our prediction is doing good job.

Finally, here is my session information for library information.

```{r,results='markup', warning=FALSE}
sessionInfo()
```

