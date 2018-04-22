Background
----------

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now
possible to collect a large amount of data about personal activity
relatively inexpensively. These type of devices are part of the
quantified self movement – a group of enthusiasts who take measurements
about themselves regularly to improve their health, to find patterns in
their behavior, or because they are tech geeks. One thing that people
regularly do is quantify how much of a particular activity they do, but
they rarely quantify how well they do it. In this project, your goal
will be to use data from accelerometers on the belt, forearm, arm, and
dumbell of 6 participants. They were asked to perform barbell lifts
correctly and incorrectly in 5 different ways. More information is
available from the website here:
<http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har>
(see the section on the Weight Lifting Exercise Dataset).

Data
----

The training data for this project are available here:

<https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv>

The test data are available here:

<https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv>

The data for this project come from this source:
<http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har>.
If you use the document you create for this class for any purpose please
cite them as they have been very generous in allowing their data to be
used for this kind of assignment.

The Goal
--------

The goal of your project is to predict the manner in which they did the
exercise. This is the "classe" variable in the training set. You may use
any of the other variables to predict with. You should create a report
describing how you built your model, how you used cross validation, what
you think the expected out of sample error is, and why you made the
choices you did. You will also use your prediction model to predict 20
different test cases.

Setup
-----

Below libraries were used in this project. One would need to install and
load them in their working enviorment

    library(rattle)

    ## Warning: package 'rattle' was built under R version 3.4.3

    ## Rattle: A free graphical interface for data science with R.
    ## Version 5.1.0 Copyright (c) 2006-2017 Togaware Pty Ltd.
    ## Type 'rattle()' to shake, rattle, and roll your data.

    library(caret)

    ## Warning: package 'caret' was built under R version 3.4.4

    ## Loading required package: lattice

    ## Loading required package: ggplot2

    library(rpart)
    library(rpart.plot)

    ## Warning: package 'rpart.plot' was built under R version 3.4.1

    library(corrplot)

    ## Warning: package 'corrplot' was built under R version 3.4.2

    ## corrplot 0.84 loaded

    library(randomForest)

    ## Warning: package 'randomForest' was built under R version 3.4.1

    ## randomForest 4.6-12

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

    ## The following object is masked from 'package:rattle':
    ## 
    ##     importance

    library(RColorBrewer)

Downloading Data
----------------

Download the data files provided. Check if they are present in the data
folder and if not download them from the url provided. We have been
provided by 2 urls, one for training data and other for test data.

    trainUrl <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
    testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

    trainFile <- "./data/pml-training.csv"
    testFile  <- "./data/pml-testing.csv"

    if (!file.exists("./data")) {
    dir.create("./data")
    }

    if (!file.exists(trainFile)) {
    download.file(trainUrl, destfile = trainFile)
    }

    if (!file.exists(testFile)) {
    download.file(testUrl, destfile = testFile)
    }

    rm(trainUrl)
    rm(testUrl)

Loading the data
----------------

Once the data is downloaded from the data source, we can read the two
csv files into two data frames. When loaded the data first time, noticed
that it contains columns with values like "\#DIV/0!". Hence passing such
values to be treated as NA when loading the data.

    train <- read.csv(trainFile,na.strings = c("NA","#DIV/0!", ""))
    test <- read.csv(testFile,na.strings = c("NA","#DIV/0!", ""))

    dim(train)

    ## [1] 19622   160

    dim(test)

    ## [1]  20 160

By looking at the structure of the train data we see that, the training
data set contains 19622 observations and 160 variables, while the
testing data set contains 20 observations and 160 variables. The
`classe` variable in the training set is the outcome to predict.

Cleaning Data
-------------

In this step, we will clean the dataset and get rid of observations with
missing values as well as some meaningless variables.

    train <- train[,colSums(is.na(train))==0]
    test <- test[,colSums(is.na(test))==0]

    ##Removing few unwanted variables

    regex <- grepl("^X|timestamp|user_name", names(train))

    train <- train[, !regex]
    test <- test[, !regex]
    dim(train)

    ## [1] 19622    55

    dim(test)

    ## [1] 20 55

    str(train)

    ## 'data.frame':    19622 obs. of  55 variables:
    ##  $ new_window          : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ num_window          : int  11 11 11 12 12 12 12 12 12 12 ...
    ##  $ roll_belt           : num  1.41 1.41 1.42 1.48 1.48 1.45 1.42 1.42 1.43 1.45 ...
    ##  $ pitch_belt          : num  8.07 8.07 8.07 8.05 8.07 8.06 8.09 8.13 8.16 8.17 ...
    ##  $ yaw_belt            : num  -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 ...
    ##  $ total_accel_belt    : int  3 3 3 3 3 3 3 3 3 3 ...
    ##  $ gyros_belt_x        : num  0 0.02 0 0.02 0.02 0.02 0.02 0.02 0.02 0.03 ...
    ##  $ gyros_belt_y        : num  0 0 0 0 0.02 0 0 0 0 0 ...
    ##  $ gyros_belt_z        : num  -0.02 -0.02 -0.02 -0.03 -0.02 -0.02 -0.02 -0.02 -0.02 0 ...
    ##  $ accel_belt_x        : int  -21 -22 -20 -22 -21 -21 -22 -22 -20 -21 ...
    ##  $ accel_belt_y        : int  4 4 5 3 2 4 3 4 2 4 ...
    ##  $ accel_belt_z        : int  22 22 23 21 24 21 21 21 24 22 ...
    ##  $ magnet_belt_x       : int  -3 -7 -2 -6 -6 0 -4 -2 1 -3 ...
    ##  $ magnet_belt_y       : int  599 608 600 604 600 603 599 603 602 609 ...
    ##  $ magnet_belt_z       : int  -313 -311 -305 -310 -302 -312 -311 -313 -312 -308 ...
    ##  $ roll_arm            : num  -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 ...
    ##  $ pitch_arm           : num  22.5 22.5 22.5 22.1 22.1 22 21.9 21.8 21.7 21.6 ...
    ##  $ yaw_arm             : num  -161 -161 -161 -161 -161 -161 -161 -161 -161 -161 ...
    ##  $ total_accel_arm     : int  34 34 34 34 34 34 34 34 34 34 ...
    ##  $ gyros_arm_x         : num  0 0.02 0.02 0.02 0 0.02 0 0.02 0.02 0.02 ...
    ##  $ gyros_arm_y         : num  0 -0.02 -0.02 -0.03 -0.03 -0.03 -0.03 -0.02 -0.03 -0.03 ...
    ##  $ gyros_arm_z         : num  -0.02 -0.02 -0.02 0.02 0 0 0 0 -0.02 -0.02 ...
    ##  $ accel_arm_x         : int  -288 -290 -289 -289 -289 -289 -289 -289 -288 -288 ...
    ##  $ accel_arm_y         : int  109 110 110 111 111 111 111 111 109 110 ...
    ##  $ accel_arm_z         : int  -123 -125 -126 -123 -123 -122 -125 -124 -122 -124 ...
    ##  $ magnet_arm_x        : int  -368 -369 -368 -372 -374 -369 -373 -372 -369 -376 ...
    ##  $ magnet_arm_y        : int  337 337 344 344 337 342 336 338 341 334 ...
    ##  $ magnet_arm_z        : int  516 513 513 512 506 513 509 510 518 516 ...
    ##  $ roll_dumbbell       : num  13.1 13.1 12.9 13.4 13.4 ...
    ##  $ pitch_dumbbell      : num  -70.5 -70.6 -70.3 -70.4 -70.4 ...
    ##  $ yaw_dumbbell        : num  -84.9 -84.7 -85.1 -84.9 -84.9 ...
    ##  $ total_accel_dumbbell: int  37 37 37 37 37 37 37 37 37 37 ...
    ##  $ gyros_dumbbell_x    : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ gyros_dumbbell_y    : num  -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 ...
    ##  $ gyros_dumbbell_z    : num  0 0 0 -0.02 0 0 0 0 0 0 ...
    ##  $ accel_dumbbell_x    : int  -234 -233 -232 -232 -233 -234 -232 -234 -232 -235 ...
    ##  $ accel_dumbbell_y    : int  47 47 46 48 48 48 47 46 47 48 ...
    ##  $ accel_dumbbell_z    : int  -271 -269 -270 -269 -270 -269 -270 -272 -269 -270 ...
    ##  $ magnet_dumbbell_x   : int  -559 -555 -561 -552 -554 -558 -551 -555 -549 -558 ...
    ##  $ magnet_dumbbell_y   : int  293 296 298 303 292 294 295 300 292 291 ...
    ##  $ magnet_dumbbell_z   : num  -65 -64 -63 -60 -68 -66 -70 -74 -65 -69 ...
    ##  $ roll_forearm        : num  28.4 28.3 28.3 28.1 28 27.9 27.9 27.8 27.7 27.7 ...
    ##  $ pitch_forearm       : num  -63.9 -63.9 -63.9 -63.9 -63.9 -63.9 -63.9 -63.8 -63.8 -63.8 ...
    ##  $ yaw_forearm         : num  -153 -153 -152 -152 -152 -152 -152 -152 -152 -152 ...
    ##  $ total_accel_forearm : int  36 36 36 36 36 36 36 36 36 36 ...
    ##  $ gyros_forearm_x     : num  0.03 0.02 0.03 0.02 0.02 0.02 0.02 0.02 0.03 0.02 ...
    ##  $ gyros_forearm_y     : num  0 0 -0.02 -0.02 0 -0.02 0 -0.02 0 0 ...
    ##  $ gyros_forearm_z     : num  -0.02 -0.02 0 0 -0.02 -0.03 -0.02 0 -0.02 -0.02 ...
    ##  $ accel_forearm_x     : int  192 192 196 189 189 193 195 193 193 190 ...
    ##  $ accel_forearm_y     : int  203 203 204 206 206 203 205 205 204 205 ...
    ##  $ accel_forearm_z     : int  -215 -216 -213 -214 -214 -215 -215 -213 -214 -215 ...
    ##  $ magnet_forearm_x    : int  -17 -18 -18 -16 -17 -9 -18 -9 -16 -22 ...
    ##  $ magnet_forearm_y    : num  654 661 658 658 655 660 659 660 653 656 ...
    ##  $ magnet_forearm_z    : num  476 473 469 469 473 478 470 474 476 473 ...
    ##  $ classe              : Factor w/ 5 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...

After the cleanup we now have cleaned training data set which contains
19622 observations and 55 variables, while the test data set contains 20
observations and 55 variables.

Split Training Set into train and validation
--------------------------------------------

Now we will split cleaned training set into a two parts. One to be used
for training (70%) the model and second to be used for validation (30%).
We will use the validation data set to conduct cross validation.

    set.seed(1200) # For reproducibile purpose
    mysplit <- createDataPartition(train$classe, p = 0.70, list = FALSE)
    training <- train[mysplit, ]
    validation <- train[-mysplit, ]

Dataset now consists of 55 variables with the observations divided as
following:  
1. Training Data: 13737 observations.  
2. Validation Data: 5885 observations.  
3. Testing Data: 20 observations.

Exploratory analysis
--------------------

The variable `classe` contains 5 levels. The plot of the outcome
variable shows the frequency of each levels in the Training data.

    plot(training$classe, col="orange", main="Levels of the variable classe", xlab="classe levels", ylab="Frequency")

![](PML_files/figure-markdown_strict/exploranalysis-1.png)

The plot above shows that Level A is the most frequent classe. D appears
to be the least frequent one.

Predictive Models
-----------------

In this section a decision tree and random forest will be applied to the
data.

### Decision Tree

Fit a predictive model for activity recognition using <b>Decision
Tree</b> algorithm.

    modelTree <- rpart(classe ~ ., data = training, method = "class")
    prp(modelTree)

![](PML_files/figure-markdown_strict/unnamed-chunk-6-1.png)

Now, we estimate the performance of the model on the <b>validation</b>
data set.

    predictTree <- predict(modelTree, validation, type = "class")

    confusionMatrix(validation$classe, predictTree)

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1529   54    9   65   17
    ##          B  271  647   62   94   65
    ##          C   21   36  836   75   58
    ##          D   81   19  120  651   93
    ##          E   46   20   66  138  812
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.7604          
    ##                  95% CI : (0.7493, 0.7713)
    ##     No Information Rate : 0.331           
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.6955          
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.7849   0.8338   0.7649   0.6364   0.7770
    ## Specificity            0.9632   0.9037   0.9604   0.9356   0.9442
    ## Pos Pred Value         0.9134   0.5680   0.8148   0.6753   0.7505
    ## Neg Pred Value         0.9005   0.9728   0.9471   0.9244   0.9515
    ## Prevalence             0.3310   0.1319   0.1857   0.1738   0.1776
    ## Detection Rate         0.2598   0.1099   0.1421   0.1106   0.1380
    ## Detection Prevalence   0.2845   0.1935   0.1743   0.1638   0.1839
    ## Balanced Accuracy      0.8740   0.8687   0.8626   0.7860   0.8606

    accuracy <- postResample(predictTree, validation$classe)
    ose <- 1 - as.numeric(confusionMatrix(validation$classe, predictTree)$overall[1])

The Estimated Accuracy of the Decision Tree model Model is 76.0407816%
and the Estimated Out-of-Sample Error is 23.9592184%.

### Random Forest

We fit a predictive model for activity recognition using <b>Random
Forest</b> algorithm because it automatically selects important
variables and is robust to correlated covariates & outliers in general.

We will use <b>5-fold cross validation</b> when applying the algorithm.

    modelRF <- train(classe ~ ., data = training, method = "rf", trControl = trainControl(method = "cv", 5), ntree = 250)
    modelRF

    ## Random Forest 
    ## 
    ## 13737 samples
    ##    54 predictor
    ##     5 classes: 'A', 'B', 'C', 'D', 'E' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 10989, 10989, 10990, 10991, 10989 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  Accuracy   Kappa    
    ##    2    0.9937393  0.9920799
    ##   28    0.9970880  0.9963165
    ##   54    0.9954858  0.9942894
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was mtry = 28.

Now, we estimate the performance of the model on the <b>validation</b>
data set.

    predictRF <- predict(modelRF, validation)

    confusionMatrix(validation$classe, predictRF)

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1672    1    0    0    1
    ##          B    1 1134    4    0    0
    ##          C    0    3 1023    0    0
    ##          D    0    0    1  963    0
    ##          E    0    0    0    0 1082
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9981          
    ##                  95% CI : (0.9967, 0.9991)
    ##     No Information Rate : 0.2843          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9976          
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9994   0.9965   0.9951   1.0000   0.9991
    ## Specificity            0.9995   0.9989   0.9994   0.9998   1.0000
    ## Pos Pred Value         0.9988   0.9956   0.9971   0.9990   1.0000
    ## Neg Pred Value         0.9998   0.9992   0.9990   1.0000   0.9998
    ## Prevalence             0.2843   0.1934   0.1747   0.1636   0.1840
    ## Detection Rate         0.2841   0.1927   0.1738   0.1636   0.1839
    ## Detection Prevalence   0.2845   0.1935   0.1743   0.1638   0.1839
    ## Balanced Accuracy      0.9995   0.9977   0.9973   0.9999   0.9995

    accuracyRF <- postResample(predictRF, validation$classe)
    oseRF <- 1 - as.numeric(confusionMatrix(validation$classe, predictRF)$overall[1])

The Estimated Accuracy of the Random Forest Model is 99.8130841% and the
Estimated Out-of-Sample Error is 0.1869159%.  
Random Forests yielded better Results, as expected. The Random Forest
model is choosen.

Predicting The Manner of Exercise for Test Data Set
---------------------------------------------------

Now, we apply the <b>Random Forest</b> model to the original testing
data set downloaded from the data source. We remove the problem\_id
column first.

    rm(accuracy)
    rm(ose)
    preTest <- predict(modelRF, test[, -length(names(test))])

Generating Files to submit as answers for the Assignment
--------------------------------------------------------

Function to generate files with predictions to submit for assignment.

    pml_write_files = function(x){
      n = length(x)
      for(i in 1:n){
        filename = paste0("./Assignment_Solutions/problem_id_",i,".txt")
        write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
      }
    }

Generating the Files.

    pml_write_files(predict(modelRF, test[, -length(names(test))]))
