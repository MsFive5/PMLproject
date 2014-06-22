Practical Machine Learning Class Project
========================================================

We can start with loading the data and useful packages.




```r
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")
library(caret)
library(randomForest)
```


9DoF sensor data including magnetometers, gyroscopes, accelerometers and yaw-pitch-roll orientation for the four sensors are used in the model.


```r
mod <- randomForest(classe ~ roll_belt + pitch_belt + pitch_belt + gyros_belt_x + 
    gyros_belt_y + gyros_belt_z + accel_belt_x + accel_belt_y + accel_belt_z + 
    magnet_belt_x + magnet_belt_y + magnet_belt_z + roll_arm + pitch_arm + pitch_arm + 
    gyros_arm_x + gyros_arm_y + gyros_arm_z + accel_arm_x + accel_arm_y + accel_arm_z + 
    magnet_arm_x + magnet_arm_y + magnet_arm_z + roll_dumbbell + pitch_dumbbell + 
    pitch_dumbbell + gyros_dumbbell_x + gyros_dumbbell_y + gyros_dumbbell_z + 
    accel_dumbbell_x + accel_dumbbell_y + accel_dumbbell_z + magnet_dumbbell_x + 
    magnet_dumbbell_y + magnet_dumbbell_z + roll_forearm + pitch_forearm + pitch_forearm + 
    gyros_forearm_x + gyros_forearm_y + gyros_forearm_z + accel_forearm_x + 
    accel_forearm_y + accel_forearm_z + magnet_forearm_x + magnet_forearm_y + 
    magnet_forearm_z, data = training)
```


The confusion matrix shows that this model is a good fit for the training dataset.

```r
mod$confusion
```

```
##      A    B    C    D    E class.error
## A 5578    1    0    0    1   0.0003584
## B   10 3783    4    0    0   0.0036871
## C    0   14 3406    2    0   0.0046756
## D    0    0   32 3182    2   0.0105721
## E    0    0    0    7 3600   0.0019407
```

Now, we evaluate the model with cross-validation.

```r
set.seed(12345)
error <- c()
for (i in 1:10) {
    intrain = sample(1:dim(training)[1], size = dim(training)[1]/10 * 9, replace = F)
    trainCross = training[intrain, ]
    testCross = training[-intrain, ]
    modCross <- randomForest(classe ~ roll_belt + pitch_belt + pitch_belt + 
        gyros_belt_x + gyros_belt_y + gyros_belt_z + accel_belt_x + accel_belt_y + 
        accel_belt_z + magnet_belt_x + magnet_belt_y + magnet_belt_z + roll_arm + 
        pitch_arm + pitch_arm + gyros_arm_x + gyros_arm_y + gyros_arm_z + accel_arm_x + 
        accel_arm_y + accel_arm_z + magnet_arm_x + magnet_arm_y + magnet_arm_z + 
        roll_dumbbell + pitch_dumbbell + pitch_dumbbell + gyros_dumbbell_x + 
        gyros_dumbbell_y + gyros_dumbbell_z + accel_dumbbell_x + accel_dumbbell_y + 
        accel_dumbbell_z + magnet_dumbbell_x + magnet_dumbbell_y + magnet_dumbbell_z + 
        roll_forearm + pitch_forearm + pitch_forearm + gyros_forearm_x + gyros_forearm_y + 
        gyros_forearm_z + accel_forearm_x + accel_forearm_y + accel_forearm_z + 
        magnet_forearm_x + magnet_forearm_y + magnet_forearm_z, data = trainCross)
    prediction <- predict(modCross, testCross)
    e <- sum(prediction != testCross$classe)/length(prediction)
    error <- c(error, e)
}
```


The cross-validation error rate is shown below.

```r
error
```

```
##  [1] 0.003057 0.006623 0.003566 0.001528 0.003566 0.003057 0.004585
##  [8] 0.002547 0.006113 0.005094
```

```r
mean(error)
```

```
## [1] 0.003974
```


Now that the proposed model is satisfactory. We can apply it to the testing set and generate the result as below

```r
predict(mod, testing)
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```

```r
pml_write_files = function(x) {
    n = length(x)
    for (i in 1:n) {
        filename = paste0("problem_id_", i, ".txt")
        write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
            col.names = FALSE)
    }
}
pml_write_files(predict(mod, testing))
```

