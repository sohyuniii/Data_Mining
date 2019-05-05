XGBoost R Tutorial
================
IMSOHYUN
2019년 5월 5일

### Dataset Loading

``` r
require(xgboost)
```

    ## Loading required package: xgboost

``` r
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
str(train)
```

    ## List of 2
    ##  $ data :Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
    ##   .. ..@ i       : int [1:143286] 2 6 8 11 18 20 21 24 28 32 ...
    ##   .. ..@ p       : int [1:127] 0 369 372 3306 5845 6489 6513 8380 8384 10991 ...
    ##   .. ..@ Dim     : int [1:2] 6513 126
    ##   .. ..@ Dimnames:List of 2
    ##   .. .. ..$ : NULL
    ##   .. .. ..$ : chr [1:126] "cap-shape=bell" "cap-shape=conical" "cap-shape=convex" "cap-shape=flat" ...
    ##   .. ..@ x       : num [1:143286] 1 1 1 1 1 1 1 1 1 1 ...
    ##   .. ..@ factors : list()
    ##  $ label: num [1:6513] 1 0 0 1 0 0 0 1 0 0 ...

### Basic Training using XGBoost

``` r
# objective = "binary:logistic": we will train a binary classification model ;
# ◾max_depth = 2: the trees won’t be deep, because our case is very simple ;
# ◾nthread = 2: the number of cpu threads we are going to use;
# ◾nrounds = 2: there will be two passes on the data, the second one will enhance the model by further reducing the difference between ground truth and prediction.

bstSparse <- xgboost(data = train$data, label = train$label, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")
```

    ## [1]  train-error:0.046522 
    ## [2]  train-error:0.022263

``` r
bstDense <- xgboost(data = as.matrix(train$data), label = train$label, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")
```

    ## [1]  train-error:0.046522 
    ## [2]  train-error:0.022263

``` r
dtrain <- xgb.DMatrix(data = train$data, label = train$label)
bstDMatrix <- xgboost(data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")
```

    ## [1]  train-error:0.046522 
    ## [2]  train-error:0.022263

``` r
# verbose = 0, no message
bst <- xgboost(data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 1)
```

    ## [1]  train-error:0.046522 
    ## [2]  train-error:0.022263

``` r
# verbose = 2, also print information about tree
bst <- xgboost(data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 2)
```

    ## [1]  train-error:0.046522 
    ## [2]  train-error:0.022263

### Basic prediction using XGBoost

``` r
pred <- predict(bst, test$data)
prediction <- as.numeric(pred > 0.5)
err <- mean(as.numeric(pred > 0.5) != test$label)
print(paste("test-error=", err))
```

    ## [1] "test-error= 0.0217256362507759"

### Advanced features

``` r
dtrain <- xgb.DMatrix(data = train$data, label=train$label)
dtest <- xgb.DMatrix(data = test$data, label=test$label)

watchlist <- list(train=dtrain, test=dtest)

bst <- xgb.train(data=dtrain, max_depth=2, eta=1, nthread = 2, nrounds=2, watchlist=watchlist, objective = "binary:logistic")
```

    ## [1]  train-error:0.046522    test-error:0.042831 
    ## [2]  train-error:0.022263    test-error:0.021726

``` r
bst <- xgb.train(data=dtrain, max_depth=2, eta=1, nthread = 2, nrounds=2, watchlist=watchlist, eval_metric = "error", eval_metric = "logloss", objective = "binary:logistic")
```

    ## [1]  train-error:0.046522    train-logloss:0.233366  test-error:0.042831 test-logloss:0.226687 
    ## [2]  train-error:0.022263    train-logloss:0.136656  test-error:0.021726 test-logloss:0.137875

``` r
bst <- xgb.train(data=dtrain, booster = "gblinear", max_depth=2, nthread = 2, nrounds=2, watchlist=watchlist, eval_metric = "error", eval_metric = "logloss", objective = "binary:logistic")
```

    ## [1]  train-error:0.014126    train-logloss:0.192128  test-error:0.017381 test-logloss:0.197006 
    ## [2]  train-error:0.005527    train-logloss:0.086203  test-error:0.007449 test-logloss:0.090275
