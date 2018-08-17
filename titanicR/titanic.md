What Do We Have?
----------------

We have two data sets, train.csv & test.csv. Here's the structure of
train:

    str(train)

    ## 'data.frame':    891 obs. of  12 variables:
    ##  $ PassengerId: int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Survived   : int  0 1 1 1 0 0 0 0 1 1 ...
    ##  $ Pclass     : int  3 1 3 1 3 3 1 3 3 2 ...
    ##  $ Name       : Factor w/ 891 levels "Abbing, Mr. Anthony",..: 109 191 358 277 16 559 520 629 417 581 ...
    ##  $ Sex        : Factor w/ 2 levels "female","male": 2 1 1 1 2 2 2 2 1 1 ...
    ##  $ Age        : num  22 38 26 35 35 NA 54 2 27 14 ...
    ##  $ SibSp      : int  1 1 0 1 0 0 0 3 0 1 ...
    ##  $ Parch      : int  0 0 0 0 0 0 0 1 2 0 ...
    ##  $ Ticket     : Factor w/ 681 levels "110152","110413",..: 524 597 670 50 473 276 86 396 345 133 ...
    ##  $ Fare       : num  7.25 71.28 7.92 53.1 8.05 ...
    ##  $ Cabin      : Factor w/ 148 levels "","A10","A14",..: 1 83 1 57 1 1 131 1 1 1 ...
    ##  $ Embarked   : Factor w/ 4 levels "","C","Q","S": 4 2 4 4 4 3 4 4 4 2 ...

Here's test:

    str(test)

    ## 'data.frame':    418 obs. of  11 variables:
    ##  $ PassengerId: int  892 893 894 895 896 897 898 899 900 901 ...
    ##  $ Pclass     : int  3 3 2 3 3 3 3 2 3 3 ...
    ##  $ Name       : Factor w/ 418 levels "Abbott, Master. Eugene Joseph",..: 210 409 273 414 182 370 85 58 5 104 ...
    ##  $ Sex        : Factor w/ 2 levels "female","male": 2 1 2 2 1 2 1 2 1 2 ...
    ##  $ Age        : num  34.5 47 62 27 22 14 30 26 18 21 ...
    ##  $ SibSp      : int  0 1 0 0 1 0 0 1 0 2 ...
    ##  $ Parch      : int  0 0 0 0 1 0 0 1 0 0 ...
    ##  $ Ticket     : Factor w/ 363 levels "110469","110489",..: 153 222 74 148 139 262 159 85 101 270 ...
    ##  $ Fare       : num  7.83 7 9.69 8.66 12.29 ...
    ##  $ Cabin      : Factor w/ 77 levels "","A11","A18",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Embarked   : Factor w/ 3 levels "C","Q","S": 2 3 2 3 3 3 2 3 1 3 ...

There are a couple of important things to notice here. First, the train
data has an extra column. The test data does not have 'Survived' column.
That's because Kaggle isn't about to give us the answer key. Once we
build our model against the training data, we fit it to the test data
and generate our submission which we then submit to be scored on
Kaggle.com.

    summary(train$Age)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.42   20.12   28.00   29.70   38.00   80.00     177

Secondly, looking at the training data summary indicates that there are
a lot of missing ages. I'm certainly going to have to do something about
that. I'll be imputing the data (If you're new, Google impute data) in a
number of different ways. That'll come later.

    head(train$Name)

    ## [1] Braund, Mr. Owen Harris                            
    ## [2] Cumings, Mrs. John Bradley (Florence Briggs Thayer)
    ## [3] Heikkinen, Miss. Laina                             
    ## [4] Futrelle, Mrs. Jacques Heath (Lily May Peel)       
    ## [5] Allen, Mr. William Henry                           
    ## [6] Moran, Mr. James                                   
    ## 891 Levels: Abbing, Mr. Anthony ... Zimmerman, Mr. Leo

Something to consider is the Name column. It may be possible to be a
little clever and split those names up to get the titles as they
sometimes indicate age. For instance, Master would be for a boy, Mr. and
Mrs. would indicate adults probably, though it's a pretty large bucket.
Miss is a bit of a gotcha, as it could either be a young girl or an old
woman. I'm not sure it's entirely worth the effort. We'll see.