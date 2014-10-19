

![](trotter_banner.png) 

## Trotter 0.6
## An R package for arranging arrangements

### Repository contents

* __README.md__ (This readme file.)
* __combinations_pv.R__
* __permutations_pv.R__
* __selections_pv.R__ (Combinations with repetition allowed.)
* __amalgams_pv.R__ (Permutations with repetition allowed.)
* __subsets_pv.R__
* __trotter_helpers.R__

### Introduction

The `trotter` package provides the R user access to five classes - `PPV` (Permutations Pseudo-Vector), `CPV` (Combinations Pseudo-Vector), `SPV` (Selections Pseudo-Vector), `APV` (Amalgams Pseudo-Vector) and `SSPV`` (Subsets Pseudo-Vector) - that simplify the process of working with permutations, combinations and subsets of objects stored in a vector. 

I use the term "selections" for combinations in which the same object may be taken more than once (e.g., a selection of three scoops of icecream from ten different flavours - all three scoops could be of the same flavour), and "amalgams" for permutations in which the same object can be taken more than once (e.g., an arrangement of coloured pegs in the board game Mastermind).

Instances of these classes are compatable with the functions:

* `length`
* `[`

The constructors for classes `PPV`, `CPV`, `SPV` and `APV` have two arguments are simply have the same names but with lowercase letters and take two arguments: 

* `k` the number of objects to take.
* `items` a vector containing the available objects.

The constructor for class `SSPV` is but only takes the argument `items`.

We can think about instances of these classes as vectors containing all the possible arrangements with the given parameters. (They are pseudo-vectors because they don't actually store the arrangements in memory and can thus be used to "store" very large numbers of arrangements.)

### Example: basic use

The `trotter` package has been uploaded to CRAN, so we can install it using:

```r
install.packages("trotter")
```

Once it is installed we can create some objects to represent structures commonly encountered in combinatorics. 

```r
library(trotter)
a.to.j <- letters[1:10]

perms.5 <- ppv(k = 5, items = a.to.j)
combs.5 <- cpv(k = 5, items = a.to.j)

# How many 5-permutations are there of the first 10 letters?
length(perms.5)
```

```
## [1] 30240
```

```r
# Let's inspect a few of them...
perms.5[1]
```

```
## [1] "a" "b" "c" "d" "e"
```

```r
perms.5[10000]
```

```
## [1] "a" "f" "c" "g" "j"
```

```r
perms.5[30240]
```

```
## [1] "g" "f" "h" "i" "j"
```

```r
# How many 5-combinations?
length(combs.5)
```

```
## [1] 252
```

```r
# Let's inspect a few of them...
combs.5[1]
```

```
## [1] "a" "b" "c" "d" "e"
```

```r
combs.5[100]
```

```
## [1] "a" "d" "e" "h" "j"
```

```r
combs.5[252]
```

```
## [1] "f" "g" "h" "i" "j"
```

```r
# The pseudo-vectors can "store" a large number of arrangements
# while being very easy on the memory and evaluation time...
perms.10.of.15 <- ppv(10, letters[1:15])
perms.10.of.15
```

```
## Instance of class PPV
##  Pseudo-vector containing 10897286400 10-permutations
##  of items taken from the list:
## [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o]
```

```r
perms.10.of.15[1]
```

```
##  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
```

```r
perms.10.of.15[10000000000]
```

```
##  [1] "m" "k" "j" "d" "e" "g" "f" "i" "c" "n"
```

```r
perms.10.of.15[10897286400]
```

```
##  [1] "g" "f" "h" "i" "j" "k" "l" "m" "n" "o"
```

### Example: application
The following example illustrates an application in which we are interested in finding which 3-combination from a set of predictors produces a linear model with a greatest R-squared value.


```r
# Load and inspect the mtcars data set 
data(mtcars)
head(mtcars)
```

```
##                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
## Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
## Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
## Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
## Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
## Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
## Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
```

```r
# Create a vector containing the variables we wish to treat as predictors
predictors <- names(mtcars)[2:11]

# Load the trotter library
library(trotter)

# Create pseudo-vector of 3-combinations of predictors
pred3c <- cpv(3, predictors)

# A helper-function that returns a linear model from a 
# combination of variables
model.from.combo <- function(combo) lm(
  as.formula(
    sprintf(
      "mpg ~ %s",
      paste(
        combo,
        collapse = "+"
      )
    )
  ),
  data = mtcars
)

# Create an array containing all the possible R-squared values
r.squared.values <- sapply(
  1:length(pred3c),
  function(i) summary(model.from.combo(pred3c[i]))$r.squared
)

# Examine the distribution of R-squared values possible in a
# linear fitting using three predictors

hist(
  r.squared.values,
  main = "Possible R-squared values from three predictors",
  xlab = "R-squared"
)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
# Retrieve the model that produced the greatest R-squared value
index <- which.max(r.squared.values)
best.three.predictors <- pred3c[index]
best.three.predictors
```

```
## [1] "wt"   "qsec" "am"
```

```r
best.three.model <- model.from.combo(best.three.predictors)
summary(best.three.model)
```

```
## 
## Call:
## lm(formula = as.formula(sprintf("mpg ~ %s", paste(combo, collapse = "+"))), 
##     data = mtcars)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -3.481 -1.556 -0.726  1.411  4.661 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    9.618      6.960    1.38  0.17792    
## wt            -3.917      0.711   -5.51    7e-06 ***
## qsec           1.226      0.289    4.25  0.00022 ***
## am             2.936      1.411    2.08  0.04672 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.46 on 28 degrees of freedom
## Multiple R-squared:  0.85,	Adjusted R-squared:  0.834 
## F-statistic: 52.7 on 3 and 28 DF,  p-value: 1.21e-11
```

Let's say that we want to look at all possible linear models of this data.
This would entail going through all combinations of _any_ length of the predictors.
Here we can use the `Subsets Pseudo-Vector` class to iterate through all possible subsets:


```r
# Create a pseudo-vector "containing" all possible subsets.
predictor.subsets <- sspv(predictors)

# How many possible linear models are there?
# (i.e. How many subsets of the predictors exist?)
length(predictor.subsets)
```

```
## [1] 1024
```

```r
# Create an array containing all the possible adjusted R-squared values
adj.r.squared.values <- sapply(
  # Start at index 2: the first subset (with none of the predictors) is NULL
  2:length(predictor.subsets), 
  function(i) summary(model.from.combo(predictor.subsets[i]))$adj.r.squared
)

# Examine the distribution of adjusted R-squared values possible in a
# linear fitting using any subset of the predictors predictors

hist(
  adj.r.squared.values,
  main = "Possible Adjusted R-squared values from any subset of predictors",
  xlab = "Adjusted R-squared"
)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
# Retrieve the model that produced the greatest adjusted R-squared value
index <- which.max(adj.r.squared.values)
best.predictors.subset <- predictor.subsets[index]
best.predictors.subset
```

```
## [1] "cyl"  "hp"   "wt"   "qsec" "am"
```

```r
best.subset.model <- model.from.combo(best.predictors.subset)
summary(best.subset.model)
```

```
## 
## Call:
## lm(formula = as.formula(sprintf("mpg ~ %s", paste(combo, collapse = "+"))), 
##     data = mtcars)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -3.535 -1.531 -0.167  1.230  4.611 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)   
## (Intercept)  19.3471    13.4095    1.44   0.1610   
## cyl          -0.1488     0.7392   -0.20   0.8421   
## hp           -0.0169     0.0149   -1.14   0.2653   
## wt           -3.1517     1.0027   -3.14   0.0041 **
## qsec          0.7382     0.5736    1.29   0.2095   
## am            2.7294     1.7243    1.58   0.1255   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.48 on 26 degrees of freedom
## Multiple R-squared:  0.858,	Adjusted R-squared:  0.831 
## F-statistic: 31.4 on 5 and 26 DF,  p-value: 3.1e-10
```

### Some details

The permutations "contained" in a `PPV` instance can be thought of as having an arrangement similar (but not necessarily the same) to that produced by the [Steinhaus-Johnson-Trotter](http://en.wikipedia.org/wiki/Steinhaus%E2%80%93Johnson%E2%80%93Trotter_algorithm) algorithm. The combinations "contained" in a `CPV` instance are arranged in an order corresponding to the order of object appearance in the vector `items`; that is, all the combinations containing the first object are followed by all the combinations containing the second object but not the first, which are followed by all the combinations containing the third object but neither the first or second, etc.
