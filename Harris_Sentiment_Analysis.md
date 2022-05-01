Final Project: Analysis 1 by Louis Harris
================

# Analysis #1 - Sentiment Analysis vs. Quantitative Review Scores

The purpose of this analysis is to observe the relationship between the
sentiment analysis of qualitative summaries for certain products, and
their quantitative review scores.

Sentiment values were collected using the bing sentiment lexicon. NRC
was originally considered, but the calculations resulted in fatal errors
in RStudio and loss of progress.

## Step 1: Loading Libraries

Here, libraries that are needed for data importation, analysis, and
presentation are loaded.

``` r
#####################################
###      LIBRARY INFORMATION      ###
#####################################
#for loading the cleaned data
library(data.table)
```

    ## Warning: package 'data.table' was built under R version 4.1.3

``` r
#text mining library, if needed
library(tm)
```

    ## Warning: package 'tm' was built under R version 4.1.2

    ## Loading required package: NLP

``` r
#multi-function sentiment analysis library (includes lexicons and NRC sentiment analysis)
library(syuzhet)
```

    ## Warning: package 'syuzhet' was built under R version 4.1.3

``` r
#for creating visual analytic graphs
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 4.1.3

    ## 
    ## Attaching package: 'ggplot2'

    ## The following object is masked from 'package:NLP':
    ## 
    ##     annotate

``` r
#expanded ggplot functionality
library(ggpubr)
```

    ## Warning: package 'ggpubr' was built under R version 4.1.3

## Step 2: Preparing Data for Analysis

During this step, the data set is loaded in and assigned to a variable.
The required data for the analysis is taken from the original set and
placed into a separate data set.

In this case, the information that is needed is index value, the review
summary text, and the actual review score.

``` r
################################
###      DATA PREP WORK      ###
################################
#import cleaned dataset
foods <- fread(file = "C:/Users/louis/Desktop/Text Mining Project/Text Mining Project/Data/finefoods.csv")

###BASIC PREP
#assign dataset to dataframe
reviewSummary <- as.data.frame(foods$`review/summary`)


###SENTIMENT ANALYSIS PREP
#create dataframe with only the columns that will be analysed 
reviewText <- foods[, c("V1","review/summary","review/score")]
#convert summary text to vector to prepare for sentiment analysis
review1 <- as.character(reviewText$`review/summary`)
```

Provided are examples of what each set looks like.

``` r
head(reviewSummary)
```

    ##     foods$`review/summary`
    ## 1    Good Quality Dog Food
    ## 2        Not as Advertised
    ## 3  ""Delight"" says it all
    ## 4           Cough Medicine
    ## 5              Great taffy
    ## 6               Nice Taffy

``` r
head(review1)
```

    ## [1] "Good Quality Dog Food"        "Not as Advertised"           
    ## [3] " \"\"Delight\"\" says it all" "Cough Medicine"              
    ## [5] "Great taffy"                  "Nice Taffy"

``` r
head(reviewText)
```

    ##    V1           review/summary review/score
    ## 1:  0    Good Quality Dog Food            5
    ## 2:  1        Not as Advertised            1
    ## 3:  2  ""Delight"" says it all            4
    ## 4:  3           Cough Medicine            2
    ## 5:  4              Great taffy            5
    ## 6:  5               Nice Taffy            4

## Step 3a: Analysis

The originally planned NRC analysis, however, the original data source
proved to be too large and command execution would result in RStudio
crashing.  
Both the NRC and bing sentiment analyses were ran with the *syuzhet*
library.

``` r
###NRC SENTIMENT ANALYSIS###
#nrc sentiment analysis - ran into issues with commands crashing rstudio, so this part was not included
#assign sentiment analysis results to variable
x <- get_nrc_sentiment(review1) 

#filter out only positive and negative sentiments and assign to a seperate variable
posneg <- x[, c("positive","negative")]
review <- cbind(reviewText, posneg) 

#create dataframe with index value, summary text, positive and negative values, the overall sentiment value, and the actual review score
totalSent <- as.data.frame(posneg$positive - posneg$negative)
reviewProperOrder <- cbind(foods$V1, foods$`review/summary`, posneg, totalSent, foods$`review/score`)
```

This is what the NRC sentiment analysis table would have looked like if
it were completed. ![NRC](nrc.JPG)

Since the NRC sentiment analysis didn’t work out, the bing lexixon was
used for sentiment analysis.

``` r
###BING SENTIMENT ANALYSIS###
#similar dataframe, but using the "bing" lexicon
bing <- as.data.frame(get_sentiment(review1, method = "bing"))
reviewBing <- cbind(foods$V1, foods$`review/summary`, bing, foods$`review/score`)

#rename columns for simplicity
names(reviewBing) [1] <- 'Index'
names(reviewBing) [2] <- 'Summary'
names(reviewBing) [3] <- 'Bing'
names(reviewBing) [4] <- 'Review'

head(reviewBing)
```

    ##   Index                  Summary Bing Review
    ## 1     0    Good Quality Dog Food    1      5
    ## 2     1        Not as Advertised    0      1
    ## 3     2  ""Delight"" says it all    1      4
    ## 4     3           Cough Medicine    0      2
    ## 5     4              Great taffy    1      5
    ## 6     5               Nice Taffy    1      4

## Step 3b: Visual Analysis w/ Graphics

To get a basic grasp on the data that has been collected we begin with
some basic information.

``` r
#Frequency table for bing sentiment analysis
table(reviewBing$Bing)
```

    ## 
    ##     -6     -5     -4     -3     -2     -1      0      1      2      3      4 
    ##      1      4     14    281   3929  47758 192535 275368  43728   4403    397 
    ##      5      6      7 
    ##     32      3      1

``` r
#mean bing sentiment analysis value
mean(reviewBing$Bing)
```

    ## [1] 0.5651557

``` r
#histogram for bing sentiment values/ with mean value as a red, dashed, vertical line
ggplot(reviewBing, aes(x=Bing)) + 
  geom_histogram(binwidth = 1, center = -6, colour="black", fill="white") +
  ggtitle("Bing Sentiment Value Histogram") +
  xlab("Bing Sentiment Value") +
  ylab("Count") +
  geom_vline(aes(xintercept=mean(Bing)),
             color="red", linetype="dashed", size=1) 
```

![](Harris_Sentiment_Analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
Just off of these three basic analytic functions, we can assume based on
the sentiment analysis results that **there will be a large number of
positive reviews**. The frequency table and histogram show how to
quantity leans towards the positive side heavily. And the **positive
mean value** shows that **overall, the reviews are positive.**

``` r
#histogram of actual quantitative reviews
ggplot(reviewBing, aes(x=Review)) + 
  geom_histogram(binwidth = 1, colour="black", fill="white") +
  ggtitle("Review Score Histogram") +
  xlab("Review Score (1-5)") +
  ylab("Count")
```

![](Harris_Sentiment_Analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
This histogram of the actual reviews collected confirms the previous
assumption that was made using sentiment analysis results. Since the
quantitative scores and qualitative summaries support one another, **we
can suggest a potential relationship between the two variables**.

**To test this further, we run a Pearson correlation test to see if this
is true. Also, to make sure that the function doesn’t have any inherent
flaws, I’ve ran it with the variables in both potential orders.**

``` r
#Pearson correlation test - ran with both variable orders in the off chance of a function error  
cor.test(reviewBing$Bing, reviewBing$Review , method = "pearson")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  reviewBing$Bing and reviewBing$Review
    ## t = 336.92, df = 568452, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.4058168 0.4101506
    ## sample estimates:
    ##      cor 
    ## 0.407986

``` r
cor.test(reviewBing$Review, reviewBing$Bing, method = "pearson")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  reviewBing$Review and reviewBing$Bing
    ## t = 336.92, df = 568452, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.4058168 0.4101506
    ## sample estimates:
    ##      cor 
    ## 0.407986

In both cases, **the p-value is much lower than 0.05**, so we can
**reject** the **null hypothesis** and confirm that the **sentiment
analysis values and the quantitative scores relate to one another**.

## Step 4: Conclusions

### The sentiment analysis results pointed to the reviews being majority positive, this was supported by the actual quantitative review scores. Since both variables matched one another in their predictions, a Pearson correlation test was ran, which confirmed that they had a statistically signification relationship.

### With all of these points, we can confirm that in this data set, when using the bing lexicon, **the sentiment analysis of the summary text aligned with the quantitative reviews.**

save.image(file = “/.RData2”, version = NULL, ascii = FALSE, compress =
!ascii, safe = TRUE)
