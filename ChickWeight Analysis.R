##########################################################################
################## Group project: ChickWeight Analysis ###################
##########################################################################

##########################################################
################## Description ###########################
##########################################################

  # This group project performs an in-depth statistical analysis of the R data preset “ChickWeight” (Weight versus 
  # age of chicks on different diets). The aim was to find out which variables affect the weight of the chicken and 
  # to formulate conclusions as if I was helping the farmer who gathered the data and who want the chicken to be as 
  # heavy as possible so he or she could sell them for a higher price.
  
  # It includes the following statistical analyses:
    
  # - Descriptive statistics
  # - Factor analysis
  # - Cluster analysis
  # - ANOVA
  # - Regression Model
  
  # The code is well commented, both from a technical as well as statistical standpoint. My aim was that anyone who 
  # finds my code on github would be able to understand, modify and use it. It is split into a lot of sections which 
  # can be collapsed. Experience R coders can therefore easily skip sections that are not relevant for them.

##########################################################
################## Clean R ###############################
##########################################################

  # I follow a general routine before any session, which mainly includes loading the libraries necessary and
  # setting my working directory. Before I close the session, I save my workspace
  
  # the getwd function shows your current working directory
  getwd()

  # the setwd function lets you specify your desired working directory
  setwd("INSERT YOUR DESIRED WORKING DIRECTORY HERE")
  
  # function to save the workspace, I use it before terminating any session. Don't forget the .RData ending
  base::save.image(file = "/INSERT YOUR DESIRED WORKING DIRECTORY HERE.RData")
  
  # function to clean the global environment. I use it when switching between sessions
  rm(list=ls())
  
  # function to load the workspace again
  load("INSERT YOUR DESIRED WORKING DIRECTORY HERE.RData")
  
  # in case the reader don't have the following packages installed yet, I would encourage him or her to do so.
  # it is a collection of packages that I at least to some extent used in other projects and quite a few of
  # the packages will be used in this project as well. If you already installed those packages, just run the
  # library code after this section before you start with the analysis code
  install.packages("lme4")
  install.packages("readxl")
  install.packages("tidyverse")
  install.packages("haven")
  install.packages("rjson")
  install.packages("nycflights13")
  install.packages("dplyr")
  install.packages("tidyr")
  install.packages("stringr")
  install.packages("ggplot2")
  install.packages("reshape2")
  install.packages("gridExtra")
  install.packages("readr")
  install.packages("rvest")
  install.packages("xml2")
  install.packages("Hmisc")
  install.packages("quanteda")
  install.packages("MASS")  
  install.packages("foreign") 
  install.packages("psych")
  install.packages("memisc")
  install.packages("tidytext")
  install.packages("tm")
  install.packages("validate")
  install.packages("UpSetR")
  install.packages("lindia")
  install.packages("naniar")
  install.packages("deducorrect")
  install.packages("visdat")
  install.packages("DMwR")
  install.packages("rpart")
  install.packages("mlbench")
  install.packages("simputation")
  install.packages("MASS")
  install.packages("OutliersO3")
  install.packages("outliers")
  install.packages("rpart.plot")
  install.packages("rattle")
  install.packages("caret")
  install.packages("lubridate")
  install.packages("twitteR")
  install.packages("rtweet")
  install.packages("wordcloud")
  install.packages("RColorBrewer")
  install.packages("SnowballC")
  install.packages("rvest")
  install.packages("imputeTS")
  install.packages("stats")
  install.packages("ggdendro")
  install.packages("ape")
  install.packages("missForest")
  install.packages("ggsn")
  install.packages("car")
  install.packages("lsr")
  install.packages("agricolae")
  install.packages("sjstats")
  install.packages("gpairs")
  install.packages("DAAG")
  install.packages("corrplot")
  install.packages("interactions")
  install.packages("dataPreparation")
  install.packages("broom")
  install.packages("lm.beta")
  install.packages("glmnet")
  install.packages("plotmo")
  install.packages("scatterplot3d")
  install.packages("lmtest")
  install.packages("ggfortify")
  install.packages("rgl")
  
  # run the library code for each of the packages which is necessary in order to use it afterwards
  library(lme4)
  library(readxl)
  library(tidyverse)
  library(haven)
  library(rjson)
  library(nycflights13)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(reshape2)
  library(gridExtra)
  library(readr)
  library(rvest)
  library(xml2)
  library(Hmisc)
  library(quanteda)
  library(MASS)  
  library(foreign) 
  library(psych)
  library(memisc)
  library(tidytext)
  library(tm)
  library(validate)
  library(UpSetR)
  library(lindia)
  library(naniar)
  library(deducorrect)
  library(visdat)
  library(DMwR)
  library(rpart)
  library(mlbench)
  library(simputation)
  library(MASS)
  library(OutliersO3)
  library(outliers)
  library(rpart.plot)
  library(rattle)
  library(caret)
  library(lubridate)
  library(twitteR)
  library(rtweet)
  library(wordcloud)
  library(RColorBrewer)
  library(SnowballC)
  library(rvest)
  library(imputeTS)
  library(stats)
  library(ggdendro)
  library(ape)
  library(missForest)
  library(ggsn)
  library(car)
  library(lsr)
  library(agricolae)
  library(sjstats)
  library(gpairs)
  library(DAAG)
  library(corrplot)
  library(interactions)
  library(dataPreparation)
  library(broom)
  library(lm.beta)
  library(glmnet)
  library(plotmo)
  library(scatterplot3d)
  library(lmtest)
  library(ggfortify)
  library(rgl)  

##########################################################
################## Descriptive statistics ################
##########################################################

  ####################
  ##### Basics #######
  ####################
  
  # the ChickWeight data is a preset in R
  # i want to have it in my global environment so I insert it into a variable with a slightly same name
  ChickWeight <- ChickWeight
  
  # In case the reader is not familiar with the ChickWeight dataset yet, run the following code to get an overview
  ?ChickWeight
  
  # there are various nice functions to get a basic summary of my data
  str(ChickWeight)
  glimpse(ChickWeight)
  summary(ChickWeight)
  describe(ChickWeight)
  class(ChickWeight)
  
  # personally, I prefer the following one to get a basic overview
  str(ChickWeight)
  
  # and then get a simple statistical analysis by using
  describe(ChickWeight)
  
  ####################
  ##### "weight" distribution plot #######
  ####################
  
  # as a next step, I want to display the ditributions of each of the varibles
  # we start with the "weight". I really like the pipe operator %>% and will use it throughout my code
  ChickWeight %>%
    # i use the ggplot2 package for all my visualizations. First: specify what is plotted
    ggplot(aes(x=weight))  +
    # then, specify which graph we want. To display the distribution, I take a simple histogram
    # i add a html color code that I like and define the binwidth
    geom_histogram(fill=c("#C0C0C0"), binwidth = 5) +
    # finally, i add some simple labels so my plot looks nice
    labs(x = "body weight of the chick (gm)",
         y = "Count",
         title = "Body weight histogram",
         caption = "Source: ChickWeight") +
    # and take one of the ggplot2 themes that I like
    theme_light()
  
  #CONCLUSION: a lot of chicken have a rather light body weight. This is also reflected by the mean, which is 121.82
  
  ####################
  ##### "Time" distribution plot #######
  ####################
  
  # I repeat the process for the time variable
  ChickWeight %>%
    ggplot(aes(x=Time))  +
    # only thing I need to adjust is the binwidth
    geom_histogram(fill=c("#C0C0C0"), binwidth = 0.5) +
    labs(x = "Number of days since birth when the measurement was made",
         y = "Count",
         title = "Days since birth histogram",
         caption = "Source: ChickWeight") +
    theme_light()
  
  #CONCLUSION: it seems that measurements were done every 2 days. if a measurement was done, it always included
  # between 45 and 50 chicken
  
  ####################
  ##### "Chick" variable #######
  ####################
  
  # for the chick varibale, it doesn't make sense to display a histogram, because it is an ordered factor. However,
  # by using the describe function again, we can see that the range is equal to 49, which means that the weight of
  # 49 different chicken was measured overall on different days after their birth
  describe(ChickWeight)
  
  ####################
  ##### "Diet" distribution plot #######
  ####################
  
  # I repeat the process for the diet variable. However, I therefore have to change the format of the variable to numeric
  # I do so by creating a help variable. Furthermore, this variable needs to be a data.frame in order to be plotted in a histogram
  ChickenVariable1 <- data.frame(as.numeric(ChickWeight$Diet))

  ChickenVariable1 %>% 
    ggplot(aes(x=as.numeric.ChickWeight.Diet.))  +
    geom_histogram(fill=c("#C0C0C0"), binwidth = 0.5) +
    labs(x = "Number of measurements of each diet",
         y = "Count",
         title = "Experimental diet which the chicken received",
         caption = "Source: ChickWeight") +
    theme_light()
  
  #CONCLUSION: there where around double the amount of measurements conducted for the first diet. This means
  # that there were more chicken on diet 1
  
  #FINAL COMMENT: by using some statistical analysis functions and plotting some distributions, I could gain
  # a good first overview over the data that I want to analyze. I can now proceed with further analyses.
  
##########################################################
################## Factor analysis #######################
##########################################################
  
  ##################################
  ##### THEORETICAL BACKGROUND #####
  ##################################
  
  # for the theoretical basis, I checked out the following Wikipedia link
  # https://en.wikipedia.org/wiki/Factor_analysis
  
  ### Wikipedia description ###
  # Factor analysis is a statistical method used to describe variability among observed, correlated variables in 
  # terms of a potentially lower number of unobserved variables called factors. For example, it is possible that 
  # variations in six observed variables mainly reflect the variations in two unobserved (underlying) variables. 
  # Factor analysis searches for such joint variations in response to unobserved latent variables. The observed 
  # variables are modelled as linear combinations of the potential factors, plus "error" terms. Factor analysis 
  # aims to find independent latent variables.
  
  ### CONCLUSION FOR OWN PROJECT ###
  # Even though I was planning on doing a factor analysis at first, it doesn't seem to make a lot of sense given
  # the dataset I had chosen to analyse. There are only 4 variables in the first place and the diet and time of
  # the body weight measurements are the only two variables that could have an impact on the body weight, which
  # is the variable that we are most interested in. The existence of unobserved latent variables in the ChickWeight
  # data is therefore highly unlikely. However, what I can still do is create a correlation matrix to check for 
  # correlation between the variables in the dataset
  
  
  ####################
  ##### ANALYSIS #####
  ####################
  
  # first, i have to create a help matrix with all the variables
  matrix1 <- ChickWeight %>% 
    # i select the variables that I want to have in the matrix (in my case all of them) with the nice select
    # function from the dplyr package
    dplyr::select(weight, Time, Chick, Diet)  
  
  # then, i create our final matrix that we can analyze for correlation, using the cbind function
  # to combine matrix1 with all of the variables again, resulting in matrix2
  matrix2 <- cbind(ChickWeight$weight, ChickWeight$Time, ChickWeight$Chick, ChickWeight$Diet)
  
  # finally, I use the rcorr function from the Hmisc package to create the correlation matrix
  Hmisc::rcorr(matrix2)
  
  #CONCLUSION: when analyzing the second matrix that the rcorr function retrieves (which returns the
  # asymptotic P-values), we see that there is no correlation with a p-value < 0.05 (the general significance
  # border value for statistical evaluations). We therefore have no unobserved latent variables in the ChickWeight
  # dataset, which makes sense given the small number of variables that were assessed in the study
  
  
##########################################################
################## Cluster analysis ######################
##########################################################
  
  ##################################
  ##### THEORETICAL BACKGROUND #####
  ##################################
  
  # for the theoretical basis, I checked out the following Wikipedia link
  # https://en.wikipedia.org/wiki/Cluster_analysis
  
  ### Wikipedia description ###
  # Cluster analysis or clustering is the task of grouping a set of objects in such a way that objects in the 
  # same group (called a cluster) are more similar (in some sense) to each other than to those in other groups 
  # (clusters). It is a main task of exploratory data mining, and a common technique for statistical data 
  # analysis, used in many fields.
  
  ### CONCLUSION FOR OWN PROJECT ###
  # The cluster analysis seems doable for the scope of my dataset, even if i am only able to do the clustering
  # based on the variables weight and Time because the variables need to be numeric for the cluster analysis
  # or we get an immediate error message
  
  
  ####################
  ##### ANALYSIS #####
  ####################
  
  # I start by creating a help matrix
  matrix3 <- ChickWeight %>% 
    # this is once again achieved by using the select function
    dplyr::select(weight, Time) %>%  # selects right vars #T use mixture of easy to identify and variables that are important for the analysis
    # the scale function standardizes the variables and therefore makes them comparable
    scale() %>%
    # and i want the resulting data to be framed as a matrix
    as.matrix()
  
  # the next step is creating a distance matrix 
  dist_matrix1 <- matrix3 %>% 
    dist()
  
 # now I will do the actual cluster analysis. To be exact, i will use two popular versions of
  # connectivity-based clustering (hierarchical clustering) which are both included in the hclust function:
  
  # first, I will do a so-called single-linkage clustering based on the minimum of object distances
  # (see https://en.wikipedia.org/wiki/Single-linkage_clustering)
  hcs1 <- hclust(dist_matrix1, "single")
  
  # I repeat this and do a so-called complete linkage clustering based on the maximum of object distances 
  # (see https://en.wikipedia.org/wiki/Single-linkage_clustering)
  hcc1 <- hclust(dist_matrix1, "complete") # alternatively "complete" or "average" for different analysis
  
  # plotting both of the dendrograms gives me an idea of how many clusters exist.
  plot(hcs1)
  plot(hcc1)
  #CONCLUSION: At the moment, this looks very, very messy, so I will reduce the number of clusters to 5 
  # in the next step, based on the single-linkage clustering
  
  # i use the cutree function to reduce the number of clusters to 10
  solution1 <- cutree(hcs1,k=10)
  
  # i add my clustering solution to the original data 
  ChickWeight$solution1 <- as.numeric(solution1)
  
  # by checking our solution with the table function, i see that the first cluster is by far the biggest with 
  # 345 observations and therefore likely the most important one, so i will get rid of the other clusters in the 
  # next step.
  table(ChickWeight$solution1)
  
  # i use the filter function to drop all the other clusters that don't meet my criteria of cluster==1 and
  # create a new matrix5
  matrix4 <- ChickWeight %>% 
    filter(solution==1)
  
  # i just quickly checked if the other clusters were effectively dropped. this is true as we now have 345
  # observations in total, which equals the number of observations in the fist cluster
  nrow(matrix4)
  
  #PRE-CONCLUSION: I now already dropped quite a few cases that don't seem to be interesting according to the
  # single-linkage clustering analysis. For the resulting 345 observations, I would like to group them into
  # 3 final clusters based on a complete linkage clustering.
  
  # # I start over by creating another help matrix, selecting the same variables as before
  matrix5 <- matrix4 %>% 
    dplyr::select(weight, Time)
  
  # the next step is creating a new distance matrix 
  dist_matrix2 <- matrix5 %>% 
    dist()
  
  # I create a complete linkage clustering based on the maximum of object distances 
  # (see https://en.wikipedia.org/wiki/Single-linkage_clustering)
  hcc2 <- hclust(dist_matrix2, "complete")

  # I plot the new dendrogram which gives me an idea of how many clusters still exist.
  plot(hcc2)
  # to compare this graph, I plot the initial complete linkage clustering, that I did before I started removing
  # observations with the single linkage clustering
  plot(hcc1)
  #CONCLUSION: The bottom part of hcc2 still looks a bit messy, which is caused by the fact that it still includes
  # 345 observations. However, what is more interesting is comparing hcc2 to hcc1: we clearly see that a clearer
  # structure was created with less dendrogramm-arms in the upper half of the plot. This means that we effectively
  # removed outliers by reducing the number of observations to 345 which we can now group into 3 final clusters
  
  # i use the cutree function again to reduce the number of clusters to 3
  solution2 <- cutree(hcc2,k=3)
  
  # by checking our solution2 with the table function, i see that we now have that the first two clusters roughly
  # have the same size (cluster 1 = 199 observations, cluster 2 = 130 observations). However, cluster 3 only
  # consists of 16 observations. I'm therefore interested what would happen if i cut hcc2 to only 2 groups.
  table(solution2)
  
  # i cut hcc2 to only 2 groups.
  solution3 <- cutree(hcc2,k=2)
  
  # when analyzing the table of solution 3, I see that clusters 1 and 2 got grouped together while cluster 3 with
  # its small number of observations was still left by itself. This means that even though cluster 3 only contains
  # 16 observations, those can be formed into a very distinct cluster. I therefore stick with our three clusters
  # in solution2 
  table(solution3)
  
  # Finally, i want to assess the mean weight and mean Time of my three clusters independently:
  # i start by attaching solution2 to my most recent matrix6
  matrix5$solution2 <- solution2
  
  # i create a tibble (specific name in the dplyr package, identical to a data.fram in the base package) which
  # returns the cluster specific weight and Time means for my final analysis
  matrix5 %>% 
    # i use the group_by function to only return observations that are in one of the three clusters
    group_by(solution2) %>%
    # the summarise function from the dplyr package returns the mean of both values
    dplyr::summarise(
      mean_weight=mean(weight),
      mean_Time=mean(Time))
  
  #FINAL CONCLUSION: By analyzing the tibble, I see that the clustering process resulted in three unique clusters
  # with unique means. The Cluster 1 (containing 199 chicken) was weighted with a mean of 3.34 days after 
  # birth and had a mean weight of 54.6 gramms. Cluster 2 (containing 199 chicken) was weighted  with a mean of 
  # 9.31 days after birth and had a mean weight of 105 gramms. Finally, small Cluster 3 (containing 16 chicken)
  # was weighted  with a mean of 11.6 days after birth and had a mean weight of 167 gramms.
  
  # the results indicate a positive relation between numbers of days since birth and the weight of the chicken.
  # This hypothesis will be tested in an ANOVA in the next step.
 
##########################################################
################## ANOVA #################################
##########################################################
  
  ##################################
  ##### THEORETICAL BACKGROUND #####
  ##################################
  
  # for the theoretical basis, I checked out the following Wikipedia link
  # https://en.wikipedia.org/wiki/Analysis_of_variance
  
  ### Wikipedia description ###
  # Analysis of variance (ANOVA) is a collection of statistical models and their associated estimation procedures 
  # (such as the "variation" among and between groups) used to analyze the differences among group means in a sample. 
  # The ANOVA is based on the law of total variance, where the observed variance in a particular variable is 
  # partitioned into components attributable to different sources of variation. In its simplest form, ANOVA provides
  # a statistical test of whether two or more population means are equal, and therefore generalizes the t-test
  # beyond two means.
  
  ### CONCLUSION FOR OWN PROJECT ###
  # The ANOVA is certainly adequate for the scope of my dataset. It is important to notice that the dependent y variable
  # needs to be numeric, which is given for the weight variable, the that i am interested in. The independent x
  # variables can be either numeric or a factor, which is given for my Diet and Time varibales. Chick is an ordered 
  # factor, simply identifying individual chicken. It is arbitrary and therefore not suited for an ANOVA. However, 
  # it can still be interesting to plot the mean weights of all individual chicken next to each other. Last but not 
  # least, an ANOVA is the perfect way of testing the relation between numbers of days since birth and the weight of 
  # the chicken that I discovered in the cluster analysis.
  
  ####################
  ##### ANALYSIS #####
  ####################
  
  # The first ANOVA will be explained in detail, also from a theoretical point of view. The two following ones will
  # only feature relevant interpretations. Please consult the code of the first one again if any theoretical
  # questions occur.
  
  ####################
  ##### ANOVA 1: y1 = weight, x1 = Time (of measurement) ######
  ####################
  
  # first, i specify the variables that I want to assess and recode them as factors. I therefore use the mutate 
  # function, which adds the variables and their values to the initial dataset
  ChickWeight <- ChickWeight %>% 
    mutate(
      # i am finally interested in the chicken' weight, which serves as my dependent y variable and test the 
      # effects of all other variables as independent x variables on my y variable.
      x1=as.factor(Time),
      x2=as.factor(Diet), 
      y1=as.numeric(weight))
  
  # I run the first ANOVA using the aov function from the stats package, checking the effect of the time of the
  # measurement on the chicken' weights and save it in anova1
  anova1 <- ChickWeight %>% 
    aov(y1 ~ x1,.)
  
  # the summary function returns a statistical summary of my first ANOVA. Remember: the general significance 
  # border value for statistical evaluations was p-value < 0.05. The three stars (***) behind the x1 line
  # indicate that the effect of the time of the measurement on the weight is highly significant
  summary(anova1)
  
  # Finally, I want to visualize this significant effect of measurement time on chicken weight
  ChickWeight %>% 
    # i group the values by the day of the measurement, which I defined to be the x1 variable
    group_by(x1) %>% 
    # i use the summarise function from the dplyr package to calculate standard error and mean of y1, which is the weight 
    dplyr::summarise(
      se_w=parameters::standard_error(y1), 
      mean_w=mean(y1)) %>% 
    # i use the ggplot2 package to generate a plot an first have to specify, what is plotted
    # furthermore, i rearrange the values with the reorder function to show them in decreasing order in the plot
    ggplot(aes(x= reorder(x1, -mean_w), y=mean_w))  +
    # geom_bar creates a nice barchart and I fill each bar with the html color #C0C0C0
    # important: don't forget the stat="identity", otherwise the code won't work
    geom_bar(stat="identity",  fill=c("#C0C0C0")) +
    # the enhance the intrepretation possibilites of my plot, I add error bars, by simply subtracting and adding
    # the standard errors from the means
    geom_errorbar(aes(ymin=mean_w-se_w, ymax=mean_w+se_w),width=.2,color="black") +
    # i add some labels to show the reader what is plotted
    labs(x = "Days since birth", 
         y = "Mean",
         title = "Effect of time of measurement on chicken weight",
         caption = "Source: ChickWeight") +
    # finally, I use a ggplot2 theme that I like
    theme_light()
  
  #CONCLUSION: In the plot, we can clearly see a positive correlation between the number of days after birth when
  # the measurement was taken and the weight of the chicken. This makes sense when thinking that the chicken
  # grow over time. As a next step, a linear regression model will show the actual size of the effect by calculating
  # the coefficients
  
  ####################
  ##### ANOVA 2: y1 = weight, x2 = Diet ######
  ####################
  
  ChickWeight <- ChickWeight %>% 
    mutate(
      x1=as.factor(Time), 
      x2=as.factor(Diet), 
      y1=as.numeric(weight))
  
  anova2 <- ChickWeight %>% 
    aov(y1 ~ x2,.)
  
  # The three stars (***) behind the x2 line indicate that the effect of the individual chicken on its weight is 
  # highly significant
  summary(anova2)
  
  # I use the same code as before to compare the effectof different diets on chicken weight but with one adjustment
  ChickWeight %>% 
    group_by(x2) %>% 
    dplyr::summarise(
      se_w=parameters::standard_error(y1), 
      mean_w=mean(y1)) %>% 
    ggplot(aes(x= reorder(x2, -mean_w), y=mean_w))  +
    geom_bar(stat="identity",  fill=c("#C0C0C0")) +
    # I add labels with the scale_x_discrete function. By specifying which label belongs to which x-value, the
    # labels will be reordered if the x-values are. This would not be the case if my label vector would just contain
    # c("Diet 1", "Diet 2", "Diet 3", "Diet 4"). You can insert this vector to check out the difference.
    scale_x_discrete(labels = c("1"="Diet 1", "2"="Diet 2", "3"="Diet 3", "4"="Diet 4")) +
    geom_errorbar(aes(ymin=mean_w-se_w, ymax=mean_w+se_w),width=.2,color="black") +
    labs(x = "Diet", 
         y = "Mean",
         title = "Effect of different diets on chicken weight",
         caption = "Source: ChickWeight") +
    theme_light()
  
  #CONCLUSION: In the plot, we can nicely see that Diet 3 was the most effective one, followed by Diet 4, Diet 2 and
  # finally Diet 1, being the least effective one. Depending on other factors, such as the price of each diet, we
  # should strictly focus on Diet 3.
  
  ####################
  ##### Mean weights of individual chicken plot ######
  ####################
  
  # As previously stated, Chick is an ordered factor, simply identifying individual chicken. It is arbitrary and 
  # therefore not suited for an ANOVA. However, it is still  interesting to plot the mean weights of all individual
  # chicken next to each other.
  
  # i visualize the mean weights of individual chicken by using the same plot as before, but grouping means
  # by the unique chicken identity number
  ChickWeight %>% 
    group_by(Chick) %>% 
    dplyr::summarise(
      se_w=parameters::standard_error(y1), 
      mean_w=mean(y1)) %>% 
    ggplot(aes(x= reorder(Chick, -mean_w), y=mean_w))  +
    geom_bar(stat="identity",  fill=c("#C0C0C0")) +
    geom_errorbar(aes(ymin=mean_w-se_w, ymax=mean_w+se_w),width=.2,color="black") +
    labs(x = "Chicken Identity Number", 
         y = "Mean",
         title = "Different chicken and their weights",
         caption = "Source: ChickWeight") +
    theme_light()
  
  #CONCLUSION: In the plot, we can now compare the mean weights of individual chicken. It is no surprise that the 
  # error bars are getting bigger with the mean weights as the the statistical range of measured weights obviously 
  # increases in relation to the individual chicken' mean weights. In further analysis, it could be interesting
  # to analyze the chicken with the heighest weights (numbers 21, 34 and 35) in more detail, to see if it's possible 
  # to find other factors that accounted for their higher mean weight, for example, how much water they drank throughout their development.
  
##########################################################
################## Regression Model ######################
##########################################################
  
  ##################################
  ##### THEORETICAL BACKGROUND #####
  ##################################
  
  # for the theoretical basis, I checked out the following Wikipedia link
  # https://en.wikipedia.org/wiki/Simple_linear_regression
  
  ### Wikipedia description ###
  # In statistics, simple linear regression is a linear regression model with a single explanatory variable. That 
  # is, it concerns two-dimensional sample points with one independent variable and one dependent variable 
  # (conventionally, the x and y coordinates in a Cartesian coordinate system) and finds a linear function 
  # (a non-vertical straight line) that, as accurately as possible, predicts the dependent variable values as a 
  # function of the independent variables. The adjective simple refers to the fact that the outcome variable is 
  # related to a single predictor.
  
  ### CONCLUSION FOR OWN PROJECT ###
  # A regression model is certainly for the scope of my dataset. I am still interested in the dependent variable 
  # weight as my dependent variable. The only suitable independent variable due to its numeric scaling is the Time
  # variable. That suitable model is therefore a simple linear regression model. In ANOVA 1 we found out that there 
  # is a positive correlation between the number of days after birth when the measurement was taken and the weight 
  # of the chicken. In this step, the model will show the actual size of the effect by calculating the coefficients.
  
  ####################
  ##### ANALYSIS #####
  ####################
  
  # As a preliminary inspection, I first plot the relationship of weight and Time. The relationship looks very linear
  # at first glance. Conduction a linear regression is therefore a valid approach.
  ChickWeight %>%
    ggplot(aes(Time,weight)) +
    # geom_point creates a scatterplot
    geom_point() +
    # geom_smooth creates a line and shows the 95% confidence interval by default. 
    geom_smooth() +
    # i introduce scale limits from 1 to 400 with the scale_y_continuous function to ensure comparability
    # when plotting the regression afterwards
    scale_y_continuous(limits = c(1, 400)) +
    labs(x="Time",
         y="Weight",
         title = "Relationship between weight and Time variables",
         caption = "Source: ChickWeight") +
    theme_minimal()
  
  # i create a simple linear regression simply by using the lm function from the stats package
  LM <- ChickWeight %>% 
    # don't forget to use the ~ in between the dependent and independent variables as well as the , . so the lm
    # function recognizes that you're still using the ChickWeight data (the pipe operator %>% is not sufficient here)
    lm(weight~Time, .) 
  
  # to get a statistical summary of my regression model, I simply use the summary function. I can already see that
  # the model is highly significant, which is indicated by the three stars (***) next to the Intercept and Time 
  # variables
  summary(LM)
  
  # next, I want to check out the 95% confidence interval and therefore use the confint function from the stats 
  # package. I can see that the 2.5% and 97.5 percent values are positive for both the intercept and Time
  # variables, which again tells me that my model is significant.
  confint(LM)

  # as a next step, i want to see what my coefficients are to determine the effect of the Time variable. The values
  # are 27.47 for the intercept and 8.8 for the Time variable. This tells us that it can be expected tha each 
  # additional day, a chicken will add around 8.8 gramms to its overall weight
  LM$coefficients
  
  # i can also plot the regression model by using the stat_smooth function.
  ChickWeight %>% 
    ggplot(aes(x=Time, y=weight)) +
    geom_point() +
    # i specify method="lm" so it plots a linear regression
    stat_smooth(method="lm") +
    scale_y_continuous(limits = c(1, 400)) +
    labs(x="Time",
         y="Weight",
         title = "Linear regression line",
         caption = "Source: ChickWeight") +
    theme_minimal()
  
  # finally, i plot the regression line, checking what it would look like if it wasn't specified to be linear. The
  # result is in this case the same as the relations graph that i did for the preliminary inspection.
  ChickWeight %>% 
    ggplot(aes(x=Time, y=weight)) +
    geom_point() +
    # i specify method="loess" so it plots a smoothed regression line
    stat_smooth(method="loess") +
    scale_y_continuous(limits = c(1, 400)) +
    labs(x="Time",
         y="Weight",
         title = "Smoothed regression line",
         caption = "Source: ChickWeight") +
    theme_minimal()
  
  #CONCLUSION: We can now easily compare the the two regression graphs with each other. We immediately see agai that 
  # the linear regression line provides a great estimate for our dependent variable. Although the 95% confidence
  # interval shadows around the linear regression line are very narrow, they open a bit towards the x-axis limits.
  # This can be explained by looking at the smoothed regression line, which has two small kinks at both x-axis limits.
  # Finally, we can conclude that the linear regression model and its coefficients will prove to be very helpful when
  # estimating what a chicken weight will be if we simply have the numbers of days since birth as an indication
  # when the measurement takes place.
  
##########################################################
################## Overall conclusion ####################
##########################################################
  
  # BOTTOM LINE: The farmer should strictly focus on Diet 3, which has proven to be the most nourishing one.
  # Furthermore, he or she can expect a chicken to add 8.8 gramms per day after birth, with a starting weight of
  # 27.47 gramms. Last but not least, he or she should look for further factors that affected the impressive
  # mean weight of the top three individual chicken (numbers 21, 34 and 35).
  
  # DESCRIPTIVE STATISTICS: by using some statistical analysis functions and plotting some distributions, I could 
  # gain a good first overview over the data that I wanted to analyze before proceeding with further analyses.
  
  # FACTOR ANALYSIS: although the existence of unobserved latent variables was highly unlikely due to the small
  # amount of variables in the ChickWeight dataset, I still analyzed the asymptotic P-values of the correlation 
  # matrix. It showed that there is no correlation with a p-value < 0.05 (the general significance border value 
  # for statistical evaluations) and therefore proved the absence of unobserved latent variables in the ChickWeight
  # dataset.
  
  # CLUSTER ANALYSIS: Clustering observations based on first the single and then complete linkage procedure 
  # resulted in three unique clusters with unique means. The Cluster 1 (containing 199 chicken) was weighted 
  # with a mean of 3.34 days after birth and had a mean weight of 54.6 gramms. Cluster 2 (containing 199 chicken) 
  # was weighted  with a mean of 9.31 days after birth and had a mean weight of 105 gramms. Finally, small but proven
  # to be distinct Cluster 3 (containing 16 chicken) was weighted  with a mean of 11.6 days after birth and had 
  # a mean weight of 167 gramms. The results indicate a positive relation between numbers of days since birth and 
  # the weight of the chicken. This hypothesis was then tested in an ANOVA in the next step.
  
  # ANOVA: Firstly, ANOVA 1 revealed a strong correlation between the number of days after birth when the measurement 
  # was taken and the weight of the chicken. As a next step, a linear regression model showed the actual size of the 
  # effect by calculating the coefficients. Secondly, inspired by ANOVA 2, plotting the weight means grouped by Diets
  # showed that Diet 3 was the most effective one, followed by Diet 4, Diet 2 and finally, Diet 1, being the least 
  # effective one. The farmer should therefore strictly focus on Diet 3 for his or her chicken. As the Chick variable
  # is an arbitrary, ordered factor it was not suited for an ANOVA. However, it was still  interesting to plot the 
  # mean weights of all individual chicken next to each other. In further analysis, it could be interesting
  # to analyze the chicken with the heighest weights (numbers 21, 34 and 35) in more detail, to see if it is possible
  # to find other factors that accounted for their higher mean weight, for example, how much water they drank 
  # throughout their development.
  
  # REGRESSION MODEL:  As ANOVA 1 revealed a strong correlation between the number of days after birth when the 
  # measurement was taken and the weight of the chicken, I did a linear regression model to show the actual size 
  # and direction of the effect by calculating the coefficients. The values of the coefficients were 27.47 for the 
  # intercept and 8.8 for the Time variable. This showed that it can be expected that each additional day, a chicken 
  # will add around 8.8 gramms to its overall weight. As a next step, linear and smoothed regression lines were
  # plotted. It was observable that the linear regression line provided a great estimate for the dependent variable,
  # weight. Although the 95% confidence interval shadowed around the linear regression line were very narrow, they 
  # open a bit towards the x-axis limits. This could be explained by looking at the smoothed regression line, which 
  # had two small kinks at both x-axis limits. Finally, I could conclude that the linear regression model and its 
  # coefficients will prove to be very helpful when estimating what a chicken weight will be if we simply have the 
  # numbers of days since birth as an indication when the measurement takes place.
  
  
  