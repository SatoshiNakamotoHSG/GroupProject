# ChickWeight Analysis
An in-depth statistical analysis of the R data preset “ChickWeight”.

# Description
This group project performs an in-depth statistical analysis of the R data preset “ChickWeight” (Weight versus age of chicks on different diets). The aim was to find out which variables affect the weight of the chicken and to formulate conclusions as if I was helping the farmer who gathered the data and who want the chicken to be as heavy as possible so he or she could sell them for a higher price.

It includes the following statistical analyses:

- Descriptive statistics
- Factor analysis
- Cluster analysis
- ANOVA
- Regression model

The code is well commented, both from a technical as well as statistical standpoint. My aim was that anyone who finds my code on github would be able to understand, modify and use it. It is split into a lot of sections which can be collapsed. Experience R coders can therefore easily skip sections that are not relevant for them.

# Results

- BOTTOM LINE: The farmer should strictly focus on Diet 3, which has proven to be the most nourishing one.
Furthermore, he or she can expect a chicken to add 8.8 gramms per day after birth, with a starting weight of
27.47 gramms. Last but not least, he or she should look for further factors that affected the impressive
mean weight of the top three individual chicken (numbers 21, 34 and 35).

- DESCRIPTIVE STATISTICS: by using some statistical analysis functions and plotting some distributions, I could 
gain a good first overview over the data that I wanted to analyze before proceeding with further analyses.
FACTOR ANALYSIS: although the existence of unobserved latent variables was highly unlikely due to the small
amount of variables in the ChickWeight dataset, I still analyzed the asymptotic p-values of the correlation 
matrix. It showed that there is no correlation with a p-value < 0.05 (the general significance border value 
for statistical evaluations) and therefore proved the absence of unobserved latent variables in the ChickWeight
dataset.

- CLUSTER ANALYSIS: Clustering observations based on first the single and then complete linkage procedure 
resulted in three unique clusters with unique means. The Cluster 1 (containing 199 chicken) was weighted 
with a mean of 3.34 days after birth and had a mean weight of 54.6 gramms. Cluster 2 (containing 130 chicken) 
was weighted  with a mean of 9.31 days after birth and had a mean weight of 105 gramms. Finally, small but proven
to be distinct Cluster 3 (containing 16 chicken) was weighted  with a mean of 11.6 days after birth and had 
a mean weight of 167 gramms. The results indicate a positive relation between numbers of days since birth and 
the weight of the chicken. This hypothesis was then tested in an ANOVA in the next step.

- ANOVA: Firstly, ANOVA 1 revealed a strong correlation between the number of days after birth when the measurement
was taken and the weight of the chicken. As a next step, a linear regression model showed the actual size of the 
effect by calculating the coefficients. Secondly, inspired by ANOVA 2, plotting the weight means grouped by Diets
showed that Diet 3 was the most effective one, followed by Diet 4, Diet 2 and finally, Diet 1, being the least 
effective one. The farmer should therefore strictly focus on Diet 3 for his or her chicken. As the Chick variable
is an arbitrary, ordered factor it was not suited for an ANOVA. However, it was still  interesting to plot the 
mean weights of all individual chicken next to each other. In further analysis, it could be interesting
to analyze the chicken with the heighest weights (numbers 21, 34 and 35) in more detail, to see if it is possible
to find other factors that accounted for their higher mean weight, for example, how much water they drank 
throughout their development.

- REGRESSION MODEL:  As ANOVA 1 revealed a strong correlation between the number of days after birth when the 
measurement was taken and the weight of the chicken, I did a linear regression model to show the actual size 
and direction of the effect by calculating the coefficients. The values of the coefficients were 27.47 for the 
intercept and 8.8 for the Time variable. This showed that it can be expected that each additional day, a chicken 
will add around 8.8 gramms to its overall weight. As a next step, linear and smoothed regression lines were
plotted. It was observable that the linear regression line provided a great estimate for the dependent variable,
weight. Although the 95% confidence interval shadowed around the linear regression line were very narrow, they 
open a bit towards the x-axis limits. This could be explained by looking at the smoothed regression line, which 
had two small kinks at both x-axis limits. Finally, I could conclude that the linear regression model and its 
coefficients will prove to be very helpful when estimating what a chicken weight will be if we simply have the 
numbers of days since birth as an indication when the measurement takes place.
