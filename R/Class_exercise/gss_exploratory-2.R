# You will need to install these if you have not done so previously
# install.packages("tidyverse")
# install.packages("ggpubr")
library(tidyverse)
library(ggpubr) 

# download the GSS 2021 data from Canvas then load as a dataframe
setwd('/Users/nithishasathishkumar/Desktop/Data_Analysis/R/Class_exercise')
gss2021 <- read.csv("gss2021.csv", header = TRUE)

# This exercise centers on exploring data related to educational attainment
# among U.S. adults.

## start with a histogram to inspect the distribution of the dependent variable, educ
ggplot(gss2021, aes(x=educ)) +
  geom_bar(fill="steelblue") +
  scale_x_continuous(breaks=seq(0,20,1)) +
  ggtitle("Years of education, 2021 General Social Survey")

## then descriptive statistics of the dependent variable
mean(gss2021$educ, na.rm = TRUE)
sd(gss2021$educ, na.rm =TRUE)

## now a scatter plot to see if there is a linear relationship between
## an individual's number of years of education and their mother's education
ggplot(gss2021, aes(x=maeduc, y=educ)) +
  geom_point(position = "jitter") +
  geom_smooth() +
  scale_x_continuous(breaks=seq(0,20,1)) +
  scale_y_continuous(breaks=seq(0,20,1)) +
  stat_cor(method = "pearson", label.x = 2, label.y = 1)
  
## estimate an individual's level of education as a function of their mother's 
## education using a simple linear regression model
educ.lm <- lm(educ ~ maeduc, data = gss2021)
summary(educ.lm)

## plot regression 
ggplot(gss2021, aes(x=educ, y=maeduc)) +
  geom_point(position = "jitter") +
  geom_smooth(method="lm", col="black") +
  stat_regline_equation()  

## suppose now that we want to test to see if having siblings matters to 
## an individual's level of education, while controlling for mother's education
## start with a scatter plot
ggplot(gss2021, aes(x=sibs, y=educ)) +
  geom_point(position = "jitter") +
  geom_smooth() +
  scale_x_continuous(breaks=seq(0,35,1)) +
  scale_y_continuous(breaks=seq(0,20,1)) +
  stat_cor(method = "pearson", label.x = 20, label.y = 1)

## estimate a linear model with multiple independent variables
educ.lm2 <- lm(educ ~ maeduc + sibs, data = gss2021)
summary(educ.lm2)

## try running another model that switches father's education (variable name is:
## paeduc) for mother's education (but keep the sibs variable in the model)
## I started the code for you - fill it in based on what you see above.
educ.lm3 <- lm(educ ~ paeduc + sibs, data = gss2021)
summary(educ.lm3) 

## we could also add father's education and mother's education simultaneously,
## but linear regression assumes the independent variables are not strongly
## correlated with each other. Adapt the code above to test that assumption:
ggplot(gss2021, aes(x=maeduc, y=paeduc)) +
  geom_point(position = "jitter") +
  geom_smooth(method="lm", col="black") +
  stat_cor(method = "pearson", label.x = 2, label.y = 1)

educ.lm4 <- lm(educ ~ maeduc + paeduc + sibs, data = gss2021)
summary(educ.lm4)
  
# Sometimes we don't need regression. For example, suppose we just want to 
# test to see if, on average, college-educated adults have the same number 
# of siblings as non-college educated adults. For that we can run
# a simple t-test:
## First, create a new variable indicating whether or not someone has a bachelor's
ba <- fct_collapse(gss2021$degree, 
                    bachelor=c("bachelor's", "graduate"),
                    no_bachelor=c("less than high school", "high school", "associate/junior college"))
## then add the new variable to the data set
gss2021 <- gss2021 %>%
  add_column(ba)

## create a table of descriptive statistics for each group
college_sibs = gss2021 %>% group_by(ba) %>% summarise(mean = mean(sibs, na.rm=TRUE), 
                                               sd = sd(sibs, na.rm = TRUE), n = n())
college_sibs 

## then use the t-test to directly test the null hypothesis (that the group means are the same)
t.test(sibs ~ ba, var.equal=FALSE, data = gss2021)

# Additional variables to test
## Use the space below to test one other regression model or t-test of group mean 
## differences that you think are associated with an individual's educational attainment
# Additional analysis: Relationship between personal income and education
educ_income.lm <- lm(educ ~ realinc, data = gss2021)
summary(educ_income.lm)

# T-test: Do men and women have different levels of education?
t.test(educ ~ sex, var.equal = FALSE, data = gss2021)

