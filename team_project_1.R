# .libPaths(c('/usr/local/lib/R/library', .libPaths()))

# load library
library(ggplot2)
library(GGally)
library(knitr)
library(xtable)
library(rms)
library(arm)
library(pROC)
library(e1071)
library(caret)
library(gridExtra)
# import data
lalonde_data <- read.csv('./lalondedata.txt')
# convert data to the appropriate type
lalonde_data$treat <- factor(lalonde_data$treat)
lalonde_data$educ <- factor(lalonde_data$educ)
levels(lalonde_data$educ) <- list('elementary'=c('0','1','2','3','4','5'), 'middle'=c('6', '7', '8'), 'high'=c('9', '10', '11', '12'), 'college' = c('13','14', '15', '16', '17', '18'))
lalonde_data$black <- factor(lalonde_data$black)
lalonde_data$hispan <- factor(lalonde_data$hispan)
lalonde_data$married <- factor(lalonde_data$married)
lalonde_data$nodegree <- factor(lalonde_data$nodegree)
# get desired response variable
lalonde_data$wage_diff <- lalonde_data$re78 - lalonde_data$re74
# create new data for analysis 
data_1 <- lalonde_data[, !names(lalonde_data) %in% c('re74', 're75', 're78', 'X')]
# data_1 <- lalonde_data[, !names(lalonde_data) %in% c('re75', 'X')]


### EDA
# assess normality of response variable
ggplot(data = data_1, aes(wage_diff)) + 
  geom_histogram(binwidth = 1000, color = 'lightblue', fill = 'white') ## looks somewhat normal
# look at categorical variables first
# look at wage_diff by treat
ggplot(data_1,aes(x=treat, y=wage_diff, fill=treat)) +
  geom_boxplot()  +
  scale_fill_brewer(palette="Reds") +
  labs(title="Treat vs Wage Difference",
       x="Treat",y="Wage Difference") + 
  theme_classic() + theme(legend.position="none") ## may be significant 
# look at wage_diff by educ
ggplot(data_1,aes(x=educ, y=wage_diff, fill = educ)) +
  geom_boxplot()  +
  scale_fill_brewer(palette="Reds") +
  labs(title="Education vs Wage Difference",
       x="Education",y="Wage Difference") + 
  theme_classic() + theme(legend.position="none") ## may be significant 
# look at wage_diff by black
ggplot(data_1,aes(x=black, y=wage_diff, fill=black)) +
  geom_boxplot()  +
  scale_fill_brewer(palette="Reds") +
  labs(title="Black vs Wage Difference",
       x="Black",y="Wage Difference") + 
  theme_classic() + theme(legend.position="none") ## maybe be significant 
# look at wage_diff by hispan
ggplot(data_1,aes(x=hispan, y=wage_diff, fill=hispan)) +
  geom_boxplot()  +
  scale_fill_brewer(palette="Reds") +
  labs(title="Hispan vs Wage Difference",
       x="Hispan",y="Wage Difference") + 
  theme_classic() + theme(legend.position="none") ## maybe significant
# look at wage_diff by marries
ggplot(data_1,aes(x=married, y=wage_diff, fill=married)) +
  geom_boxplot()  +
  scale_fill_brewer(palette="Reds") +
  labs(title="Married vs Wage Difference",
       x="Married",y="Wwage Difference") + 
  theme_classic() + theme(legend.position="none") ## maybe significant
# # look at wage_diff by nodegree
ggplot(data_1,aes(x=nodegree, y=wage_diff, fill=nodegree)) +
  geom_boxplot()  +
  scale_fill_brewer(palette="Reds") +
  labs(title="Nodegree vs Wage Difference",
       x="Nodegree",y="Wage Differenc") + 
  theme_classic() + theme(legend.position="none") ## maybe significant
# look at continuous variable
# look at wage_diff by age
ggplot(data_1, aes(x = age, y = wage_diff, fill=age)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") +
  labs(title="Wage Difference vs Age", 
       x="Age", y="Wage Difference") + 
  theme_classic() + theme(legend.position = "none") ## looks like a downward trend
# look at average wage_diff by age
data_1_age <- aggregate(data_1$wage_diff,list(age=data_1$age),mean)
colnames(data_1_age)[2] <- "COUNTS_AGG"
ggplot(data_1_age, aes(x =age, y = COUNTS_AGG)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + 
  labs(title="Mean Wage Difference vs Age") ## a downward trend, may be significant 

## take away: all predictor variables look significant through visual inspection.

# explore interactions
# wage_diff and nodegree by educ
ggplot(data_1,aes(x=nodegree, y=wage_diff, fill=nodegree)) +
  geom_boxplot()  +
  scale_fill_brewer(palette="Reds") +
  labs(title="Wage Difference vs Nodegree by Education",
       x="Nodegree",y="Wage") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ educ,ncol=1)  ## nodegree and educ contain similar info, no need to explore interaction
# wage_diff and treat by XXXX
# look at effects of categorical variables first
# wage_dff and treat by educ
ggplot(data_1,aes(x=treat, y=wage_diff, fill=treat)) +
  geom_boxplot()  +
  scale_fill_brewer(palette="Reds") +
  labs(title="Wage Difference vs Treat by Education",
       x="Education",y="Wage Difference") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ educ,ncol=1)  ## similar trend, might not be significant 
# wage_diff and treat by black
ggplot(data_1,aes(x=treat, y=wage_diff, fill=treat)) +
  geom_boxplot()  +
  scale_fill_brewer(palette="Reds") +
  labs(title="Wage Difference vs Treat by Black",
       x="Black",y="Wage Difference") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ black,ncol=2) ## similar trend, might not be significant 
# wage_diff and treat by hispan
ggplot(data_1,aes(x=treat, y=wage_diff, fill=treat)) +
  geom_boxplot()  +
  scale_fill_brewer(palette="Reds") +
  labs(title="Wage Difference vs Treat by Hispan",
       x="Hispanic Ethnicity",y="Wage Difference") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ hispan,ncol=2) ## similar trend, might not be significant
# wage_diff and treat by married
ggplot(data_1,aes(x=treat, y=wage_diff, fill=treat)) +
  geom_boxplot()  +
  scale_fill_brewer(palette="Reds") +
  labs(title="Wage Difference vs Treat by Married",
       x="Marriage Status",y="Wage Difference") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ married,ncol=2) ## similar trend, might not be significant
# wage_diff and treat by nodegree
ggplot(data_1,aes(x=treat, y=wage_diff, fill=treat)) +
  geom_boxplot()  +
  scale_fill_brewer(palette="Reds") +
  labs(title="Wage Difference vs Treat by No Degree",
       x="Nodegree",y="Wage Difference") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ nodegree,ncol=2) ## similar trend, might not be significant
# look at effects of continuous variables
# wage_diff and age by treat
ggplot(data_1,aes(x=age,y=wage_diff)) + 
  geom_point() + 
  facet_wrap(~treat,ncol=2) +
  geom_smooth(method='lm',col='red3') ## different trend, might be significant
# average wage_diff and age by treat
data_1_age_int <- aggregate(data_1$wage_diff,list(AGE = data_1$age,TREAT = data_1$treat),mean)
colnames(data_1_age_int)[3] <- "COUNTS_AGG"
ggplot(data_1_age_int, aes(x =AGE, y = COUNTS_AGG)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + 
  labs(title="Mean Wage Difference Age by Treat") +
  facet_wrap( ~ TREAT,ncol=2)  ## no different trend, might be significant

## take away: interaction between age and treat could be significant


### Model validation
# center continuous variable first
data_1$age_c <- data_1$age - mean(data_1$age)
# build a naive model with all main effects
model_naive <- lm(wage_diff ~ treat + age_c + educ + black + hispan + married + nodegree,
                   data = data_1)
summary(model_naive) ## significant variables: treat, age_c, educ, married
# assess normality assumption
plot(model_naive, which = 2, col = c('blue4')) ## looks ok
# assess independence and equal variance
plot(model_naive, which = 1, col = c('blue4')) ## the spread looks equal, no obvious pattern
# assess linearity
ggplot(data_1,aes(x=age_c, y=model_naive$residual)) +
  geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + 
  theme_classic() + 
  labs(title="Residuals vs Age (Centered)",x="Age (Centered)",y="Residuals") ## no obvious pattern

## take away, linear regression assumptions are held. For the naive model, the model summary shows that some predictors are statistically significant. Namely, which are treat, age_c, educ, married. Even though other predictor variables don't show any statistical significance, it could that the linear relationship between each one of these variable is not strong enough to be detected by this sample. Next, I will perform model selection, with null_model only capturing the predictor that we or our client care about - treatment. And the full_model will include all main effects as well as the interactions between treat and other variables. If the any variables that were previously determined to be statistically significant are removed through the model selection process, we will perform F-test on these terms to evaluate their significance.


### model selection
null_model <- lm(wage_diff ~ treat, data = data_1)
full_model <- lm(wage_diff ~ treat + age_c + educ + black + hispan + married + nodegree +
                   treat*age_c + treat*educ + treat*black + treat*hispan + treat*married + treat*nodegree,
                 data = data_1)
# use BIC as selection criterion 
n <- nrow(data_1)
# forward selection
model_forward <- step(null_model, scope = formula(full_model), direction = 'forward', trace = 0, k = log(n))
model_forward$call ## lm(formula = wage_diff ~ treat + age_c + treat:age_c, data = data_1)
# stepwise selection
model_stepwise <- step(null_model, scope = formula(full_model), direction="both", trace=0, k = log(n))
model_stepwise$call ## lm(formula = wage_diff ~ treat + age_c + treat:age_c, data = data_1)
# backward selection 
model_backward <- step(full_model, direction="backward", trace=0, k = log(n))
model_backward$call ## lm(formula = wage_diff ~ treat + age_c + treat:age_c, data = data_1)

## take away, all three model selection criteria returned the same model, which all use treat, age_c and treat:age_c as the predictors. The next step would be to create a new model based on these predictors, and evaluate the significance of the predictors that we previously deemed significant through visual inspection.

# create a new model
model_1 <- lm(formula = wage_diff ~ treat + age_c + treat:age_c, data = data_1)
# create a different model that includes educ
model_2 <- lm(formula = wage_diff ~ treat + age_c + treat:age_c + educ,
              data = data_1)
# perform anova test
anova(model_1, model_2) ## p-value is 0.05965, might be worth keeping
# create a different model that includes black
model_3 <- lm(formula = wage_diff ~ treat + age_c + treat:age_c + black,
              data = data_1)
anova(model_1, model_3) ## p-value is 0.5834, to be dropped
# create a different model that includes hispan
model_4 <- lm(formula = wage_diff ~ treat + age_c + treat:age_c + hispan,
              data = data_1)
anova(model_1, model_4) ## p-value is 0.4182, to be dropped
# create a different model that includes married
model_5 <- lm(formula = wage_diff ~ treat + age_c + treat:age_c + married,
              data = data_1)
anova(model_1, model_5) ## p-value is 0.0144, to be kept
# create a different model that includes nodegree
model_6 <- lm(formula = wage_diff ~ treat + age_c + treat:age_c + nodegree,
              data = data_1)
anova(model_1, model_6) ## p-value is 0.7417, to be dropped
# create a different model that includes educ and married
model_7 <- lm(formula = wage_diff ~ treat + age_c + treat:age_c + educ + married,
              data = data_1)
anova(model_1, model_7) ## p-value is 0.01328, will keep both educ and married

## take away, final model will include treat, age_c, educ, married and treat:age_c

## model assessment on the final model
model_final = lm(formula = wage_diff ~ treat + age_c + educ + married + treat:age_c,
                 data = data_1)
summary(model_final) ## significant variables: treat, age_c, educ, married, treat:age_c
# assess normality assumption
plot(model_final, which = 2, col = c('blue4')) ## looks ok
# assess independence and equal variance
plot(model_final, which = 1, col = c('blue4')) ## the spread looks equal, except for 1 point, no obvious pattern
# assess linearity
ggplot(data_1,aes(x=age_c, y=model_final$residual)) +
  geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + 
  theme_classic() + 
  labs(title="Residuals vs Age (Centered)",x="Age (Centered)",y="Residuals") ## no obvious pattern


## check for outlier
plot(model_final, which = 5, col = c('blue4')) ## point 132 is an outlier with low cook's distance and leverage, will be kept


## check for multicolinearity
vif(model_final) ## all values are below 5, which suggests that there should be no multicolinearity isses.


## 95% CI
confint(model_final)




