# Dear class,
# Here's a summary of the R code complied according to the questions I learned during the TA hour.
# But please remember, they were pooled from different sources, so they can't be run smoothly in R studio, though I guess it provides a good reference for the core code you could use to complete Dr. Lin's assignments.
# Feel free to email me if you find any error regarding syntax in this file. Thank you!
# I will try to update it according to the new questions I learned from you guys.
# Hope this helps and good luck with your assignments!


library(rio)
library(tidyverse)
library(srvyr)
library(survey)

#detach("package:srvyr", unload = TRUE)

dt <- read.csv("dt.csv", header = TRUE)

dt <- dt[which(dt$v != 0), ]


# Note: Always create survey design after (not before) you revised the data because R won't do that automatically.
# Method 1: srvyr (Please use this one)
dt_survey <- dt %>%
  as_survey_design(ids = PSU, strata = STSTR, weights = LLCPWT, nest = TRUE)

# Method 2: survey
# dt_survey <-svydesign(id=~PSU, strata = ~STSTR, weights = ~LLCPWT, data = dt, nest = TRUE) 

options(survey.lonely.psu = "adjust")


# Summary statistic

# 1.mean & proportion

# 1.1 Continuous variable
a <- cg17_pe_design  %>% summarize(mean = survey_mean(x1, vartype=c("se", "ci"), na.rm = TRUE)) 

# 1.2 Categorical variable
# 1 var
a <- dt_design %>% group_by(x1) %>% summarize(proportion = survey_mean(vartype=c("se"), na.rm = TRUE)) 
# 2 vars
a <- dt_design %>% group_by(y, x1) %>% summarize(proportion = survey_mean(vartype=c("se"), na.rm = TRUE)) 
b <- dt_design %>% group_by(y, x2) %>% summarize(proportion = survey_mean(vartype=c("se"), na.rm = TRUE)) 
table <- bind_rows(a, b)
export(table, "table.csv")

# The following method can be used for the case when y is categorical and x is either continuous or categorical.
svyby(~x, ~y, cg17_pe_design, svymean, vartype = c("se"), na.rm = TRUE)


# 2.Raw sample size (obs)
# 1 var
cg17_pe %>% count(X_RFSMOK3)
a <- cg17_pe %>% count(X_RFSMOK3)  #The result can be stored in an object, e.g., a, which eases bind and export. The same below.
# 2 vars
cg17_pe %>% count(vars = X_RFSMOK3, by = SEX)


# 3. Weighted sample size (count)
# 1 var
# Method 1
cg17_pe %>% count(vars = X_RFSMOK3, wt = X_LLCPWT)
# Method 2
svytable(~X_RFSMOK3, design = cg17_pe_design)

# 2 vars
# Method 1
cg17_pe %>% count(vars = X_RFSMOK3, by = SEX, wt = X_LLCPWT)
# Method 2
svytable(~X_RFSMOK3 + SEX, design = cg17_pe_design)   # Put by_var in the 2nd place (e.g., SEX)



# Correlation test between 2 vars https://cran.r-project.org/web/packages/jtools/vignettes/svycor.html
# Attention! Need to install several 2 additional packages and library them.
# install.packages("jtools")
# install.packages("weights")
library(jtools)
library(weights)
svycor(~x1 + x2, design = dstrat, digits = 3, sig.stats = TRUE, bootn = 2000, mean1 = TRUE)
a <- svycor(~x1 + x2, design = dstrat, digits = 3, sig.stats = TRUE, bootn = 2000, mean1 = TRUE)
b <- as.data.frame(unlist(a))


# t test https://stats.oarc.ucla.edu/r/seminars/survey-data-analysis-with-r/
# A one-sample t-test is shown below.
svyttest(pad675~0, nhc, na = TRUE) # na = TRUE If there are missing values, then the mean function returns NA. To drop the missing values from the calculation use na. rm = TRUE. 
tt <- svyttest(outcome ~ 0, dt_survey)  #0/1结果一样
tt <- svyttest(outcome ~ 1, dt_survey)
confint(tt, level = 0.95)
# Paired-samples t-test. the “I” is used to tell R to leave the part in parentheses “as is”, meaning do the subtraction between the two variables. Hence, the formula means that the difference between
svyttest(I(pad660-pad675)~0, nhc, na = TRUE)
# independent-samples t-test (two-sample t test for difference of means)
# var_of_focus can be either continuous or binary. If it's binary, it should be 0/1, not 1/2. And of course, it can't have more than 2 categories, like 1/2/3.
tt <- svyttest(var_of_focus ~ categorical_group_var, dt_survey)
tt
confint(tt, level = 0.95)


# Chi square test
# Method 1
a <-svychisq(~y + x, dt_design, statistic = c("Chisq"), na.rm = TRUE) 
# svychisq(formula, design, statistic = c("F",  "Chisq","Wald","adjWald","lincom", "saddlepoint","wls-score"),na.rm=TRUE,...)  
# If you just write svychisq(~y + x, dt_design), by default, that will be F test. 
a <-unlist(a) 
b <-as.data.frame(a) 
chi_square <-b[1, ] 
chi_square <-as.data.frame(chi_square) 
pvalue <-b[3, ] 
pvalue <-as.data.frame(pvalue)

p2 <-cbind(chi_square, pvalue) 
print(p2)
export(p2, "p2.csv")

# Method 2
svytable(~y + x, design = dt_design) %>% as.matrix() %>% chisq.test()



# Regressions
# 1. Linear regression
# Univariate Linear regression (package survey used)
model_ln <- svyglm(y ~ x, design = dt_survey)
summary(model_ln)

# Multivariate Linear regression
model_ln <- svyglm(y ~ x, design = dt_survey)
summary(model_ln)

# 2. Logit regression
# 2.1 Dependent variable (y) is binary (only 2 categories)

# Univariate Logistic Regression
model_lg <- svyglm(y ~ x, family = quasibinomial, design = dt_survey)
summary(model_lg)

# Multivariate Logistic Regression
alr <- svyglm(y ~ x1 + x2 + x3 + x4, family = quasibinomial, design = dt_design)
summary(alr)    # Find p-value and statistical significance (*, **, ***) here.
confint(alr)

exp(coef(alr))
exp(cbind(AOR = coef(alr), confint(alr)))
exp(cbind(AOR = coef(alr), confint(alr)))[-1, ]
adj_or <- exp(cbind(AOR = coef(alr), confint(alr)))[-1, ]
adj_or <- as.data.frame(adj_or)
adj_or$var <- rownames(adj_or)
export(adj_or, "adj_or.csv")   # Find the adjusted odds ratio and 95% CI here.


# 2.2 Dependent variable (y) has 3 categories or more.
# If dependent variable has 3 categories or more., you can't use the logistic regression code above which requires only 2 categories in dependent variable. The new logistic regression here is called ordinal/ordered logistic regression. However, this method is kind of complicated. Dr. Lin didn't teach that, either. You can choose whether to dive deeper.
# Ordinal/Ordered logistic regression (Method 2 preferred)

# Method 1  https://rpubs.com/BAFlores/872025 
install.packages("svyVGAM")  # This is a new package. 
library(svyVGAM)
result <- svy_vglm(as.ordered(y) ~ x1 + x2 + x3 + x4 , design = dt_design, family=cumulative(parallel = TRUE, reverse = TRUE))
summary(result)

# Method 2 - See svyolr in https://stats.oarc.ucla.edu/r/seminars/survey-data-analysis-with-r/
# y x1 x2 categorical, x3 continuous.
orderlogit_results <- svyolr(factor(y)~factor(x1)+factor(x2)+x3, design = nhc, method = c("logistic"))

summary(orderlogit_results)

# Know more about this function
?svyolr 

# Generate P values as there are no P values above. 
coef_table <- data.frame(coef(summary(orderlogit_results)))
coef_table$pval = round((pnorm(abs(coef_table$t.value), lower.tail = FALSE) * 2), 4)
# Export the results.
export(coef_table, "coef_table.csv")

# The coefficients above are not odds ratio, to obtain odds ratio, please do the following transformation. 
oddsratio <- as.data.frame(exp(orderlogit_results$coefficients))
export(oddsratio, "oddsratio.csv")
ci <- as.data.frame(exp(confint(orderlogit_results)))
export(ci, "ci.csv")
# P value will always be the same, no need to transform.

# How to interpret the results of logit regression?
# https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/
# https://stats.oarc.ucla.edu/other/mult-pkg/faq/ologit/
# https://www.princeton.edu/~otorres/LogitR101.pdf
# Also, note that as we are using survey data with specific strata and other settings in this course, we may not use the function in the websites above but use svy_vglm and svyolr as illustrated in Code_Summary.R. However, the resources above provide really nice illustrations on how to interpret the results.


