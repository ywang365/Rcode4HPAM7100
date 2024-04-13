# Dear class,
# Here's a summary of R code complied according to the questions I learned during the TA hour.
# But please remember, they were pooled from different sources, so they can't be run smoothly in R studio, though I guess it provides good reference for the core code you could use to complete Dr. Lin's assignments.
# Hope this helps and good luck to your assignments!


library(rio)
library(tidyverse)
library(srvyr)
library(survey)

#detach("package:srvyr", unload = TRUE)

dt <- read.csv("dt.csv", header = TRUE)

dt <- dt[which(dt$v != 0), ]


# Note: Always create survey design after (not before) you revised the data because R won't do that automatically.
# Method 1: srvyr
dt_survey <- dt %>%
  as_survey_design(ids = PSU, strata = STSTR, weights = LLCPWT, nest = TRUE)

# Method 2: survey
dt_survey <-svydesign(id=~PSU, strata = ~STSTR, weights = ~LLCPWT, data = dt, nest = TRUE) 

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
svytable(~X_RFSMOK3 + SEX, design = cg17_pe_design)



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
# independent-samples t-test
tt <- svyttest(continuous_outcome_var ~ categorical_group_var, dt_survey)
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



# Univariate Linear regression: package survey
model_ln <- svyglm(y ~ x, design = dt_survey)
summary(model_ln)

# Univariate Logistic Regression: package survey
model_lg <- svyglm(y ~ x, family = quasibinomial, design = dt_survey)
summary(model_lg)

# Multivariate Logistic Regression
alr <- svyglm(y ~ x1 + x2 + x3 + x4, family = quasibinomial, design = dt_design)
summary(alr)
confint(alr)

exp(coef(alr))
exp(cbind(AOR = coef(alr), confint(alr)))
exp(cbind(AOR = coef(alr), confint(alr)))[-1, ]
adj_or <- exp(cbind(AOR = coef(alr), confint(alr)))[-1, ]
adj_or <- as.data.frame(adj_or)

table1 <- bind_cols(all_or, adj_or)
colnames(table1)
table1 <- table1 %>% select("hv", "OR", "X2.5..", "X97.5..", "AOR", "2.5 %", "97.5 %")
table1$orll <- table1$X2.5..
table1$orul <- table1$X97.5..
table1$aorll <- table1$"2.5 %"
table1$aorul <- table1$"97.5 %"
table1 <- table1 %>% select(hv, OR, orll, orul, AOR, aorll, aorul)


# Ordinal/Ordered logistic regression
# Method 1  https://rpubs.com/BAFlores/872025 
install.packages("svyVGAM")  # This is a new package. As your dependent variable has 5 categories, you can't use multivariate logistic regression which requires only 2 categories (0/1) in dependent variable. However, this method is kind of complicated. Dr. Lin didn't teach that, either. You can choose if you wish to dive deeper. Here's a link for it: https://rpubs.com/BAFlores/872025 
library(svyVGAM)
result <- svy_vglm(as.ordered(y) ~ x1 + x2 + x3 + x4 , design = dt_design, family=cumulative(parallel = TRUE, reverse = TRUE))
summary(result)

# Method 2 https://stats.oarc.ucla.edu/r/seminars/survey-data-analysis-with-r/
ologit1 <- svyolr(factor(dmdeduc2)~factor(female)+factor(dmdborn4)+pad680, design = nhc, method = c("logistic"))
summary(ologit1)





