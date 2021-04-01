#DONE
library(haven)
library(dplyr)
library(ggplot2)
library(MatchIt)
library(psych) # for dummy variables
homework1 <- read_sas("~/Dropbox/marketing models 2021/Session 6/HW/homework1.sas7bdat", NULL)
homework1$Socio_Economical_Class <- as.character(homework1$Socio_Economical_Class)
homework1$adopt <- as.factor(homework1$adopt)
homework1$nonadopter <- as.factor(homework1$nonadopter)
socio_dummy <- dummy.code(homework1$Socio_Economical_Class)
colnames(socio_dummy) <- c("Socio_Economical_Class_3","Socio_Economical_Class_2", "Socio_Economical_Class_1", "Socio_Economical_Class_4")
homework1 <- data.frame(homework1, socio_dummy)
homework2 <- read_sas("~/Dropbox/marketing models 2021/Session 6/HW/homework2.sas7bdat", NULL)

## Homework 1 NOT READY
#To secure their piece of the online grocery pie, many retailers are rushing into the click and collect (Drive) format, where shoppers place orders online and pick up the goods themselves later.  We want to study how the adoption of such a concept changes consumers’ spending at the focal retailer (French retailer Leclerc) who opened the Drive format

#Does adoption change the spending behavior at the focal retailer?

#This exercise shows the steps of adopting the "Drive" (Treated), as opposed to not adopting (Control), on consumer spending. Propensity score matching is used to match adopters wih non-adopters. 

#The following steps were taken:
  
 # **** Change **** 
#  * Estimate the propensity score.
#* Examine the region of common support.
#Choose and execute a matching algorithm. In this tutorial we’ll use nearest neighbor propensity score matching.
#Examine covariate balance after matching.
#Estimate treatment effects.

# 1. Pre-Analysis NOT READY

## 1.1 Difference-in-means: Share of Wallet

#The following table shows the differences in means of share of wallet for the independent variable of interest (nonadopter), which classifies panelists(1) that have adopted and those who have not yet adopted (0). 


## 1.1 READY
homework1 %>%
  group_by(nonadopter) %>%
  summarise(n_panelmembers = n(),
            mean_sow = mean(sow_before),
            std_error = sd(sow_before) / sqrt(n_panelmembers))


#There is statistical significance in the difference in means of the share of wallet between adopters and non adopters
#```{r 1.1.1}
with(homework1, t.test(sow_before ~ nonadopter))

with(homework1, t.test(PERIOREF ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(Age ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(HHS ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(Socio_Economical_Class_1 ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(Socio_Economical_Class_2 ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(Socio_Economical_Class_3 ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(Socio_Economical_Class_4 ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(households ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(lbsktsize_rev_pan ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(lnr_majors_pan ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(lnr_shoptrips_per ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(localshop ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(ltotalspent_internet ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(ltotalspent_leclerc ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(ms_sq_meters_lec ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(nr_compdrives ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(nr_hd_lec ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(nr_hd_nonlec ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(nr_hmsm_lec ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(nr_hmsm_nonlec ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(numdrives ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(zone1 ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(zone2 ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(zone3 ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(zone4 ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(zone5 ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(zone6 ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(zone7 ~ nonadopter))  #(repeat for each covariate)
with(homework1, t.test(zone8 ~ nonadopter))  #(repeat for each covariate)
#```

## 1.2 Difference-in-means of covariates  ADD MORE COVARIATES AND REASON WHY WHERE THEY ADDED

#* PERIOREF:	identifies 4-weekly period
#* Age: age panelmember
#* HHS: Number of people in the household
#* Socio_Economical_Class:	categorical variable; indicates social class
#* households: total number of households in the local market
#* lbsktsize_rev_pan	average  basket size per trip (lagged)
#* lnr_majors_pan	number of major trips made by household
#* lnr_shoptrips_per	number of trips made by household per month (lagged)
#* localshop	number of local shops in the local market in which Drive is opened
#* ltotalspent_internet	total budget spent on line by household (lagged)
#* ltotalspent_leclerc	total spent at focal retailer (leclerc) lagged
#* ms_sq_meters_lec	share of store floor owned by focal retailer in local market in which Drive concept opens
#* nr_compdrives	Number of competing Drives in local market
#* nr_hd_lec	number of competing price fighters owned by focal retailer
#* nr_hd_nonlec	number of competing price fighters owned bycompeting  retailers in focal market
#* nr_hmsm_lec	number of competing super and hypermarkets owned by focal retailer
#* nr_hmsm_nonlec	number of competing super and hypermarketss owned by competing  retailers in focal market
#* numdrives	
#* zone1	dummy variables indicating type of market (rural, urban etc….)
#* zone2	
#* zone3	
#* zone4	
#* zone5	
#* zone6	
#* zone7	
#* zone8	
#```{r 1.2}
homework1_cov <- c('PERIOREF', 'Age', 'HHS', 
                   'Socio_Economical_Class_1', 'Socio_Economical_Class_2', 'Socio_Economical_Class_3', 'Socio_Economical_Class_4',
                   'households', 'lbsktsize_rev_pan', 'lnr_majors_pan', 'lnr_shoptrips_per', 'localshop', 'ltotalspent_internet', 
                   'ltotalspent_leclerc', 'ms_sq_meters_lec', 'nr_compdrives', 'nr_hd_lec', 'nr_hd_nonlec', 'nr_hmsm_lec', 
                   'nr_hmsm_nonlec', 'numdrives', 'zone1', 'zone2', 'zone3', 'zone4', 'zone5', 'zone6', 'zone7', 'zone8') 


### Maybe not include this
homework1 %>%
  group_by(nonadopter) %>%
  select(one_of(homework1_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))
#```


#**COMMENT ON WHAT I SEE!!!**
  
  
#  ```{r 1.2.1} # maybe not include this (why do i need it?)
#with(homework1, t.test(Age ~ nonadopter))  #(repeat for each covariate)
#```

# 2 Propensity Score Estimation

#**change test**
#  We estimate the propensity score by running a logit model (probit also works) where the outcome variable is a binary variable indicating treatment status. What covariates should you include? For the matching to give you a causal estimate in the end, you need to include any covariate that is related to both the treatment assignment and potential outcomes. I choose just a few covariates below—they are unlikely to capture all covariates that should be included. You’ll be asked to come up with a potentially better model on your own later.

#```{r 2} # ready
m1 <- glm(nonadopter ~ PERIOREF + Age + HHS + Socio_Economical_Class_1 + 
            Socio_Economical_Class_2 + Socio_Economical_Class_3 + Socio_Economical_Class_4 + 
            households + lbsktsize_rev_pan + lnr_majors_pan + lnr_shoptrips_per + localshop + 
            ltotalspent_internet + ltotalspent_leclerc + ms_sq_meters_lec + nr_compdrives +
            nr_hd_lec + nr_hd_nonlec + nr_hmsm_lec + nr_hmsm_nonlec + numdrives + 
            zone1 + zone2 + zone3 + zone4 + zone5 + zone6 + zone7 + zone8 - 1,
          family = binomial(), data = homework1)
summary(m1)
#```


#**change text**
#  Using this model, we can now calculate the propensity score for each student. It is simply the student’s predicted probability of being Treated, given the estimates from the logit model. Below, I calculate this propensity score using predict() and create a dataframe that has the propensity score as well as the student’s actual treatment status.

#```{r 2.0.1}
m1_df <- data.frame(propensity_score = predict(m1, type = "response"),
                    nonadopter = m1$model$nonadopter)
head(m1_df)
summary(m1_df)
#```

## 2.1 Examine output
#**change text**
#  After estimating the propensity score, it is useful to plot histograms of the estimated propensity scores by treatment status:
  
  
#  ```{r} DONE
labs <- paste("Drive", c("Non-Adopter", "Adopter"))
m1_df %>%
  mutate(nonadopter = ifelse(nonadopter == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = propensity_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~nonadopter) +
  xlab("Probability of adopting Drive") +
  theme_bw()
#```

# 3 Matching Algorithm 

#**change text**
#  A simple method for estimating the treatment effect of Catholic schooling is to restrict the sample to observations within the region of common support, and then to divide the sample within the region of common support into 5 quintiles, based on the estimated propensity score. Within each of these 5 quintiles, we can then estimate the mean difference in student achievement by treatment status. Rubin and others have argued that this is sufficient to eliminate 95% of the bias due to confounding of treatment status with a covariate.

#However, most matching algorithms adopt slightly more complex methods. The method we use below is to find pairs of observations that have very similar propensity scores, but that differ in their treatment status. We use the package MatchIt for this. This package estimates the propensity score in the background and then matches observations based on the method of choice (“nearest” in this case).

#```{r 3.0} DONE
homework1_nomiss <- homework1 %>%  # MatchIt does not allow missing values
  select(sow_before, nonadopter, adopt, one_of(homework1_cov)) %>%
  na.omit() %>%
  mutate(adopter = as.factor(ifelse(nonadopter == 1, 0, 1)))


# Warning: Fewer control units than treated units; not all treated units will get a match. 
#changed the treated variable to adopter to avoid this error
m2 <- matchit(adopter ~ PERIOREF + Age + HHS + Socio_Economical_Class_1 + 
                Socio_Economical_Class_2 + Socio_Economical_Class_3 + Socio_Economical_Class_4 + 
                households + lbsktsize_rev_pan + lnr_majors_pan + lnr_shoptrips_per + localshop + 
                ltotalspent_internet + ltotalspent_leclerc + ms_sq_meters_lec + nr_compdrives +
                nr_hd_lec + nr_hd_nonlec + nr_hmsm_lec + nr_hmsm_nonlec + numdrives + 
                zone1 + zone2 + zone3 + zone4 + zone5 + zone6 + zone7 + zone8 - 1,
              method = "nearest", data = homework1_nomiss)

#```

#**change text**
#  We can get some information about how successful the matching was using summary(mod_match) and plot(mod_match) (try this yourself).

#To create a dataframe containing only the matched observations, use the match.data() function:
  
 # ```{r 3.0.1}
df2 <- match.data(m2)
dim(df2)

m2.sum <- summary(m2)
plot(m2.sum,  var.order = "unmatched")
plot(m2, type = "qq", which.xs = c("lbsktsize_rev_pan", "ltotalspent_leclerc", "zone7"))


#```


# ```{r 3.0.1}
dfe <- match.data(me)
dim(dfe)
#```
#**update values**
 # The final dataset contains 15456 observations, or 7728 pairs of treated and control variables. 

# 4 Examine Covariate Balance

## 4.1 Visual Inspection 

#```{r 4.1, echo=FALSE}
#df2 <- unlist(df2)
# Examine Covariate Balance
df2$adopter <- as.factor(df2$adopter)

p_Age <- ggplot(df2, aes(x = distance, y = Age, color = adopter)) +
  geom_point(alpha = 0.2, size = 1.3) +
  geom_smooth(method = "loess", se = F) +
  xlab("Propensity score") +
  ylab("Age") +
  theme_bw() 

p_HHS <- ggplot(df2, aes(x = distance, y = HHS, color = adopter)) +
  geom_point(alpha = 0.2, size = 1.3) +
  geom_smooth(method = "loess", se = F) +
  xlab("Propensity score") +
  ylab("HHS") +
  theme_bw() 

p_households <- ggplot(df2, aes(x = distance, y = households, color = adopter)) +
  geom_point(alpha = 0.2, size = 1.3) +
  geom_smooth(method = "loess", se = F) +
  xlab("Propensity score") +
  ylab("households") +
  theme_bw() 

p_lbsktsize_rev_pan <- ggplot(df2, aes(x = distance, y = lbsktsize_rev_pan, color = adopter)) +
  geom_point(alpha = 0.2, size = 1.3) +
  geom_smooth(method = "loess", se = F) +
  xlab("Propensity score") +
  ylab("lbsktsize_rev_pan") +
  theme_bw() 
p_Age 
p_HHS 
p_households
p_lbsktsize_rev_pan 
#```

# 4.2 Difference-in-means

#**change the text**
 # The means below indicate that we have attained a high degree of balance on the five covariates included in the model.

#```{r 4.2}
df2 %>%
  group_by(adopter) %>%
  select(one_of(homework1_cov)) %>%
  summarise_all(funs(mean))
#```

#```{r 4.2.1}


#* PERIOREF:	identifies 4-weekly period
#* Age: age panelmember
#* HHS: Number of people in the household
#* Socio_Economical_Class:	categorical variable; indicates social class
#* households: total number of households in the local market
#* lbsktsize_rev_pan	average  basket size per trip (lagged)
#* lnr_majors_pan	number of major trips made by household
#* lnr_shoptrips_per	number of trips made by household per month (lagged)
#* localshop	number of local shops in the local market in which Drive is opened
#* ltotalspent_internet	total budget spent on line by household (lagged)
#* ltotalspent_leclerc	total spent at focal retailer (leclerc) lagged
#* ms_sq_meters_lec	share of store floor owned by focal retailer in local market in which Drive concept opens
#* nr_compdrives	Number of competing Drives in local market
#* nr_hd_lec	number of competing price fighters owned by focal retailer
#* nr_hd_nonlec	number of competing price fighters owned bycompeting  retailers in focal market
#* nr_hmsm_lec	number of competing super and hypermarkets owned by focal retailer
#* nr_hmsm_nonlec	number of competing super and hypermarketss owned by competing  retailers in focal market
#* numdrives	
#* sow_before	share of wallet of panelmember at focal retailer
#* zone1	dummy variables indicating type of market (rural, urban etc….)
#* zone2	
#* zone3	
#* zone4	
#* zone5	
#* zone6	
#* zone7	
#* zone8	
with(df2, t.test(PERIOREF ~ adopter))  #(repeat for each covariate)
with(df2, t.test(Age ~ adopter))  #(repeat for each covariate)
with(df2, t.test(HHS ~ adopter))  #(repeat for each covariate)
with(df2, t.test(Socio_Economical_Class_1 ~ adopter))  #(repeat for each covariate)
with(df2, t.test(Socio_Economical_Class_2 ~ adopter))  #(repeat for each covariate)
with(df2, t.test(Socio_Economical_Class_3 ~ adopter))  #(repeat for each covariate)
with(df2, t.test(Socio_Economical_Class_4 ~ adopter))  #(repeat for each covariate)
with(df2, t.test(households ~ adopter))  #(repeat for each covariate)
with(df2, t.test(lbsktsize_rev_pan ~ adopter))  #(repeat for each covariate)
with(df2, t.test(lnr_majors_pan ~ adopter))  #(repeat for each covariate)
with(df2, t.test(lnr_shoptrips_per ~ adopter))  #(repeat for each covariate)
with(df2, t.test(localshop ~ adopter))  #(repeat for each covariate)
with(df2, t.test(ltotalspent_internet ~ adopter))  #(repeat for each covariate)
with(df2, t.test(ltotalspent_leclerc ~ adopter))  #(repeat for each covariate)
with(df2, t.test(ms_sq_meters_lec ~ adopter))  #(repeat for each covariate)
with(df2, t.test(nr_compdrives ~ adopter))  #(repeat for each covariate)
with(df2, t.test(nr_hd_lec ~ adopter))  #(repeat for each covariate)
with(df2, t.test(nr_hd_nonlec ~ adopter))  #(repeat for each covariate)
with(df2, t.test(nr_hmsm_lec ~ adopter))  #(repeat for each covariate)
with(df2, t.test(nr_hmsm_nonlec ~ adopter))  #(repeat for each covariate)
with(df2, t.test(numdrives ~ adopter))  #(repeat for each covariate)
with(df2, t.test(zone1 ~ adopter))  #(repeat for each covariate)
with(df2, t.test(zone2 ~ adopter))  #(repeat for each covariate)
with(df2, t.test(zone3 ~ adopter))  #(repeat for each covariate)
with(df2, t.test(zone4 ~ adopter))  #(repeat for each covariate)
with(df2, t.test(zone5 ~ adopter))  #(repeat for each covariate)
with(df2, t.test(zone6 ~ adopter))  #(repeat for each covariate)
with(df2, t.test(zone7 ~ adopter))  #(repeat for each covariate)
with(df2, t.test(zone8 ~ adopter))  #(repeat for each covariate)



#```

# 5 Estimating treatment effects
#**change the text**
#  Estimating the treatment effect is simple once we have a matched sample that we are happy with. We can use a t-test:
 # ```{r 5.0}
with(df2, t.test(sow_before ~ adopter))
#```

#Or we can use OLS with or without covariates:
  
df2 <- df2 %>% mutate(ln_s = log(sow_before+ 0.0000001))

#  ```{r}
m3 <- lm(ln_s ~  adopt*PERIOREF + adopter, data = df2)
summary(m3)
#```



