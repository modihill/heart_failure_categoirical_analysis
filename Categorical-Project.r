#include libraries

library(readr)
library(dplyr)
library(ggplot2)
library(knitr)
library(gridExtra)
library(naniar)
library(egg)
library(janitor)
library(ggcorrplot)
library(plotly)
library(GGally)
library(factoextra)
library(kableExtra)
library(glmulti)
library(plyr)



# Data Cleaning and Preprocessing
#load the dataset

df <- read_csv("ProjectGroups120_Data.csv")


## Heart Failure Datafrmae 

#display the dataset

kbl(head(df)) %>%
  kable_paper()


## Cheking for missing Values

#Summary for missing values
na<-miss_var_summary(df)

kbl(na) %>%
  kable_paper()


## Summary Statistics

### Summary of Explanatory Variables

#Factoring the Categorical Columns 
df$anaemia_ <- factor(df$anaemia, 
                      levels = c(1, 0),
                      labels = c("Yes","No"),
                      ordered = FALSE)

df$diabetes_ <- factor(df$diabetes, 
                       levels = c(1, 0),
                       labels = c("Yes","No"),
                       ordered = FALSE)

df$high_blood_pressure_ <- factor(df$high_blood_pressure, 
                                  levels = c(1, 0),
                                  labels = c("Yes","No"),
                                  ordered = FALSE)

df$sex_ <- factor(df$sex, 
                  levels = c(1, 0),
                  labels = c("Male","Female"),
                  ordered = FALSE)

df$smoking_ <- factor(df$smoking, 
                      levels = c(1, 0),
                      labels = c("Yes","No"),
                      ordered = FALSE)

df$DEATH_EVENT_ <- factor(df$DEATH_EVENT, 
                          levels = c(1, 0),
                          labels = c("Yes","No"),
                          ordered = FALSE)


#creating new dataframe with explanatory columns
df_explanatory <- data.frame("age" = df$age,
                             "creatinine_phosphokinase" = df$creatinine_phosphokinase,
                             "ejection_fraction" = df$ejection_fraction,
                             "platelets"= df$platelets,
                             "serum_creatinine"=df$serum_creatinine,
                             "serum_sodium"=df$serum_sodium,
                             "time"=df$time,
                             "DEATH_EVENT"=df$DEATH_EVENT_
)

#creating new dataframe with catagorical columns
df_categorical <- data.frame("anaemia" = df$anaemia_,
                             "diabetes" = df$diabetes_,
                             "high_blood_pressure" = df$high_blood_pressure_,
                             "sex"= df$sex_,
                             "smoking"=df$smoking_,
                             "DEATH_EVENT"=df$DEATH_EVENT_
)

#Summary of explanatory datafrmae
ex_sum<-summary(df_explanatory)
kbl(ex_sum) %>%
  kable_paper()


#### When Alive

#Summary of Explanatory variables when alive

sum_ex_alive<-summary(df_explanatory %>% filter(DEATH_EVENT=="No"))
kbl(sum_ex_alive) %>%
  kable_paper()

Here, is the Summary statistics of explanatory variable when individuals survive the heart attack.

#### When Dead

#Summary of explanatory variable when ded

sum_ex_dead<-summary(df_explanatory %>% filter(DEATH_EVENT=="Yes"))
kbl(sum_ex_alive) %>%
  kable_paper()

Here, is the Summary statistics of explanatory variable when individuals did not survive the heart attack.

### Summary of Categorical Variables

#Summary of Categorical Dataframe

cat_sum<-summary(df_categorical)
kbl(cat_sum) %>%
  kable_paper()


Here is the Summary Statistics of Categorical Variables.
In which, it is depicted as there are 194 male, 105 individuals have high blood pressure, etc,

#### When Alive

#Summary of Catagorical variables when alive
sum_cat_alive<-summary(df_categorical %>% filter(DEATH_EVENT=="No"))

kbl(sum_cat_alive) %>%
  kable_paper()

Here, is the Summary statistics of categorical variables when individuals survive the heart attack.


#### When Dead

#Summary of categorical variable when dead

sum_cat_dead<-summary(df_categorical %>% filter(DEATH_EVENT=="Yes"))
kbl(sum_cat_dead) %>%
  kable_paper()

Here, is the Summary statistics of categorical variables when heart attack turned into death.

# Data Exploration and Visualisation

## Explanatory Variables
### Density Plots of 1 variables and 2 Variables with Comparison

# Change density plot fill colors by groups
h1<-ggplot(df, aes(x=age)) +
  geom_density(color="black", fill="skyblue", alpha=0.4)

h2<-ggplot(df, aes(x=ejection_fraction)) +
  geom_density(color="black", fill="skyblue", alpha=0.4)

h3<-ggplot(df, aes(x=platelets)) +
  geom_density(color="black", fill="skyblue", alpha=0.4)

h4<-ggplot(df, aes(x=time)) +
  geom_density(color="black", fill="skyblue", alpha=0.4)


# density plots (2 variables)
d1<-ggplot(df, aes(x=age, fill=DEATH_EVENT_)) +
  geom_density(alpha=0.4)

d2<-ggplot(df, aes(x=ejection_fraction, fill=DEATH_EVENT_)) +
  geom_density(alpha=0.4) 

d3<-ggplot(df, aes(x=platelets , fill=DEATH_EVENT_)) +
  geom_density(alpha=0.4) 

d4<-ggplot(df, aes(x=time , fill=DEATH_EVENT_)) +
  geom_density(alpha=0.4) 

gridExtra::grid.arrange(h1,d1)


From above graph, majority of population is aged near 70, while as the age increases, probability of heart attack turns into death in increases. 


gridExtra::grid.arrange(h2,d2)

gridExtra::grid.arrange(h3,d3)

gridExtra::grid.arrange(h4,d4)


### Boxplots of Explanatory Variables(2 variables)

#boxplots of explanatory columns (2 variables)
b1<-ggplot(data = df, mapping = aes(x = DEATH_EVENT_, y = age)) + 
  geom_boxplot()

b2<-ggplot(data = df, mapping = aes(x = DEATH_EVENT_, y = creatinine_phosphokinase)) + 
  geom_boxplot()

b3<-ggplot(data = df, mapping = aes(x = DEATH_EVENT_, y = ejection_fraction)) + 
  geom_boxplot()

b4<-ggplot(data = df, mapping = aes(x = DEATH_EVENT_, y = platelets)) + 
  geom_boxplot()

b5<-ggplot(data = df, mapping = aes(x = DEATH_EVENT_, y = serum_creatinine)) + 
  geom_boxplot()

b6<-ggplot(data = df, mapping = aes(x = DEATH_EVENT_, y = serum_sodium)) + 
  geom_boxplot()

b7<-ggplot(data = df, mapping = aes(x = DEATH_EVENT_, y = time)) + 
  geom_boxplot()

gridExtra::grid.arrange(b1,b2,b3,b4,b5,b6,b7)


## Categorical Variables
### Barplots of Categorical Variables with Frequency and Proportion (2 variate)



#barplots of catagorical variables (2 variavte plots)
g1<-ggplot(data = df) + 
  geom_bar(mapping = aes(x = anaemia_, fill = DEATH_EVENT_), position = "dodge")

g2<-ggplot(data = df) + 
  geom_bar(mapping = aes(x = anaemia_, fill = DEATH_EVENT_), position = "fill")

g3<-ggplot(data = df) + 
  geom_bar(mapping = aes(x = diabetes_, fill = DEATH_EVENT_), position = "dodge")

g4<-ggplot(data = df) + 
  geom_bar(mapping = aes(x = diabetes_, fill = DEATH_EVENT_), position = "fill")

g5<-ggplot(data = df) + 
  geom_bar(mapping = aes(x = high_blood_pressure_, fill = DEATH_EVENT_), position = "dodge")

g6<-ggplot(data = df) + 
  geom_bar(mapping = aes(x = high_blood_pressure_, fill = DEATH_EVENT_), position = "fill")

g7<-ggplot(data = df) + 
  geom_bar(mapping = aes(x = sex_, fill = DEATH_EVENT_), position = "dodge")

g8<-ggplot(data = df) + 
  geom_bar(mapping = aes(x = sex_, fill = DEATH_EVENT_), position = "fill")

g9<-ggplot(data = df) + 
  geom_bar(mapping = aes(x = smoking_, fill = DEATH_EVENT_), position = "dodge")

g10<-ggplot(data = df) + 
  geom_bar(mapping = aes(x = smoking_, fill = DEATH_EVENT_), position = "fill")

gridExtra::grid.arrange(g1,g2,g3,g4,g5,g6)



gridExtra::grid.arrange(g7,g8,g9,g10)



## Whole Dataset
### Scattermatrix of Explanatory Variables with respect to death event.

Blue colour = Alive,
Red Colour = Dead.

#Scatterplot matrix of df_explanatory (Multivariate)
pairs(df_explanatory[,1:7], 
      col=ifelse(df$DEATH_EVENT_=="Yes", 
                 "red", 
                 "blue"))



### Correlation Matrix of Dataset

#correlation Matrix of df 
corr<-cor(df[,1:13]) %>% round(3) 
ggcorrplot(corr,
           method="square",
           outline.color = "black",
           hc.order = TRUE,
           type="lower",
           lab=TRUE)



### Multivariate Plot (Principle Component Analysis)


#Multivariate Analysis
data <- df[,1:12] 
res.pca <- prcomp(data, scale = TRUE)
fviz_pca_biplot(res.pca, col.ind = df$DEATH_EVENT_,
                palette = "jco", geom = "point")


### Scatterplots (3 variate)

#3 variavte plots (Scatterplots)
s1 <- ggplot(data = df) + 
  geom_point(mapping = aes(x = age, y = time, color = DEATH_EVENT_)) +
  geom_smooth(
    mapping = aes(x = age, y = time, color = DEATH_EVENT_),
    show.legend = TRUE
  )


s2 <- ggplot(data = df) + 
  geom_point(mapping = aes(x = platelets, y = ejection_fraction, color = DEATH_EVENT_)) + 
  geom_smooth(
    mapping = aes(x = platelets, y = ejection_fraction, color = DEATH_EVENT_),
    show.legend = TRUE
  )


s3 <- ggplot(data = df) + 
  geom_point(mapping = aes(x = serum_sodium, y = ejection_fraction, color = DEATH_EVENT_)) +
  geom_smooth(
    mapping = aes(x = serum_sodium, y = ejection_fraction, color = DEATH_EVENT_),
    show.legend = TRUE
  )

s4 <- ggplot(data = df) + 
  geom_point(mapping = aes(x = serum_sodium, y = time, color = DEATH_EVENT_)) +
  geom_smooth(
    mapping = aes(x = serum_sodium, y = time, color = DEATH_EVENT_),
    show.legend = TRUE
  )

grid.arrange(s1,s2,s3,s4)

# Statistical Modelling
## Model Fitting

**#Stepwise Search Algorithm**
  
empty.mod <- glm(formula = DEATH_EVENT ~ 1, family =
                   binomial(link = logit), data = subset (df, select = -c(high_blood_pressure_, DEATH_EVENT_)))
full.mod <- glm(formula = DEATH_EVENT ~ ., family =
                  binomial(link = logit), data = subset (df, select = -c(high_blood_pressure_,DEATH_EVENT_)))

forward_selection <- step(object = empty.mod, 
                          scope = list(upper = full.mod), 
                          direction = "forward", k = 2, trace = TRUE)
summary(forward_selection)


#Model Evaluation

w <- aggregate(formula = DEATH_EVENT ~ age + ejection_fraction + serum_creatinine + serum_sodium + time + high_blood_pressure + anaemia, 
               data = df,
               FUN = sum)

n <- aggregate(formula = DEATH_EVENT ~ age + ejection_fraction + serum_creatinine + serum_sodium + time + high_blood_pressure + anaemia,
               data = df,
               FUN = length)

w.n <- data.frame(w[,-c(8)],
                  total_cases = n$DEATH_EVENT, 
                  total_deaths = w$DEATH_EVENT,
                  proportion = round(w$DEATH_EVENT/n$DEATH_EVENT,4))

kbl(head(w.n)) %>%
  kable_paper()



#Logistic Regression Analysis


## Logistic Regression Analysis
mod.fit <- glm(
                formula = proportion ~ 1 + age + ejection_fraction + serum_creatinine + serum_sodium + time + high_blood_pressure + anaemia , 
                family = binomial(link = logit), 
                data = w.n
              )
summary(mod.fit)


## Resudial Analysis


# age
stand.resid <- rstandard(model = mod.fit, 
                         type = "pearson")
plot(
     x = w.n$age, 
     y = stand.resid, 
     ylim = c(min(-3, stand.resid), max(3, stand.resid)), 
     xlab = "age",
     ylab = "Standardized Pearson residuals"
     )

abline(
       h = c(3, 2, 0, -2, -3), 
       lty = 3, 
       col = "blue"
       )

smooth.stand <- loess(
                      formula = stand.resid ~ age,
                      data = w.n
                      )

ord.age <- order(w.n$age)

lines(
      x = w.n$age[ord.age], 
      y = predict(smooth.stand)[ord.age], 
      lty = "solid", 
      col = "red"
      )



# ejection_fraction
stand.resid_e <- rstandard(model = mod.fit, 
                         type = "pearson")
plot(
  x = w.n$ejection_fraction, 
  y = stand.resid_e, 
  ylim = c(min(-3, stand.resid_e), max(3, stand.resid_e)), 
  xlab = "ejection_fraction",
  ylab = "Standardized Pearson residuals"
)

abline(
  h = c(3, 2, 0, -2, -3), 
  lty = 3, 
  col = "blue"
)

smooth.stand_e <- loess(
  formula = stand.resid_e ~ ejection_fraction,
  data = w.n
)

ord.ejection_fraction <- order(w.n$ejection_fraction)

lines(
  x = w.n$ejection_fraction[ord.ejection_fraction], 
  y = predict(smooth.stand)[ord.ejection_fraction], 
  lty = "solid", 
  col = "red"
)



# serum_sodium
stand.resid_ss <- rstandard(model = mod.fit, 
                           type = "pearson")
plot(
  x = w.n$serum_sodium, 
  y = stand.resid_ss, 
  ylim = c(min(-3, stand.resid_ss), max(3, stand.resid_ss)), 
  xlab = "serum_sodium",
  ylab = "Standardized Pearson residuals"
)

abline(
  h = c(3, 2, 0, -2, -3), 
  lty = 3, 
  col = "blue"
)

smooth.stand_ss <- loess(
  formula = stand.resid_ss ~ serum_sodium,
  data = w.n
)

ord.serum_sodium <- order(w.n$serum_sodium)

lines(
  x = w.n$serum_sodium[ord.serum_sodium], 
  y = predict(smooth.stand_ss)[ord.serum_sodium], 
  lty = "solid", 
  col = "red"
)


# time
stand.resid_t <- rstandard(model = mod.fit, 
                            type = "pearson")
  plot(
  x = w.n$time, 
  y = stand.resid_t, 
  ylim = c(min(-3, stand.resid_t), max(3, stand.resid_t)), 
  xlab = "time",
  ylab = "Standardized Pearson residuals"
)

abline(
  h = c(3, 2, 0, -2, -3), 
  lty = 3, 
  col = "blue"
)

smooth.stand_t <- loess(
  formula = stand.resid_t ~ serum_sodium,
  data = w.n
)

ord.time <- order(w.n$time)

lines(
  x = w.n$time[ord.time], 
  y = predict(smooth.stand_t)[ord.time], 
  lty = "solid", 
  col = "red"
)

## Responce Analysis


# high blood pressure - frequency
g1 <- ggplot(data = df) + 
  geom_bar(mapping = aes(x = high_blood_pressure_, fill = DEATH_EVENT_), position = "dodge")

# high blood pressure - proportion
g2 <- ggplot(data = df) + 
  geom_bar(mapping = aes(x = high_blood_pressure_, fill = DEATH_EVENT_), position = "fill")


# Density plot of ejection fraction
mu_ejection_fraction <- ddply(df, "DEATH_EVENT_", summarise, grp.mean=mean(ejection_fraction))

g3 <- ggplot(df, aes(x = ejection_fraction, 
                     color = DEATH_EVENT_,
                     fill = DEATH_EVENT_
                     )
             ) +
             geom_density(alpha = 0.4) +
             geom_vline(
                        data = mu_ejection_fraction, 
                        aes( 
                            xintercept = grp.mean, 
                            color = DEATH_EVENT_
                            ),
                        linetype="dashed"
                        ) 

# density ploy of age
mu_age <- ddply(df, "DEATH_EVENT_", summarise, grp.mean=mean(age))

g4 <- ggplot(df, aes(x = age, 
                     color = DEATH_EVENT_,
                     fill = DEATH_EVENT_
                     )
             ) +
     geom_density(alpha = 0.4) +
     geom_vline(
                data = mu_age, aes( 
                                xintercept = grp.mean, 
                                color = DEATH_EVENT_
                                ),
                linetype="dashed"
                ) 

gridExtra::grid.arrange(g1,g2,g3,g4)


## Goodness of fit
GOF <- mod.fit$deviance / mod.fit$df.residual

text_tbl <- data.frame(
  Goodness_of_fit = c(
                      "Our Model",
                      "Good Fit",
                      "Potential Problem",
                      "Poor Fit" 
                      ),
  Values = (round(c(GOF,
            1 + 1*sqrt(2/mod.fit$df.residual),
            1 + 2*sqrt(2/mod.fit$df.residual),
            1 + 3*sqrt(2/mod.fit$df.residual)
            ), 
          2)))


kbl(text_tbl) %>%
  kable_paper()



## Confidence Intervals

# Alpha for 95% Confidence Interval
alpha <- 0.05

# Linear Prediction Model with Link
linear.pred_link <- predict(object = mod.fit, 
                       type = "link", 
                       se = TRUE)

# Linear Prediction Model with type = responce for probability of success
linear.pred_response <- predict(object = mod.fit, 
                       type = "response", 
                       se = TRUE)


# CI For 1st Observation
CI.lin.pred_1_lower <- linear.pred_link$fit[1] + qnorm(p = c(alpha/2))*linear.pred_link$se[1]
CI.lin.pred_1_upper <- linear.pred_link$fit[1] + qnorm(p = c(1 - alpha/2))*linear.pred_link$se[1]

CI.pi_1_lower <- exp(CI.lin.pred_1_lower)/(1+exp(CI.lin.pred_1_lower))
CI.pi_1_upper <- exp(CI.lin.pred_1_upper)/(1+exp(CI.lin.pred_1_upper))


# CI For 2nd Observation
CI.lin.pred_2_lower <- linear.pred_link$fit[2] + qnorm(p = c(alpha/2))*linear.pred_link$se[2]
CI.lin.pred_2_upper <- linear.pred_link$fit[2] + qnorm(p = c(1 - alpha/2))*linear.pred_link$se[2]

CI.pi_2_lower <- exp(CI.lin.pred_2_lower)/(1+exp(CI.lin.pred_2_lower))
CI.pi_2_upper <- exp(CI.lin.pred_2_upper)/(1+exp(CI.lin.pred_2_upper))


# CI For 5th Observation
CI.lin.pred_5_lower <- linear.pred_link$fit[5] + qnorm(p = c(alpha/2))*linear.pred_link$se[5]
CI.lin.pred_5_upper <- linear.pred_link$fit[5] + qnorm(p = c(1 - alpha/2))*linear.pred_link$se[5]

CI.pi_5_lower <- exp(CI.lin.pred_5_lower)/(1+exp(CI.lin.pred_5_lower))
CI.pi_5_upper <- exp(CI.lin.pred_5_upper)/(1+exp(CI.lin.pred_5_upper))

text_tbl <- data.frame(
  index = c( 
    "1st Record",
    "2nd Record",
    "5th Record" 
    ),
  
  link_lower = (round(c(
                        CI.lin.pred_1_lower,
                        CI.lin.pred_2_lower,
                        CI.lin.pred_5_lower
                        ),
                        2)
                ),
  link_value = (round(c(
                        linear.pred_link$fit[1],
                        linear.pred_link$fit[2],
                        linear.pred_link$fit[5]
                        ),
                      2)
                ),
  link_upper = (round(c(
                        CI.lin.pred_1_upper,
                        CI.lin.pred_2_upper,
                        CI.lin.pred_5_upper
                        ),
                      2)
                ),
  probability_lower = (round(c(
                                CI.pi_1_lower,
                                CI.pi_2_lower,
                                CI.pi_5_lower
                                ),
                                2)
                      ),
  probability_value = (round(c(
                            linear.pred_response$fit[1],
                            linear.pred_response$fit[2],
                            linear.pred_response$fit[5]
                            ),
                          2)
                    ),
  probability_upper = (round(c(
                                CI.pi_1_upper,
                                CI.pi_2_upper,
                                CI.pi_5_upper
                                ),
                             2)
                       )
  )


CI.lin.pred_lower <- linear.pred_link$fit + qnorm(p=c(alpha/2))*linear.pred_link$se

CI.lin.pred_upper <- linear.pred_link$fit + qnorm(p = c(1 - alpha/2))*linear.pred_link$se

CI.pi_lower <- exp(CI.lin.pred_lower)/(1+exp(CI.lin.pred_lower))
CI.pi_upper <- exp(CI.lin.pred_upper)/(1+exp(CI.lin.pred_upper))


Confidence_Interval <- data.frame(CI.lin.pred_lower,linear.pred_link$fit,CI.lin.pred_upper,CI.pi_lower,linear.pred_response$fit, CI.pi_upper) %>% round(3)

#kbl(head(Confidence_Interval)) %>% kable_paper()


kbl(text_tbl) %>%
  kable_paper()



## Hypotheses Test

summary(mod.fit)





text_tbl <- data.frame(
  name = c(
    "H0",
    "Ha",
    "Z0",
    "p-value",
    "Decision",
    "Interpretation"
  ),
  age = c(
    "??1 = 0",
    "??1 != 0",
    "2.830",
    "0.00465",
    "Reject H0 because p-value is small",
    "There is marginal evidence to indicate age has an effect on the probability of success given above variable are in the model"
    ),
  ejection_fraction = c(
    "??2 = 0",
    "??2 != 0",
    "-4.651",
    "3.30e-06",
    "Reject H0 because p-value is small",
    "There is sufficient evidence to indicate ejection_fraction has an effect on the probability of success given above variable are in the model"
    ),
  serum_creatinine = c(
    "??3 = 0",
    "??3 != 0",
    "3.926",
    "8.65e-05",
    "Reject H0 because p-value is small",
    "There is sufficient evidence to indicate serum-creatinine has an effect on the probability of success given above variable are in the model"
    ),
  serum_sodium = c(
    "??4 = 0",
    "??4 != 0",
    "-1.673",
    "0.09429",
    "Do not reject H0 because p-value is little large than significant level of 0.05",
    "There is not sufficient evidence to indicate serum_sodium has an effect on the probability of success given above variable are in the model"
    ),
  time = c(
    "??5 = 0",
    "??5 != 0",
    "-7.075",
    "1.49e-12",
    "Reject H0 because p-value is small",
    "There is sufficient evidence to indicate time has an effect on the probability of success given above variable are in the model"
    ),
  high_blood_pressure = c(
    "??6 = 0",
    "??6 != 0",
    "-0.163",
    "0.87072",
    "Do not reject H0 because p-value is very large than significant level of 0.05",
    "There is not sufficient evidence to indicate high_blood_pressure has an effect on the probability of success given above variable are in the model"
    ),
  anaemia = c(
    "??7 = 0",
    "??7 != 0",
    "-0.042",
    "0.96668",
    "Do not reject H0 because p-value is very large than significant level of 0.05",
    "There is not sufficient evidence to indicate anaemia has an effect on the probability of success given above variable are in the model"
    )
)

kbl(text_tbl) %>%
  kable_paper()






## Sensitivity Analysis


# Odds Ratio
odd_age <- exp(10*mod.fit$coefficients[2])
odd_ejection_fraction <- 1/exp(10*mod.fit$coefficients[3])
odd_serum_creatinine <- exp(1*mod.fit$coefficients[4])
odd_serum_sodium <- exp(10*mod.fit$coefficients[5])
odd_time <- exp(10*mod.fit$coefficients[6])


text_tbl <- data.frame(
  Features = c(
    "Age",
    "Ejection Fraction",
    "Serum Creatinine",
    "Serum Sodium",
    "Time"
  ),
  Unit = c(
    "10 years",
    "10 Percentage",
    "1 mg/dL",
    "10 mEq/L",
    "10 Days"
    ),
  odds_ratio = c(
    as.numeric(odd_age %>% round(3)),
    as.numeric(odd_ejection_fraction %>% round(3)),
    as.numeric(odd_serum_creatinine %>% round(3)),
    as.numeric(odd_serum_sodium %>% round(3)),
    as.numeric(odd_time %>% round(3))
  ),
  Interpretation = c(
    "Estimated odds of Death changes by 1.530 times for every 10 year increases of the Patient",
    "Estimated odds of death changes by 2.085 times for decreasing of every 10 percentage of blood leaving the heart at each contraction ",
    "Estimated odds of death changes by 1.985 times for every 1 mg/dL of serum creatinine is increased in blood",
    "Estimated odds of death changes by 0.525 times for every 10 mEq/L of serum sodium is increased in blood",
    "Estimated odds of Death changes by 0.812 times for every 10 days increases in the follow-up period"
  )
)

kbl(text_tbl) %>%
  kable_paper()