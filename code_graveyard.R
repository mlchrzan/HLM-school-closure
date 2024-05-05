```{r compare-m6-m7}
anova(m7, m6)
```

## A model with random slopes

```{r random-slopes}
m8 <- lmer(num_closed ~ 
             I(year*year)
           + I(avg_schl_pct_dist_enroll*avg_schl_pct_dist_enroll)
           + avg_schl_st_ratio 
           + per_pupil_expend
           + st_ratio
           + I(num_schools*num_schools)
           + location_type
           + (1 + year + location_type | agency_id),
           data = closure_dist_CA_std,
           REML = F)

summary(m8)
```

```{r compare-m5andm6-m8}
anova(m8, m7)
anova(m8, m6)
anova(m8, m5)
```

```{r check-REML}
m9 <- lmer(num_closed ~ 
             I(year*year)
           + I(avg_schl_pct_dist_enroll*avg_schl_pct_dist_enroll)
           + avg_schl_st_ratio 
           + per_pupil_expend
           + st_ratio
           + I(num_schools*num_schools)
           + location_type
           + (1 + year + location_type | agency_id),
           data = closure_dist_CA_std,
           REML = T)
summary(m8)
summary(m9)
```

```{r generate-result-table}
tab_model( m2, m3, m5, m6, m8,
           dv.labels = c('with Random Intercept', 
                         "with time-varying predictors", 
                         "with level 2 predictors", 
                         "with covariance", 
                         "with random slopes"),
           show.se = TRUE,
           show.dev = TRUE,
           p.style = 'stars',
           show.ci = FALSE)
```


```{r make-testing-samples}
set.seed(1234)
down_sample <- closure_CA |> 
  downSample(y = closure_CA$closed)

down_sample <- down_sample |> droplevels()
```

```{r down-sample}
m0_d <- glm(closed ~ year,  
            data = down_sample,
            family = binomial(link = "logit"))


m1_d <- glmer(closed ~ year + (1 | school_id) ,  
              data = down_sample,
              family = binomial(link = "logit"))

summary(m0_d)
summary(m1_d)
```

up_sample <- closure_CA |> 
  upSample(y = closure_CA$closed)

up_sample <- up_sample |> droplevels()


# Modified down sample
mod_down_samp <- anti_join(closure_CA, down_sample, 
                           by = join_by(year, school_id)) |> 
  filter(closed == 0) |> 
  slice_sample(n = 2640) |> 
  bind_rows(down_sample)



# Generate a random sample that maintains the proportions of the outcome variable
sample <- closure_CA |> 
  group_by(closed) |> 
  slice_sample(prop = 0.1) |> 
  ungroup()


# Check Sample Proportions
sample |> 
  group_by(closed) |> 
  summarize(n = n()) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup()

down_sample |> 
  group_by(closed) |> 
  summarize(n = n()) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup()

up_sample |> 
  group_by(closed) |> 
  summarize(n = n()) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup()

mod_down_samp |> 
  group_by(closed) |> 
  summarize(n = n()) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup()


{r up-sample}
m0 <- glm(closed ~ year,  
          data = up_sample,
          family = binomial(link = "logit"))



m1 <- glmer(closed ~ year + (1 | school_id) ,  
            data = up_sample,
            family = binomial(link = "logit"))

summary(m0)
summary(m1)

#start with regular glm and then input the values in a MLGLM as starting values
#try to find a way to manually downsample the data


{r modified-down-sample}
m0_mod_d <- glm(closed ~ year,  
                data = mod_down_samp,
                family = binomial(link = "logit"))



m1_mod_d <- glmer(closed ~ year + (1 | school_id) ,  
                  data = mod_down_samp,
                  family = binomial(link = "logit"))

summary(m0_mod_d)
summary(m1_mod_d)


#See states with most closure
closure |> 
  group_by(closed, dist_state_abbr) |> 
  summarize(n = n()) |> 
  mutate(prop = n / sum(n)) |> 
  ungroup() |> 
  filter(closed == 1) |> 
  arrange(desc(prop))


{r cleaning-year-factor}
# closure_CA <- closure_CA |> 
#   filter(year != 2000 & year != 2001 & year != 2002)
# 
# closure_CA <- closure_CA |> 
#   mutate(year = factor(year, levels = c(2003:2017)))


# OLD AND WRONG

## 5 Hypothesis

```{r mean-center}
# vars <- closure_CA  |>  
#   select(where(is.numeric))  |>  
#   names() 
# vars <- setNames(vars, str_c(vars, "_c")) 
# #vars
# 
# closure_CA <- closure_CA |>
#   mutate(across(all_of(vars), ~ . - mean(., na.rm = TRUE))) 
```

```{r check-mean-center}
# closure_CA |>
#   select(ends_with("_c")) |>
#   summary()
```

## Step 1

```{r}
m1 <- glm(closed ~ 1 ,
          data = closure_CA, 
          family = 'binomial')
summary(m1)
```

```{r}
logLik(m1) #deviance
```

## Step 2

```{r random-intercept}
m2 <- glmer(closed ~ (1 | school_id),  
            data = closure_CA, 
            family = 'binomial') 

summary(m2)
```

```{r icc}
icc(m2)
```

```{r anova-m2-m1}
(2* logLik(m2)) - (2* logLik(m1))

anova(m2,m1)
```

## Step 3: A model with a level 1 predictor

```{r variables}
#SCHOOL LEVEL
#pct_dist_enroll
#st_ratio
#school_level
#school_more_diverse ???

#DISTRICT LEVEL
#dist_five_year_enroll 
#dist_st_ratio
#dist_per_pupil_expend
#num_schools
```

```{r}
closure_CA |> 
  ggplot(aes(x = year)) + geom_histogram()
```

```{r level-1-predictors}
m3 <- glmer(closed ~ 
              pct_dist_enroll 
            #+ st_ratio 
            #+ school_level 
            + (1 | school_id),  
            data = closure_CA, 
            family = "binomial") 

summary(m3)
```

```{r model-comparison-2}
anova(m3, m2)
#(2* logLik(m2)) - (2* logLik(m1))
#(2* logLik(m3)) - (2* logLik(m2))

#Not nested models! There was some listwise deletion from one of the covariates missing variables

#Take out all the missing values after selecting the variables, do some naniar analysis on the missing data 

# logLik(m1)
# logLik(m2)
# logLik(m3)
```

## Step 4 a model with level 2 predictors

```{r level-2-predictors}
#DISTRICT LEVEL
#dist_five_year_enroll 
#dist_st_ratio
#dist_per_pupil_expend
#num_schools

m4 <- glmer(closed ~ pct_di5st_enroll + st_ratio + school_level 
            + three_yr_enroll_dist + dist_st_ratio + dist_per_pupil_expend 
            + dist_num_schools + (1 | agency_id),  
            data = closure_CA, 
            family = "binomial") 

summary(m4)
```

{r select-vars-and-standardize}
closure_CA <- closure_CA |> 
  select(agency_id, school_id, school_level,
         year, closed, dist_state_abbr, 
         location_type,
         pct_dist_enroll, 
         school_level, 
         st_ratio, 
         school_more_diverse,
         three_yr_enroll_dist, 
         dist_st_ratio, 
         dist_per_pupil_expend, 
         dist_num_schools) |> 
  mutate(across(where(is.numeric) & !year, scale),
         across(where(is.numeric) & !year, as.vector), 
         year = year - min(year)) |> 
  na.omit()

closure_CA


{r clean-first-three-years}
#Clean out first three years (due to lags earlier)
closure_CA <- closure_CA |> 
  filter(year > 2) |> 
  mutate(year = year - min(year))