
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