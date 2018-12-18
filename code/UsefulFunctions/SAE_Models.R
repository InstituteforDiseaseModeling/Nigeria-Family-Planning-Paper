
#######################################################
### Models to consider for Small Area Estimates for FP

# -- model 1: no interactions -- #
# -- model 2: space-time interactions -- #
# -- model 3: space-time + survey RE -- #
# -- model 4: space-time + survey RE + survey-space -- #
# -- model 5: space-time + survey RE + survey-time -- #
# -- model 6: space-time + survey RE + survey-space + survey-time -- #

# -- model 1: no interactions -- #
model1 <- outcome ~ f(dist.id, model=models$space[i], param=prior.iid, graph=mat) +  # state effect
  f(period.id, model=models$time[i],  param=prior.iid) +  # random walk 
  f(period.id2, model="iid",  param=prior.iid)   # iid time

# -- model 2: space-time interactions -- #
model2 <- outcome ~ f(dist.id, model=models$space[i], param=prior.iid, graph=mat) +  # state effect
  f(period.id, model=models$time[i],  param=prior.iid) +  # random walk 
  f(period.id2, model="iid",  param=prior.iid) +  # iid time
  f(period.id3, model=models$time[i], replicate=dist.id, param=prior.besag)  # state X time
  
# -- model 3: space-time + survey RE -- #
model3 <- outcome ~ f(dist.id, model=models$space[i], param=prior.iid, graph=mat) +  # state effect
  f(period.id, model=models$time[i],  param=prior.iid) +  # random walk 
  f(period.id2, model="iid",  param=prior.iid) +  # iid time
  f(period.id3, model=models$time[i], replicate=dist.id, param=prior.besag) +  # state X time
  f(survey, model="iid", param=prior.iid) # survey RE

# -- model 4: space-time + survey RE + survey-space -- #
model4 <- outcome ~ f(dist.id, model=models$space[i], param=prior.iid, graph=mat) +  # state effect
  f(period.id, model=models$time[i],  param=prior.iid) +  # random walk 
  f(period.id2, model="iid",  param=prior.iid) +  # iid time
  f(period.id3, model=models$time[i], replicate=dist.id, param=prior.besag) +  # state X time
  f(survey, model="iid", param=prior.iid) + # survey RE
  f(survey2, model="iid",replicate=dist.id, param=prior.iid) # survey-state

# -- model 5: space-time + survey RE + survey-time -- #
model5 <- outcome ~ f(dist.id, model=models$space[i], param=prior.iid, graph=mat) +  # state effect
  f(period.id, model=models$time[i],  param=prior.iid) +  # random walk 
  f(period.id2, model="iid",  param=prior.iid) +  # iid time
  f(period.id3, model=models$time[i], replicate=dist.id, param=prior.besag) +  # state X time
  f(survey, model="iid", param=prior.iid) + # survey RE
  f(survey3, model="iid",replicate=period.id, param=prior.iid) # survey-time

# -- model 6: space-time + survey RE + survey-space + survey-time -- #
model6 <- outcome ~ f(dist.id, model=models$space[i], param=prior.iid, graph=mat) +  # state effect
  f(period.id, model=models$time[i],  param=prior.iid) +  # random walk 
  f(period.id2, model="iid",  param=prior.iid) +  # iid time
  f(period.id3, model=models$time[i], replicate=dist.id, param=prior.besag) +  # state X time
  f(survey, model="iid", param=prior.iid) + # survey RE
  f(survey2, model="iid",replicate=dist.id, param=prior.iid) + # survey-state
  f(survey3, model="iid",replicate=period.id, param=prior.iid) # survey-time
