###################### S610 Final ######################
##################### Myung Jin Lee ######################

setwd('/Users/jin/OneDrive - Indiana University/Fall 2021/S610/final')
getwd()

library(dplyr)

######### Q1 ###########
source('/Users/jin/OneDrive - Indiana University/Fall 2021/S610/final/final_q1.R')

set.seed(1234567890)

tt <- matrix(rep(NA, 5*20), nrow = 5)

for (i in 1:20) {
  x = matrix(rnorm(1000*500), nrow = 1000)
  y = c(rnorm(1000))
  
  
  # Rprof for fun 1
  Rprof(filename='final.q1_1.out', filter.callframes = TRUE, interval = 0.01)
  r_squared_fun_1(x,y)
  Rprof(NULL)
  tt1 = summaryRprof(filename = 'final.q1_1.out')$by.total
  tt1_1 = unname(tt1 %>% filter(row.names(tt1) %in% c("\"r_squared_fun_1\"")) %>% 
                   select(c(1)) )
  row.names(tt1_1) = NULL
  
  # Rprof for fun 2
  Rprof(filename='final.q1_2.out', filter.callframes = TRUE, interval = 0.01)
  r_squared_fun_2(x,y)
  Rprof(NULL)
  #summaryRprof(filename = 'final.q1_2.out')$by.total
  tt2 = summaryRprof(filename = 'final.q1_2.out')$by.total
  tt2_1 = unname(tt2 %>% filter(row.names(tt2) %in% c("\"r_squared_fun_2\"")) %>% 
                   select(c(1)) )
  row.names(tt2_1) = NULL
  
  # Rprof for fun 3
  Rprof(filename='final.q1_3.out', filter.callframes = TRUE, interval = 0.01)
  r_squared_fun_3(x,y)
  Rprof(NULL)
  #summaryRprof(filename = 'final.q1_3.out')$by.total
  tt3 = summaryRprof(filename = 'final.q1_3.out')$by.total
  tt3_1 = unname(tt3 %>% filter(row.names(tt3) %in% c("\"r_squared_fun_3\"")) %>% 
                   select(c(1)) )
  row.names(tt3_1) = NULL
  
  # Rprof for fun 4
  Rprof(filename='final.q1_4.out', filter.callframes = TRUE, interval = 0.01)
  r_squared_fun_4(x,y)
  Rprof(NULL)
  #summaryRprof(filename = 'final.q1_4.out')$by.total
  tt4 = summaryRprof(filename = 'final.q1_4.out')$by.total
  tt4_1 = unname(tt4 %>% filter(row.names(tt4) %in% c("\"r_squared_fun_4\"")) %>% 
                   select(c(1)) )
  row.names(tt4_1) = NULL
  
  # Rprof for fun 5
  Rprof(filename='final.q1_5.out', filter.callframes = TRUE, interval = 0.01)
  r_squared_fun_5(x,y)
  Rprof(NULL)
  #summaryRprof(filename = 'final.q1_5.out')$by.total
  tt5 = summaryRprof(filename = 'final.q1_5.out')$by.total
  tt5_1 = unname(tt5 %>% filter(row.names(tt5) %in% c("\"r_squared_fun_5\"")) %>% 
                   select(c(1)) )
  row.names(tt5_1) = NULL
  
  tt[,i] = unlist(c(tt1_1, tt2_1, tt3_1, tt4_1, tt5_1))

}

# r-squared_fun_2 is generally the fastest in computing R square. Using linear system appears to help computation faster than 
# using direct inversion, eigen system, or svd. 




