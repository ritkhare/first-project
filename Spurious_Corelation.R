library(tidyverse)

### created a simulation table with 2 non-correlated variables X and Y 

N<- 25
G<- 1000000
sim_data <- tibble(group= rep(1:G, each=N), X = rnorm(G*N), Y= rnorm(G*N))
head(sim_data)

## summarize correlation between X and Y for all the groups

res<- sim_data %>% group_by(group) %>% summarize(r=cor(X,Y)) %>% arrange(desc(r))

## For the group with the max correlation , draw a scatter plot

sim_data %>% filter((group== res$group[which.max(res$r)])) %>% 
  ggplot(aes(X,Y)) +geom_point() + geom_smooth(method="lm")

### Observation : even though X and Y Are not coorelated , they have bery high correlations. 
#### This is called Spurious Correlation 

#### SpearmanCorrelation : Problem with Outliers

x <- rnorm(100,100,1)
y <- rnorm(100,84,1)
x[-23]<- scale(x[-23])
y[-23]<- scale(y[-23])

cor(x,y) 
## notice the coorelation . IT will be high due to entry 23 which is incorrect
tibble(x,y) %>% ggplot(aes(x,y)) + geom_point()

## Actual correlation will be given by removing entry 23
cor(x[-23],y[-23]) 

## spreaman corelation compute the correlation on ranks of the values?