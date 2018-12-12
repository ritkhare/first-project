# this script covers some sample codes for stratification and multivariate regression

library(Lahman)
library(tidyverse)
library(HistData)
head(Teams)
head(GaltonFamilies)

# shows how we are stratifying data by HomeRuns per game.
dat <- Teams%>% filter(yearID %in% 1961:2001) %>%
      mutate(HR_strata = round(HR/G,1), BB_per_game=BB/G, R_per_game=R/G)%>%
      filter(HR_strata >= 0.4 & HR_strata <= 1.2)

head(dat)

# checking correlation between Base on balls and runs per game for different
# strata of Home runs. Since there are multiple variables , it is multivariate regression

dat %>% ggplot(aes(BB_per_game,R_per_game)) + geom_point() +
  geom_smooth(method="lm") + facet_wrap(~HR_strata)

### 
#m <- r*s_y/s_x
#b<- mu_y- (m*mu_x)

dat %>% group_by(HR_strata) %>% 
  summarize(slope = cor(BB_per_game,R_per_game)* (sd(R_per_game)/sd(BB_per_game)))


# By stratifying BB_per_game , we will see the difference
dat <- Teams%>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata= round(BB/G,1),HR_per_game=HR/G, R_per_game=R/G)
max(dat$BB_strata)
min(dat$BB_strata)

dat <- Teams%>% filter(yearID %in% 1961:2001) %>% 
  mutate(BB_strata= round(BB/G,1),HR_per_game=HR/G, R_per_game=R/G) %>%
  filter(BB_strata>=2.8 & BB_strata <=3.9) 
dat %>% ggplot(aes(HR_per_game,R_per_game)) +geom_point()+ geom_smooth(method="lm") +
  facet_wrap(~BB_strata)


#########
# LSE : use of lm()

dat <- Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(R_per_game=R/G,BB_per_game=BB/G,HR_per_game=HR/G)

lm(dat$R_per_game ~ (BB_per_game+ HR_per_game),dat)


# Monte Carlo Simulation , correlatio after standardizing father's height

B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})

cor(lse[1,], lse[2,]) 
