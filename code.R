library(HistData)
data("GaltonFamilies")
head(GaltonFamilies)


galton_heights <- GaltonFamilies %>% filter(childNum==1 & gender=="male") %>% 
  select(father,childHeight) %>% rename(son=childHeight)
head(galton_heights)

# scatter plot
galton_heights %>% ggplot(aes(father,son)) + geom_point()

# Monte Carlo simulation 
B <-  1000
N <- 25
R <- replicate(B, { sample_n(galton_heights,N,replace=TRUE) %>% summarize(cor(father,son)) %>% .$r })
data.frame(R) %>% ggplot(aes(R)) + geom_histogram(binwidth=0.05)

# calculation of conditional avg by stratifying father's heights 
conditional_avg <- galton_heights %>% filter(round(father)==72) %>% summarize(avg=mean(son)) %>% .$avg
print(conditional_avg)

#actual dataset
galton_heights$father[1:30]

#stratified dataset
stratefied_father_height <- galton_heights %>% mutate(father_strata=factor(round(father)))
head(stratefied_father_height)

galton_heights %>% mutate(father=round(father)) %>% group_by(father) %>%
  summarize(son_conditional_avg= mean(son)) %>% ggplot(aes(father,son_conditional_avg)) + geom_point()


#Notice use of scale(). it changes the axis to -2 to +2 values. This is useful when using slope and intercept. 
#Try the code without scale and notice the change in axis
galton_heights %>% mutate(father=round(father)) %>% group_by(father) %>%
  summarize(son=mean(son)) %>% mutate(z_father=scale(father),z_son=scale(son)) %>%
  ggplot(aes(z_father,z_son)) + geom_point() + geom_abline(intercept=0,slope=r)


# Regression LInes: calculating intercept and slope

mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father,galton_heights$son)
m <- r*s_y/s_x
b<- mu_y- (m*mu_x)

galton_heights %>% ggplot(aes(father,son)) + geom_point(alpha=0.5) + geom_abline(intercept=b,slope=r)
