### load packages
library(brms)

### read in data
df <- read.csv('data_cayo_repro_vals.csv')

# check kin categories
table(df$categoryMatR)
table(df$categoryPatR)

### factor variables
cols <- c('actor','recip','dyad2','group','year','categoryMatR','categoryPatR')
for (col in cols) { df[[col]] <- as.factor(df[[col]]) }

### integer variables
df$count_groom <- as.integer(df$count_groom)

str(df)

### specify priors for model
priors <- c(
	prior(normal(0, 4),   class = Intercept), 
	prior(normal(0, 0.5), class = b), 
	prior(exponential(2), class = sd),
	prior(lkj(4),         class = cor))

### simpler within- and between- individual model
### total effects of actor age
### negative binomial (over-dispersed poisson)
m <- brm(
	count_groom | rate(exposure) ~ 1 + 
		# actor age effects
		scale(actor_age) * scale(actor_age_max) + 
		# random intercepts and random slopes
		(1 + scale(actor_age) | actor) + 
		(1 + scale(actor_age) | recip) + 
		(1 + scale(actor_age) | dyad2) + 
		(1 + scale(actor_age) | categoryMatR) + 
		(1 + scale(actor_age) | categoryPatR) + 
		(1 | group:year), 
	data = df, family = negbinomial, init = 0, 
	prior = priors, seed = 2001,  #A Space Odyssey!!!
	chains = 4, cores = 4, warmup = 1000, iter = 4000, thin = 3, 
	control = list(adapt_delta = 0.9), 
	file = "rhe_model_2.rds")

summary(m)
bayes_R2(m)

##### ##### #####

library(tidybayes)

m |> spread_draws(b_scaleactor_age) |> mode_hdi(.width = 0.89)
# -0.664 -0.875 -0.454   0.89 mode   hdi   
m |> spread_draws(b_scaleactor_age_max ) |> mode_hdi(.width = 0.89)
# 0.256  0.155  0.356   0.89 mode   hdi  
m |> spread_draws(`b_scaleactor_age:scaleactor_age_max`) |> mode_hdi(.width = 0.89)
# -0.0602 -0.128 -0.00723   0.89 mode   hdi 

m |> spread_draws(cor_dyad2__Intercept__scaleactor_age) |> mode_hdi(.width = 0.89)
# 0.308 0.0883  0.603   0.89 mode   hdi 
