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

### VxR remaining after removing mVxR
df$recip_VxR_res <- df$recip_VxR - df$recip_mVxR

### specify priors for model
priors <- c(
	prior(normal(0, 4),   class = Intercept), 
	prior(normal(0,.5),   class = b), 
	prior(exponential(2), class = sd),
	prior(lkj(4),         class = cor))

### within- and between- individual model
### negative binomial (over-dispersed poisson)
m <- brm(
	count_groom | rate(exposure) ~ 1 + 
		# the stuff we care about
		scale(recip_mVxR) + scale(recip_VxR_res) + 
		# age effects
		scale(actor_age) * scale(actor_age_max) + 
		scale(recip_age) * scale(recip_age_max) + 
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
	file = "rhe_model_1.rds")

summary(m)
bayes_R2(m)

##### ##### #####

library(tidybayes)

m |> spread_draws(b_scaleactor_age) |> mode_hdi(.width = 0.89)
# -0.309 -0.757 0.0928   0.89 mode   hdi 
m |> spread_draws(b_scalerecip_age) |> mode_hdi(.width = 0.89)
# -0.228 -0.639  0.157   0.89 mode   hdi   

m |> spread_draws(b_scaleactor_age_max ) |> mode_hdi(.width = 0.89)
# -0.00669 -0.420  0.413   0.89 mode   hdi 
m |> spread_draws(b_scalerecip_age_max ) |> mode_hdi(.width = 0.89)
# 0.106 -0.296  0.533   0.89 mode   hdi

m |> spread_draws(`b_scaleactor_age:scaleactor_age_max`) |> mode_hdi(.width = 0.89)
# -0.0909 -0.153 -0.0238   0.89 mode   hdi 
m |> spread_draws(`b_scalerecip_age:scalerecip_age_max`) |> mode_hdi(.width = 0.89)
# 0.0108 -0.0323 0.0539   0.89 mode   hdi  

m |> spread_draws(cor_dyad2__Intercept__scaleactor_age) |> mode_hdi(.width = 0.89)
# 0.241 0.0617  0.499   0.89 mode   hdi 


