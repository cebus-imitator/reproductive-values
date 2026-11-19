### load packages
library(brms)

### read in data
df <- read.csv('data_cayo_repro_vals.csv')

### factor variables
cols <- c('actor','recip','dyad2','group','year')
for (col in cols) { df[[col]] <- as.factor(df[[col]]) }

### integer variables
df$count_groom <- as.integer(df$count_groom)

### specify priors
priors <- c(
	prior(normal(0, 4),   class = Intercept), 
	prior(normal(0, 0.5), class = b), 
	prior(exponential(2), class = sd),
	prior(lkj(4),         class = cor))

### run model
m <- brm(
	count_groom | rate(exposure) ~ 1 + 
		scale(mat_r) * scale(recip_Vx) + 
		scale(actor_age) * scale(actor_age_max) + 
		(1 + scale(actor_age) | actor) + 
		(1 + scale(actor_age) | recip) + 
		(1 + scale(actor_age) | dyad2) + 
		(1 | group:year), 
	data = df, family = negbinomial, 
	prior = priors, init = 0, seed = 123, 
	chains = 4, cores = 4, warmup = 1000, iter = 3000, thin = 2,  
	control = list(adapt_delta = .9), 
	file = "rhe_model_SI.rds")

summary(m)
bayes_R2(m)

##### ##### #####

library(dplyr)

vals <- m$data |> select(mat_r, `scale(mat_r)`) |> distinct() |>
	filter(mat_r %in% c(0, .0625, 0.125, 0.25, .5)) |>
	arrange(mat_r) |> select(`scale(mat_r)`) |> pull()
# -0.2571053  0.8249244  1.9069541  4.0710134  8.3991321

draws <- as_draws_df(m)

for (z_val in vals[,1]) {
	print(z_val)
	slope_at_z <- draws$b_scalerecip_Vx +
		draws$`b_scalemat_r:scalerecip_Vx` * z_val
	print(round(MCMCglmm::posterior.mode(as.mcmc(slope_at_z)), 3))
	print(round(coda::HPDinterval(as.mcmc(slope_at_z), prob=.89), 3))
}
# 0.163 [0.086 0.204]
# 0.171 [0.117 0.226]
# 0.181 [0.144 0.253]
# 0.254 [0.187 0.318]
# 0.38  [0.251 0.468]

library(tidybayes)

m |> spread_draws(b_scalerecip_Vx) |> mode_hdi(.width = 0.89)
# 0.159 0.0926  0.208   0.89 mode   hdi  
m |> spread_draws(b_scalemat_r) |> mode_hdi(.width = 0.89)
# 0.633  0.616  0.653   0.89 mode   hdi 
m |> spread_draws(`b_scalemat_r:scalerecip_Vx`) |> mode_hdi(.width = 0.89)
# 0.0250 0.0113 0.0386   0.89 mode   hdi  

