# load libraries
library(tidyverse)
library(ggthemes)
library(cowplot)
library(brms)
library(tidybayes)

lvls4 <- c(
	"5_Daughter", "5_Mother", 
	"4_Sister", "4_GDaughter", "4_GMother", 
	"3_Niece", "3_Aunt", "3_GGDaughter", "3_GGMother", 
	"2_Cousin", "2_GNiece", "2_GAunt", 
	"0_Distant")

lbls4 <- c(
	"Daughter", "Mother", 
	"Sister", "G.Daughter"," G.Mother", 
	"Niece", "Aunt", "GG.Daughter", "GG.Mother", 
	"Cousin", "G.Niece", "G.Aunt", 
	"Distant")

lineal <- c("5_Daughter", "5_Mother", "4_GMother", "4_GDaughter", 
						"3_GGMother", "3_GGDaughter")

lateral <- c("4_Sister", "3_Aunt", "3_Niece", 
						 "2_Cousin", "2_GAunt", "2_GNiece", 
						 "1_CousNiece", "1_CousAunt", "1_GGNiece", "1_GGAunt", 
						 "0_Distant")

allkin <- append(lineal, lateral)

controls <- c("actor_age", "recip_age", "actor_age_max", "recip_age_max", 
							"recip_mVxR", "recip_VxR_res", "obs_hrs", "exposure")

##################################
##### functions for plotting #####
##################################

### minimum/maximum range per cluster
getX <- function(df, col, cluster) {
	df$xrange <- df[[col]]
	df$cluster <- as.factor(df[[cluster]])
	# cluster-specific ranges
	xrng <- df |> 
		group_by(cluster) |>
		mutate(x_min = min(xrange), x_max = max(xrange)) |>
		dplyr::select(cluster, x_min, x_max) |>
		dplyr::distinct()
	return(xrng) }

### function for plotting slopes by kinship
genSlp <- function(
		m=G1c, age='actor_age', clstr='categoryMatR', lbls=lbls4,
		obs='obs_hrs', kin=allkin, xu=1, xi=.67, yi=.6, 
		ylb='grooming given', xlb='age', ttl='', sttl='') {
	
	re <- paste0("~ (1 + scale(", age, ") | ", clstr, ")")
	xrng <- getX(m$data, col=age, cluster=clstr)
	names(xrng)[names(xrng) == 'cluster'] <- clstr
	
	mu_x <- mean(m$data[[age]])
	
	int <- exp(fixef(m)[1,1])
	int_u <- exp(fixef(m)[1,3])
	int_l <- exp(fixef(m)[1,4])
	
	newdata <- crossing(
		age = seq(min(m$data[[age]]),max(m$data[[age]]),length.out=100),
		category = unique(m$data[[clstr]]), 
		group = NA, year = NA, dyad2 = NA, actor = NA, recip = NA, 
		type = '0-follow', obs = 1)
	
	cols <- unlist(rownames(fixef(m)))
	cols <- gsub('scale', '', cols)
	cols <- unlist(unique(cols[cols %in% controls]))
	cols <- cols[cols != age]
	print(cols)
	for (col in cols) { 
		val <- mean(m$data[[col]])
		newdata[[col]] <- val }
	
	names(newdata)[names(newdata) == 'age'] <- age
	names(newdata)[names(newdata) == 'category'] <- clstr
	names(newdata)[names(newdata) == 'obs'] <- obs
	
	### predictions
	re_epred <- add_epred_draws(
		newdata, object = m, scale = "response", ndraws = 1000, 
		re_formula = re, allow_new_levels = TRUE)
	
	re_epred <- re_epred |> left_join(xrng)
	re_epred <- re_epred[which(re_epred[[age]]>re_epred[['x_min']]),]
	
	re_epred <- re_epred[which(re_epred[[age]]<re_epred[['x_max']]),]
	re_epred$level <- as.factor(substr(re_epred[[clstr]], 1, 1))
	
	re_epred[['age']] = re_epred[[age]]
	re_epred[['category']] = re_epred[[clstr]]
	re_epred[['label']] = paste(re_epred[['.draw']],re_epred[['category']],sep='_')
	
	sub <- subset(re_epred, category %in% kin)
	oth <- subset(re_epred, category == '0_Distant') |> droplevels()
	
	### main plot
	main <- ggplot(sub, aes(x = age, y = .epred, color = level)) + 
		theme_classic(base_size = 18) + 
		stat_lineribbon(aes(y = .epred, group = category), 
										alpha = .5, .width = c(.95, .89, .5)) +
		scale_fill_brewer( palette = "Greys") +
		scale_color_brewer(palette = "OrRd", drop=FALSE) + 
		
		geom_vline(aes(xintercept=mu_x), linetype=2, color = "black") + 
		
		geom_hline(aes(yintercept=int),   linetype=2, color = "black") + 
		geom_hline(aes(yintercept=int_u), linetype=3, color = "black") + 
		geom_hline(aes(yintercept=int_l), linetype=3, color = "black") + 
		theme(legend.position = 'none', 
					axis.text.y = element_text(angle = 90)) + 
		coord_cartesian(ylim=c(0,xu), xlim=c(5,32)) + 
		scale_x_continuous(breaks = seq(5,32,by=5)) + 
		labs(title=ttl,subtitle=sttl,y=ylb,x=xlb)
	### inset plot
	inset <- ggplot(oth, aes(x = age, y = .epred, color = level)) + 
		stat_lineribbon(linetype = 2, .width = c(.95, .89, .5)) + 
		theme_classic(base_size = 14) + 
		geom_vline(aes(xintercept=mu_x), 
							 linetype=2, linewidth=.5, color='black') + 
		scale_fill_brewer( palette = "Greys") +
		scale_color_brewer(palette = "OrRd", drop=FALSE) + 
		labs(title = 'distant/non-kin') + 
		theme(legend.position = 'none', 
					axis.text.y=element_blank(), axis.text.x=element_blank(), 
					axis.title.x=element_blank(),axis.title.y=element_blank())
	fig <- ggdraw() + 
		draw_plot(main) +
		draw_plot(inset,  x = xi,  y = yi,  width = .3, height = .25)
	return(fig)
}

##############################################################################
##############################################################################
##############################################################################

m <- readRDS('rhe_model_2.rds')

x1 <- fixef(m)[1, 1]
x2 <- fixef(m)[1, 3:4]

s1 <- fixef(m)['scaleactor_age', 1]
s2 <- fixef(m)['scaleactor_age', 3:4]

##### random intercepts, posterior distributions

lbls <- lbls4
lvls <- lvls4

ri_mat_1 <- m |> 
	spread_draws(b_Intercept, r_categoryMatR[category,Intercept]) |>
	filter(Intercept == "Intercept") |> 
	# add the grand mean to the group-specific deviations
	mutate(mu = b_Intercept + r_categoryMatR, 
				 level = substr(category,1,1)) |> 
	ungroup() |> 
	mutate(category = str_replace_all(category, "[.]", " "), 
				 category = factor(category,levels=lvls)) |> 
	# plot
	ggplot(aes(x = mu, y = category)) + 
	theme_classic(base_size = 18) + 
	# random intercepts
	geom_vline(xintercept = x1, color = "black", linewidth = .5, linetype = 2) +
	# random intercept 95%CIs
	geom_vline(xintercept = x2, color = "black", linetype = 3) +
	stat_slab(color='black',fill='white', linewidth=.25) + 
	stat_halfeye(aes(fill=level), color='black', 
							 point_interval = mode_hdi, .width = c(0.89, 0.95)) +
	coord_cartesian(xlim = c(-11,0)) + 
	scale_y_discrete(labels = lbls) + 
	labs(title = "", 
			 subtitle = 'random intercepts', 
			 x = "estimate (log scale)",
			 y = "") + 
	scale_fill_brewer(palette = 'OrRd') + 
	theme(legend.position='none', 
			#	axis.text.y = element_text(angle = 45, vjust = 0.5, hjust=1), 
				axis.title.y = element_blank())

#### random slopes, posterior distributions

rs_mat_1 <- m |> 
	tidybayes::spread_draws(b_scaleactor_age, r_categoryMatR[category,Intercept]) |>
	dplyr::filter(Intercept == "scaleactor_age") |> 
	# add the grand mean to the group-specific deviations
	mutate(mu = b_scaleactor_age + r_categoryMatR, 
				 level = substr(category,1,1)) |> 
	ungroup() |> 
	mutate(category = str_replace_all(category, "[.]", " "), 
				 category = factor(category,levels=lvls)) |> 
	# plot
	ggplot(aes(x = mu, y = category)) + 
	theme_classic(base_size = 18) + 
	# slopes
	geom_vline(xintercept = s1, color = "black", linewidth = .5, linetype=2) +
	# slope 95%CIs
	geom_vline(xintercept = s2, color = "black", linewidth = .5, linetype=3) +
	stat_slab(color='black',fill='white', linewidth=.25) + 
	stat_halfeye(aes(fill=level), color='black', 
							 point_interval = mode_hdi, .width = c(0.89, 0.95)) + 
	coord_cartesian(xlim = c(-2,1)) + 
	scale_y_discrete(labels = lbls4) + 
	geom_vline(xintercept = 0, color = 'blue', linewidth = .5) + 
	labs(title = "", 
			 subtitle = "random slopes for actor age", 
			 x = "estimate (log scale)",
			 y = "") + 
	scale_fill_brewer(palette = 'OrRd') + 
	theme(legend.position='none', 
				axis.text.y = element_blank())

rhe_RE_mat_1 <- cowplot::plot_grid(ri_mat_1, rs_mat_1, nrow=1, rel_widths = c(1.15,1))

##### plot random slopes #####

rhe_kin_mat_1 <- genSlp(
	m, obs='exposure', xu=.45, kin=allkin, 
	ttl="effect of actor age on grooming given", 
	ylb='rate of giving grooming', xlb="actor age", 
	sttl="moderated by matrilineal kinship (unbroken female lines)")

plt_mat_1 <- plot_grid(rhe_kin_mat_1, rhe_RE_mat_1)

##### paternal/son side #####
#############################

##### random intercepts, posterior distributions

lbls <- lbls[!(lbls %in% c('Mother','Daughter'))] 
lbls

lvls <- lvls[!(lvls %in% c('5_Mother','5_Daughter'))] 
lvls

ri_pat_1 <- m |> 
	spread_draws(b_Intercept, r_categoryPatR[category,Intercept]) |>
	filter(Intercept == "Intercept") |> 
	# add the grand mean to the group-specific deviations
	mutate(mu = b_Intercept + r_categoryPatR, 
				 level = substr(category,1,1)) |> 
	ungroup() |> 
	mutate(category = str_replace_all(category, "[.]", " "), 
				 category = factor(category,levels=lvls)) |> 
	# plot
	ggplot(aes(x = mu, y = category)) + 
	theme_classic(base_size = 18) + 
	# random intercepts
	geom_vline(xintercept = fixef(m)[1, 1], 
						 color = "black", linewidth = .5, linetype = 2) +
	# random intercept 95%CIs
	geom_vline(xintercept = fixef(m)[1, 3:4], 
						 color = "black", linetype = 3) +
	stat_slab(color='black',fill='white', linewidth=.25) + 
	stat_halfeye(aes(fill=level), color='black', 
							 point_interval = mode_hdi, .width = c(0.89, 0.95)) +
	coord_cartesian(xlim = c(-11,0)) + 
	scale_y_discrete(labels = lbls) + 
	labs(title = "", 
			 subtitle = 'random intercepts', 
			 x = "estimate (log scale)",
			 y = "") + 
	scale_fill_brewer(palette = 'OrRd') + 
	theme(legend.position='none', 
				#	axis.text.y = element_text(angle = 45, vjust = 0.5, hjust=1), 
				axis.title.y = element_blank())

#### random slopes, posterior distributions

rs_pat_1 <- m |> 
	spread_draws(b_scaleactor_age, r_categoryPatR[category,Intercept]) |>
	filter(Intercept == "scaleactor_age") |> 
	# add the grand mean to the group-specific deviations
	mutate(mu = b_scaleactor_age + r_categoryPatR, 
				 level = substr(category,1,1)) |> 
	ungroup() |> 
	mutate(category = str_replace_all(category, "[.]", " "), 
				 category = factor(category,levels=lvls)) |> 
	# plot
	ggplot(aes(x = mu, y = category)) + 
	theme_classic(base_size = 18) + 
	# slopes
	geom_vline(xintercept = fixef(m)['scaleactor_age', 1], 
						 color = "black", linewidth = .5, linetype=2) +
	# slope 95%CIs
	geom_vline(xintercept = fixef(m)['scaleactor_age', 3:4], 
						 color = "black", linewidth = .5, linetype=3) +
	stat_slab(color='black',fill='white', linewidth=.25) + 
	stat_halfeye(aes(fill=level), color='black', 
							 point_interval = mode_hdi, .width = c(0.89, 0.95)) + 
	coord_cartesian(xlim = c(-2,1)) + 
	scale_y_discrete(labels = lbls) + 
	geom_vline(xintercept = 0, color = 'blue', linewidth = .5) + 
	labs(title = "", 
			 subtitle = "random slopes for actor age", 
			 x = "estimate (log scale)",
			 y = "") + 
	scale_fill_brewer(palette = 'OrRd') + 
	theme(legend.position='none', 
				axis.text.y = element_blank())

rhe_RE_pat_1 <- cowplot::plot_grid(ri_pat_1, rs_pat_1, nrow=1, rel_widths = c(1.15,1))

##### plot random slopes #####

rhe_kin_pat_1 <- genSlp(
	m, obs='exposure', xu=.45, kin=allkin, clstr='categoryPatR', 
	ttl="effect of actor age on grooming given", 
	ylb='rate of giving grooming', xlb="actor age", 
	sttl="moderated by one-male-link kinship (one father or son pathway)")

plt_pat_1 <- plot_grid(rhe_kin_pat_1, rhe_RE_pat_1)

##############################################################################

plt_rhe_1 <- plot_grid(plt_mat_1, plt_pat_1, nrow=2)
(plt_rhe_1)

ggsave('Fig3.tif', 
			 device = 'tiff', width=10*4, height=10*4, 
			 units='cm', dpi = 300)

rm(ri_mat_1,rs_mat_1,rhe_RE_mat_1,rhe_kin_mat_1)

###############################################################################
###############################################################################
###############################################################################

m <- readRDS('rhe_model_1.rds')

##### random intercepts, posterior distributions

lbls <- lbls4
lvls <- lvls4

x1 <- fixef(m)[1, 1]
x2 <- fixef(m)[1, 3:4]

s1 <- fixef(m)['scaleactor_age', 1]
s2 <- fixef(m)['scaleactor_age', 3:4]

ri_mat_2 <- m |> 
	spread_draws(b_Intercept, r_categoryMatR[category,Intercept]) |>
	filter(Intercept == "Intercept") |> 
	# add the grand mean to the group-specific deviations
	mutate(mu = b_Intercept + r_categoryMatR, 
				 level = substr(category,1,1)) |> 
	ungroup() |> 
	mutate(category = str_replace_all(category, "[.]", " "), 
				 category = factor(category,levels=lvls)) |> 
	# plot
	ggplot(aes(x = mu, y = category)) + 
	theme_classic(base_size = 18) + 
	# random intercepts
	geom_vline(xintercept = fixef(m)[1, 1], 
						 color = "black", linewidth = .5, linetype = 2) +
	# random intercept 95%CIs
	geom_vline(xintercept = fixef(m)[1, 3:4], 
						 color = "black", linetype = 3) +
	stat_slab(color='black',fill='white', linewidth=.25) + 
	stat_halfeye(aes(fill=level), color='black', 
							 point_interval = mode_hdi, .width = c(0.89, 0.95)) +
	coord_cartesian(xlim = c(-11,0)) + 
	scale_y_discrete(labels = lbls) + 
	labs(title = "", 
			 subtitle = 'random intercepts', 
			 x = "estimate (log scale)",
			 y = "") + 
	scale_fill_brewer(palette = 'OrRd') + 
	theme(legend.position='none', 
				#	axis.text.y = element_text(angle = 45, vjust = 0.5, hjust=1), 
				axis.title.y = element_blank())

#### random slopes, posterior distributions

rs_mat_2 <- m |> 
	tidybayes::spread_draws(b_scaleactor_age, r_categoryMatR[category,Intercept]) |>
	dplyr::filter(Intercept == "scaleactor_age") |> 
	# add the grand mean to the group-specific deviations
	mutate(mu = b_scaleactor_age + r_categoryMatR, 
				 level = substr(category,1,1)) |> 
	ungroup() |> 
	mutate(category = str_replace_all(category, "[.]", " "), 
				 category = factor(category,levels=lvls)) |> 
	# plot
	ggplot(aes(x = mu, y = category)) + 
	theme_classic(base_size = 18) + 
	# slopes
	geom_vline(xintercept = fixef(m)['scaleactor_age', 1], 
						 color = "black", linewidth = .5, linetype=2) +
	# slope 95%CIs
	geom_vline(xintercept = fixef(m)['scaleactor_age', 3:4], 
						 color = "black", linewidth = .5, linetype=3) +
	stat_slab(color='black',fill='white', linewidth=.25) + 
	stat_halfeye(aes(fill=level), color='black', 
							 point_interval = mode_hdi, .width = c(0.89, 0.95)) + 
	coord_cartesian(xlim = c(-2,1)) + 
	scale_y_discrete(labels = lbls4) + 
	geom_vline(xintercept = 0, color = 'blue', linewidth = .5) + 
	labs(title = "", 
			 subtitle = "random slopes for actor age", 
			 x = "estimate (log scale)",
			 y = "") + 
	scale_fill_brewer(palette = 'OrRd') + 
	theme(legend.position='none', 
				axis.text.y = element_blank())

rhe_RE_mat_2 <- cowplot::plot_grid(ri_mat_2, rs_mat_2, nrow=1, rel_widths = c(1.15,1))

##### plot random slopes #####

rhe_kin_mat_2 <- genSlp(
	m, obs='exposure', xu=.45, kin=allkin, 
	ttl="partial effect of actor age on grooming given", 
	ylb='rate of giving grooming', xlb="actor age", 
	sttl="moderated by matrilineal kinship (unbroken female lines)")

##### combine, groom give #####

plt_mat_2 <- plot_grid(rhe_kin_mat_2, rhe_RE_mat_2, nrow=1)
(plt_mat_2)

ggsave('/Fig4.tif', 
			 device = 'tiff', width=10*4, height=10*2, 
			 units='cm', dpi = 300)

rm(ri_mat_2,rs_mat_2,rhe_RE_mat_2,rhe_kin_mat_2)

