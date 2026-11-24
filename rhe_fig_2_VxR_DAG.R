### load libraries
library(tidyverse)
library(bayesplot)
library(tidybayes)
library(brms)
library(ggthemes)
library(cowplot)
library(dagitty)
library(ggdag)

###############################################################################
###############################################################################
###############################################################################

# DAG for groom-give
GG <- dagitty( "dag { 

    GROOM [outcome]
    recip_VxR [exposure]
    
    recip_VxR -> GROOM
    
    relatedness -> GROOM
    recip_VxR -> GROOM
    
    relatedness -> recip_VxR
    recip_Vx -> recip_VxR
    
    recip_age -> recip_Vx
    kinship_category -> relatedness

    kinship_category -> GROOM
    relatedness -> GROOM
    actor_age -> GROOM
    recip_age -> GROOM
    
    actor_age -> kinship_category

}")

DAG <- tidy_dagitty(GG)

genPlot <- function(dag = DAG, var = 'actor_age', type = 'anc') {
	if (type=='anc') {
		plt <- ggdag_ancestors(dag, var, text_col = "blue", stylized = TRUE)
	} else if (type=='chi') {
		plt <- ggdag_children(dag, var, text_col = "blue", stylized = TRUE)
	} else if (type=='par') {
		plt <- ggdag_parents(dag, var, text_col = "blue", stylized = TRUE)
	}
	plt <- plt + 
		theme_classic(base_size = 18) + 
		theme(axis.text = element_blank(), 
					panel.background = element_rect(fill='transparent'), 
					plot.background = element_rect(fill='transparent', color=NA), 
					legend.position = 'none', 
					legend.background = element_rect(fill='transparent'), 
					legend.box.background = element_rect(fill='transparent')) + 
		labs(title = var, x='', y='')
	return(plt)
}

DAG_mVxR <- genPlot(var='recip_VxR') + 
	labs(title = 'A) directed acyclic graph')
(DAG_mVxR)

rm(DAG, GG)

###############################################################################
###############################################################################
###############################################################################

##### plots for VxR effects #####

genPlt <- function(m=m, var='recip_mVxR', ttl='', sttl='', y1=0, y2=1, 
									 xlb='maternal VxR', ylb='groom give rate') { 
	
	mu_x <- mean(m$data[[var]])
	mu_y <- exp(fixef(m)['Intercept', 1]) * mean(m$data$exposure)
	
	plt <- plot(conditional_effects(m, var,  prob=.89), plot=F)[[1]]
	
	plt <- plt + 
		theme_classic(base_size = 18) + 
		coord_cartesian(ylim=c(y1,y2), xlim=c(0,1.1)) + 
		labs(y=ylb, x=xlb, title=ttl) + 
		geom_vline(xintercept = mu_x, linetype = 3, color = 'grey') + 
		geom_hline(yintercept = mu_y, linetype = 3, color = 'grey')
	bys <- mcmc_areas(m, pars=paste0('b_scale',var), prob=.89) + 
		theme_minimal(base_size = 18) + 
		coord_cartesian(xlim=c(-.1,.3)) + 
		theme(axis.text.y = element_blank())
	fig <- ggdraw() + 
		draw_plot(plt) +
		draw_plot(bys,  x=.25,  y=.6, width=.35, height=.25)
	return(fig) }

###############################################################################
###############################################################################
###############################################################################

m <- readRDS('rhe_model_1.rds')

plt_mVxR <- genPlt(
	m, var='recip_mVxR', 
	xlb='matrilineal VxR', ylb='rate of grooming given', y1=0, y2=1.1, 
	ttl = 'B) effect of matrilineal VxR')

plt_VxR <- genPlt(
	m, var='recip_VxR_res', 
	xlb='(total VxR - matrilineal VxR)', 
	ylb='rate of grooming given', y1=0, y2=1.1, 
	ttl = 'C) effect of residual VxR')

##### join plots #####

plot_grid(DAG_mVxR, plt_mVxR, plt_VxR, nrow=1)

ggsave('Fig2.tif', device = 'tiff', bg = "white",  
			 width=10*4, height=10*1.5, units='cm', dpi = 300)

