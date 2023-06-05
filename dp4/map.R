#!/usr/bin/R

library(tidyverse)
library(sf)

f_jitter = .2
a_jitter = 2
alpha = .4

tmp = "
ira = read_sf('ira_centroid.shp')

print('jittering 1/7')
ira_wrk = ira %>%
	filter(!is.na(D_WKFC_89)) %>%
	st_jitter(a_jitter, factor=f_jitter)

print('jittering 2/7')
ira_wtr = ira %>%
	filter(!is.na(D_WTR_EOMI)) %>%
	st_jitter(a_jitter, factor=f_jitter)

print('jittering 3/7')
ira_hsg = ira %>%
	filter(!is.na(D_HSG_EOMI)) %>%
	st_jitter(a_jitter, factor=f_jitter)

print('jittering 4/7')
ira_clt = ira %>%
	filter(!is.na(D_CLT_EOMI)) %>%
	st_jitter(a_jitter, factor=f_jitter)

print('jittering 5/7')
ira_eny = ira %>%
	filter(!is.na(D_ENY_EOMI)) %>%
	st_jitter(a_jitter, factor=f_jitter)

print('jittering 6/7')
ira_pln = ira %>%
	filter(!is.na(D_PLN_EOMI)) %>%
	st_jitter(a_jitter, factor=f_jitter)

print('jittering 7/7')
ira_trn = ira %>%
	filter(!is.na(D_TRN_EOMI)) %>%
	st_jitter(a_jitter, factor=f_jitter)

ira_t = read_sf('ira.shp') %>%
	filter(!is.na(D_T)) %>%
	st_transform(crs=st_crs('EPSG:5070')) 
"
plt_fill_ta = ggplot() + 
	theme_void() +
	geom_sf(data=ira_t, aes(size=D_T, fill='#8c1515'), alpha=alpha) +
	geom_sf(data=ira_wrk, aes(size=D_WKFC_89, color='#7f7776'), alpha=alpha) +
	geom_sf(data=ira_hsg, aes(size=D_HSG_EOMI, color='#8f993e'), alpha=alpha) +
	geom_sf(data=ira_trn, aes(size=D_TRN_EOMI, color='#e04f39'), alpha=alpha) +
	geom_sf(data=ira_eny, aes(size=D_ENY_EOMI, color='#fedd5c'), alpha=alpha) +
	geom_sf(data=ira_pln, aes(size=D_PLN_EOMI, color='#e98300'), alpha=alpha) +
	geom_sf(data=ira_clt, aes(size=D_CLT_EOMI, color='#651c32'), alpha=alpha) +
	geom_sf(data=ira_wtr, aes(size=D_WTR_EOMI, color='#007c92'), alpha=alpha) +
	scale_size(
		name="Future Spending\nPer Capita", 
		breaks=c(0, 5e6, 1e7, 1.5e7, 2e7, 2.5e7),
		labels=c('< $0.5m', '$0.5m', '$1m', '$1.5m', '$2m', '$2.5m'),
		range=c(1, 10),
		guide = guide_legend(order=1),
	) +
	scale_color_identity(
		name = 'Disadvantaged\nCategory',
		guide = guide_legend(order=2),
		breaks = c('#7f7776', "#8f993e", "#fedd5c", "#e98300", "#e04f39", "#651c32", "#8c1515", "#007c92"),
		labels = c('Workforce', "Housing", "Energy", "Pollution", "Transportation", "Climate Change", "Tribal Area", "Water"),
	) +
	scale_fill_identity(
		name=NULL,
		guide=guide_legend(order=3),
		breaks='#8c1515',
		labels='Tribal Area',
	) + 	
	guides(
		shape=guide_legend(order=1),
		color=guide_legend(order=2),
		fill=guide_legend(order=3),
	) + 
	labs(
		title = 'The Justice40 Initiative',
		subtitle = 'How 40% of the Infaltion Reduction Act will address Enviormental Justice',
	) +
	theme(
		plot.title = element_text(
			family='mono', face='bold', size=28, hjust=.5
		),
		plot.subtitle = element_text(
			family='mono', size=22, hjust=.5
		),
		plot.caption = element_text(
			family='mono', hjust=.5
		),
		legend.title = element_text(family='mono', size=18),
		legend.text = element_text(family='mono', size=16),
		plot.margin = unit(c(0,1,0,0), 'in'),
	) 


