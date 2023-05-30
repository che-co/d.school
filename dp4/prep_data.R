#!/usr/bin/R

library(tidyverse)
library(sf)

ira = read_csv('IRA_categorized_tribal.csv') %>%
	rename(
		N_CLT_EOMI = climate_change,
		N_ENY_EOMI = energy,
		N_TRN_EOMI = transportarion,
		N_HSG_EOMI = housing,
		N_PLN_EOMI = legacy_pollution,
		N_WTR_EOMI = water,
		N_WKFC_89  = workforce,
		SN_T = tribal,
	) %>%
	unlist()

geo_usa = read_sf('usa.shp') %>%
	select(GEOID10, SF, CF, TPF, ends_with('_EOMI'), N_WKFC_89, SN_T) %>%
	filter(!(is.na(TPF) | SF %in% c('Puerto Rico', 'Guam', 'Virgin Islands' , 'Northern Mariana Islands', 'American Samoa') | CF == 'Aleutians West Census Area' | GEOID10 == 15003981200))

L = lapply(names(ira), function(i){
	v = paste0('D_', str_replace(i, '[S]*N_', ''))
	n = 'd'
	names(n) = v
	d = geo_usa[which(geo_usa[[i]] == 1), ]
	d[[v]] = with(d, (ira[i] * .4 / sum(TPF)) * TPF) 
	tbl = tibble(GEOID10=d[['GEOID10']], d=d[[v]]) %>%
		rename(any_of(n))
	return(tbl)
})

ira_usa = full_join(L[[1]], L[[2]]) %>%
	full_join(L[[3]]) %>%
	full_join(L[[4]]) %>%
	full_join(L[[5]]) %>%
	full_join(L[[6]]) %>%
	full_join(L[[7]]) %>%
	full_join(L[[8]]) %>%
	full_join(geo_usa)
	
write_sf(ira_usa, 'ira.shp', append=FALSE)
