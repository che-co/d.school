#!/usr/bin/R

library(tidyverse)
library(sf)

tbl_columns = read_csv('columns.csv')

v_colnames = select(tbl_columns, column_name) %>% 
	unlist()

names(v_colnames) = select(tbl_columns, shapefile_column) %>%
	unlist()

tbl_tracts = read_csv('1.0-communities.csv') %>%
	rename(
		TRACT = `Census tract 2010 ID`,
		life = `Greater than or equal to the 90th percentile for low life expectancy and is low income?`,
		asth = `Greater than or equal to the 90th percentile for asthma and is low income?`,
		hear = `Greater than or equal to the 90th percentile for heart disease and is low income?`,
		ling = `Greater than or equal to the 90th percentile for households in linguistic isolation and has low HS attainment?`,
		inco = `Greater than or equal to the 90th percentile for low median household income as a percent of area median income and has low HS attainment?`,
		pove = `Greater than or equal to the 90th percentile for households at or below 100% federal poverty level and has low HS attainment?`,
		unem = `Greater than or equal to the 90th percentile for unemployment and has low HS attainment?`,
	) %>%
	mutate(
		N_HLTH_EOMI = life | asth | hear,
		N_WKFC_EOMI = ling | inco | pove | unem
	) %>%
	rename(any_of(v_colnames)) %>%
	select(
		TRACT,
		ends_with('_EOMI'),
	)

tbl_ira = read_csv('IRA_categorized_tribal.csv') %>%
	unlist()

geo_usa = read_sf('usa.shp') %>%
	select(GEOID10, TPF, ends_with('_EOMI'), N_HLTH_88, N_WKFC_89)



