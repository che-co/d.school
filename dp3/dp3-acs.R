#!/usr/bin/R

library('tidyverse')
library('RJSONIO')
library('sf')

api_key = 'f04632091f5c62a6f271cb572a313cc41e8cec03'
vars_pop = c('B01001_001E', 'B02001_003E', 'B25075_001E')
vars_pop_names = c('N', 'N_BLACK', 'HOME_V')
names(vars_pop) = vars_pop_names
vars_mob = paste0('B07004', LETTERS[1:9]) %>%
	sapply(function(i) paste0(i, '_00', 1:6, 'E')) %>%
	sort()
vars_mob_names = paste0('MOB_', c('N', 'SHM', 'SCT', 'SST', 'DST', 'DNT')) %>%
	sapply(function(i) paste0(i, c('_WA', '_BA', '_NA', '_AA', '_HA', '_OA', '_R2', '_NL', '_HL'))) %>%
	t() %>%
	as.vector()
names(vars_mob) = vars_mob_names

vars = c(vars_pop, vars_mob) 

varset0 = vars[1:50]
varset1 = vars[51:length(vars)]
call0_vars = paste0(varset0, collapse=',')
call1_vars = paste0(varset1, collapse=',')

call0_2021 = "https://api.census.gov/data/2021/acs/acs5?get={call0_vars}&for=tract:*&in=state:06&in=county:075&key={api_key}"
call1_2021 = "https://api.census.gov/data/2021/acs/acs5?get={call1_vars}&for=tract:*&in=state:06&in=county:075&key={api_key}"

call0_2012 = "https://api.census.gov/data/2012/acs/acs5?get={call0_vars}&for=tract:*&in=state:06&in=county:075&key={api_key}"
call1_2012 = "https://api.census.gov/data/2012/acs/acs5?get={call1_vars}&for=tract:*&in=state:06&in=county:075&key={api_key}"

get_data = function(call){
	L = str_glue(call) %>%
		fromJSON()
	tmp_names = L[[1]]
	tmp = do.call(rbind, L[2:length(L)])
	colnames(tmp) = tmp_names
	return(tmp)
}

tbl_2021 = get_data(call0_2021) %>%
	as_tibble() %>%
	full_join(as_tibble(get_data(call1_2021))) %>%
	mutate(across(starts_with('B'), ~as.integer(.x))) %>%
	rename(all_of(vars)) %>%
	rename(TRACTCE = tract, ST = state, CT = county) %>%
	mutate(
		YEAR = 2021L,
		HOME_V2021 = HOME_V * 1000,
		TRACTCE = factor(TRACTCE),
		ST = factor(ST),
		CT = factor(CT)
	)

tbl_2012 = get_data(call0_2012) %>%
	as_tibble() %>%
	full_join(as_tibble(get_data(call1_2012))) %>%
	mutate(across(starts_with('B'), ~as.integer(.x))) %>%
	rename(all_of(vars)) %>%
	rename(TRACTCE = tract, ST = state, CT = county) %>%
	mutate(
		YEAR = 2012L,
		HOME_V2021 = HOME_V * 1000 * 1.15,
		TRACTCE = factor(TRACTCE),
		ST = factor(ST),
		CT = factor(CT)
	)

tbl = bind_rows(tbl_2021, tbl_2012) %>%
	mutate(
		MOB_N_NB = rowSums(across(matches('MOB_N_[^B][A2]'))),
		MOB_SHM_NB = rowSums(across(matches('MOB_SHMsee_[^B][A2]'))), 
		MOB_SCT_NB = rowSums(across(matches('MOB_SCT_[^B][A2]'))), 
		MOB_SST_NB = rowSums(across(matches('MOB_SST_[^B][A2]'))), 
		MOB_DST_NB = rowSums(across(matches('MOB_DST_[^B][A2]'))), 
		MOB_DNT_NB = rowSums(across(matches('MOB_SNT_[^B][A2]'))), 
		MOB_N = MOB_N_NB + MOB_N_BA,
		FRAC_BA = MOB_N_BA / MOB_N,
		HOME_V2021_PERCAP = HOME_V2021 / MOB_N,
	) %>%
	group_by(YEAR) %>%
	mutate(FRAC_BA_DECY = ntile(FRAC_BA, 10)) %>%
	ungroup() %>%
	select(
		YEAR, ST, CT, TRACTCE, N, MOB_N,
		starts_with('FRAC'),
		starts_with('HOME_V'), 
		ends_with('NB'),
		ends_with('BA'),
	)
write_csv(tbl, file='homeval_mob_acs5_2012-2021.csv')
