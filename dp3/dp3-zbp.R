#!/usr/bin/R

library('tidyverse')
library('RJSONIO')

api_key = 'f04632091f5c62a6f271cb572a313cc41e8cec03'
table_zip_codes = read_csv('San_Francisco_ZIP_Codes.csv', show_col_types=FALSE)

zip_codes = select(table_zip_codes, zip_code) %>%
	unlist() %>%
	unique() %>%
	paste0(collapse=',')

call_zbp_2017 = "https://api.census.gov/data/2018/zbp?get=ZIPCODE,YEAR,NAME,GEO_ID,GEOCOMP,NAICS2017,NAICS2017_LABEL,EMP,EMPSZES,EMPSZES_LABEL,ESTAB,PAYANN,PAYQTR1,SUMLEVEL,INDGROUP,INDLEVEL,SECTOR,SUBSECTOR&for=zipcode:{zip_codes}&key={api_key}"

call_zbp_2012 = "https://api.census.gov/data/2012/zbp?get=ZIPCODE,YEAR,GEO_ID,NAICS2012,NAICS2012_TTL,EMP,EMPSZES,EMPSZES_TTL,ESTAB,PAYANN,PAYQTR1&for=zipcode:{zip_codes}&key={api_key}"


L = fromJSON(str_glue(call_zbp_2017)) %>% 
	lapply(function(i) sapply(i, function(j) if(is.null(j)) NA else j))
n = str_replace(L[[1]], ' ', '_')
table_zbp_2018 = do.call(rbind, L[2:length(L)])
colnames(table_zbp_2018) = n
table_zbp_2018 = as_tibble(table_zbp_2018)

L = fromJSON(str_glue(call_zbp_2012)) %>%
       lapply(function(i) sapply(i, function(j) if(is.null(j)) NA else j))
n = str_replace(L[[1]], ' ', '_')
table_zbp_2012 = do.call(rbind, L[2:length(L)])
colnames(table_zbp_2012) = n
table_zbp_2012 = as_tibble(table_zbp_2012)

#get_table = function(call_str, year){
#	call = str_glue(call_str)
#	L = fromJSON(call)
#	n = str_replace(L[[1]], ' ', '_') %>%
#		str_to_upper()
#	d = do.call(rbind, L[2:length(L)])
#	colnames(d) = n
#	d = as_tibble(d)
#	rcensuseturn(d)
#}

#call_zbp_2007 = "https://api.census.gov/data/{year}/zbp?get=ZIPCODE,GEO_ID,GEO_TTL,NAICcensusS2007,EMP,ESTAB,PAYANN,PAYQTR1&for=zipcode:{zip_codes}&key={api_key}"
#call_zbp_2002 = "https://api.census.gov/data/{year}/zbp?get=ZIPCODE,GEO_ID,GEO_TTL,NAICS2002,NACIS_TTL,EMP,ESTAB,PAYANN,PAYQTR1&for=zipcode:{zip_codes}&key={api_key}"
#call_zbp_1997 = "https://api.census.gov/data/{year}/zbp?get=ZIPCODE,GEO_ID,GEO_TTL,NAICS1997,NAICS_TTL,EMP,ESTAB,PAYANN,PAYQTR1&for=zipcode:{zip_codes}&key={api_key}"


