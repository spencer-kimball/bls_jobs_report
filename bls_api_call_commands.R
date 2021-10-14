library(blsAPI)
library(jsonlite)
library(googlesheets4)


resp_nonfarm_sectors <- blsAPI(payload_sectors, 2)
json_nonfarm_sectors <- fromJSON(resp_nonfarm_sectors)
nonfarm_sectors_data <- json_nonfarm_sectors$Results$series$data
nonfarm_sectors_df <- nonfarm_sectors_funct(nonfarm_sectors_data) 

1 

resp_unemp_rate <- blsAPI(payload_unemp_rate,2)
json_unemp_rate <- fromJSON(resp_unemp_rate)
unemp_rate_data <- json_unemp_rate$Results$series$data
unemp_rate_df <- unemp_rate_funct(unemp_rate_data)

resp_youth_unemp_rate <- blsAPI(payload_youth_unemp_rate,2)
json_youth_unemp_rate <- fromJSON(resp_youth_unemp_rate)
youth_unemp_rate_data <- json_youth_unemp_rate$Results$series$data
youth_unemp_rate_df <- youth_unemp_funct(youth_unemp_rate_data)

resp_labor_force <- blsAPI(payload_labor_force_particip, 2)
json_labor_force <- fromJSON(resp_labor_force)
labor_force_data <- json_labor_force$Results$series$data
labor_force_df <- labor_force_funct(labor_force_data)

resp_labor_force_level <- blsAPI(payload_labor_force_level, 2) 
json_labor_force_level <- fromJSON(resp_labor_force_level)
labor_force_level_data <- json_labor_force_level$Results$series$data
labor_force_level_df <- labor_level_funct(labor_force_level_data)

resp_pop_ratio_emp <- blsAPI(payload_emp_pop_ratio, 2)
json_pop_ratio_emp <- fromJSON(resp_pop_ratio_emp)
pop_ratio_emp_data <- json_pop_ratio_emp$Results$series$data
pop_ratio_emp_df   <- emp_pop_ratio_funct(pop_ratio_emp_data)

resp_nlf_desire <- blsAPI(payload_discouraged, 2)
json_nlf_desire <- fromJSON(resp_nlf_desire)
nlf_desire_data <- json_nlf_desire$Results$series$data
nlf_desire_df <- nlf_desire_funct(nlf_desire_data)


resp_unemp_level <- blsAPI(payload_unemp_level, 2)
json_unemp_level <- fromJSON(resp_unemp_level)
unemp_level_data <- json_unemp_level$Results$series$data
unemp_level_df <- unemp_level_funct(unemp_level_data)

resp_emp_level <- blsAPI(payload_emp_level, 2)
json_emp_level <- fromJSON(resp_emp_level)
emp_level_data <- json_emp_level$Results$series$data
emp_level_df <- emp_level_funct(emp_level_data)

resp_unemp_dur <- blsAPI(payload_unemp_duration, 2)
json_unemp_dur <- fromJSON(resp_unemp_dur)
unemp_dur_data <- json_unemp_dur$Results$series$data
unemp_dur_df <- unemp_dur_funct(unemp_dur_data)


resp_reason_unemp <- blsAPI(payload_reason_unemp, 2)
json_reason_unemp <- fromJSON(resp_reason_unemp)
reason_unemp_data <- json_reason_unemp$Results$series$data
reason_unemp_df <- reason_unemp_funct(reason_unemp_data)

resp_reason_pct <- blsAPI(payload_reason_pct,2)
json_reason_pct <- fromJSON(resp_reason_pct)
reason_pct_data <- json_reason_pct$Results$series$data
reason_pct_df <- reason_pct_funct(reason_pct_data)

resp_black_reason_unemp <- blsAPI(payload_black_reason_unemp, 2)
json_black_reason_unemp <- fromJSON(resp_black_reason_unemp)
black_reason_unemp_data <- json_black_reason_unemp$Results$series$data
black_reason_unemp_df <- black_reason_unemp_funct(black_reason_unemp_data)

resp_white_reason_unemp <- blsAPI(payload_white_reason_unemp, 2)
json_white_reason_unemp <- fromJSON(resp_white_reason_unemp)
white_reason_unemp_data <- json_white_reason_unemp$Results$series$data
white_reason_unemp_df <- white_reason_unemp_funct(white_reason_unemp_data)

resp_hispanic_reason_unemp <- blsAPI(payload_hispanic_reason_unemp, 2)
json_hispanic_reason_unemp <- fromJSON(resp_hispanic_reason_unemp)
hispanic_reason_unemp_data <- json_hispanic_reason_unemp$Results$series$data
hispanic_reason_unemp_df <- hispanic_reason_unemp_funct(hispanic_reason_unemp_data)

resp_asian_reason_unemp <- blsAPI(payload_asian_reason_unemp, 2)
json_asian_reason_unemp <- fromJSON(resp_asian_reason_unemp)
asian_reason_unemp_data <- json_asian_reason_unemp$Results$series$data
asian_reason_unemp_df <- asian_reason_unemp_funct(asian_reason_unemp_data)


resp_weekly_earnings <- blsAPI(payload_weekly_earnings,2)
json_weekly_earnings <- fromJSON(resp_weekly_earnings)
weekly_earnings_data <- json_weekly_earnings$Results$series$data
weekly_earnings_df <- weekly_earnings_funct(weekly_earnings_data)

resp_ind_hourly_earnings <- blsAPI(payload_ind_hourly_earnings,2)
json_ind_hourly_earnings <- fromJSON(resp_ind_hourly_earnings)
ind_hourly_earnings_data <- json_ind_hourly_earnings$Results$series$data
ind_hourly_earnings_df <- ind_hourly_earnings_funct(ind_hourly_earnings_data)


resp_industry_unemp <- blsAPI(payload_unemp_rate_ind,2)
json_industry_unemp <- fromJSON(resp_industry_unemp)
industry_unemp_data <- json_industry_unemp$Results$series$data
industry_unemp_df <- unemp_ind_funct(industry_unemp_data)

resp_women_ind <- blsAPI(payload_women_uemp_ind,2)
json_women_ind <- fromJSON(resp_women_ind_unemp)
women_ind_data <- json_women_ind_unemp$Results$series$data
women_ind_df <- women_ind_funct(women_ind_data) 

resp_white_ind <- blsAPI(payload_white_ind,2)
json_white_ind <- fromJSON(resp_white_ind)
white_ind_data <- json_white_ind$Results$series$data
white_ind_df <- white_ind_funct(white_ind_data)

resp_black_ind <- blsAPI(payload_black_ind,2)
json_black_ind <- fromJSON(resp_black_ind)
black_ind_data <- json_black_ind$Results$series$data
black_ind_df <- black_ind_funct(black_ind_data)

resp_hispanic_ind <- blsAPI(payload_hispanic_ind,2)
json_hispanic_ind <- fromJSON(resp_hispanic_ind)
hispanic_ind_data <- json_hispanic_ind$Results$series$data
hispanic_ind_df <- hispanic_ind_funct(hispanic_ind_data)

resp_asian_ind <- blsAPI(payload_asian_ind,2)
json_asian_ind <- fromJSON(resp_asian_ind)
asian_ind_data <- json_asian_ind$Results$series$data
asian_ind_df <- asian_ind_funct(asian_ind_data)

resp_education_unemp <- blsAPI(payload_education_unemp,2)
json_education_unemp <- fromJSON(resp_education_unemp)
education_unemp_data <- json_education_unemp$Results$series$data
education_unemp_df <- education_unemp_funct(education_unemp_data)

resp_education_genderrace <- blsAPI(payload_education_genderrace,2)
json_education_genderrace <- fromJSON(resp_education_genderrace)
education_genderrace_data <- json_education_genderrace$Results$series$data
education_genderrace_df <- education_genderrace_funct(education_genderrace_data)

