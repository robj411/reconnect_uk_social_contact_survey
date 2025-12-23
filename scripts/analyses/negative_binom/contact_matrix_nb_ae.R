
## reformatting survey data ##

part <- reconnect_participant %>% 
  rename(p_id = part_id,
         p_age_group = part_age_group,
         p_adult_child = part_adult_child,
         p_gender = part_gender,
         p_ethnicity = part_ethnicity,
         p_sec_input = part_ses) %>% 
  mutate(p_gender = case_when(p_gender == 'F' ~ 'Female',
                              p_gender == 'M' ~ 'Male',
                              T ~ NA),
         p_age_group = cut(part_age_exact,
                           breaks = age_breaks,
                           labels = age_labels,
                           right = F))

contacts <- reconnect_contact %>% 
  rename(c_id = cont_id,
         p_id = part_id,
         c_location = cnt_location,
         c_age_group = cnt_age_group,
         c_sex = cnt_gender,
         c_ethnicity = cnt_ethnicity,
         c_sec_input = cnt_ses) %>% 
  mutate(c_sex = case_when(c_sex == 'F' ~ 'Female',
                           c_sex == 'M' ~ 'Male',
                           T ~ NA),
         c_age_group = cut(cnt_age_exact,
                           breaks = age_breaks,
                           labels = age_labels,
                           right = F))

# if results folder doesn't exist, create
if(!file.exists('results')){dir.create('results')}





#################################
#### AGE GROUP AND ETHNICITY ####
#################################

nb_age_group_ethnicity <- nb_matrix_fit(
  participant_data = part,
  contact_data = contacts,
  participant_var = c("p_age_group",'p_ethnicity'),
  contact_var = c("c_age_group",'c_ethnicity'),
  n_bootstrap = 10,
  trunc = max_n_contacts,
  polymod_weighting = polymod_wts,
  weighting_vec = c('p_gender','day_week','p_sec_input'),
  locations = c("Total", "Home", "Work", "School", "Other"),
  save = T,
  impute_contact = F,
  impute_vars = c('p_ethnicity')
)

# nb_age_group_ethnicity <- readRDS(here::here('results','nb_age_group_ethnicity_1000.rds'))

cat('\nAge/ethnicity done, n bootstraps = ', n_distinct(nb_age_group_ethnicity[[1]]$bs_index), '\n', sep = '')

# normalise wrt population structure
eth_age <- eth_age %>% mutate(p_key = paste(p_age_group,p_ethnicity,sep='_'))
nb_age_group_ethnicity_norm <- map(
  .x = nb_age_group_ethnicity,
  .f = ~normalise_matrix(
    matrix = .x,
    p_var_name = 'p_key',
    population_dt = eth_age
  )
)

age_eth_assortativity <- map(
  .x = nb_age_group_ethnicity_norm,
  .f = ~fcn_assortativity(
    beta = .x,
    n_input = eth_age,
    order = unique(eth_age$p_key)
  )
)
names(age_eth_assortativity) <- names(nb_age_group_ethnicity_norm)
age_eth_assortativity_dt <- rbindlist(age_eth_assortativity, idcol = 'location')
age_eth_assortativity_dt[, grouping := 'Age group and ethnicity']

age_eth_matrices <- plot_and_save_mu_matrix(matrix_data = nb_age_group_ethnicity_norm,
                                        p_var = 'p_key',
                                        c_var = 'c_var',
                                        variable = 'mu', 
                                        uncertainty = F)

plot_and_save_mu_matrix(nb_age_group_ethnicity_norm,
                        p_var = 'p_key',
                        c_var = 'c_var',
                        uncertainty = T)

plot_and_save_mu_matrix(nb_age_group_ethnicity,
                        variable = 'k',
                         p_var = 'p_key',
                         c_var = 'c_var')








