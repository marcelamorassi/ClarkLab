library(lubridate)
nested_data = data1_new %>% 
  mutate(real_date = as.Date(day_date, "%m/%d/%Y %H:%M")) %>% 
  as_tibble() %>% 
  mutate(full_id = paste(plate_id, seedling, sep = "_")) %>% 
  group_by(full_id, genotype, ATP_conc) %>% 
  arrange(full_id, real_date) %>% 
  select(-plate_id, -seedling) %>% 
  # nest collapses each tree's data into a seprate data frame
  nest()
nested_data %>% unnest(data) %>% 
  ggplot(aes(x = real_date, y = root, group = full_id)) + 
  geom_smooth(method = 'lm', se= FALSE) + 
  geom_point() + 
  facet_wrap(~full_id) + cowplot::theme_nothing()

# Define a function that works w/ one group
get_growth_rate = function(data) {
  # Fits a straight line to your root growth data
  regress = lm(root ~ real_date, data = data)
  # Extract the slope of that line (average growth rate)
  regress$coef[2]
}

growth_rates = nested_data %>% 
  # Create a new column in nested data that contains growth rate
  # map_dbl applies a function (get_growth_rate) to each dataframe in the data column
  # And returns a single numerical value (which is the growth rate)
  mutate(growth_rate = map_dbl(data, get_growth_rate)) %>% 
  select(-data) %>%  # get rid of the nested data
  ungroup()
