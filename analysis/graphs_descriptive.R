# Census Data: 
# Indiana Population Example 

# Setup: clean environment and load libraries 
# Get Census Data 
rm(list = ls())
### Setup 
if(!require(pacman)) {install.packages("pacman")}
if(!require(tidyverse)) {install.packages("tidyverse")}
if(!require(sf)) {install.packages("sf")}
if(!require(lubridate)) {install.packages("lubridate")}
if(!require(tidycensus)) {install.packages("tidycensus")}
if(!require(tigris)) {install.packages("tigris")}
if(!require(purrr)) {install.packages("purrr")}
if(!require(rio)) {install.packages("rio")}
if(!require(here)) {install.packages("here")}
if(!require(haven)) {install.packages("haven")}
if(!require(scales)) {install.packages("scales")}

library(pacman)
pacman::p_load(tidyverse, sf, lubridate, 
               tidycensus, tigris, purrr, scales)
options(scipen = 999)
#path = "C:/Users/luise/OneDrive - Indiana University/Research/School_Districts/"
path <- here("/Users/luisenriquenavarro/Library/CloudStorage/OneDrive-SharedLibraries-IndianaUniversity/[Sec-E] BL-SPEA-FoodDonations - [Sec-E] BL-SPEA-FoodDonations")
input <- here(path, 'Raw Data','county_data_bls_irs')
input_bls <- here(input, 'qcew')
input_soi <- here(input, 'irs_soi')
bi = here(path,'stata','build', 'input')
bc = here(path,'stata','build', 'code')
bo = here(path,'stata','build', 'output')
bt = here(path,'stata','build', 'temp')
ai = here(path,'stata','analysis','input')
ac = here(path,'stata','analysis','code')
ao = here(path,'stata','analysis','output')
at = here(path,'stata','analysis','temp')

save_graph <- function(graph, name, size = "small"){
  
  if(size == "big"){
    cowplot::ggsave2(filename = here(ao, name), 
                     plot = graph, dpi = 200, 
                     width = 40, height = 40, units = "cm")
  } else {
    cowplot::ggsave2(filename = here(ao, name), 
                     plot = graph, dpi = 200, 
                     width = 40, height = 20, units = "cm")
  } 
}

format_plot <- function(graph){
  # Format the Graph
  fontsize = 12
  formatted_plot <- graph + 
    ggthemes::theme_stata() + 
    theme(
      axis.text.x = element_text(angle = 0, size = fontsize, face = "plain"), 
      axis.text.y = element_text(angle = 0, size = fontsize, face = "plain"), 
      axis.title.x = element_text(size = fontsize, face = "plain"),
      axis.title.y = element_text(size = fontsize, face = "plain"),
      legend.text = element_text(size = fontsize , face = "plain", hjust = 0),
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.position = "top", 
      legend.justification = "left",
      plot.title = element_text(angle = 0, size = fontsize + 3, face = "bold", hjust = 0),
      plot.subtitle = element_text(angle = 0, size = fontsize + 1, face = "italic", hjust = 0),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.line = element_line(),
      strip.text = element_text(size = fontsize + 2), 
      strip.background = element_blank(), 
      text = element_text(family = "Helvetica"),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      panel.grid.major = element_blank(), # Optionally remove major grid lines
      panel.grid.minor = element_blank()  # Optionally remove minor grid lines
    )
  return(formatted_plot)
}
gleaners_food_donations <- haven::read_dta(here(bt, 'GleanersCleanDonations.dta'))

# Manually reverse the color palette
color_palette <- c("gray50", "magenta2", "lightblue1", "lightgreen", "steelblue4", 
                   "orange2", "purple", "salmon", "beige", "firebrick", "gold", "forestgreen")
# Define the order of Product_Group based on healthiness
product_group_order <- data.frame(
  Product_Group = c(
    "Fruits and Vegetables",
    "Grains",
    "Protein",
    "Dairy",
    "Non-Dairy Alternatives",
    "Beverages",
    "Mixed Dishes",
    "Processed and Packaged Snacks",
    "Desserts",
    "Condiments and Cooking Staples",
    "Other Miscellaneous Items",
    "Other"
  ),
  order = 1:12
)

gleaners_food_donations_clean <- gleaners_food_donations %>% 
  mutate_if(is.labelled, as_factor) %>%
  filter(!is.na(id)) %>% 
  mutate(ProductDes = as.character(ProductDes)) %>% 
  mutate(Product_Group = case_when(
    ProductDes %in% c("Fresh Fruits/Vegetables", "Fruit: Canned and Frozen", "Vegetables - Canned & Frozen") ~ "Fruits and Vegetables",
    ProductDes %in% c("Cereal: Hot and Cold", "Grain: Flour, Corn Meal, Matzo Meal", "Pasta: Macaroni, Spaghetti, Noodles", "Rice") ~ "Grains",
    ProductDes %in% c("Meat/Fish/Poultry", "Protein - Non-Meat: Peanut Butter, Beans, Eggs, Pork & Beans, Nuts") ~ "Protein",
    ProductDes %in% c("Dairy: Yogurt, Cheese, Milk, Butter, Sour cream Ice Cream") ~ "Dairy",
    ProductDes %in% c("Non-Dairy Dairy Substitute") ~ "Non-Dairy Alternatives",
    ProductDes %in% c("Beverage: Coffee, Tea, Soda, Drinks", "Juice: 100% Fruit or Vegetable") ~ "Beverages",
    ProductDes %in% c("Complete Meal/Entree, Soup", "Mixed and Assorted Food", "Prepared and Perishable Food") ~ "Mixed Dishes",
    ProductDes %in% c("Snack Food/Cookies: Candy, Crackers, Marshmallows") ~ "Processed and Packaged Snacks",
    ProductDes %in% c("Dessert: Cakes, Pies, Pudding, Frozen Confections") ~ "Desserts",
    ProductDes %in% c("Dressing: Salad dressing, Mayonnaise", "Spice/Condiment/Sauce: Herbs, Salt, Sugar, Mixes, Vinegar, Extracts, Mustard, Syrup, Gravy, Jelly, Sauces, Salad Oil") ~ "Condiments and Cooking Staples",
    ProductDes %in% c("Assorted Non-Food: Household goods, Toys, Books, Clothing", "Baby Food/Formula", "Bread/Bakery: Bread, Biscuits, Rolls, Batter, Tortillas, Pie Crusts", "Dough - Uncooked", "Health/Beauty Care: Shampoo, Conditioner, Soap, Cosmetics, Deodorants, All Dental Care", "Household Cleaning Product: Detergent, Cleanser, Bleach, Fabric Softener", "Nutritional Aid: Drinks, Vitamins, Diet Supplements", "Paper Product - Household: Plates, Napkins, Towels, Toilet Paper, Facial Tissue", "Paper Product - Personal: Diapers, Adult Sanitary Products, Feminine Products", "Pet Food/Pet Care", "Salvage - Unsorted") ~ "Other Miscellaneous Items",
    TRUE ~ "Other"
  )) %>% 
  left_join(product_group_order, by = "Product_Group") %>%
  mutate(MonthYear = floor_date(date, unit = "months")) %>% 
  mutate(`Storage Type` = case_when(
    Storage %in% c("FRONZEN", "FRZTOTE", "CHEMICAL", "FROZEN", "REF") ~ "Refrigerated", 
    Storage %in% c("DRY", "DRYTOTE", "NF") ~ "Dry")) %>% 
  filter(PalletTotalWeight >= 0, PalletTotalWeight <= 1000000)

# Verify the join results
gleaners_food_donations_clean %>% 
  group_by(Product_Group, ProductDes) %>% 
  summarize(n = n(), .groups = 'drop')

#------------------------------------------------------------------------------
library(RColorBrewer)


donations_plot <- gleaners_food_donations_clean %>%
  group_by(`Storage Type`, Product_Group, Year, order) %>% 
  filter(is.na(`Storage Type`) == FALSE) %>% 
  summarize(value = sum(PalletTotalWeight), .groups = 'drop') %>% 
  group_by(Year, `Storage Type`) %>% 
  mutate(percentage = value / sum(value) * 100) %>% 
  ggplot(mapping = aes(x = Year, y = percentage, 
                       fill = reorder(Product_Group, -order))) + 
  geom_col(position = "stack", color = "black") + 
  scale_y_continuous(labels = scales::percent_format(scale = 1), n.breaks = 10) +
  scale_x_continuous(breaks = 2010:2022) + 
  labs(y = "Percentage", x = "", title = "Provisions by Type of Product and Storage") + 
  facet_wrap(~`Storage Type`) 

donations_plot <- format_plot(donations_plot) +
  scale_fill_manual(values = color_palette) + 
  theme(legend.position = "bottom") + 
  theme(axis.text.x.bottom = element_text(angle = 90)) 
  

save_graph(donations_plot, name = "donations_plot.png")


#-------------------------------------------------------------------------------
# Dry Refriferated
storage_plot <- gleaners_food_donations_clean %>%
  group_by(`Storage Type`, Year) %>% 
  filter(is.na(`Storage Type`) == FALSE) %>% 
  summarize(value = sum(PalletTotalWeight), .groups = 'drop') %>% 
  mutate(percentage = value / sum(value) * 100) %>% 
  ggplot(mapping = aes(x = Year, y = value, 
                       fill = `Storage Type`)) + 
  geom_col(position = "fill", color = "black") + 
  scale_y_continuous(labels = scales::percent_format(scale = 100), n.breaks = 10) +
  scale_x_continuous(breaks = 2010:2022) + 
  labs(y = "Percentage", x = "", title = "Provisions by Type of Storage") + 
  guides(fill = guide_legend(reverse = TRUE))


storage_plot <- format_plot(storage_plot) +
  #scale_fill_manual(values = c("lightblue3", "firebrick4")) + 
  theme(legend.position = "bottom") + 
  theme(axis.text.x.bottom = element_text(angle = 90)) 


#-----------------------------------------------------------------------------
# Donor Type 
donor_plot <- gleaners_food_donations_clean %>%
  filter(is.na(Donor) == FALSE) %>% 
  mutate(Donor = case_when(
    Donor %in% c("Unknown", "Co-Packer", "Drug Store", "Hiome Center", "Food Service", "Processor", 
                 "Wholesaler", "Restaurant", "Super Market", "Reclamation") ~ "Others",
    Donor == "Feeding America Affiliate" ~ "FAA", 
    Donor == "Warehouse Club" ~ "WH Club", 
    TRUE ~ Donor
  )) %>% 
  group_by(Donor, Year) %>% 
  summarize(value = sum(PalletTotalWeight), .groups = 'drop') %>% 
  mutate(percentage = value / sum(value) * 100) %>% 
  mutate(order = case_when(
    Donor == "Retailer" ~ 8 , 
    Donor == "Manufacturer" ~ 7 , 
    Donor == "Distributor" ~ 6 , 
    Donor == "FAA" ~ 5 , 
    Donor == "Other Class of Trade" ~ 4 , 
    Donor == "Grower" ~ 3 , 
    Donor == "WH Club" ~ 2 , 
    Donor == "Others" ~ 1
  )) %>% 
  mutate(Donor = factor(Donor, levels = c("Others", "WH Club", "Grower", "Other Class of Trade",
                                          "FAA", "Distributor", "Manufacturer", "Retailer"))) %>%
  ggplot(mapping = aes(x = Year, y = value, 
                       fill = reorder(Donor, order))) + 
  geom_col(position = "fill", color = "black") + 
  scale_y_continuous(labels = scales::percent_format(scale = 100), n.breaks = 10) +
  scale_x_continuous(breaks = 2010:2022) + 
  labs(y = "Percentage", x = "", title = "Provisions by Type of Donor") + 
  guides(fill = guide_legend(reverse = TRUE))

donor_plot <- format_plot(donor_plot) +
  scale_fill_manual(values = color_palette) + 
  theme(legend.position = "bottom") + 
  theme(axis.text.x.bottom = element_text(angle = 90)) 

save_graph(plot_grid(storage_plot, donor_plot, nrow = 1), name = "storage_donor_plot.png", size = "small")

combined_plot <- plot_grid(
  plot_grid(storage_plot, donor_plot, nrow = 1),
  donations_plot,
  nrow = 2,
  rel_heights = c(1, 1), # Adjust relative heights if necessary
  label_fontfamily = "Helvetica"
)

save_graph(combined_plot, name = "combined_plots.png", size = "big")
#------------------------------------------------------------------------------

# Four Panel Figures 

# Read data descriptive: Quarter by County Data 
data_descriptive <- haven::read_dta(here(bt, 'data_descriptive.dta')) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  mutate(
    mofd = floor_date(as.Date(date), "month"),
    year = year(as.Date(date)),
    qofd = floor_date(as.Date(date), "quarter")
  ) 

# Data Panels A and B 
data_panel_ab <- data_descriptive %>%
  group_by(qofd) %>% 
  summarize(food_donations = sum(PalletTotalWeight), 
            food_donations_ng = sum(PalletTotalWeight_NonGov), 
            money_donations = sum(donation_sum)) %>% 
  mutate(year = year(qofd)) %>% 
  mutate(
    food = log(food_donations), 
    food_ng = log(food_donations_ng), 
    money = log(money_donations)
  ) %>% select(-c(food_donations, food_donations_ng, money_donations))

mean_food_a <- data_panel_ab %>% 
  filter(year <= 2019) %>% 
  summarize(mean = mean(food)) %>% 
  as.double()

# Data Panel C 
data_panel_c <- data_descriptive %>% 
  group_by(qofd) %>% 
  summarize(food_donations = sum(PalletTotalWeight), 
            food_donations_ng = sum(PalletTotalWeight_NonGov), 
            tefap = sum(tefap)) %>% 
  mutate(year = year(qofd)) 

foodng_init = data_panel_c %>% filter(qofd == ymd("2013-01-01")) %>% pull(food_donations_ng) %>% as.double()
tefap_init = data_panel_c %>% filter(qofd == ymd("2013-01-01")) %>% pull(tefap) %>% as.double()

data_panel_c <- data_panel_c %>% 
  mutate(`Non Government Provisions` = food_donations_ng/foodng_init, 
         `TEFAP` = tefap/tefap_init) %>% 
  select(c(qofd, `Non Government Provisions`, `TEFAP`)) 

# Data Panel D 
data_panel_d <- data_descriptive %>% 
  group_by(qofd, Quartile) %>% 
  summarize(food_donations = sum(PalletTotalWeight)) %>% 
  mutate(food_donations = log(food_donations)) %>% 
  mutate(year = year(qofd)) %>% 
  mutate(food_init = ifelse(qofd == ymd("2013-01-01"), food_donations, NA)) %>% 
  ungroup() %>% group_by(Quartile) %>% 
  mutate(food_init = mean(food_init, na.rm = TRUE)) %>% 
  mutate(index = food_donations/food_init) %>% 
  select(qofd, Quartile, index)


#-----------------------------------------------------------------------------
# Panel A
panel_a_plot <- data_panel_ab %>%
  mutate(`In-Kind (Mean 2013:2019)` = ifelse(year <= 2019, mean_food_a, NA)) %>%
  rename(`In-Kind` = food, `NG In-Kind` = food_ng) %>%
  select(-c(money, year)) %>% 
  filter(qofd >= as.Date("2013-01-01") & qofd <= as.Date("2022-04-01")) %>%
  gather(key = "variable", value = "value", -qofd) %>%
  ggplot(mapping = aes(x = qofd, y = value, color = variable, shape = variable, linetype = variable)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = ymd("2019-10-01"), linetype = "dashed", color = "black") + 
  labs(title = "Panel A: Total In-Kind Provisions", 
       subtitle = "(Log Pounds)", 
       x = "", y = "") + 
  scale_x_date(date_breaks = "3 months", labels = function(x) paste0(year(x), "q", quarter(x)),
               limits = c(as.Date("2013-01-01"), as.Date("2022-04-01"))) +
  scale_y_continuous(n.breaks = 10) + 
  coord_cartesian(xlim = c(ymd("2013-04-01"), ymd("2022-01-01")))
  

panel_a_plot <- format_plot(panel_a_plot) + 
  scale_color_manual(values = c(`In-Kind` = "black", `NG In-Kind` = "blue", `In-Kind (Mean 2013:2019)` = "firebrick")) +
  scale_shape_manual(values = c(`In-Kind` = 16, `NG In-Kind` = 15, `In-Kind (Mean 2013:2019)` = NA)) +
  scale_linetype_manual(values = c(`In-Kind` = "solid", `NG In-Kind` = "solid", `In-Kind (Mean 2013:2019)` = "dashed")) +
  #theme(legend.position = "bottom") + 
  theme(axis.text.x.bottom = element_text(angle = 90)) 
#-----------------------------------------------------------------------------
# Panel B 
mean_food_b <- data_panel_ab %>% 
  filter(year <= 2019) %>% 
  summarize(mean = mean(money)) %>% 
  as.double()

panel_b_plot <- data_panel_ab %>%
  mutate(`Mean 2013:2019` = ifelse(year <= 2019, mean_food_b, NA)) %>%
  rename(`Financial Provisions` = money) %>%
  select(-c(food, food_ng, year)) %>% 
  filter(qofd >= as.Date("2013-01-01") & qofd <= as.Date("2022-04-01")) %>%
  gather(key = "variable", value = "value", -qofd) %>%
  ggplot(mapping = aes(x = qofd, y = value, color = variable, shape = variable, linetype = variable)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = ymd("2019-10-01"), linetype = "dashed", color = "black") + 
  labs(title = "Panel B: Total Financial Provisions", 
       subtitle = "(Log US $)", 
       x = "", y = "") + 
  scale_x_date(date_breaks = "3 months", labels = function(x) paste0(year(x), "q", quarter(x)),
               limits = c(as.Date("2013-01-01"), as.Date("2022-04-01"))) +
  scale_y_continuous(n.breaks = 10) + 
  coord_cartesian(xlim = c(ymd("2013-04-01"), ymd("2022-01-01")))


panel_b_plot <- format_plot(panel_b_plot) + 
  scale_color_manual(values = c(`Financial Provisions` = "black", `Mean 2013:2019` = "firebrick")) +
  scale_shape_manual(values = c(`Financial Provisions` = 16, `Mean 2013:2019` = NA)) +
  scale_linetype_manual(values = c(`Financial Provisions` = "solid", `Mean 2013:2019` = "dashed")) +
  #theme(legend.position = "bottom") + 
  theme(axis.text.x.bottom = element_text(angle = 90)) 

#------------------------------------------------------------------------------
# Panel C: Index: TEFAP, Non-Government Donations 

panel_c_plot <- data_panel_c %>% 
  gather(key = "variable", value = "value", -qofd) %>% 
  ggplot(mapping = aes(x = qofd, y = value, color = variable, shape = variable, linetype = variable)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = ymd("2019-10-01"), linetype = "dashed", color = "black") + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") + 
  labs(title = "Panel C: In-Kind Provisions", 
       subtitle = "(Log Pounds, Index = 1 (2013q1))", 
       x = "", y = "") + 
  scale_x_date(date_breaks = "3 months", labels = function(x) paste0(year(x), "q", quarter(x)),
               limits = c(as.Date("2013-01-01"), as.Date("2022-04-01"))) +
  scale_y_continuous(n.breaks = 10) + 
  coord_cartesian(xlim = c(ymd("2013-04-01"), ymd("2022-01-01")))
  
panel_c_plot <- format_plot(panel_c_plot) + 
  scale_color_manual(values = c(`TEFAP` = "black", `Non Government Provisions` = "firebrick")) +
  scale_shape_manual(values = c(`TEFAP` = 16, `Non Government Provisions` = 15)) +
  scale_linetype_manual(values = c(`TEFAP` = "solid", `Non Government Provisions` = "dashed")) +
  #theme(legend.position = "bottom") + 
  theme(axis.text.x.bottom = element_text(angle = 90)) 

#-------------------------------------------------------------------------------
# Panel D: Total In-Kind Donations (Log Pounds )
panel_d_plot <- data_panel_d %>% 
  ggplot(mapping = aes(x = qofd, y = index, color = Quartile, shape = Quartile, linetype = Quartile)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = ymd("2019-10-01"), linetype = "dashed", color = "black") + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") + 
  labs(title = "Panel D: In-Kind Provisions by Quartile", 
       subtitle = "(Log Pounds, Index = 1 (2013q1))", 
       x = "", y = "") + 
  scale_x_date(date_breaks = "3 months", labels = function(x) paste0(year(x), "q", quarter(x)),
               limits = c(as.Date("2013-01-01"), as.Date("2022-04-01"))) +
  scale_y_continuous(n.breaks = 10) + 
  coord_cartesian(xlim = c(ymd("2013-04-01"), ymd("2022-01-01")))

panel_d_plot <- format_plot(panel_d_plot) + 
  scale_color_manual(values = c("black", "firebrick", "steelblue4", "forestgreen")) +
  scale_shape_manual(values = c(16,17,16,17)) +
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed")) +
  #theme(legend.position = "bottom") + 
  theme(axis.text.x.bottom = element_text(angle = 90)) 

#------------------------------------------------------------------------------
panels_combined <- cowplot::plot_grid(
  panel_a_plot, panel_b_plot, 
  panel_c_plot, panel_d_plot, 
  nrow = 2, ncol = 2
)


save_graph(panels_combined, name = "panels_combined.png", size = "small")

#-------------------------------------------------------------------------------
# Extra Figures 

#-----------------------------------------------------------------------------

# Outcome plots 

gleaners_food_donations_clean %>% 
  mutate(quarter_date = floor_date(date, "3 months")) %>% 
  group_by(quarter_date) %>% 
  summarize(value = sum(PalletTotalWeight), .groups = 'drop') %>% 




outcome_plot <- function(data, title ){
  
  plot <- data %>% 
    rename(time = Year) %>% 
    group_by(time, Category) %>% 
    summarize(mean = mean(outcome, na.rm = TRUE), 
              sd = sd(outcome, na.rm = TRUE), 
              p50 = quantile(outcome, probs = 0.50, na.rm = TRUE), 
              p25 = quantile(outcome, probs = 0.25, na.rm = TRUE), 
              p75 = quantile(outcome, probs = 0.75, na.rm = TRUE)) %>% 
    #mutate(p25 = mean - sd, p75 = mean + sd) %>% 
    ggplot(mapping = aes(y = mean, x = time, 
                         color = Category, fill = Category, 
                         shape = Category, linetype = Category)) + 
    geom_line() + geom_point(size = 1) +  
    geom_ribbon(mapping = aes(ymin = p25, ymax = p75), alpha = 0.2, color = NA) +
    #geom_vline(xintercept = treat_date, linetype = "dashed", color = "gray50") + 
    #scale_x_date(breaks = "1 month", labels = date_format("%b-%Y")) + 
    scale_y_continuous(n.breaks = 10) + 
    labs(x = "", y = "Pounds", 
         title = title, subtitle = "Mean and Interquartile Range by Month and Donation Category")
  
  plot <- format_plot(plot) + theme(axis.text.x = element_text(angle = 90))  

  return(plot)
}


outcome_plot(data = gleaners_food_donations_clean %>% 
               mutate(outcome = PalletTotalWeight), 
             title = "Donations by Category (Pounds Donated)")


desc_stats_category <- gleaners_food_donations_clean %>% 
  group_by(Category) %>%
  summarize(`Total (Million Pounds)` = sum(PalletTotalWeight, na.rm = TRUE), 
            `Average` = mean(PalletTotalWeight, na.rm = TRUE), 
            `Std.Dev` = sd(PalletTotalWeight, na.rm = TRUE),
            `Min` = min(PalletTotalWeight, na.rm = TRUE),
            `P05` = quantile(PalletTotalWeight, probs = 0.05, na.rm = TRUE),
            `P25` = quantile(PalletTotalWeight, probs = 0.25, na.rm = TRUE),
            `Median` = quantile(PalletTotalWeight, probs = 0.50, na.rm = TRUE), 
            `P75` = quantile(PalletTotalWeight, probs = 0.75, na.rm = TRUE), 
            `P95` = quantile(PalletTotalWeight, probs = 0.95, na.rm = TRUE), 
            `Max` = max(PalletTotalWeight, na.rm = TRUE)) %>% 
  mutate(`Total (Million Pounds)` = `Total (Million Pounds)`/1000000) 

food_donations_tex <- xtable::xtable(desc_stats_category)
print(food_donations_tex, 
      file = here(ao, 'food_donations_desc_stat.tex'))

donations_plot_fun <- function(data){

boxplot_donations_category <- data %>% 
  group_by(catvar) %>% 
  mutate(order_category = mean(PalletTotalWeight, na.rm=TRUE)) %>% 
  ggplot(mapping = aes(y = reorder(catvar, order_category), x = PalletTotalWeight, 
                       fill = catvar)) + 
  geom_boxplot(outlier.shape = NA, alpha = 0.8, color ="black") + 
  coord_cartesian(xlim = c(0,4000)) + 
  scale_x_continuous(n.breaks = 10) +
  labs(x = "Pounds", y = "", title = "Distribution of Pounds Donated by Type of Donation") 

boxplot_donations_category <- format_plot(boxplot_donations_category) + 
  theme(
    legend.position = c(0.60, 0.10),  # Positioning the legend inside the plot area
    legend.direction = "vertical"
  )

barplot_donations_category <- data %>% 
  mutate(year = year(date)) %>% 
  group_by(catvar, year) %>% 
  summarize(PalletTotalWeight = sum(PalletTotalWeight, na.rm = TRUE)) %>% 
  ungroup() %>% mutate(total = sum(PalletTotalWeight)) %>% 
  mutate(percent = PalletTotalWeight/total) %>% 
  ggplot(mapping = aes(y = percent, x = year,  
                       fill = catvar)) + 
  geom_col(stat = "identity", position = "stack", alpha = 0.8, color = "black") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  
  scale_x_continuous(n.breaks = 10) +
  labs(y = "Percentage of Total Donations", x = "", title = "Pounds Donated by Type of Donation") 

barplot_donations_category <- format_plot(barplot_donations_category)
donations_combined <- plot_grid(
  barplot_donations_category, boxplot_donations_category, 
  ncol = 2
)

return(donations_combined)
}

donations_combined_category <- donations_plot_fun(data = gleaners_food_donations_clean %>% mutate(catvar = Category))
save_graph(donations_combined_category, name = "distribution_donations_category.png")

donations_combined_storage <- donations_plot_fun(data = gleaners_food_donations_clean %>% mutate(catvar = Storage_Cat) %>% 
                                               filter(is.na(Storage_Cat) == FALSE))
#save_graph(donations_combined_storage, name = "distribution_donations_storage2.png")
#-------------------------------------------------------------------------------

# financial donations
gleaners_financial_donations <- haven::read_dta(here(bt, 'GleanersFinancialDonations.dta')) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  ungroup() %>% 
  mutate(cdf_donation = percent_rank(donation_value)) %>% 
  relocate(date, id, donation_value, cdf_donation) %>% 
  arrange(-donation_value)


# Percentage of Observations above One Million 
outlier_cutoof = gleaners_financial_donations %>% filter(donation_value > 1000000) %>% pull(cdf_donation) %>% min()
100*(1 - outlier_cutoof)

#High Nutritional Value: Groups items that are generally considered to have high nutritional content, such as meats, vegetables, fruits, baby food, nutritional aids, and 100% juices.
#Staples: Includes dairy products, bread, pasta, grains, cereals, rice, and non-dairy substitutes, which are considered staple foods.
#Snacks and Beverages: Contains desserts, snack foods, beverages, dough, canned and frozen fruits, and various condiments.
#Non-Food Items: Groups all non-food items such as household goods, personal care products, pet food, and cleaning products.
#Other: A catch-all category for any items not fitting into the above categories, including mixed and assorted foods or unsorted salvage items.

gleaners_financial_donations_desc_stat <- gleaners_financial_donations %>% 
  mutate(cdf_donation = percent_rank(donation_value)) %>% 
  mutate(Year = year(date)) %>% 
  group_by(Year) %>% 
  summarize(`Observations` = n(),
            `Average` = mean(donation_value, na.rm = TRUE), 
            `Std.Dev` = sd(donation_value, na.rm = TRUE),
            `Min` = min(donation_value, na.rm = TRUE),
            `P05` = quantile(donation_value, probs = 0.05, na.rm = TRUE),
            `P25` = quantile(donation_value, probs = 0.25, na.rm = TRUE),
            `Median` = quantile(donation_value, probs = 0.50, na.rm = TRUE), 
            `P75` = quantile(donation_value, probs = 0.75, na.rm = TRUE), 
            `P95` = quantile(donation_value, probs = 0.95, na.rm = TRUE), 
            `Max` = max(donation_value, na.rm = TRUE)) 

financial_donations_tex <- xtable::xtable(gleaners_financial_donations_desc_stat)
print(financial_donations_tex, 
      file = here(ao, 'financial_donations_desc_stat.tex'))


financial_donations_tsplot <- gleaners_financial_donations %>% 
  mutate(year = year(date)) %>% 
  filter(year >= 2013) %>% 
  filter(donation_value <= 1000000) %>% 
  mutate(MonthYear = floor_date(date, unit = "month")) %>% 
  group_by(MonthYear) %>% 
  summarize( `Average` = mean(donation_value, na.rm = TRUE), 
             `Std.Dev` = sd(donation_value, na.rm = TRUE),
             `Min` = min(donation_value, na.rm = TRUE),
             `P05` = quantile(donation_value, probs = 0.05, na.rm = TRUE),
             `P25` = quantile(donation_value, probs = 0.25, na.rm = TRUE),
             `Median` = quantile(donation_value, probs = 0.50, na.rm = TRUE), 
             `P75` = quantile(donation_value, probs = 0.75, na.rm = TRUE)) %>% 
  #mutate(meanlow = `Average` - `Std.Dev`, meanupp = `Average` + `Std.Dev`) %>% 
  mutate(logvalue = log( `Average`)) %>% 
  ggplot(mapping = aes(y = logvalue, x = MonthYear)) + 
  geom_line() +  
  #geom_ribbon(mapping = aes(ymin = meanlow, ymax = meanupp), alpha = 0.2, color = NA) +
  #geom_vline(xintercept = treat_date, linetype = "dashed", color = "gray50") + 
  #scale_x_date(breaks = "1 month", labels = date_format("%b-%Y")) + 
  scale_y_continuous(n.breaks = 10) + 
  scale_x_date(breaks = "1 year") + 
  labs(x = "", y = "Log Dollars", 
       title = "Financial Donations Distribution Over Time", subtitle = "Mean +/- Std.Dev. by Month")

financial_donations_tsplot <- format_plot(financial_donations_tsplot) + theme(axis.text.x = element_text(angle = 90))  

boxplot_financial_year <- gleaners_financial_donations %>% 
  mutate(Year = factor(year(date))) %>% 
  group_by(Year) %>% 
  #mutate(order_category = mean(PalletTotalWeight, na.rm=TRUE)) %>% 
  ggplot(mapping = aes(y = donation_value, x = Year)) + 
  geom_boxplot(outlier.shape = NA, alpha = 0.8, color ="black", fill = "steelblue2") + 
  coord_cartesian(ylim = c(0,300)) + 
  scale_y_continuous(n.breaks = 10) +
  labs(y = "Dollars", y = "", title = "Distribution of Financial Donations over Time")

boxplot_financial_year <- format_plot(boxplot_financial_year) 

boxplot_financial_fips <- gleaners_financial_donations %>% 
  filter(is.na(fips) == FALSE) %>% 
  left_join(
    tigris::fips_codes %>% filter(state == "IN") %>% 
      mutate(fips = as.double(paste(state_code,county_code, sep = ''))) %>% 
      select(fips, county), 
    by = "fips", relationship = "many-to-one") %>% 
  mutate(county_name = gsub("(?i)\\s*county", "", county, perl = TRUE)) %>% 
  group_by(county, county_name) %>% 
  summarize(`Observations` = n(),
            `Average` = mean(donation_value, na.rm = TRUE), 
            `Std.Dev` = sd(donation_value, na.rm = TRUE)) %>% 
  #mutate(order_category = mean(PalletTotalWeight, na.rm=TRUE)) %>% 
  ggplot(mapping = aes(y = `Average`, x = reorder(county_name, `Average`))) + 
  geom_col(stat ="identity", alpha = 0.8, color ="black", fill = "firebrick4") + 
  geom_boxplot(outlier.shape = NA, ) + 
  scale_y_continuous(n.breaks = 10) +
  labs(x = "Dollars", y = "", title = "Average Donation by County") 
  
boxplot_financial_fips <- format_plot(boxplot_financial_fips) + 
    theme(axis.text.x = element_text(angle = 90, size = 9))

donations_financial_combined <- plot_grid(
  boxplot_financial_year, boxplot_financial_fips, 
  nrow = 2
)

save_graph(donations_financial_combined, name = "distribution_donations_financial.png")
#-------------------------------------------------------------------------------  
#GFBI_IU_Data_updated <- haven::read_dta(here(path,'Raw Data',"GFBI_IU_Data_updated.dta"))
#GleanersFoodRaw <-      haven::read_dta(here(path,'Raw Data',"GleanersFoodRaw.dta"))
## data characteristics 
#product_type <-         haven::read_dta(here(bt, "ProductTypeCode.dta"), )
#city_code <-            haven::read_dta(here(bt, "CityCode.dta"))
#donor_class <-          haven::read_dta(here(bt, "DonorClass.dta"))
#product_category <-     haven::read_dta(here(bt, "ProductCategory.dta"))
#product_source <-       haven::read_dta(here(bt, "ProductSource.dta"))
#fbc_product_category <- haven::read_dta(here(bt, "FBCProductCategory.dta"))
#unc_product_category <- haven::read_dta(here(bt, "UNCProductCategory.dta"))
#
#
#product_type <- haven::read_dta(here(bt, "ProductTypeCode.dta")) %>% 
#  mutate(ProductDes = haven::as_factor(ProductDes))
#str(product_type)
## Append data2 to data1
#data_gleaners <- bind_rows(GFBI_IU_Data_updated, GleanersFoodRaw) %>%
#  # Replace State with "In" if County is "IN"
#  mutate(State = ifelse(County == "IN", "In", State)) %>% 
#  left_join(product_type, by = "ProductType") %>%
#  left_join(city_code, by = c("City", "State")) %>%
#  left_join(donor_class, by = "DonorClassofTrade") %>%
#  left_join(product_category, by = "ProductCategoryCode") %>%
#  left_join(product_source, by = "ProductSourceCode") %>%
#  left_join(fbc_product_category, by = "FBCProductCategory") %>%
#  left_join(unc_product_category, by = "UNCProductCategory") %>% 
#  # Drop unnecessary variables
#  select(-c(ProductType, County, DonorClassofTrade, ProductCategoryCode, 
#            ProductSourceCode, UNCProductCategory, UNCProductCategoryCode, UNCDonorClassofTradeCode)) %>% 
#  # Encode variables
#  mutate(Storage = as.factor(StorageZone),
#         ItemCategory = as.factor(ItemCategoryCode)) %>%
#  select(-c(StorageZone, ItemCategoryCode)) %>%
#  rename(County = county_name, Donor = DonorDes) %>% 
## Create date variables
#  mutate(Month = month(DateReceived),
#         Year = year(DateReceived),
#         Quarter = quarter(DateReceived),
#         Qdate = make_date(Year, Quarter * 3, 1),
#         Week = week(DateReceived),
#         MonthlyDate = floor_date(DateReceived, "month")) %>% 
#  # Rename and label variables (labels in comments)
#  mutate(County = "County",
#         ProductDes = "Product Description",
#         Donor = "Donor",
#         ProductCategory = "Product Category",
#         ProductSource = "Product Source",
#         FBCProdCat = "FBC Product Category",
#         UNCProdCat = "UNC Product Category",
#         Month = "Month",
#         Year = "Year",
#         Quarter = "Quarter No",
#         Qdate = "Quarter Date",
#         Week = "Week No",
#         MonthlyDate = "Monthly Date")
#
## Correct State typos
#state_corrections <- c("Ak" = "AK", "Ak." = "AK", "Ar." = "AR", "Arkansas" = "AR",
#                       "Az" = "AZ", "Az." = "AZ", "Ca" = "CA", "Ca." = "CA",
#                       "California" = "CA", "Co" = "CO", "De." = "DE", "Fl" = "FL",
#                       "Fl." = "FL", "Florida" = "FL", "Ga" = "GA", "Ga." = "GA",
#                       "Ia" = "IA", "Iowa" = "IA", "Id" = "ID", "Il" = "IL",
#                       "Il." = "IL", "Illiinois" = "IL", "Illinois" = "IL",
#                       "IN " = "IN", "In" = "IN", "In." = "IN", "Indiana" = "IN",
#                       "Indiana " = "IN", "KY." = "KY", "Kentucky" = "KY", "Kansas" = "KS",
#                       "La" = "LA", "MD." = "MD", "Massachusetts" = "MA", "Mi" = "MI",
#                       "Mi." = "MI", "NC." = "NC", "NE." = "NE", "Ne." = "NE",
#                       "NJ." = "NJ", "Oh." = "OH", "PA." = "PA", "Texas" = "TX",
#                       "Tx" = "TX", "Tn" = "TN", "Va." = "VA", "Wa" = "WA",
#                       "Wi" = "WI", "al" = "AL", "ia" = "IA", "il" = "IL",
#                       "ky" = "KY", "mi" = "MI", "wi" = "WI")
#
#data_gleaners <- data_gleaners %>%
#  mutate(State = recode(State, !!!state_corrections))
#
## Correct County and City names
#data_gleaners <- data_gleaners %>%
#  mutate(County = ifelse(City == "Indianapolis", "Marion", County),
#         City = recode(City,
#                       "Battle Creek " = "Battle Creek",
#                       "Bellefountaine" = "Bellefontaine",
#                       "Broomfield " = "Broomfield",
#                       "Cincinatti" = "Cincinnati",
#                       "Cinciinnati" = "Cincinnati",
#                       "Dade City " = "Dade City",
#                       "Dallas " = "Dallas",
#                       "Dowers Grove " = "Downers Grove",
#                       "Edinburg" = ifelse(State == "IN", "Edinburgh", City),
#                       "Fontana." = "Fontana",
#                       "Jefferson City " = "Jefferson City",
#                       "Liberal " = "Liberal",
#                       "Little Chute " = "Little Chute",
#                       "McCook " = "McCook",
#                       "Minnneapolis" = "Minneapolis",
#                       "Monte Vista " = "Monte Vista",
#                       "North Hollywood." = "North Hollywood",
#                       "Santa Barabara" = "Santa Barbara",
#                       "Seattle " = "Seattle",
#                       "Shepperdsville" = "Shepherdsville",
#                       "Springfield " = "Springfield",
#                       "St.louis" = "St. Louis",
#                       "Zionsville " = "Zionsville",
#                       "indianapolis" = "Indianapolis",
#                       "depere" = "Depere",
#                       "le mars" = "LeMars",
#                       "marshfield" = "Marshfield",
#                       "woodridge" = "Wooldridge"))
#
## Keep only observations from Indiana
#data_gleaners <- data_gleaners %>%
#  filter(State == "IN") %>% 
## Rename identifiers
#  rename(id = DonorRandomID, date = DateReceived) %>% 
## Drop outliers
#  filter(PalletTotalWeight <= 1000000)  %>% 
## Sort and arrange data
#  select(id, Donor, date, State, County, City, ProductDonated, ProductDes, QuantityPerPallet,
#         WeightPerCase, PalletTotalWeight, ValuePerUnitofMeasure, TotalPalletValue,
#         ProductCategory, ProductSource, ItemCategory, PalletNo, UnitofMeasure,
#         Storage, Month, Year, Quarter, Qdate, Week) %>%
#  arrange(State, County, City, id, date)
#
#
#
#data_gleaners %>% glimpse()
#
#
## Save the cleaned data
#write_dta(data, file.path(bt, "GleanersCleanDonations.dta"))
#
## Data for bar graphs describing composition of donations
#summary_data <- data %>%
#  group_by(id, fips, date, UNCProdCat, ProductSource, Donor, Storage) %>%
#  summarise(PalletTotalWeight = sum(PalletTotalWeight, na.rm = TRUE))
#
## Save the summarized data
#write_dta(summary_data, file.path(bt, "GleanersFoodFull.dta"))
#
## Save the cleaned data again
#write_dta(data, file.path(bt, "GleanersCleanDonations.dta"))
##--------------------------------------------------------------------------------#