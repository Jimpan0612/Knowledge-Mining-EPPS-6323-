rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")  

library(dplyr)
library(ggplot2)
library(broom)


str(df15)


df15$affinity <- as.numeric(df15$affinity)

# Fit a linear regression model
model15 <- lm(affinity ~ gdp_growth_annual + gdp_cap_growth + health_exp_cap + health_exp_gdp + gdp_cap_ppp + gdp_ppp + edu_exp + nat_resc_rent + women_seats + women_bus_law_score + life_exp + mort + rd_exp + ict_good_exp + ict_ser_exp + trade_balance + total_score + pol_rights + civil_lib -1 
            , data = df15)

summary(model15)

coef_data <- tidy(model15, conf.int = TRUE)

#plot
ggplot(coef_data, aes(x = term, y = estimate, color = term)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  coord_flip() +
  labs(x = "Variables", y = "Coefficient") +
  ggtitle("Coefficient plot with confidence intervals 2015") +
  scale_color_discrete(name = "Variables")

                                 
# 2015~2019

#change factor to num
df15$affinity <- as.numeric(df15$affinity)
df16$affinity <- as.numeric(df16$affinity)
df17$affinity <- as.numeric(df17$affinity)
df18$affinity <- as.numeric(df18$affinity)
df19$affinity <- as.numeric(df19$affinity)

# combine df15 and df19 datasets
df_combined <- bind_rows(df15, df16, df17, df18, df19)


modelall <- lm(affinity ~ gdp_growth_annual + gdp_cap_growth + health_exp_cap + health_exp_gdp + gdp_cap_ppp + gdp_ppp + edu_exp + nat_resc_rent + women_seats + women_bus_law_score + life_exp + mort + rd_exp + ict_good_exp + ict_ser_exp + trade_balance + total_score + pol_rights + civil_lib -1, data = df_combined)

summary(modelall)

# extract coefficient data with confidence intervals
coef_data <- tidy(modelall, conf.int = TRUE)

# add a column to identify the dataset
coef_data <- coef_data %>% mutate(dataset = ifelse(row.names(coef_data) %in% colnames(df15)[-1], "df15", "df19"))

# plot
ggplot(coef_data, aes(x = term, y = estimate, color = term)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  coord_flip() +
  labs(x = "Variables", y = "Coefficient", color = "Dataset") +
  ggtitle("Coefficient plot with confidence intervals 2015~2019")


                                                                                                              