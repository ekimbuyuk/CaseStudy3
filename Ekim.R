#Ekim's File

new_data = merged_data %>%
  group_by(COUNTY,race,hisp) %>%
  count()

new_data$log_totals = log(new_data$n)

#Complete Pooling On County
lm.pooled <- lm(count ~ race + hisp)

#No pooling regression
lm.unpooled <- lm(count ~ race + hisp + factor(COUNTY) -1)

#Hierarchical Model
hier_county = lmer(log(new_data$n) ~ new_data$race + new_data$hisp + (1|new_data$COUNTY))
summary(hier_county)
coef(hier_county)
fitted(hier_county)

hier_county_log = lmer(log(new_data$n) ~ new_data$race + new_data$hisp + (1|new_data$COUNTY))
fitted(hier_county_log)

hier_hisp = lmer(new_data$n ~ new_data$race + new_data$COUNTY + (1|new_data$hisp))
summary(hier_hisp)
coef(hier_hisp)

hier_race = lmer(new_data$n ~ new_data$hisp + new_data$COUNTY + (1|new_data$race))
summary(hier_race)
coef(hier_race)

#Let Race Vary By County
hier_county_race <-lmer(new_data$n ~ new_data$race + new_data$hisp + (1+new_data$race|new_data$COUNTY))
summary(hier_county_race)
coef(hier_county_race)

