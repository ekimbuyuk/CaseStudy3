#Ekim's File

#Clean Death Data
new_data = merged_data %>%
  filter(year == 2015) %>%
  group_by(cores,race,hisp) %>%
  count()

#Clean Birth Data
Yr1116Birth = read_csv("Yr1116Birth.csv")
birth_data = Yr1116Birth %>%
  filter(YOB == 2015)

birth_data$MRACER[birth_data$MRACER == 0] = 4
birth_data$MRACER[birth_data$MRACER == 1] = 1
birth_data$MRACER[birth_data$MRACER == 2] = 2
birth_data$MRACER[birth_data$MRACER == 3] = 3
birth_data$MRACER[birth_data$MRACER == 4] = 4
birth_data$MRACER[birth_data$MRACER == 5] = 4
birth_data$MRACER[birth_data$MRACER == 6] = 4
birth_data$MRACER[birth_data$MRACER == 7] = 4
birth_data$MRACER[birth_data$MRACER == 8] = 4

birth_data = birth_data %>%
  group_by(CORES, MRACER, MHISP) %>%
  count()

birth_data$MRACER[birth_data$MRACER == 0] = "Other"
birth_data$MRACER[birth_data$MRACER == 1] = "White"
birth_data$MRACER[birth_data$MRACER == 2] = "Black or African American"
birth_data$MRACER[birth_data$MRACER == 3] = "American Indian or Alaska Native"
birth_data$MRACER[birth_data$MRACER == 4] = "Other"
birth_data$MRACER[birth_data$MRACER == 5] = "Other"
birth_data$MRACER[birth_data$MRACER == 6] = "Other"
birth_data$MRACER[birth_data$MRACER == 7] = "Other"
birth_data$MRACER[birth_data$MRACER == 8] = "Other"

birth_data$MHISP[birth_data$MHISP == "C"] = "Cuban"
birth_data$MHISP[birth_data$MHISP == "M"] = "Mexican"
birth_data$MHISP[birth_data$MHISP == "N"] = "Non-Hispanic"
birth_data$MHISP[birth_data$MHISP == "O"] = "Other Hispanic"
birth_data$MHISP[birth_data$MHISP == "P"] = "Puerto Rican"
birth_data$MHISP[birth_data$MHISP == "S"] = "Central/South American"
birth_data$MHISP[birth_data$MHISP == "U"] = "Unknown"

colnames(new_data)[colnames(new_data) == "n"] <- "Total_Deaths"

colnames(birth_data)[colnames(birth_data) == "CORES"] <- "cores"
colnames(birth_data)[colnames(birth_data) == "MRACER"] <- "race"
colnames(birth_data)[colnames(birth_data) == "MHISP"] <- "hisp"

birth_deaths <- left_join(birth_data, new_data, by = c("CORES" = "cores", "MRACER" = "race", "MHISP" = "hisp"))

birth_deaths <- merge(birth_data, new_data, by = c("cores", "race","hisp"), all.x=TRUE)

birth_deaths <- left_join(birth_data, new_data, by = c("cores", "race","hisp"))

birth_deaths$Total_Deaths[is.na(birth_deaths$Total_Deaths)] = 0
birth_deaths$rate = birth_deaths$Total_Deaths/birth_deaths$n 

new_data$log_totals = log(new_data$n)
merged_data$count = 1
M1<-lmer(y~x+(1|county))

model1 = lm(new_data$count ~ new_data$race + new_data$hisp + factor(new_data$COUNTY) -1)

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

