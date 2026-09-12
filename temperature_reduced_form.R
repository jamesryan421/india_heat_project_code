# Urban Heat in India
# Fixed-Effects Regression for Heat-Index Days across Years

# Set working directory
setwd("C:\\Users\\jdr42\\Documents\\Projects\\india_heat_project\\temperature_data\\output")

vis_directory="C:\\Users\\jdr42\\Documents\\Projects\\india_heat_project\\temperature_data\\vis"

# Load Packages
library(tidyverse)
library(summarytools)
library(plm)
library(stargazer)

# Import data
df=read_csv("condensed_heat_indices.csv")
head(df)

# Sum number of days observed in a given risk category across an entire year
sid_annual=df %>% group_by(SID,Year) %>% 
  summarize(hrisk_EDy=sum(hrisk_ED),
            hrisk_Dy=sum(hrisk_D),
            hrisk_ECy=sum(hrisk_EC),
            hrisk_Cy=sum(hrisk_C),
            ndays_y=sum(ndays)) %>%
  ungroup()
sid_annual
# Subset to years with at least 300 daily observations
sid_annual_filtered=sid_annual[sid_annual$ndays_y>=300,]

# Number of stations in each year
sid_annual_filtered %>% group_by(Year) %>% count()

# Summary stats of output variables
sid_annual_filtered %>% 
  select(hrisk_Cy,hrisk_ECy,hrisk_Dy,hrisk_EDy) %>%
  descr()

sid_annual %>% freq(ndays_y)

# Scatter plots of heat index days over each year
g1=ggplot(data=sid_annual_filtered)+
  geom_point(mapping=aes(x=as.factor(Year),y=hrisk_Cy,color="Caution"),alpha=0.6,position="jitter")+
  geom_point(mapping=aes(x=as.factor(Year),y=hrisk_ECy,color="Extreme Caution"),alpha=0.6,position="jitter")+
  geom_point(mapping=aes(x=as.factor(Year),y=hrisk_Dy,color="Danger"),alpha=0.6,position="jitter")+
  geom_point(mapping=aes(x=as.factor(Year),y=hrisk_EDy,color="Extreme Danger"),alpha=0.6,position="jitter")+
  labs(
    title="Observed Days in HI Category",
    x="Year",
    y="Observations",
    caption="Source: NCEI Data"
  )+
  scale_color_manual(name="Risk Category",
                     values=c("Caution"="yellow","Extreme Caution"="orange",
                              "Danger"="red","Extreme Danger"="darkred"))
g1

# First regression: outcome variables on year with station-level fixed effects

model1_C=plm(hrisk_Cy ~ Year,data=sid_annual_filtered,
           index=c("SID"),model="within")
model1_EC=plm(hrisk_ECy ~ Year,data=sid_annual_filtered,
              index=c("SID"),model="within")
model1_D=plm(hrisk_Dy ~ Year,data=sid_annual_filtered,
             index=c("SID"),model="within")
model1_ED=plm(hrisk_EDy ~ Year,data=sid_annual_filtered,
              index=c("SID"),model="within")

summary(model1_C)
summary(model1_EC)
summary(model1_D)
summary(model1_ED)



# Stargazer output
stargazer(model1_C,model1_EC,model1_D,model1_ED,
          dep.var.labels=c("Caution","Extreme Caution","Danger","Extreme Danger"),
          dep.var.caption="Days with HI Category Observation",
          type="latex",
          add.lines=list(c("Fixed Effects","Station","Station","Station","Station")))


# Takeaways
# There is a statistically significant year-over-year trend in the number of days
# in a given category at the station level.
# However, R^2 is pretty low for all models and effects are quite small for ED category.
# Trend is steepest for EC category - maybe because baseline temp is already pretty hot?
# Scatter plot shows similar trend, and also many more stations begin reporting in 2016


# Normalize by number of observations
# Again, use filtered data for stations with more than 300 days observed for a year
sid_norm=tibble(
  SID=sid_annual_filtered$SID,
  Year=sid_annual_filtered$Year,
  hrisk_EDp=sid_annual_filtered$hrisk_EDy/sid_annual_filtered$ndays_y,
  hrisk_Dp=sid_annual_filtered$hrisk_Dy/sid_annual_filtered$ndays_y,
  hrisk_ECp=sid_annual_filtered$hrisk_ECy/sid_annual_filtered$ndays_y,
  hrisk_Cp=sid_annual_filtered$hrisk_Cy/sid_annual_filtered$ndays_y
)

sid_norm %>%
  select(hrisk_EDp,hrisk_Dp,hrisk_ECp,hrisk_Cp) %>%
  descr()
# Second regression: normalized by number of observed days


# First regression: outcome variables on year with station-level fixed effects
model2_C=plm(hrisk_Cp ~ Year,data=sid_norm,
             index=c("SID"),model="within")
model2_EC=plm(hrisk_ECp ~ Year,data=sid_norm,
              index=c("SID"),model="within")
model2_D=plm(hrisk_Dp ~ Year,data=sid_norm,
             index=c("SID"),model="within")
model2_ED=plm(hrisk_EDp ~ Year,data=sid_norm,
              index=c("SID"),model="within")

summary(model2_C)
summary(model2_EC)
summary(model2_D)
summary(model2_ED)

# Stargazer output
stargazer(model2_C,model2_EC,model2_D,model2_ED,
          dep.var.labels=c("Caution","Extreme Caution","Danger","Extreme Danger"),
          dep.var.caption="Pct. Days with HI Category Observation",
          type="text")

# Third model: only analyzing summers (March-May)
summer_months=c(3,4,5)
sid_summer=df[df$Month %in% summer_months,] %>% group_by(SID,Year) %>% 
  summarize(hrisk_EDs=sum(hrisk_ED),
            hrisk_Ds=sum(hrisk_D),
            hrisk_ECs=sum(hrisk_EC),
            hrisk_Cs=sum(hrisk_C),
            ndays_s=sum(ndays)) %>%
  ungroup()
sid_summer

# Subset to station-year pairs with at least 81 observations
sid_summer_filtered=sid_summer[sid_summer$ndays_s>=81,]

# Scatter plots of heat index days over each summer
g2=ggplot(data=sid_summer_filtered)+
  geom_point(mapping=aes(x=as.factor(Year),y=hrisk_Cs,color="Caution"),alpha=0.6,position="jitter")+
  geom_point(mapping=aes(x=as.factor(Year),y=hrisk_ECs,color="Extreme Caution"),alpha=0.6,position="jitter")+
  geom_point(mapping=aes(x=as.factor(Year),y=hrisk_Ds,color="Danger"),alpha=0.6,position="jitter")+
  geom_point(mapping=aes(x=as.factor(Year),y=hrisk_EDs,color="Extreme Danger"),alpha=0.6,position="jitter")+
  labs(
    title="Observed Days in HI Category, Summer Months",
    x="Year",
    y="Observations",
    caption="Source: NCEI Data"
  )+
  scale_color_manual(name="Risk Category",
                     values=c("Caution"="yellow","Extreme Caution"="orange",
                              "Danger"="red","Extreme Danger"="darkred"))
g2
ggsave(file.path(vis_directory,"hi_cat_scatter_summer.png"),plot=g2,
       width=583,height=382,units="px")

# Regressions using summer data

model3_C=plm(hrisk_Cs ~ Year,data=sid_summer_filtered,
             index=c("SID"),model="within")
model3_EC=plm(hrisk_ECs ~ Year,data=sid_summer_filtered,
              index=c("SID"),model="within")
model3_D=plm(hrisk_Ds ~ Year,data=sid_summer_filtered,
             index=c("SID"),model="within")
model3_ED=plm(hrisk_EDs ~ Year,data=sid_summer_filtered,
              index=c("SID"),model="within")

summary(model3_C)
summary(model3_EC)
summary(model3_D)
summary(model3_ED)

# Stargazer output
stargazer(model3_C,model3_EC,model3_D,model3_ED,
          dep.var.labels=c("Caution","Extreme Caution","Danger","Extreme Danger"),
          dep.var.caption="Summer Days with HI Category Observation",
          type="latex",
          add.lines=list(c("Fixed Effects","Station","Station","Station","Station")))

# DEBUG - lines of best fit don't make sense
summary(lm(hrisk_Cs ~ as.factor(Year),data=sid_summer_filtered))
summary(lm(hrisk_ECs ~ Year,data=sid_summer_filtered))
summary(lm(hrisk_Ds ~ Year,data=sid_summer_filtered))
summary(lm(hrisk_EDs ~ Year,data=sid_summer_filtered))

# Stargazer output
stargazer(model3_C,model3_EC,model3_D,model3_ED,
          dep.var.labels=c("Caution","Extreme Caution","Danger","Extreme Danger"),
          dep.var.caption="Summer Days with HI Category Observation",
          type="text",
          add.lines=list(c("Fixed Effects","Station","Station","Station","Station")))

g2+geom_abline(slope=model3_C$coefficients,color="yellow",
               intercept=mean(sid_summer_filtered$hrisk_Cs),size=1.25)+
  geom_abline(slope=model3_EC$coefficients,color="orange",
              intercept=mean(sid_summer_filtered$hrisk_ECs),size=1.25)+
  geom_abline(slope=model3_D$coefficients,color="red",
            intercept=mean(sid_summer_filtered$hrisk_Ds),size=1.25)+
  geom_abline(slope=model3_ED$coefficients,color="darkred",
            intercept=mean(sid_summer_filtered$hrisk_EDs),size=1.25)
