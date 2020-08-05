
library(tidyverse)
library(ggsci)
library(lubridate)
library(ggthemes)

#import data
june_sex_age <- read_csv("https://www.stats.govt.nz/assets/Uploads/Employment-indicators/Employment-indicators-June-2020/Download-data/employment-indicators-june-2020-filled-jobs-by-sex-and-age.csv") %>%
  mutate(date=ymd(paste(Period,".01")))

june_region_age <- read_csv("https://www.stats.govt.nz/assets/Uploads/Employment-indicators/Employment-indicators-June-2020/Download-data/employment-indicators-june-2020-filled-jobs-by-age-and-region.csv")%>%
  mutate(date=ymd(paste(Period,".01")))

#set levels for age groups
age_levels <-unique(june_sex_age$Age)
age_grp_levels <-unique(june_region_age$Age_group)[1:11]
age_grp_levels <-c("Under 15", age_grp_levels)
june_region_age$Age_group <- factor(june_region_age$Age_group, levels=age_grp_levels)

#chart 1 - filled jobs by sex and age
ggplot(june_sex_age, aes(date, Value, col=Sex, group=Sex)) + geom_line() + 
  facet_wrap(~factor(Age, levels=age_levels), scales="free_y") + theme_few() +
  scale_color_nejm() +
  theme(legend.position="none",
        text=element_text(family="Avenir"),
        plot.title=element_text(size=30)) +
  labs(x="", y="Filled jobs", title="Filled jobs by sex and age, NZ, 2019-2020") +
  theme(text=element_text(family="Avenir"))

#chart 2 - filled jobs by age group filled with trend
ggplot(june_region_age %>%
         filter(Region == "Total NZ"), aes(date, Value)) + 
  geom_line() + geom_area(fill="#62c799") +
  facet_grid(~Age_group, scales="free_y") + theme_few() +
  theme(legend.position="none",
        text=element_text(family="Avenir"),
        plot.title=element_text(size=30),
        axis.text.x=element_blank(),
        axis.ticks = element_blank()) +
  scale_y_continuous(breaks=c(0,100000,200000), 
                     labels=c("0","100,000", "200,000")) +
  labs(y="Filled jobs", x="", 
       title="Employment levels holding strong in NZ",
       subtitle="Filled jobs by age, NZ, Jan quarter 2019 - Jun quarter 2020")
ggsave("filled_age.png", dpi="retina")

#calculate year on year change
june_comparison <- june_sex_age %>% select(-Period) %>% 
  filter(date =="2019-06-01"|date=="2020-06-01") %>% 
  pivot_wider(values_from=Value, names_from=date) %>% 
  mutate(change=`2020-06-01`/`2019-06-01`-1)

#chart 3 - plot year on year change
ggplot(june_comparison, aes(factor(Age, levels=age_levels), change, size=`2020-06-01`, col=Sex)) + 
  geom_point(alpha=0.5) + geom_segment(aes(y=0, yend=change, xend=factor(Age, levels=age_levels)), col="black", size=2, alpha=0.5) +
  theme_few() + geom_hline(yintercept = 0, alpha=0.5) +
  scale_y_continuous(labels=scales::percent) + facet_wrap(~Sex) +
  scale_size(range = c(2,10)) + scale_color_lancet() +
  theme(legend.position="none",
        text=element_text(family="Avenir"),
        plot.title=element_text(size=30)) +
  labs(title="Falling employment levels for the young", 
       subtitle="Change in filled jobs by age group, New Zealand, June quarter 2019 to June quarter 2020",
       x="Age group", y="Change (year on year)", caption="Source: Stats NZ")
ggsave("filled_young.png", dpi="retina")
