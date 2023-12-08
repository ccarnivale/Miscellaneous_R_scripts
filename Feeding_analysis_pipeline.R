#Code for calculating and plotting feeding experiments in Sanders labs
#should work regardless of the times or number of replicates
#would only need to change the groupby() function for additions
#data is stored in "/Data_from_sarah"

#packages necessary to load data for downstream analysis--------

library(magrittr)
library(tidyverse)
library(stringr)
library(knitr)

#Import files----------
#files need to be saved in CSV format
ing <- read.csv(file.choose())

ing_5 <- rename(ing, "Light" = treatment_light, "Nutrient Percentage" = treatment_percent)
#data tidying-------

ing_1 <- ing %>% 
  mutate(bac_conc_perML = bac_avg/bac_micro_filtered*dil_factor*cor_factor_grid,
         micro_conc_perML = micro_avg/bac_micro_filtered*dil_factor*cor_factor_grid,
         BM_ratio = bac_conc_perML/micro_conc_perML,
         BSP_ingested = (conc_bkgrd_per*BM_ratio) + conc_bkgrd_per,
         ing_percell_perhour = BSP_ingested*2) #based on 30 mins feeding exp

ing_2 <- ing_1 %>%
  group_by(treatment_light, treatment_percent) %>% 
  summarise(avg_ing = mean(ing_percell_perhour), 
            STD_ing = sd(ing_percell_perhour),
            STDerr_ing = STD_ing/sqrt(3)) #getting standard error for box plots

#Visualizing data-----------
mutate(ing_1, treatment_percent = paste0(treatment_percent, "%")) %>% 
ggplot(ing_1, mapping = aes(treatment_light, ing_percell_perhour))+
  geom_boxplot()+
  facet_wrap("treatment_percent")+ 
  theme(axis.title = element_text(size = 15))+ 
  xlab("Light Level")+
  ylab("BSP Ingestions/cell*hr")

ggplot(ing_2, aes(treatment_light, avg_ing))+
  geom_point()+
  geom_errorbar(aes(ymin = avg_ing - STDerr_ing, ymax = avg_ing + STDerr_ing))+
  facet_wrap("treatment_percent")


str(ing_2)


#Statistical Analysis Pedinella only---------
ped_aov2way <- aov(ing_percell_perhour ~ treatment_light*treatment_percent,ing_1)
summary(ped_aov2way)

ped_aov <- aov(ing_percell_perhour ~ treatment_light,ing_1)
summary(ped_aov)

ped_aov_nut <- aov(ing_percell_perhour ~ treatment_percent, ing_1)
summary(ped_aov_nut)

#Dinobryon Analysis-----------

dino_ped <- read.csv("Arctic_counts_4_degrees_wDinobryon.csv")

dino_ped_ing <- dino_ped %>% 
  mutate(bac_conc_perML = bac_avg/bac_micro_filtered*dil_factor*cor_factor_grid,
         micro_conc_perML = micro_avg/bac_micro_filtered*dil_factor*cor_factor_grid,
         BM_ratio = bac_conc_perML/micro_conc_perML,
         BSP_ingested = (conc_bkgrd_per*BM_ratio) + conc_bkgrd_per,
         ing_percell_perhour = BSP_ingested*2)

dino_ped_ing_1 <- dino_ped_ing %>% 
  group_by(species, treatment_percent, treatment_light) %>% 
  summarise(avg_ing = mean(ing_percell_perhour),
            STD_ing = sd(ing_percell_perhour),
            STDerr_ing = STD_ing/sqrt(3))

ggplot(dino_ped_ing, aes(treatment_light, ing_percell_perhour))+
  geom_boxplot()+
  facet_grid(treatment_percent~species)

dino_only_plot <- dino_ped_ing %>% filter(strain == 2290) %>% 
  ggplot(aes(treatment_light,ing_percell_perhour))+
  geom_boxplot() + facet_wrap("treatment_percent")
dino_only_plot

dino_only_ing <- dino_ped_ing %>% filter(strain == 2290)

#Statistical analysis Dinobryon only ---------
dino_aov2way <- aov(ing_percell_perhour ~ treatment_light*treatment_percent,dino_only_ing)
dino_2way_summary <- summary(dino_aov2way)

dino_2way_summary

dino_aov_light <- aov(ing_percell_perhour ~ treatment_light,dino_only_ing)
summary(dino_aov_light)


dino_aov_nutr <- aov(ing_percell_perhour ~ treatment_percent, dino_only_ing)
summary(dino_aov_nutr)
#Making anova table------------------
dino_2way_sum <- dino_2way_sum_list[[1]][[1]]

options(knitr.kable.NA = '')

kable(dino_2way_sum, digits = 3)

#Statistical analysis both------------
ped_dino_aov2way <- aov(ing_percell_perhour ~ treatment_light*treatment_percent, dino_ped_ing)
summary(ped_dino_aov2way)

ped_dino_aov_light <- aov(ing_percell_perhour ~ treatment_light, dino_ped_ing)
summary(ped_dino_aov_light)

ped_dino_aov_nutr <- aov(ing_percell_perhour ~ treatment_percent, dino_ped_ing)
summary(ped_dino_aov_nutr)

str(dino_2way_summary)
