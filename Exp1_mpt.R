rm(list=ls()) #clears the environment 
#install.packages("patchwork")
# Install the readxl package (if not already installed)
#install.packages("readxl")

# Load the readxl package
library(readxl)
library(openxlsx)

library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library("writexl")
library(RColorBrewer)
library(multcompView)
# install.packages("viridis")
# install.packages("extrafont")
# install.packages("multcomp")
library(viridis)
library(extrafont)
library(multcomp)



#size summary using dvalues 

size_summary<-read.csv(file = "data/size_summary.csv")

size_summary
size_dvalue <-
  size_summary%>% 
  gather(4:8, key="parameters", value = 'value') %>%
  #filter(!sample_name=="CF_sterile_5%3") %>% 
  mutate(size= case_when(size==2 ~ "2 mm",
                              size==1 ~ "1 mm",
                              size==0.5~ "0.5 mm")) %>% 
  
  mutate(substrate=factor(substrate),
         rep=factor(rep),
         size=factor(size),
         parameters=factor(parameters)) 

#calculating mean
size_d10_mean <-
  size_dvalue %>% 
  filter(parameters=="d10") %>% 
  group_by(substrate,size) %>%
  summarise(mean=mean(value),
            stdev=sd(value),
            max=max(value),
            min=min(value))



#size d50 spent grain

size_d50_sg <-size_dvalue %>% 
  filter(substrate=="sg") %>% 
  filter(parameters=="d50")

size_d50_sg
anova_d50_sg <- aov(value ~ size, data = size_d50_sg)


size_d90_sg <-size_dvalue %>% 
  filter(substrate=="sg") %>% 
  filter(parameters=="d90")



#doing anova for gc for all   

size_d10_gc <-size_dvalue %>% 
  filter(substrate=="gc") %>% 
  filter(parameters=="d10")

size_d10_gc_mean <-size_d10_mean %>% 
  filter(substrate=="gc")

size_d10_gc_mean
# 


size_d50_gc <-size_dvalue %>% 
  filter(substrate=="gc") %>% 
  filter(parameters=="d50")

size_d50_gc


size_d90_gc <-size_dvalue %>% 
  filter(substrate=="gc") %>% 
  filter(parameters=="d90")

size_d90_gc

#calculating mean for gc for d50
size_d50_mean <-
  size_dvalue %>% 
  filter(parameters=="d50") %>% 
  group_by(substrate,size) %>%
  summarise(mean=mean(value),
            stdev=sd(value),
            max=max(value),
            min=min(value))

size_d50_mean

#protein content of the larvae for mechanical pretreatment on milled spent grain
#and grass clippings

protein<-read.csv(file = "data/protein.csv")

#gathering the data and placing each treatment condition

protein_clean <-
  protein%>% 
  gather(4, key="parameters", value = 'value') %>%
  mutate(treatment= case_when(treatment=="ut"~ "Untreated",
                            treatment==2 ~ "2-mm",
                           treatment==1 ~ "1-mm",
                          treatment==0.5~ "0.5-mm")) %>%
  
  mutate(substrate=factor(substrate),
         rep=factor(rep),
         treatment=factor(treatment),
         parameters=factor(parameters)) 
#protein_clean
protein_clean$treatment<- factor(protein_clean$treatment, 
                                 levels = c("Untreated", "2-mm",
                                                             "1-mm", "0.5-mm"))
#filtering for grass cliipings. filtering 1-mm since same as 2-mm. written in manuscript
gc_protein <-protein_clean %>% 
  filter(substrate=="gc") %>% 
  filter(!treatment=="1-mm")
 
#calculating mean for grass clippings
gc_protein_mean <- gc_protein%>% 
  filter(substrate=="gc") %>% 
  group_by(substrate,treatment) %>%
  summarise(mean=mean(value),
            stdev=sd(value),
            max=max(value),
            min=min(value))

#plotting figure of protein conversion on grass clippings 

ggplot(gc_protein_mean, aes(x=treatment, y=mean, fill=treatment)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=mean-stdev, ymax=mean+stdev), position=position_dodge(width=0.9), width=0.2) +
  labs(x="Treatment", y="Protein Conversion [% DM]", fill="Treatment") +
  
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
 panel.grid.minor = element_blank() )+
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 2)) +
  scale_fill_grey() +
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    legend.position = "none") 
  

ggsave(filename ="output/protein/gc_larval_protein_conversion_v2.jpeg", units = "cm", width = 10, 
       height = 10, dpi = 1200)

#spent grain protein
sg_protein <-protein_clean %>% 
  filter(substrate=="sg") 

sg_protein_mean <- sg_protein%>% 
  filter(substrate=="sg") %>% 
  group_by(substrate,treatment) %>%
  summarise(mean=mean(value),
            stdev=sd(value),
            max=max(value),
            min=min(value))
sg_protein_mean
#plotting figure of protein conversion on spent grain 

ggplot(sg_protein_mean, aes(x=treatment, y=mean, fill=treatment)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=mean-stdev, ymax=mean+stdev), position=position_dodge(width=0.9), width=0.2) +
  labs(x="Treatment", y="Protein Conversion [% DM]", fill="Treatment") +
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank() )+
  scale_fill_grey() +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 2)) +
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    legend.position = "none") 


ggsave(filename ="output/protein/sg_larval_protein_conversion_v2.jpeg", units = "cm", width = 10, 
       height = 10, dpi = 1200)




#loading file with all data
alldata_mpt<- read_excel("data/larval feeding_experiments_laura.xlsx", sheet = "Raw Data M-PT")
alldata_mpt<- alldata_mpt <- na.omit(alldata_mpt)

str(alldata_mpt)
alldata_mpt

#cleaned up data and changing name in column of treatment
mpt_clean <-
  alldata_mpt %>% 
  gather(5:10, key="parameters", value = 'value') %>%
  #filter(!sample_name=="CF_sterile_5%3") %>% 
  mutate(treatment= case_when(Condition==0~"Untreated",
                              Condition==2 ~ "2-mm",
                              Condition==1 ~ "1-mm",
                              Condition==0.5~ "0.5-mm")) %>% 

   mutate(Substrate=factor(Substrate),
          Pretreatment=factor(Pretreatment),
         rep=factor(Rep),
         treatment=factor(treatment),
         parameters=factor(parameters)) 
 
mpt_clean
str(mpt_clean)
 
#setting order for treatment conditions
mpt_clean$treatment<- factor(mpt_clean$treatment, levels = c("Untreated", "2-mm",
                                                                         "1-mm", "0.5-mm"))
#filtering for spent grain
sg_clean<-mpt_clean %>% 
  filter(Substrate=="SG")
  

sg_clean_mean<-sg_clean%>% 
  group_by(treatment,parameters) %>%
  summarise(mean = mean(value), stdev = sd(value))

sg_clean_mean
sg_bio_mean <-sg_clean_mean %>% 
  filter(parameters=="Bioconversion rate")

sg_bio<-sg_clean %>% 
  filter(parameters=="Bioconversion rate")

#plotting spent grain bioconversion rate

sg_bio_mean %>% 
  ggplot(aes(treatment,mean))+
  geom_line(aes(group = parameters), linetype = "dashed") +
  geom_point(data=sg_bio, aes(treatment, value, color = "Replicas"),
             size = 4, alpha = 0.6) +
  geom_point(size = 4, aes(color = "Mean")) +
  
  labs(x = "Treatment", y = "Bioconversion rate [% DM]") +
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 2)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
  )+
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    legend.position = "none") + # Set legend position
  
  scale_color_manual(values = c("Mean" = "black", "Replicas" ="grey")) 


ggsave("output/bioconversion/sg_bio_v2.jpeg", units = "cm",width = 10,height = 10,dpi = 1200)


#sg waste reduction
sg_waste_mean <-sg_clean_mean %>% 
  filter(parameters=="Waste Reduction")

sg_waste<-sg_clean %>% 
  filter(parameters=="Waste Reduction")

#plotting spent grain waste reduction
sg_waste_mean %>% 
  ggplot(aes(treatment,mean))+
  geom_line(aes(group = parameters), linetype = "dashed") +
  geom_point(data=sg_waste, aes(treatment, value, color = "Replicas"),
             size = 4, alpha = 0.6) +
  geom_point(size = 4, aes(color = "Mean")) +
  
  labs(x = "Treatment", y = "Waste reduction [% DM]") +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
  )+
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    legend.position = "none") + # Set legend position
  
  scale_color_manual(values = c("Mean" = "black", "Replicas" ="grey")) 


ggsave("output/waste/sg_waste_v2.jpeg", units = "cm",width = 10,height = 10,dpi = 1200)

#sg larval weight in dry mass 

sg_weight_mean <-sg_clean_mean %>% 
  filter(parameters=="Final Larval weight")

sg_weight<-sg_clean %>% 
  filter(parameters=="Final Larval weight")

# plotting spent grain larval weight in dry msas 
sg_weight_mean %>% 
  ggplot(aes(treatment,mean))+
  geom_line(aes(group = parameters), linetype = "dashed") +
  geom_point(data=sg_weight, aes(treatment, value, color = "Replicas"),
             size = 4, alpha = 0.6) +
  geom_point(size = 4, aes(color = "Mean")) +
  
  labs(x = "Treatment", y = "Dry larval weight [mg DM]") +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 10)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
  )+
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    legend.position = "none") + # Set legend position
  
  scale_color_manual(values = c("Mean" = "black", "Replicas" ="grey")) 


ggsave("output/weight/sg_weight_v2.jpeg", units = "cm",width = 10,height = 10,dpi = 1200)

#sg fresh larval weight in wet mass

sg_fresh_weight_mean <-sg_clean_mean %>% 
  filter(parameters=="Fresh Larval weight")
sg_fresh_weight_mean

sg_fresh_weight<-sg_clean %>% 
  filter(parameters=="Fresh Larval weight")

# plotting spent grain fresh larval weight in wet msas 
sg_fresh_weight_mean %>% 
  ggplot(aes(treatment,mean))+
  geom_line(aes(group = parameters), linetype = "dashed") +
  geom_point(data=sg_fresh_weight, aes(treatment, value, color = "Replicas"),
             size = 4, alpha = 0.6) +
  geom_point(size = 4, aes(color = "Mean")) +
  
  labs(x = "Treatment", y = "Fresh larval weight [mg WM]") +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 25)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
  )+
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    legend.position = "none") + # Set legend position
  
  scale_color_manual(values = c("Mean" = "black", "Replicas" ="grey")) 


ggsave("output/weight/sg_fresh_weight.jpeg", units = "cm",width = 10,height = 10,dpi = 1200)


#same but for grass clippings

gc_clean<-mpt_clean %>% 
  filter(Substrate=="GC") %>% 
  filter(!treatment=="1-mm")

gc_clean

gc_clean_mean<-gc_clean%>% 
  group_by(treatment,parameters) %>%
  summarise(mean = mean(value), stdev = sd(value))

#filtering for biconversion rate of grass clippings and calculating the mean

gc_bio_mean <-gc_clean_mean %>% 
  filter(parameters=="Bioconversion rate")

gc_bio<-gc_clean %>% 
  filter(parameters=="Bioconversion rate")
#plotting bioconvserion rate of grass cliippings 

gc_bio_mean %>% 
  ggplot(aes(treatment,mean))+
  geom_line(aes(group = parameters), linetype = "dashed") +
  geom_point(data=gc_bio, aes(treatment, value, color = "Replicas"),
             size = 4, alpha = 0.6) +
  geom_point(size = 4, aes(color = "Mean")) +
  
  labs(x = "Treatment", y = "Bioconversion rate [% DM]") +
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 2)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
  )+
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    legend.position = "none") + # Set legend position
  
  scale_color_manual(values = c("Mean" = "black", "Replicas" ="grey")) 
 

ggsave("output/bioconversion/gc_bio_v2.jpeg", units = "cm",width = 10,height = 10,dpi = 1200)


#gc waste reduction excluding 1-mm
gc_clean<-mpt_clean %>% 
  filter(Substrate=="GC") %>% 
  filter(!treatment=="1-mm")


gc_clean_mean<-gc_clean%>% 
  group_by(treatment,parameters) %>%
  summarise(mean = mean(value), stdev = sd(value))


#calculating mean of waste reduction 
gc_waste_mean <-gc_clean_mean %>% 
  filter(parameters=="Waste Reduction")

gc_waste<-gc_clean %>% 
  filter(parameters=="Waste Reduction")

#plotting grass clippigns waste reduction 

gc_waste_mean %>% 
  ggplot(aes(treatment,mean))+
  geom_line(aes(group = parameters), linetype = "dashed") +
  geom_point(data=gc_waste, aes(treatment, value, color = "Replicas"),
             size = 4, alpha = 0.6) +
  geom_point(size = 4, aes(color = "Mean")) +
  
  labs(x = "Treatment", y = "Waste reduction [% DM]") +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
  )+
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    legend.position = "none") + # Set legend position
  
  scale_color_manual(values = c("Mean" = "black", "Replicas" ="grey")) 


ggsave("output/waste/gc_waste_v2.jpeg", units = "cm",width = 10,height = 10,dpi = 1200)



gc_clean<-mpt_clean %>% 
  filter(Substrate=="GC") %>% 
  filter(!treatment=="1-mm")

gc_clean_mean<-gc_clean%>% 
  group_by(treatment,parameters) %>%
  summarise(mean = mean(value), stdev = sd(value))

#dry larval weight on grass clippings 
gc_weight_mean <-gc_clean_mean %>% 
  filter(parameters=="Final Larval weight")

gc_weight<-gc_clean %>% 
  filter(parameters=="Final Larval weight")

#plotting dry larval weight 
gc_weight_mean %>% 
  ggplot(aes(treatment,mean))+
  geom_line(aes(group = parameters), linetype = "dashed") +
  geom_point(data=gc_weight, aes(treatment, value, color = "Replicas"),
             size = 4, alpha = 0.6) +
  geom_point(size = 4, aes(color = "Mean")) +
  
  labs(x = "Treatment", y = "Dry larval weight [mg DM]") +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 10)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
  )+
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    legend.position = "none") + # Set legend position
  
  scale_color_manual(values = c("Mean" = "black", "Replicas" ="grey")) 


ggsave("output/weight/gc_weight_v2.jpeg", units = "cm",width = 10,height = 10,dpi = 1200)



#grass clippings fresh larval weight in wet mass

gc_fresh_weight_mean <-gc_clean_mean %>% 
  filter(parameters=="Fresh Larval weight")

gc_fresh_weight_mean

gc_fresh_weight<-gc_clean %>% 
  filter(parameters=="Fresh Larval weight")

# plotting grass clippings fresh larval weight in wet msas 
gc_fresh_weight_mean %>% 
  ggplot(aes(treatment,mean))+
  geom_line(aes(group = parameters), linetype = "dashed") +
  geom_point(data=gc_fresh_weight, aes(treatment, value, color = "Replicas"),
             size = 4, alpha = 0.6) +
  geom_point(size = 4, aes(color = "Mean")) +
  
  labs(x = "Treatment", y = "Fresh larval weight [mg WM]") +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 25)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
  )+
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    legend.position = "none") + # Set legend position
  
  scale_color_manual(values = c("Mean" = "black", "Replicas" ="grey")) 


ggsave("output/weight/gc_fresh_weight.jpeg", units = "cm",width = 10,height = 10,dpi = 1200)


#degradation hemicellulose for spent grain and grass clippings 

hemi <-read.csv(file = "data/hemi.csv")

hemi<-hemi %>% 
  gather(4:27,key="parameters", value = 'value') %>%
  mutate(treatment_new= case_when(treatment=="untreated"~"Untreated",
                                  treatment==2 ~ "2-mm",
                                  treatment==1 ~ "1-mm",
                                  treatment==0.5~ "0.5-mm")) %>% 
  
  mutate(substrate=factor(substrate),
         treatment_new=factor(treatment_new),
         rep=factor(rep),
         treatment=factor(treatment))


#setting order for treatments
hemi$treatment_new<- factor(hemi$treatment_new, levels = c("Untreated", "2-mm",
                                                           "1-mm", "0.5-mm"))

gc_mean_hemi<-hemi %>% 
  filter(substrate == "gc") %>% 
  filter(parameters=="hemi_reduc_perc") %>% 
  filter(!treatment_new=="1-mm") %>% 
  group_by(substrate, parameters, treatment_new) %>% 
  summarise(mean = mean(value), sd = sd(value))



sg_mean_hemi<-hemi %>% 
  filter(substrate == "sg") %>% 
  filter(parameters=="hemi_reduc_perc") %>% 
  group_by(substrate, parameters, treatment_new) %>% 
  summarise(mean = mean(value), sd = sd(value))


# Plotting grass clippings degradation for all treatment conditions
ggplot(data = gc_mean_hemi, aes(treatment_new, mean, fill = treatment_new)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), position = position_dodge(width = 0.8), width = 0.2) +
  labs(x = "Treatment", y = "Hemicellulose Degradation [%]", title = "") +
  scale_y_continuous(breaks = seq(-100, 100, 25)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
  )+
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())+
  scale_fill_grey()+
  theme(strip.text.x = element_text(size = 12, color= "black"))+
  theme(strip.text.y = element_text(size = 12, color= "black"))+
  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.title.y = element_text(size = 12, color = "black"))+
  theme(axis.title.x = element_text(size = 12, color = "black"))+
  theme(axis.text.x = element_text(size=12, color = "black"))+
  theme(legend.text = element_text(size = 12, color="black"))

ggsave("output/hemicellulose/fiber_degrad_gc_hemi_v2.jpeg", units = "cm",
       width = 11,height = 11,dpi = 1200)

# Plotting sg degradation for all treatment conditions
ggplot(data = sg_mean_hemi, aes(treatment_new, mean, fill = treatment_new)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), position = position_dodge(width = 0.8), width = 0.2) +
  labs(x = "Treatment", y = "Hemicellulose Degradation [%]", title = "") +
  scale_y_continuous(breaks = seq(0, 100, 25), limits=c(0,100)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
  )+
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())+
  scale_fill_grey()+
  theme(strip.text.x = element_text(size = 12, color= "black"))+
  theme(strip.text.y = element_text(size = 12, color= "black"))+
  theme(axis.text.y = element_text(size=12, color="black")) +
  theme(axis.title.y = element_text(size = 12, color = "black"))+
  theme(axis.title.x = element_text(size = 12, color = "black"))+
  theme(axis.text.x = element_text(size=12, color = "black"))+
  theme(legend.text = element_text(size = 12, color="black"))

ggsave("output/hemicellulose/fiber_degrad_sg_hemi_v2.jpeg", units = "cm",
       width = 11,height = 11,dpi = 1200)













#second larval feeding experiment

# 
# second_experiment <-read.csv(file = "data/larval_second_trial.csv")
# second_experiment
# 
# #cleaniing up data for bd and whc
# larval_performance <-
#   second_experiment %>% 
#   gather(4:6, key="parameters", value = 'value') %>%
#  
#   mutate(treatment= case_when(treatment=="ut"~"Untreated",
#                               treatment==0.5~ "0.5 mm")) %>% 
#   
#   mutate(substrate=factor(substrate),
#          rep=factor(rep),
#          treatment=factor(treatment),
#          parameters=factor(parameters)) 
# 
# larval_performance
# 
# 
# 
# gc_weight_2_mean <-gc_weight_2 %>% 
#   group_by(treatment) %>% 
#   summarise(mean=mean(value),
#             stdev=sd(value),
#             max=max(value),
#             min=min(value))
# 
# 




