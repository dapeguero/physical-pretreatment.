rm(list=ls()) #clears the environment 

library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library("writexl")
library(viridis)
library(extrafont)
library(multcomp)
library(patchwork)

larval_wet_mass<-read.csv(file = "data/microbial_respiration.csv")

larva_mg_clean <-larval_wet_mass %>% 
  gather(6, key="parameters", value = 'value') %>%
  mutate(treatment= case_when(treatment=="ut"~"Untreated",
                              treatment==0.5~ "0.5-mm")) %>% 
  mutate(days=factor(days)) %>% 
  mutate(rep=factor(rep)) %>% 
  mutate(substrate=factor(substrate)) %>% 
  mutate(treatment=factor(treatment))

larva_mg_clean$treatment<- factor(larva_mg_clean$treatment, levels = c("Untreated", "0.5-mm"))


larva_mg_sg_mean<- larva_mg_clean %>% 
  filter(substrate == "sg") %>% 
  group_by(days,treatment) %>% 
  summarise(mean = mean(value), sd = sd(value))
larva_mg_sg_mean

larva_mg_sg<- larva_mg_clean %>% 
  filter(substrate == "sg") %>% 
  group_by(days,treatment) 
 

larva_mg_sg
larva_mg_gc_mean<- larva_mg_clean %>% 
  filter(substrate == "gc") %>% 
  group_by(days,treatment) %>% 
  summarise(mean = mean(value), sd = sd(value))


larva_mg_gc<- larva_mg_clean %>% 
  filter(substrate == "gc") %>% 
  group_by(days,treatment) 

  
# Plotting the mean as a line and replicates as points
  larva_mg_sg_mean %>%
    ggplot(aes(days, mean, linetype = treatment, group = treatment)) +
    geom_point(size=3)+
    geom_line()+
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
    #geom_point(data = larva_mg_gc, aes(days, value),
   
    scale_color_manual(values = c(untreated = "black", `0.5 mm` = "black")) +
    labs(x = "Days", y = "Fresh larval weight (mg WM) ") +
    theme_bw() +
    theme(
      panel.grid.major = element_blank()
    )+
  
    theme(
      axis.text.x = element_text(size = 12, color = "black"),
      axis.text.y = element_text(size = 12, color = "black"),
      axis.title.y = element_text(size = 12, color = "black"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 12, color = "black"),
      legend.key.size = unit(1, "cm")
    )
  
   
  
  ggsave(filename ="output/exp2_weight/sg_larval_mg_v2.jpeg", units = "cm", width = 10, 
         height = 10, dpi = 1200)

  # Plotting the mean as a line and replicates as points
larva_mg_gc_mean
  larva_mg_gc_mean %>%
    ggplot(aes(days, mean, linetype = treatment, group = treatment)) +
    geom_point(size=3)+
    geom_line()+
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
    #geom_point(data = larva_mg_gc, aes(days, value),
     #          size = 3, alpha = 0.3) +
    #scale_color_manual(values = c(untreated = "black", `0.5 mm` = "black")) +
    labs(x = "Days", y = "Fresh Larval weight (mg WM)") +
      scale_linetype_manual(values = c("solid", "dotted"),
                            labels = c("Untreated", "0.5-mm"),
                            name = NULL) +
    scale_y_continuous(limits = c(0, 80))+
    theme_bw() +
    theme(
      panel.grid.major = element_blank()
    )+
    theme(
      axis.text.x = element_text(size = 12, color = "black"),
      axis.text.y = element_text(size = 12, color = "black"),
      axis.title.y = element_text(size = 12, color = "black"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 12, color = "black"),
      legend.key.size = unit(1, "cm")
    )
    
  
  
  
  ggsave(filename ="output/exp2_weight/gc_larval_mg_v2.jpeg", units = "cm", width = 10, 
         height = 10, dpi = 1200)
  

#microbial respiration


co2 <-read.csv(file = "data/microbial_respiration.csv")

co2

co2_clean <-co2 %>% 
  gather(5, key="parameters", value = 'value') %>%
  mutate(treatment= case_when(treatment=="ut"~"Untreated",
                              treatment==0.5~ "0.5-mm")) %>% 
  mutate(days=factor(days)) %>% 
  mutate(rep=factor(rep)) %>% 
  mutate(substrate=factor(substrate)) %>% 
  mutate(treatment=factor(treatment))

co2_clean$treatment<- factor(co2_clean$treatment, levels = c("Untreated", "0.5-mm"))

co2_clean

sg_co2_clean_mean<- co2_clean %>% 
  filter(substrate == "sg") %>% 
  group_by(days,treatment) %>% 
  summarise(mean = mean(value), sd = sd(value))
sg_co2_clean_mean

sg_co2_0.5<- sg_co2_clean_mean %>% 
    filter(treatment=="0.5-mm")

sg_co2_ut <-sg_co2_clean_mean %>% 
  filter(treatment=="Untreated")

sg_co2<-co2_clean %>% 
  filter(substrate == "sg") %>% 
  group_by(days,treatment)
sg_co2

gc_co2<-co2_clean %>% 
  filter(substrate == "gc") %>% 
  group_by(days,treatment)


gc_co2_clean_mean<- co2_clean %>% 
  filter(substrate == "gc") %>% 
  group_by(days,treatment) %>% 
  summarise(mean = mean(value), sd = sd(value))

gc_co2_clean_mean
gc_co2_ut <-gc_co2_clean_mean %>% 
  filter(treatment=="Untreated")

gc_co2_0.5<-gc_co2_clean_mean%>% 
  filter(treatment=="0.5-mm")  
  
gc_co2_ut


#plotting microbial respiration for sg

sg_co2_clean_mean
sg_co2_clean_mean %>% 
  filter(!(days %in% c(2, 5))) %>%
  ggplot(aes(days, mean, linetype = treatment, group = treatment)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1)  +  # Add error bars
  labs(x = "Days", y = "Microbial respiration (mg CO"[2]~"/min)") +
  scale_linetype_manual(values = c("solid", "dotted"),
                        labels = c("Untreated", "0.5-mm"),
                        name = NULL) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  theme_bw() +
  theme(legend.position = "bottom",legend.text = element_text(size = 12))+
  theme(
    panel.grid.major = element_blank()
  )+
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"))

ggsave(filename ="output/co2/sg_co2_v2.jpeg", units = "cm", width = 10, 
       height = 10, dpi = 1200)
  




#plotting microbial respiration for gc

gc_co2_clean_mean
gc_co2_clean_mean %>% 
  filter(!(days %in% c(2, 5))) %>% 
  ggplot(aes(days, mean, linetype = treatment, group = treatment)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1)  +
  labs(x = "Days", y = "Microbial respiration (mg CO"[2]~"/min)") +
  scale_linetype_manual(values = c("solid", "dotted"),
                        labels = c("Untreated", "0.5-mm"),
                        name = NULL) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank()
  )+
  theme(legend.position = "bottom",legend.text = element_text(size = 12))+
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"))


ggsave(filename ="output/co2/gc_co2_v2.jpeg", units = "cm", width = 10, 
       height = 10, dpi = 1200)
  


#respiration for larvae and total container in spent grain

co2_all <-co2 %>% 
  gather(7:8, key="parameters", value = 'value') %>%
  mutate(treatment= case_when(treatment=="ut"~"Untreated",
                              treatment==0.5~ "0.5-mm")) %>% 
  mutate(days=factor(days)) %>% 
  mutate(rep=factor(rep)) %>% 
  mutate(substrate=factor(substrate)) %>% 
  mutate(treatment=factor(treatment))

co2_all$treatment<- factor(co2_clean$treatment, levels = c("Untreated", "0.5-mm"))

co2_all

sg_co2_all_total_mean<- co2_all %>% 
  filter(substrate == "sg") %>% 
  filter(parameters=="total_mg_co2") %>% 
  group_by(days,treatment) %>% 
  summarise(mean = mean(value), sd = sd(value))

sg_co2_all_total_mean

sg_co2_all_larvae<- co2_all %>% 
  filter(treatment=="0.5-mm")

sg_co2_all_larvae_mean<- co2_all %>% 
  filter(substrate == "sg") %>% 
  filter(parameters=="larvae_mg_co2") %>% 
  group_by(days,treatment) %>% 
  summarise(mean = mean(value), sd = sd(value))

sg_co2_all_larvae_mean
sg_co2_ut <-sg_co2_clean_mean %>% 
  filter(treatment=="Untreated")

combined_data <- rbind(
  mutate(sg_co2_all_larvae_mean, Dataset = "Larvae"),
  mutate(sg_co2_all_total_mean, Dataset = "Total")
)


ggplot(combined_data %>% filter(!(days %in% c(2, 5))), aes(days, mean, 
                                                           linetype = treatment, group = interaction(treatment, Dataset))) +
  geom_point(aes(shape = Dataset), size = 3, fill = "black") +  # Fill the shapes with black
  geom_line() +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 3)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  labs(x = "Days", y = "Respiration (mg CO"[2]~"/min)") +
  scale_linetype_manual(
    values = c("solid", "dotted"),
    labels = c("Untreated", "0.5-mm"),
    name = NULL
  ) +
  scale_shape_manual(
    values = c("Larvae" = 1, "Total" = 23),  # Use shape 2 and 1 for both "Larvae" and "Total"
    labels = c("Larvae", "Total"),
    name = NULL
  ) +
  
  # scale_fill_manual(
  #   values = c("Larvae" = "black", "Total" = "black")  # Fill both shapes with black
  # ) +
  #guides(fill = guide_legend(override.aes = list(shape = 2))) +  # Override the legend shape to be filled
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    legend.position = "bottom",  # Set the legend at the bottom
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    legend.box = "vertical",  # Split the legend into two columns
    legend.title = element_blank()  # Remove the legend title
  )

ggsave(filename ="output/co2/sg_co2_all.jpeg", units = "cm", width = 10, 
       height = 10, dpi = 1200)

#same thing for grass clippings


co2_all <-co2 %>% 
  gather(7:8, key="parameters", value = 'value') %>%
  mutate(treatment= case_when(treatment=="ut"~"Untreated",
                              treatment==0.5~ "0.5-mm")) %>% 
  mutate(days=factor(days)) %>% 
  mutate(rep=factor(rep)) %>% 
  mutate(substrate=factor(substrate)) %>% 
  mutate(treatment=factor(treatment))

co2_all$treatment<- factor(co2_clean$treatment, levels = c("Untreated", "0.5-mm"))

co2_all

gc_co2_all_total_mean<- co2_all %>% 
  filter(substrate == "gc") %>% 
  filter(parameters=="total_mg_co2") %>% 
  group_by(days,treatment) %>% 
  summarise(mean = mean(value), sd = sd(value))

gc_co2_all_total_mean

gc_co2_all_larvae_mean<- co2_all %>% 
  filter(substrate == "gc") %>% 
  filter(parameters=="larvae_mg_co2") %>% 
  group_by(days,treatment) %>% 
  summarise(mean = mean(value), sd = sd(value))

gc_co2_all_larvae_mean

gc_combined_data <- rbind(
  mutate(gc_co2_all_larvae_mean, Dataset = "Larvae"),
  mutate(gc_co2_all_total_mean, Dataset = "Total")
)


ggplot(gc_combined_data %>% filter(!(days %in% c(2, 5))), aes(days, mean, 
                                                           linetype = treatment, group = interaction(treatment, Dataset))) +
  geom_point(aes(shape = Dataset), size = 3, fill = "black") +  # Fill the shapes with black
  geom_line() +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  labs(x = "Days", y = "Respiration (mg CO"[2]~"/min)") +
  scale_linetype_manual(
    values = c("solid", "dotted"),
    labels = c("Untreated", "0.5-mm"),
    name = NULL
  ) +
  scale_shape_manual(
    values = c("Larvae" = 1, "Total" = 23),  # Use shape 2 and 1 for both "Larvae" and "Total"
    labels = c("Larvae", "Total"),
    name = NULL
  ) +
  
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    legend.position = "bottom",  # Set the legend at the bottom
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    legend.box = "vertical",  # Split the legend into two columns
    legend.title = element_blank()  # Remove the legend title
  )

ggsave(filename ="output/co2/gc_co2_all.jpeg", units = "cm", width = 10, 
       height = 10, dpi = 1200)









temp <-read.csv(file = "data/temp.csv")
temp

#cleaned up data and changing name in column of treatment
temp_clean <-
  temp %>% 
  gather(2:16, key="parameters", value = 'value') %>%
  mutate(day=factor(day))
 # mutate(parameters= case_when(parameters=="ambient_temp"~"Temperature",
  #                           parameters=="ambient_humidity"~ "Humidity"))
 
temp_clean
humidity_temp<-
  temp_clean %>% 
  filter(parameters %in% c("Temperature", "Humidity"))


humidity_temp
# Assuming you have the 'temp_clean' data frame as defined in your code

# Assuming you have the 'temp_clean' data frame as defined in your code

# Create the line graph with two y-axes
humidity_temp %>% 
  
  ggplot(aes(x = day, y=value, group=parameters, linetype = parameters))+ 
  geom_line()+
   scale_linetype_manual(name = "Variables",
                     values = c("Temperature" = "solid", "Humidity" = "dashed")) +
    labs(x = "Days", y = "Temperature (°)C") +
    theme_bw() +
  theme(
    panel.grid.major = element_blank())+
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
  scale_y_continuous(
    name = "Temperature (°C)",
    sec.axis = sec_axis(
      ~ .,
      name = "% Humidity",
      breaks = pretty(humidity_temp$value, n = 5)
    ))+
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y.left = element_text(size = 12, color = "black"),
    axis.text.y.right = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.title.y.left = element_text(size = 12, color = "black"),
    axis.title.y.right = element_text(size = 12, color = "black")
  )


ggsave(filename ="output/ambient_temp.jpeg", units = "cm", width = 18, 
       height = 8, dpi = 1200)
temp_clean

# Filter and summarize data for "sg_ut" treatment
sg_ut_temp <- temp_clean %>%
  filter(parameters %in% c("sg_ut_1", "sg_ut_2", "sg_ut_3")) %>%
  group_by(day) %>%
  summarize(avg_value = mean(value),
            stdev_value = sd(value))
sg_ut_temp

# Filter and summarize data for "sg_0.5" treatment
sg_0.5_temp <- temp_clean %>%
  filter(parameters %in% c("sg_0.5_1", "sg_0.5_2", "sg_0.5_3")) %>%
  group_by(day) %>%
  summarize(avg_value = mean(value),
            stdev_value = sd(value))

sg_0.5_temp

sg_temp_mean<-temp_clean %>% 
  filter(parameters %in% c("sg_ut_1", "sg_ut_2", "sg_ut_3",
                           "sg_0.5_1", "sg_0.5_2", "sg_0.5_3")) %>% 
  
   group_by(day) %>%
    summarize(avg_value = mean(value),
            stdev_value = sd(value))
sg_temp_mean
gc_temp<-temp_clean %>% 
  filter(parameters %in% c("gc_ut_1", "gc_ut_2", "gc_ut_3",
                           "gc_0.5_1", "gc_0.5_2", "gc_0.5_3"))

# Merge the two data frames and add a "treatment" column
sg_ut_temp$treatment <- "sg_ut"
sg_0.5_temp$treatment <- "sg_0.5"

# Combine the data frames using rbind
sg_merged_temp <- rbind(sg_ut_temp, sg_0.5_temp)


sg_merged_temp
# Reorder the factor levels of the "treatment" variable
sg_merged_temp$treatment <- factor(sg_merged_temp$treatment, levels = c("sg_ut", "sg_0.5"))
# Convert "day" to numeric to ensure it is treated as continuous
sg_merged_temp$day <- as.numeric(as.character(sg_merged_temp$day))
#sg_merged_temp$day <- factor(sg_merged_temp$day, levels = unique(sg_merged_temp$day))



# Plot the line graph for spent grain using ggplot()
ggplot(sg_merged_temp, aes(x = day, y = avg_value,
                        linetype= treatment, group = treatment)) +
  geom_line() +
  geom_point(size=3) + # Add points for the mean values
  geom_errorbar(aes(ymin = avg_value - stdev_value, ymax = avg_value + stdev_value),
                width = 0.1)+   # Add error bars (stdev) to the points
 # scale_x_continuous(breaks = seq(min(sg_merged_temp$day), max(sg_merged_temp$day), by = 1)) +
  #scale_x_continuous(breaks = seq(0, 6, by = 1)) 
  scale_x_continuous(breaks = seq(0, 6, by = 1), labels = seq(0, 6, by = 1)) +
  scale_y_continuous(limits = c(26, 40), breaks = seq(20, 40, by = 2)) +
  scale_linetype_manual(values=c("solid", "dotted"),
                        labels= c("Untreated", "0.5-mm"),
                        name = NULL) +
  labs(   x = "Days",
       y = bquote("Average temperature ("*degree*C*")")) + 
  theme_bw()+
  theme(
    panel.grid.major = element_blank()
  ) +
  theme(legend.position = "bottom", legend.text = element_text(size = 12) )+
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"))
  
ggsave(filename ="output/temp/sg_temp_v2.jpeg", units = "cm", width = 10, 
       height = 10, dpi = 1200)

# Filter and summarize data for "gc_ut" treatment


gc_ut_temp <- temp_clean %>%
  filter(parameters %in% c("gc_ut_1", "gc_ut_2", "gc_ut_3")) %>%
  group_by(day) %>%
  summarize(avg_value = mean(value),
            stdev_value = sd(value))

# Filter and summarize data for "gc_0.5" treatment
gc_0.5_temp <- temp_clean %>%
  filter(parameters %in% c("gc_0.5_1", "gc_0.5_2", "gc_0.5_3")) %>%
  group_by(day) %>%
  summarize(avg_value = mean(value),
            stdev_value = sd(value))
gc_0.5_temp
# Merge the two data frames and add a "treatment" column
gc_ut_temp$treatment <- "gc_ut"
gc_0.5_temp$treatment <- "gc_0.5"

# Combine the data frames using rbind
gc_merged_temp <- rbind(gc_ut_temp, gc_0.5_temp)
# Reorder the factor levels of the "treatment" variable
gc_merged_temp$treatment <- factor(gc_merged_temp$treatment, levels = c("gc_ut", "gc_0.5"))

# Convert "day" to numeric to ensure it is treated as continuous
#merged_temp$day <- as.numeric(as.character(merged_temp$day))

# Plot the line graph  for gc using ggplot()
ggplot(gc_merged_temp, aes(x = day, y = avg_value, 
                        linetype= treatment, group = treatment)) +
  geom_line() +
  geom_point(size=3)+ # Add points for the mean values
  scale_linetype_manual(values=c("solid", "dotted"),
                        labels= c("Untreated", "0.5-mm"),
                        name = NULL) +
  geom_errorbar(aes(ymin = avg_value - stdev_value, ymax = avg_value + stdev_value),
                width = 0.1) +
 # scale_x_continuous(breaks = seq(min(merged_temp$day), max(merged_temp$day), by = 1)) +
  scale_y_continuous(limits = c(26, 40), breaks = seq(20, 40, by = 2)) +
  labs(
       x = "Days",
       y = bquote("Average temperature ("*degree*C*")")) + 
  theme_bw()+
  theme(
    panel.grid.major = element_blank()
  )+
  theme(legend.position = "bottom", legend.text = element_text(size = 12) )+
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"))
  ggsave(filename ="output/temp/gc_temp_v2.jpeg", units = "cm", width = 10, 
       height = 10, dpi = 1200)







#correlation between co2 and temp
# Assuming sg_co2_0.5 and sg_0.5_temp are vectors representing CO2 and temperature data
# Convert them to numeric if needed
sg_co2_0.5_num <- as.numeric(sg_co2_0.5)
sg_0.5_temp <- as.numeric(sg_0.5_temp)

sg_co2_0.5
sg_0.5_temp

# Calculate the correlation between sg_co2_0.5 and sg_0.5_temp
correlation_sg_0.5 <-  cor(sg_co2_0.5$mean, sg_0.5_temp$avg_value)
print(correlation_sg_0.5)

correlation_sg_ut <-  cor(sg_co2_ut$mean, sg_ut_temp$avg_value)
print(correlation_sg_ut)

correlation_gc_0.5 <-  cor(gc_co2_0.5$mean, gc_0.5_temp$avg_value)
print(correlation_gc_0.5)

correlation_gc_ut <-  cor(gc_co2_ut$mean, gc_ut_temp$avg_value)
print(correlation_gc_ut)



#conduct anova on tvc
tvc <-read.csv(file = "data/tvc.csv")

tvc
tvc_clean <-tvc %>% 
  gather(5:6, key="parameters", value = 'value') %>%
  mutate(treatment= case_when(treatment=="ut"~"Untreated",
                              treatment==0.5~ "0.5 mm")) %>% 
  mutate(day=factor(day)) %>% 
  mutate(rep=factor(rep)) %>% 
  mutate(substrate=factor(substrate)) %>% 
  mutate(treatment=factor(treatment))
tvc_clean

sg_tvc<-tvc_clean %>% 
  filter(substrate=="sg") %>% 
  filter(parameters=="log") %>% 
  filter(!value=="#NUM!")



#conduction for grass clippings

gc_tvc<-tvc_clean %>% 
  filter(substrate=="gc") %>% 
  filter(parameters=="log") %>% 
  filter(!value=="#NUM!")





