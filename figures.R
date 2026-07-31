library(tidyverse)
library(patchwork)
library(janitor)
library(rstatix)
library(scales)
library(hms)

theme_update(text = element_text(size = 12),
             axis.text.x = element_text(size = 12, color = "black"), 
             axis.title.x = element_text(size = 14),
             axis.text.y = element_text(size = 12,  color = "black"), 
             axis.title.y = element_text(size = 14), 
             panel.background = element_blank(),panel.border = element_blank(), 
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             axis.ticks.length=unit(.25, "cm"), 
             legend.key = element_rect(fill = "white")) 

load("whole-model-prediction_4s.RData")
ds_restraint <- ds %>% mutate(time_join = round(as.numeric(time_start))) %>% 
  select(id, session, unrestrained, time_join) 
ds_imu <- read_csv("imu-raw-position.csv") %>% mutate(time_join = round(as.numeric(time_start)))

ds <- left_join(ds_imu,ds_restraint)

ds <- ds %>% mutate(age_group = factor(ifelse(agemo < 8, 0, 1), labels = c("Younger","Older")),
                    walker = ifelse(unique_id == "161/3", "Non-Walker", walker),
                    sitter = factor(sitter, levels = c("Non-Sitter","Sitter")),
                    crawler = factor(crawler, levels = c("Non-Crawler","Crawler")),
                    walker = factor(walker, levels = c("Non-Walker","Walker")),
                    tripod = factor(tripod, levels = c("Non-Tripod","Tripod"), labels = c("Non-Sitter","Sitter")))


# Exclude pilot participants and non-compliant sessions
ds <- filter(ds, id != 185, id != 104, unique_id != "178/3")

# Exclude participants based on a minimum hour duration of data
ds %>%  filter(exclude_period == 0, nap_period == 0) %>% count(unique_id)  %>% 
  mutate(duration = n/3600 * 2) %>% 
  arrange(duration) -> nhours

keep_ppts <- nhours %>% filter(duration > 3) %>% pull(unique_id)
ds <- filter(ds, unique_id %in% keep_ppts)

ds <- ds %>% filter(exclude_period == 0, nap_period == 0)

ds_sum_imu <- ds %>% group_by(id, session) %>% 
  mutate(total_samples = n()) %>% group_by(id, session, pos, agemo) %>% 
  summarize(pos_n  = n(), total_samples = mean(total_samples)) %>% ungroup %>% 
  mutate(age_group = factor(ifelse(agemo < 8, 0, 1), labels = c("Younger","Older")))
ds_sum_imu <- ds_sum_imu %>% complete(nesting(id, session), pos, fill = list(pos_n = 0, total_samples = 1))
ds_sum_imu$pos_prop = ds_sum_imu$pos_n/ds_sum_imu$total_samples*100
ds_sum_imu <- ds_sum_imu %>% rename(Position = pos) %>% 
  mutate(age = case_when(
    session == 1 & age_group == "Younger" ~ 4,
    session == 2 & age_group == "Younger" ~ 5,
    session == 3 & age_group == "Younger" ~ 6,
    session == 4 & age_group == "Younger" ~ 6.75,
    session == 1 & age_group == "Older" ~ 11.25,
    session == 2 & age_group == "Older" ~ 12,
    session == 3 & age_group == "Older" ~ 13,
    session == 4 & age_group == "Older" ~ 14)) %>% select(-agemo)

ds_sum_imu_rest <- ds %>% group_by(id, session) %>% 
  mutate(total_samples = n()) %>% group_by(id, session, pos, unrestrained, agemo) %>% 
  summarize(pos_n  = n(), total_samples = mean(total_samples)) %>% ungroup %>% 
  mutate(age_group = factor(ifelse(agemo < 8, 0, 1), labels = c("Younger","Older")))
ds_sum_imu_rest <- ds_sum_imu_rest %>% complete(nesting(id, session), pos, unrestrained, fill = list(pos_n = 0, total_samples = 1))
ds_sum_imu_rest$pos_prop = ds_sum_imu_rest$pos_n/ds_sum_imu_rest$total_samples*100
ds_sum_imu_rest <- ds_sum_imu_rest %>% rename(Position = pos, Restraint = unrestrained) %>% 
  mutate(age = case_when(
    session == 1 & age_group == "Younger" ~ 4,
    session == 2 & age_group == "Younger" ~ 5,
    session == 3 & age_group == "Younger" ~ 6,
    session == 4 & age_group == "Younger" ~ 6.75,
    session == 1 & age_group == "Older" ~ 11.25,
    session == 2 & age_group == "Older" ~ 12,
    session == 3 & age_group == "Older" ~ 13,
    session == 4 & age_group == "Older" ~ 14)) %>% select(-agemo)

# Pull in SiP
library(pins)
library(nanoparquet)
library(googledrive)
ds_sip <- board_gdrive(path = as_id("1OZlphhu6vYm1A2Bm2-zD7a4luS5nGWgS")) %>%
  pin_read("imu_raw_samples")  %>%
  filter(nap_period == 0, exclude_period == 0)
#version = "20260627T184334Z-8940e"

ds_sum_sip <- ds_sip %>% group_by(id, session) %>% 
  mutate(total_samples = n()) %>% group_by(id, session, pos) %>% 
  summarize(pos_n  = n(), total_samples = mean(total_samples)) %>% ungroup
ds_sum_sip <- ds_sum_sip %>% complete(nesting(id, session), pos, fill = list(pos_n = 0, total_samples = 1))
ds_sum_sip$pos_prop = ds_sum_sip$pos_n/ds_sum_sip$total_samples*100
ds_sum_sip$session = as.numeric(ds_sum_sip$session)
ds_sum_sip <- ds_sum_sip %>% mutate(
  age = case_when(
    session == 1 ~ 7.25,
    session == 2 ~ 9,
    session == 3 ~ 10.75
  )
) %>% rename(Position = pos) %>% mutate(id = as.numeric(id))
ds_sum_sip$age_group <- "Middle"

ds_sum_sip_rest <- ds_sip %>% group_by(id, session) %>% 
  mutate(total_samples = n()) %>% group_by(id, session, pos, restraint) %>% 
  summarize(pos_n  = n(), total_samples = mean(total_samples)) %>% ungroup
ds_sum_sip_rest <- ds_sum_sip_rest %>% complete(nesting(id, session), pos, restraint, fill = list(pos_n = 0, total_samples = 1))
ds_sum_sip_rest$pos_prop = ds_sum_sip_rest$pos_n/ds_sum_sip_rest$total_samples*100
ds_sum_sip_rest$session = as.numeric(ds_sum_sip_rest$session)
ds_sum_sip_rest <- ds_sum_sip_rest %>% mutate(
  age = case_when(
    session == 1 ~ 7.25,
    session == 2 ~ 9,
    session == 3 ~ 10.75
  )
) %>% rename(Position = pos, Restraint = restraint) %>% mutate(id = as.numeric(id))
ds_sum_sip_rest$age_group <- "Middle"

## Merge for posture only

ds_merged <- ds_sum_sip %>% bind_rows(ds_sum_imu) %>% 
  mutate(Position = ifelse(Position == "Upright", "Standing", as.character(Position)),
         id_uni = str_glue("{id}_{session}"),
         model = ifelse(age_group == "Middle", "Axivity","Biostamp"))

ggplot(ds_merged) +
  stat_summary(aes(x = age, y = pos_prop, group = id_uni, color = age_group), geom = "point", shape = 21,  size = 2.5, alpha = .85) +
  geom_smooth(aes(x = age, y = pos_prop)) +   
  facet_wrap("Position", ncol = 2) + 
  scale_x_continuous(name = "Age (months)", breaks = 4:14, limits = c(3,15)) + 
  scale_y_continuous(name = "Percentage of awake time", breaks = seq(0, 100, 25), limits = c(-5, 105)) +
  guides(color = guide_legend(nrow = 2)) +
  theme(
    legend.position = c(0.95, 0.02),             
    legend.justification = c("right", "bottom"),
    legend.margin = margin(4, 6, 4, 6)
  )

## Merge for posture and restraint

ds_merged_rest <- ds_sum_sip_rest %>% bind_rows(ds_sum_imu_rest) %>% 
  mutate(Position = ifelse(Position == "Upright", "Standing", as.character(Position)),
         id_uni = str_glue("{id}_{session}"),
         model = ifelse(age_group == "Middle", "Axivity","Biostamp"))

ggplot(ds_merged_rest) +
  stat_summary(aes(x = age, y = pos_prop, group = id_uni, shape = model), geom = "point", size = 2.5, color = "gray") +
  geom_smooth(aes(x = age, y = pos_prop), se = F, color = "black") +   
  facet_grid(Position ~ Restraint) + 
  scale_shape_manual(values = c(24,21), name = "Model") + 
  scale_x_continuous(name = "Age (months)", breaks = 4:14, limits = c(3,15)) + 
  scale_y_continuous(name = "Percentage of awake time", breaks = seq(0, 100, 25), limits = c(-5, 105)) +
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom"  )

## Merge in as total
ds_total <- ds_merged %>% mutate(Restraint = "Total")  %>%  
  bind_rows(ds_merged_rest) %>% 
  mutate(model = factor(model, levels = c("Biostamp","Axivity")),
        Restraint = factor(Restraint, levels = c("Restrained","Unrestrained","Total")),
         Position = factor(Position, levels = c("Held","Supine","Prone","Sitting","Standing")))

ggplot(ds_total) +
  geom_hline(yintercept = -Inf, color = "black", linewidth = 0.8) +
  stat_summary(aes(x = age, y = pos_prop, group = id_uni, shape = model), geom = "point", size = 1.75, color = "darkgray", stroke = .25) +
  geom_smooth(aes(x = age, y = pos_prop), se = F, color = "black") +   
  facet_grid(Restraint ~ Position) + 
  scale_shape_manual(values = c(24,21), name = "Model") + 
  scale_x_continuous(name = "Age (months)", breaks = c(5,7,9,11,13), limits = c(3,15)) + 
  scale_y_continuous(name = "Percentage of awake time", breaks = seq(0, 100, 25), limits = c(-1, 101)) +
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom", axis.line = element_line(color = "black", linewidth = 0.4))
ggsave("age_figure.png",width = 7.5, height = 5.5, unit = "in", scale = 1)

## Timeline
lims <- as_hms(c('07:00:00', '21:59:00'))
hour_breaks = as_hms(c('07:00:00','08:00:00','09:00:00', '10:00:00', '11:00:00', '12:00:00', '13:00:00', '14:00:00', '15:00:00', '16:00:00', '17:00:00', '18:00:00', '19:00:00', '20:00:00', '21:00:00'))
label_breaks = c("7am","","9am","","11am","","1pm","","3pm","","5pm","","7pm","","9pm")

temp_data <- ds_sip %>% filter(id == 18, session == 3) %>% 
  mutate(pos = factor(pos, levels=c("Held","Supine", "Prone", "Sitting", "Standing")))
temp_data  %>% 
  ggplot(aes(x = time_plot, y = pos)) + 
  geom_raster() +  facet_wrap(~ restraint, nrow = 2) + 
  scale_x_time(breaks = hour_breaks, name = "", limits = lims, labels = label_breaks) + 
  theme(
    axis.title.y = element_blank(),
    legend.position = "bottom"
  ) 
ggsave("timeline_older.png",width = 7.5, height = 4.5, unit = "in", scale = 1)

temp_data2 <- ds_sip %>% filter(id == 29, session == 1) %>% 
  mutate(pos = factor(pos, levels=c("Held","Supine", "Prone", "Sitting", "Standing")))

temp_data2  %>% 
  ggplot(aes(x = time_plot, y = pos)) + 
  geom_raster() +  facet_wrap(~ restraint, nrow = 2) + 
  scale_x_time(breaks = hour_breaks, name = "", limits = lims, labels = label_breaks) + 
  theme(
    axis.title.y = element_blank(),
    legend.position = "bottom"
  ) 
ggsave("timeline_younger.png",width = 7.5, height = 4.5, unit = "in", scale = 1)

