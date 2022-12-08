
# Duration in RP 5 --------------------------------------------------------
# 1st rat to complete both the alone and 50-300ms duration allowing for direct comparison.

Rxn_overall_by_Duration %>%
  filter(ID %in% c("LP2")) %>%
  mutate(Genotype = str_extract(Genotype, "KO|WT"),
         `Stim Length (ms)` = as.factor(`Dur (ms)`),
         Phase =  case_when(Phase == "BBN Rotating" ~ "Rotating",
                            TRUE ~ "")) %>%
  ggplot(aes(x = Intensity, y = Rxn)) +
  stat_summary(aes(linetype = `Stim Length (ms)`, group = `Stim Length (ms)`),
               fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 1, position = position_dodge(0.1)) +
  stat_summary(aes(shape = `Stim Length (ms)`, group = `Stim Length (ms)`),
               fun = mean,
               geom = "point", position = position_dodge(0.1), size = 3) +
  stat_summary(aes(linetype = `Stim Length (ms)`, group = `Stim Length (ms)`), fun = mean, geom = "line") +
  labs(title = "Presentation of single or mixed duration stimuli for LP 2",
       caption = paste("Date:", head(df$Date)),
       x = "Intensity (dB)",
       y = "Mean Reaction time (ms)") +
  scale_x_continuous(breaks = seq(20, 80, by = 10)) +
  facet_wrap(Duration ~ Phase, ncol = 2) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(color = rgb(235, 235, 235, 255, maxColorValue = 255))
  )


# Duration Effect ---------------------------------------------------------

Rxn_overall_by_Duration %>%
  filter(Intensity %in% c(125, 30, 35, 40, 45, 50, 60, 70, 80)) %>%
  filter(!(ID %in% partial.data)) %>%
  mutate(Genotype = str_extract(Genotype, "KO|WT"),
         `Dur (ms)` = as.factor(`Dur (ms)`)) %>%
  filter(Genotype == "WT" & Duration == "Alone") %>%
  ggplot(aes(x = Intensity, y = Rxn)) +
  # geom_point(aes(group = ID, color = Genotype), alpha = 0.3)+
  # geom_line(aes(group = ID, color = Genotype), alpha = 0.3)+
  stat_summary(aes(color = `Dur (ms)`, group = `Dur (ms)`),
               fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 1, position = position_dodge(0.1)) +
  stat_summary(aes(shape = `Dur (ms)`, color = `Dur (ms)`, group = `Dur (ms)`),
               fun = mean,
               geom = "point", position = position_dodge(0.1), size = 3) +
  stat_summary(aes(color = `Dur (ms)`, group = `Dur (ms)`), fun = mean, geom = "line") +
  labs(title = "Fmr1-LE WT effect of Duration on Alone",
       caption = paste("Date:", head(sort(df$Date, decreasing = TRUE))),
       x = "Intensity (dB)",
       y = "Reaction time (ms, mean +/- SE)") +
  scale_x_continuous(breaks = seq(20, 80, by = 10)) +
  scale_y_continuous(limits=c(100, 450)) +
  # facet_wrap(~ `Dur (ms)` , ncol = 1) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(color = rgb(235, 235, 235, 255, maxColorValue = 255))
  )

Rxn_overall_by_Duration %>%
  filter(Intensity %in% c(25, 30, 35, 40, 45, 50, 60, 70, 80)) %>%
  filter(!(ID %in% partial.data)) %>%
  mutate(Genotype = str_extract(Genotype, "KO|WT"),
         `Dur (ms)` = as.factor(`Dur (ms)`)) %>%
  filter(Genotype == "KO" & Duration == "Alone") %>%
  ggplot(aes(x = Intensity, y = Rxn)) +
  # geom_point(aes(group = ID, color = Genotype), alpha = 0.3)+
  # geom_line(aes(group = ID, color = Genotype), alpha = 0.3)+
  stat_summary(aes(color = `Dur (ms)`, group = `Dur (ms)`),
               fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 1, position = position_dodge(0.1)) +
  stat_summary(aes(shape = `Dur (ms)`, color = `Dur (ms)`, group = `Dur (ms)`),
               fun = mean,
               geom = "point", position = position_dodge(0.1), size = 3) +
  stat_summary(aes(color = `Dur (ms)`, group = `Dur (ms)`), fun = mean, geom = "line") +
  labs(title = "Fmr1-LE KO effect of Duration on Alone",
       caption = paste("Date:", head(sort(df$Date, decreasing = TRUE))),
       x = "Intensity (dB)",
       y = "Reaction time (ms, mean +/- SE)") +
  scale_x_continuous(breaks = seq(20, 80, by = 10)) +
  scale_y_continuous(limits=c(100, 450)) +
  # facet_wrap(~ `Dur (ms)` , ncol = 1) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(color = rgb(235, 235, 235, 255, maxColorValue = 255))
  )

