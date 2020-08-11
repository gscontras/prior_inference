keep <- c("workerid","blockNr","evalNum","evalNumModel","ambiguousUtteranceCount")
data <- inputDataCondensed[,keep]
head(data)

data_long <- rbind(data,data)
first_half <- seq(1:(length(data_long$workerid)/2))
second_half <- first_half + length(data$workerid)
human <- data_long[first_half,]
human$evalNumModel <- NULL
human$group <- "human"

model <- data_long[second_half,]
model$evalNum <- NULL
model$group <- "model"

colnames(human) <- c("workerid","blockNr","evalNum","ambiguousCount","group")
colnames(model) <- c("workerid","blockNr","evalNum","ambiguousCount", "group")

data <- rbind(human,model)
data$success <- ifelse(data$evalNum == 3, 1, 0)

#data$humanSuccess <- ifelse(data$evalNum == 3, 1, 0)
#data$modelSuccess <- ifelse(data$evalNumModel == 3, 1, 0)
library(gmodels)


# Barplot

 data %>% 
  group_by(ambiguousCount,group) %>%
  summarise(mean = mean(success), sd = sd(success), n = n()) %>%
   mutate(se = sd / sqrt(n),
          lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
          upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>%
      ggplot(aes(ambiguousCount,mean, fill = group)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(x=ambiguousCount, ymin=upper.ci, ymax=lower.ci), 
                 width=0.2, size=0.5, color="black",position = position_dodge(width=0.9)) +
      ylab("Learning success") +
      xlab("Number of ambiguous utterances per block") +
      scale_fill_manual(values=c("grey","grey45"))+
      ggtitle("Success of learning the preferences hierarchy")
 
# Lineplot

 data %>% 
   group_by(ambiguousCount,group) %>%
   summarise(mean = mean(success), sd = sd(success), n = n()) %>%
   mutate(se = sd / sqrt(n),
          lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
          upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>%
   ggplot(aes(ambiguousCount,mean, group = group)) +
   geom_line(aes(color=group)) +
   geom_point(aes(color = group)) +
   ylab("Learning success") +
   xlab("Number of ambiguous utterances per block") +
   scale_color_manual(values = c("deepskyblue","grey"))+
   ggtitle("Learning the preferences hierarchy") +
   theme_minimal() +
   theme(legend.position="right",
         plot.title = element_text(size=9),
         axis.title.x = element_text(size=8),
         axis.title.y = element_text(size=8))
   ggsave("X9_Plots/success_plot.pdf", width = 3.5, height = 2.5, unit = "in", dpi = 300)
