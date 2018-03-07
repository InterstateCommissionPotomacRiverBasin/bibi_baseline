library(tidyverse)

test <- tibble(round = c(rep("Round 1", 80), rep("Round 2", 80), rep("Round 3", 80)),
               ibi_score = c(
                 rep(1, 30), rep(2, 30), rep(3, 10), rep(4, 5), rep(5, 5), # Round 1
                 rep(1, 20), rep(2, 20), rep(3, 20), rep(4, 10), rep(5, 10), # Round 2
                 rep(1, 10), rep(2, 10), rep(3, 20), rep(4, 20), rep(5, 20) # Round 3
               ))
kruskal.test(ibi_score ~ as.factor(round), data = test)
table = dunn.test::dunn.test(test$ibi_score, test$round)
table = cbind.data.frame(table$comparisons,table$Z,table$P.adjusted)
table[order(table$`table$P.adjusted`),]


test %>% 
  ggplot(aes(round, ibi_score)) + 
  geom_boxplot() +
  annotate("rect", xmin = 0, xmax = 4, ymin = 0.8, ymax = 2, fill = "red", alpha = 0.25) +
  annotate("rect", xmin = 0, xmax = 4, ymin = 2, ymax = 3, fill = "orange", alpha = 0.25) +
  annotate("rect", xmin = 0, xmax = 4, ymin = 3, ymax = 4, fill = "yellow", alpha = 0.25) +
  annotate("rect", xmin = 0, xmax = 4, ymin = 4, ymax = 5.2, fill = "lightgreen", alpha = 0.25) +
  geom_boxplot() +
  xlab("Round") +
  ylab("IBI Score") +
  #ggtitle("Example") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0.8, 5.2), expand = c(0, 0))  
   
  
