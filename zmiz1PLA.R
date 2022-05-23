setwd("~/Desktop/ZMIZ1 PLA")
zmiz1PLA<-read.csv('ZMIZ1plaData.csv')

library(ggpubr)



zmiz1PLA<-zmiz1PLA[!zmiz1PLA$Target.2 == "FOXK1",]


zmiz1PLA$Cell.line<-relevel(zmiz1PLA$Cell.line,   "T47D"   )
zmiz1PLA$Cell.line<-relevel(zmiz1PLA$Cell.line,   "MCF7"   )


library(rstatix)


#If the Shapiro-Wilk Test p-value is greater than 0.05, the data is normal. If it is below 0.05, the data significantly
# deviate from a normal distribution therefore we use a there wilcox test.
shapiro.test(zmiz1PLA$Dots.Nuclea)

#repeating on individual data groups, givbes mixed results. Majority not normally distributed
shaprio.test<-  zmiz1PLA %>%
  group_by(Cell.line, Target.2) %>%
  shapiro_test(Dots.Nuclea) 
shaprio.test

# Add p-values onto the box plots
stat.test <- zmiz1PLA %>%
  group_by(Cell.line, Target.2) %>%
  wilcox_test(Dots.Nuclea ~ Treatment, alternative = "t" ) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")




stat.test <- stat.test %>%
  add_xy_position(fun = "mean_sd", x = "Cell.line")
  
p<-ggboxplot(zmiz1PLA, x = "Cell.line", y = "Dots.Nuclea",
             color = "Black", fill = "Treatment", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
             facet.by="Target.2")

p +   stat_pvalue_manual(
      stat.test, tip.length = 0.02,
      step.increase = 0.05,
      label = "{p.adj.signif}",
      hide.ns = TRUE
    ) + xlab("Cell Line") +ylab("Dots/Nuclea")
