tmp1 = data.frame(r = c(.4, .4, .4), type = rep("No confounding"      , 3), N = 100, ci.lower = c(NA), ci.upper = c(NA), p = c(.01),group = c("Pop", "DZ", "MZ"))
tmp2 = data.frame(r = c(.4, .2, .0), type = rep("Complete confounding", 3), N = 100, ci.lower = c(NA), ci.upper = c(NA), p = c(.01),group = c("Pop", "DZ", "MZ"))
tmp3 = data.frame(r = c(.4, .3, .2), type = rep("Partial confounding" , 3), N = 100, ci.lower = c(NA), ci.upper = c(NA), p = c(.01),group = c("Pop", "DZ", "MZ"))
tmp  = rbind(tmp1, tmp2, tmp3)
tmp$group = factor(tmp$group, levels=c("Pop", "DZ", "MZ"))
tmp$type  = factor(tmp$type , levels=c("No confounding", "Complete confounding", "Partial confounding"))

bar = ggplot(tmp, aes(x = type, y = r, fill = group))
bar = bar + geom_bar(position = position_dodge(), stat = "identity", size = .1, colour = "black") # thin black outline
bar = bar + geom_errorbar(aes(ymin = ci.lower, ymax = ci.upper), size = .3, width = .2, position = position_dodge(.9))
bar = bar + xlab("Zygosity") + ylab("Correlation")
bar = bar + ggtitle(paste0("The Effect of ", selVar, " on ", var2)) + theme_bw()

# bar = bar + scale_y_continuous(breaks = 0:20*4)
# Legend label, use darker colors
bar = bar + scale_fill_hue(name= "Group", breaks= c("Pop", "MZ", "DZ"), labels= c("Unselected", "DZ discordant", "MZ discordant"))
print(bar)
