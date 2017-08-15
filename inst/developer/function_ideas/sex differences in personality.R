library(ggplot2)
pf16 = c("Warmth", "Emotional\nstability", "Dominance", "Liveli-\nness", "Rule-\nConsciousness", "Social\nBoldness", "Sensitivity", "Vigilance", "Abstract-\nness", "Private\nness", "Appre-\nnhensiveess", "Openness\nto change", "Self-\nreliance", "Perfection\nism", "Tension")
pf16d = c(-.89, .53, .54, -.05, .39, .18, -2.29, .36, -.01, .15, -.6, .21,-.12, .04, -.27)
df16 = data.frame(Dimension = pf16, Sex_difference = pf16d)

df16 = df16[order(df16$Sex_difference),]
df16$Dimension = reorder(df16$Dimension, 1:15)
p <- qplot(Dimension, Sex_difference, data = df16)
p = p + ggtitle("Sex differences in personality") + 
p = p +	geom_hline(yintercept = 0, colour = "blue"); p
p = p + ylim = c(-2.5, 1), ylab= "Sex difference (negative = female greater than male)", xlab = "Personality Dimension"
p
Alex Harper