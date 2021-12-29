data(twinData)
df = umx_scale_wide_twin_data(varsToScale= c("ht", "wt"), sep = "", data= twinData, twins = 1:2)
df$xDiff = df[, "ht1"] - df[, "ht2"]
df$yDiff = df[, "wt1"] - df[, "wt2"]
df$sim = df$yDiff - df$xDiff
p = umxDiffTwin(x="ht", y="wt", r2pos=c(x=-.4,y=1), data = df[abs(df$sim) < .1, ], sep = "")
p + labs(x="Difference in rain (T1 - T2)", y="Difference in mud (T1 - T2")
# plot(yDiff~ xDiff, data = df[abs(df$sim) < .1, ])
