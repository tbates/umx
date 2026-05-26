library(umx)
data(demoOneFactor)
latents  = c("G")
manifests = names(demoOneFactor)
m1 = mxModel("fact", type="RAM", manifestVars=manifests, latentVars=latents,
	mxPath(latents  , to = manifests),
	mxPath(manifests, arrows = 2),
	mxPath(latents  , arrows = 2, free = FALSE, values = 1),
	mxData(cov(demoOneFactor), type = "cov", numObs=500)
)
m1 = umxRun(m1)
# Initial run, start2LL should be NULL, so no message about change.

# Modify model to make it worse/better
# umxModify calls xmu_safe_run_summary internally? Yes.
# But let's use umxRun on a run model.
m1_rerun = umxRun(m1)
# Should see "Change in -2LL = 0" (or close to 0)

# Modify manually to force a change
m3 = m1
m3$S$values[1,1] = 0.5 # Change start value
m3 = umxRun(m3)
# m3 has output from m1 (it is a copy). So start2LL is m1's fit.
# New run will find optimum (which should be same as m1 if only start value changed and convex).
# So change 0.

# What if we fix a parameter?
m4 = omxSetParameters(m1, labels="G_to_x1", free=FALSE, values=.1)
# m4 still has m1's output? Yes, omxSetParameters preserves output slot?
# Let's assume yes.
m4 = umxRun(m4)
# Should show large change.
if(!is.null(m4$output$changeLL)){
    message("Test passed: changeLL found in output")
    print(m4$output$changeLL)
} else {
    message("Test failed: changeLL not found in output")
}
