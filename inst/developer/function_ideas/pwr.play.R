P0 = c(11/15,  4/15)
P1 = c( 2/15, 13/15)
ES.w1(P0,P1)
pwr.chisq.test(w=ES.w1(P0,P1), N = 30, df=(2-1)*(2-1))

pwr.chisq.test: take df automatically?

pwr.chisq.test(odds= c(11, 15), c(2, 15), N = 10, df=(2-1))

pwr.chisq.2by2.test(hitsGrp1 = 11, nGrp1 = 15, hitsGrp2 = 2)

pwr.chisq.2by2.test <- function(hitsGrp1, nGrp1, hitsGrp2, nGrp2 = nGrp1) {
	pwr.chisq.test(N = nGrp1 + nGrp2, df = 1,
		w = ES.w1(c(hitsGrp1/nGrp1, (nGrp1 - hitsGrp1)/nGrp1),
				  c(hitsGrp2/nGrp2, (nGrp2 - hitsGrp2)/nGrp2))
	)
}

umx_time("start")
sims = 10000
pvals = rep(NA, sims)
bvals = rep(NA, sims)
N      = 280
for (i in 1:sims) {
	momIQ  = rnorm(n=N, mean = 100, sd = 15)
	SES    = rnorm(n=N)
	kidPRS = scale(momIQ + rnorm(N, 100, sd=12)) # generate r = ~ .75
	kidIQ  = (.5 * kidPRS) + (.2 * SES) + (.05 * (kidPRS*SES)) + .5* rnorm(N)
	kidIQ  = (kidIQ *15)+100
	# cor.test(~momIQ+kidIQ)
	# hist(kidIQ)
	# plot(kidIQ~(SES*kidPRS))
	tmp = data.frame(kidIQ = scale(kidIQ), kidPRS = kidPRS, momIQ= scale(momIQ), SES= SES)
	m1 = lm(kidIQ ~ kidPRS*SES, data = tmp);
	pvals[i] = anova(m1)$`Pr(>F)`[3]
	bvals[i] = m1$coefficients["kidPRS:SES"]
	# umxAPA(m1, std=T)
}
umx_time("stop")
sum(pvals<.05)/sims
mean(bvals)

tmp = umx_make_TwinData(nMZpairs = 2000, nDZpairs = 2000,  AA = .3, CC = 0, varNames = "var", mean = 0, empirical = TRUE)
mzData = subset(tmp, zygosity == "MZ")
dzData = subset(tmp, zygosity == "DZ")

ace = umxACE(selDVs = "var", sep = "_T", mzData = mzData, dzData = dzData, tryHard = "yes")

momIQ = rnorm(n=1000, mean = 100, sd = 15)
kidPRS = momIQ

tmp= umx_make_raw_from_cov(qm(1, .3| .3, 1), n=10, varNames= c("momIQ", "y"), means=c(100, 0))
cov(tmp)

The power is not for a correlation. It can't be looked up in a table from Cohen.

the model is 

m1 = lm(ChildIQ ~ 1 + 𝛽₀ * momIQ + 𝛽₁ * SES + 𝛽₂ * I(momIQ * SES))

The authors need to simulate, random momIQe and SES, plug in sensible values for beta, then generate data from a population where these parameters hold, and see what proportion of studies generate significant 𝛽₂

I'm not sure what the parameter for 𝛽₂ would be. momIQ is a phenotype, and you'd need to figure out how to link a` ~ .05 in a Purcell model to 𝛽₂ in this linear model.

Doing this for a repeated measures model will be a head ache, but just doing it for a linear model would be a start.

