# My reasoning is that a long rich tail might create an artifact of higher heritability, and because older people were born in earlier times, and were therefore lower SES on average, the age=SES correlation will work against the prediction.

### How does school raise IQ?
# Developing intuition: System I intuition doesn't require reasoning. A good intuition, therefore, decouples ability from g. If you "get the procedure"
# Procedural knowledge: This is a mechanism for intuition. 

fit1 = mxModel("BatAndBallProblem",
	mxMatrix("Full", nrow = 1, ncol = 1, free= T, name = "bat"), 
	mxMatrix("Full", nrow = 1, ncol = 1, free= T, name = "ball"), 
	mxConstraint((bat + ball) == 1.10, name = "total"),
	mxConstraint((bat - ball) == 1.00 , name = "batOneMoreThanBall"),
	# OpenMx works by finding values for each free parameter which jointly make the model fit best
	# This "Optimization" means that some value has to provided as a target: the Objective
	# Typically that would be the predicting the variances and covariances in a dataframe, 
	# but it can also be an algebra. Here's an example where we just solve some simple constraints: the 
	mxAlgebraObjective(algebra = "ball", numObs = NA, numStats = NA)
)
fit1 = mxRun(fit1)
mxEval(list(bat = bat, ball = ball), fit1)


# This version runs, and returns bad values (because bat and ball are fixed at 0 by default.
# 
# Even though the constraints are not met, there's not error. Isn't that wrong?

fit2 = mxModel("Bad_BatAndBall",
	mxMatrix("Full", nrow = 1, ncol = 1, name = "bat" ), 
	mxMatrix("Full", nrow = 1, ncol = 1, name = "ball"), 
	mxConstraint((bat + ball) == 1.10 , name = "total"),
	mxConstraint((bat - ball) == 1.00  , name = "batOneMoreThanBall"),
	mxAlgebraObjective(algebra = "ball", numObs = NA, numStats = NA)
)
fit2 = mxRun(fit2); mxEval(list(bat = bat, ball = ball), fit1)

demo(OneFactorModelDemo)

 motherVIQ_m = c("m1", "m2")
  childVIQ_m = c("c1", "c2")
read2Child_m = c("r1", "r2")
manifests = c(read2Child_m, motherVIQ_m, childVIQ_m)
latents   = c("Read2Child", "MothersVIQ", "ChildsVIQ")

cors = c(
1,		.56,	.38,	.34,	.50,	.50,
.56,	  1,	.43,	.38,	.58,	.58,
.38,	.43,	  1,	.72,	.43,	.43,
.34,	.38,	.72,	  1,	.38,	.38,
.50,	.58,	.43,	.38,	  1,	.64,
.50,	.58,	.43,	.38,	.64,	  1)

readingNexus = matrix(cors, nrow = 6, byrow = T,  dimnames = list(r = manifests, c = manifests))

ma <- mxModel("LoehlinPage", type="RAM",
	manifestVars = manifests,
	latentVars   = latents,
	# Build Latents from manifests
    mxPath(from = "MothersVIQ", to = motherVIQ_m),
    mxPath(from = "ChildsVIQ"  , to = childVIQ_m),
    mxPath(from = "Read2Child", to = read2Child_m),

	# Connect Latents
    mxPath(from = "MothersVIQ" , to = "Read2Child"),
    mxPath(from = "Read2Child" , to = "ChildsVIQ"),
    mxPath(from = "MothersVIQ" , to = "ChildsVIQ"),

    mxPath(from = manifests, arrows = 2), # manifest residuals 
	mxPath(from = latents, arrows = 2, free = F, values = 1), # latents fixed@1
	mxData(readingNexus, type = "cor", numObs = 500)
)
ma = umxRun(ma, setLabels = T, setStart = T)
umxSummary(ma)
# Let's free the variance on read 2 child and Child's VIQ: Absolutely no effect (of course?)
ma_free <- mxModel(ma, mxPath(from = c("Read2Child", "ChildsVIQ"), arrows = 2, free = T, values = 1), name="freeLatVar")
ma_free = umxRun(ma_free, setLabels = T, setStart = T)
umxSummary(ma_free)
mxCompare(ma_free, ma)
 
umxGraph_RAM(ma, std = T, precision = 3, dotFilename = "name",  pathLabels = "none", showFixed = F)
umxGraph_RAM(ma_free, std = T, precision = 3, dotFilename = "name",  pathLabels = "none", showFixed = F)


# ==================
# = John's model B =
# ==================
mb <- mxModel("LoehlinPage", type="RAM",
	manifestVars = manifests,
	latentVars   = latents,
	# Build Latents from manifests
    mxPath(from = "MothersVIQ", to = motherVIQ_m),
    mxPath(from = "ChildsVIQ"  , to = childVIQ_m),
    mxPath(from = "Read2Child", to = read2Child_m),

	# Connect Latents
    mxPath(from = "MothersVIQ", to = "Read2Child"),
    mxPath(from = "Read2Child", to = "ChildsVIQ"),
    mxPath(from = "ChildsVIQ" , to = "MothersVIQ"),

    mxPath(from = manifests, arrows = 2), # manifest residuals 
	mxPath(from = latents, arrows = 2, free = F, values = 1), # latents fixed@1
	mxData(readingNexus, type = "cor", numObs = 500)
)
mb = umxRun(mb, setLabels = T, setStart = T)
umxSummary(mb)
mxCompare(ma, mb)



digraph G {
	splines="FALSE";
	/* Entities */
	MR  [label="Mother\nstory reading" , shape="circle"]
	MV  [label="Mother's\nverbal ability", shape="circle"]
	CV  [label="Child's\nverbal ability" , shape="circle"]

	/* Relationships */
	MV -> {m1 m2}
	MR -> {r1 r2}
	CV -> {c1 c2}

	MV ->  MR[label=".60"]
	MV ->  CV[label=".09"]
	MR ->  CV[label=".85"]
	/* Ranks */
	{ rank=same; MR MV;};
}


# html based slider tutorial for BGA?
# http://statgen.iop.kcl.ac.uk/bgim/tuts/tut_mult.html

# http://t.co/4Q2bQgrmAv
# 
 
# Great to see the idea of truth getting a look in! I would rank the mis-funding of science in the top-10 solvable problems with the biggest potential pay off. Think of the productivity (think 5% increase in real-terms income/yr) and sheer wonder that optimised science funding could create. Right alongside optimising education in terms of effect on humanity!
# 
# Tim Bates @Roger Millsap: The true ones are the ones that replicated. Ioannidis also showed that almost all candidate studies were wrong, and almost all GWAS 10e-8 hits were true. He even recommended lowering the GWAS hit criterion as it seemed, in practice to generate too few false positives...




digraph G {
	splines="FALSE";
	/* Entities */
	MR  [label="Mother\nstory reading" , shape="circle"]
	MV  [label="Mother's\nverbal ability", shape="circle"]
	CV  [label="Child's\nverbal ability" , shape="circle"]

	/* Relationships */
	MV -> {m1 m2}
	MR -> {r1 r2}
	CV -> {c1 c2}

	MV ->  MR[label=".60"]
	MV ->  CV[label=".09"]
	MR ->  CV[label=".85"]
	/* Ranks */
	{ rank=same; MR MV;};
}

digraph H {
	splines="FALSE";
	/* Entities */
	MR  [label="Mother\nstory reading" , shape="circle"]
	MV  [label="Mother's\nverbal ability", shape="circle"]
	CV  [label="Child's\nverbal ability" , shape="circle"]

	/* Relationships */
	MV -> {m1 m2}
	MR -> {r1 r2}
	CV -> {c1 c2}

	MV -> MR[label=".10"]
	MV -> CV[label=".60"]
	CV -> MR[label=".84"]
	/* Ranks */
	{ rank=same; MR MV;};
}

digraph I {
	splines="FALSE";
	/* Entities */
	MR  [label="Mother\nstory reading" , shape="circle"]
	MV  [label="Mother's\nverbal ability", shape="circle"]
	CV  [label="Child's\nverbal ability" , shape="circle"]

	/* Relationships */
	MV -> {m1 m2}
	MR -> {r1 r2}
	CV -> {c1 c2}

	MR -> MV[label=".32"]
	CV -> MV[label=".30"]
	MR -> CV[label=".90"]
	/* Ranks */
	{ rank=same; MR MV;};
}
