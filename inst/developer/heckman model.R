# =======================================================================
# = Selection modeling Monte Carlo study Muthen-Joreskog (1983), p. 146
# = with data generated similar to Model 1, p. 158 y missing if u=0
# =======================================================================
http://www.statmodel.com/download/Heckman.pdf

montecarlo:
names = y u x;
nobs = 4000;
nreps = 100;
categorical = u; # u = 1 if y observed generate = u(1 p);
missing = y; model population:

data(myFADataRaw, package="OpenMx")
manifests = names(myFADataRaw)
myFADataRaw = myFADataRaw[, manifests]
latents   = c("G")
m1 <- mxModel("m1", type="RAM",
	manifestVars = manifests,
	latentVars   = latents,
	# Factor loadings
	mxPath(from = latents, to = manifests),
	mxPath(y on x*1),
	mxPath([y*0]),
	mxPath(y*1),
	mxPath(f by y@1 u*-1), # gives residual corr = -0.5 f@1)
	mxPath(u on x*-1),
)
m1 = umxRun(m1, setLabels = T, setValues = T)
umxSummary(m1)

model missing:
# binary y = 1 denotes missing on continuous 
y [y@-15]; # probability one of missing on y if u = 0
y on u@30; # probability zero of missing on y if u=1
analysis: estimator = mlr; link = probit; process = 8;
model:
y on x*1 (g);
[y*0];
y*1 (v);
f by y@1
u*-1 (cov); # gives -0.5 res. correlation f@1;
u on x*-1;
model constraint:
new (corr*-.5);
corr = cov/(sqrt(g*g+v)*sqrt(cov*cov+1));
output: tech8 tech9;


# =======================
# = original Mplus code =
# =======================

# title:
# Selection modeling Monte Carlo study Muthen-Joreskog (1983), p. 146
# with data generated similar to Model 1, p. 158 y missing if u=0
# montecarlo:
# names = y u x;
# nobs = 4000;
# nreps = 100;
# categorical = u; ! u = 1 if y observed generate = u(1 p);
# missing = y; model population:
# x@1;
# y on x*1;
# [y*0];
# y*1;
# f by y@1 u*-1; ! gives residual corr = -0.5 f@1;
# u on x*-1;
# model missing:
# ! binary y = 1 denotes missing on continuous
# y [y@-15]; ! probability one of missing on y if u = 0
# y on u@30; ! probability zero of missing on y if u=1
# analysis: estimator = mlr; link = probit; process = 8;
# model:
# y on x*1 (g);
# [y*0];
# y*1 (v);
# f by y@1
# u*-1 (cov); ! gives -0.5 res. correlation f@1;
# u on x*-1;
# model constraint:
# new (corr*-.5);
# corr = cov/(sqrt(g*g+v)*sqrt(cov*cov+1));
# output: tech8 tech9;
