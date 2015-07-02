#R Code behind Figure 2 in DataColada[39] "Power Naps"
#www.datacolada.org
#Written by:Uri Simonsohn
#uws@wharton.upenn.edu
#Last update: 2015 06 22
#
#Please contact Uri directy first if you spot any errors.

library(pwr)

#' nequiv3
#'
#' Function to find the betweenp-subject equivalent to a within-subject manipulation for a given correlation across measures.
#'
#' @param n The sample size (# of participants)
#' @param r test-retest reliability (the correlation across measures)
#' @param power The power of the test using a between-subject design
#' @return - c(between-n, between-power, within-n equivalent, within-power)
#' @export
#' @family replication
#' @family power
#' @references - \url{}
#' @examples
#' # ===================================================================
#' # = Example 1. Verify is equivalent to twice sample-size when r = 0 =
#' # ===================================================================
#'   nequiv3(n = 20,r = 0, power = .5)
#'   nequiv3(n = 33,r = 0, power = .7)
#'   nequiv3(n = 41,r = 0, power = .33)
#' 
#' # ====================================================================
#' # = Example 2. If r = .5, sample is n = 53 regardless of effect size =
#' # ====================================================================
#' nequiv3(n = 20,r = .5, power = .5)
#' nequiv3(n = 20,r = .5, power = .25)
#' nequiv3(n = 20,r = .5, power = .75)
nequiv3 <- function(n, r, power){
  df = 2 * n - 2                                # d.f. from between-subject est
  d.true = pwr.t.test(n = n,power = power)$d    # effect size giving a between-subject design the desired power
  tc = qt(.975,df = 4*n - 2)                    # critical t-value
  d.within = d.true/sqrt(1 - r^2)
  ncp = sqrt(n/2) * d.true                      # noncentrality parameter for between subject
  ncp.within = d.within * sqrt(n)
  p.within = 1 - pt(tc, df = 4*n - 2, ncp = ncp.within) + pt(-tc, df = 4*n - 2, ncp = ncp.within) # power with covariate
  n.within = round(pwr.t.test(power=p.within,d=d.true)$n)  # n yielding an effect-size the obtained within subject power
  return(c(n, power, n.within, round(p.within, 3))) # return between n, betwen power, within n equivalent, within power.
}

# Now generate numbers behind figure in blogpost, all n equivalents for r=.0 to .095

# Set of test-retest correlations to consider
rs = seq(from = 0, to = .95, by = .05)
# Setup loop
i = 1                        # counter
res20=matrix(nrow=20,ncol=4) # store results when power=20%
res70=matrix(nrow=20,ncol=4) # store results when power=70%
# Loop over possible r() values
for (r in rs) {
  res20[i,] = nequiv3(n = 20, r = r, power = .2)  # Save n equivalent for 20% power
  res70[i,] = nequiv3(n = 20, r = r, power = .7)  # now 70%
  i=i+1
}

# Ugly chart, done for 20%, but same chart for 70%
plot(rs, res20[,3], type = 'b', ylim = c(0,200))
text(rs, res20[,3] + 10, res20[,3])
abline(h = 20)

# Copy-paste onto excel to make nicer chart easily
# R Code behind Figure 1 in DataColada[39] "Power Naps"
# www.datacolada.org
# Written by:Uri Simonsohn (uws@wharton.upenn.edu)
# Last update: 2015 06 17


#' nequiv
#'
#' Function that finds between subject equivalent to mixed design with n=x participants,
#' subtracting baseline before treatment, with given correlation for test,retest reliability;
#'
#' @param n The number of participants per cell
#' @param r test-retest reliability (the correlation across measures)
#' @param power The power of the test using a between-subject design
#' @return - c(between-n, between-power, within-n equivalent, within-power)
#' @export
#' @family replication
#' @family power
#' @references - \url{}
#' @examples
#' # Example 1. If r=.5 power and n is the same regardless of power
#' nequiv(n=20,r=.5,power=.06)
#' nequiv(n=20,r=.5,power=.76)
#' # Example 2. if r=.75 as if sample (nearly) doubles regardless of effect size or sample size
#' nequiv(n=20,r=.75,power=.5)
#' nequiv(n=20,r=.75,power=.25)
#' nequiv(n=20,r=.75,power=.75)
#' nequiv(n=50,r=.75,power=.5)
#' nequiv(n=100,r=.75,power=.25)
#' nequiv(n=200,r=.75,power=.75)

nequiv <- function(n, r, power){
	library(pwr) # require power library
    df = 2 * n - 2                                            # d.f. from between subject est
    d.true=pwr.t.test(n=n,power=power)$d                      # effect size that gives between subject design desired power
    tc=qt(.975,df=df)                                         # critical t-vale for p=.05 two-sided, given sample size
    ncp=sqrt(n/2)*d.true                                      # noncentrality parameter for between subject
    ncp.w=ncp*sqrt((1/(2-2*r)))                               # noncentrality parameter for within subject (computed by reducing SE of statistic based on r)
    pw=1-pt(tc,df=2*n-2,ncp=ncp.w)+pt(-tc,df=2*n-2,ncp=ncp.w) # power subtracting baseline
    nw=round(pwr.t.test(power=pw,d=d.true)$n)                 # find n that would give that effect size the obtained within subject power
    cat("\nTest-retest: r=",r, "(entered by user)")
    cat("\nActual sample size n=",n," (entered by user)")
    cat("\nPower=",power," (entered by user)")
    cat("\nAs if sample had been n=",nw," (calculated)")
    cat("\nPower obtained with subtraction=",round(pw,3)," (calculated)\n\n")
    return(c(n,power,nw,round(pw,3)))                         #return: actual sample size, power for that of between-subject, within n equivalent, within-power.    
}
  
# LET'S MAKE THE FIGURE NOW.
# Set of test-retest correlations to consider, between 0 and .95 (post ended up plotting up to r=.9)
rs = seq(from=0,to=.95,by=.05)
res20 = matrix(nrow=20,ncol=4)     #store results when power=20%
res70 = matrix(nrow=20,ncol=4)     #store results when power=70%
# Setup loop i = 1 #counter
# Loop over possible r() values
i = 1  #counter
for (r in rs){
  # for every value of r in the set to consider
  res20[i,] = nequiv(n = 20, r = r, power = .2)  #Save nequivalent() for power=20%
  res70[i,] = nequiv(n = 20, r = r, power = .7)  #now 70%
  i = i + 1
}

#Ugly chart
plot(rs,res20[,3],type='b',ylim=c(0,200), 
      main='Subtracting baseline when n=20 is like running...', #this is for 20% power
      xlab='test-retest r',
      ylab='equivalent sample size'
) 
lines(rs,res70[,3],col='red')             #in red for 70%, same exact line.
text(rs,res20[,3]+10,res20[,3])           #add value labels
abline(h=20)                              #add horizonatl line at n=20
  
# Copy paste onto excel the matrix res[20] to make nicer chart easily





