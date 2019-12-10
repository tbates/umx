library(umx)
library(testthat)

umx_set_silent(TRUE)

roundtrip <- function(modelStr, verbose=FALSE) {
  m1 <- umxLav2RAM(modelStr, autoRun=FALSE, printTab=FALSE, lavaanMode="lavaan")
  lav <- umxRAM2Lav(m1)
  if (verbose) cat(lav)
  m2 <- umxLav2RAM(lav, autoRun=FALSE, printTab=FALSE, lavaanMode="lavaan")
  expect_setequal(colnames(m1$F), colnames(m2$F))
  expect_setequal(rownames(m1$F), rownames(m2$F))
  perm <- match(colnames(m1$F), colnames(m2$F))
  expect_equivalent(m1$F$values[,rownames(m1$F)], diag(nrow(m1$F)))
  expect_equivalent(m2$F$values[,rownames(m2$F)], diag(nrow(m2$F)))
  expect_equal(is.null(m1[['M']]), is.null(m2[['M']]))
  expect_equal(m1$A$free, m2$A$free[perm,perm])
  expect_equal(m1$A$values, m2$A$values[perm,perm])
  expect_equal(m1$A$labels, m2$A$labels[perm,perm])
  expect_equal(m1$S$free, m2$S$free[perm,perm])
  expect_equal(m1$S$values, m2$S$values[perm,perm])
  expect_equal(m1$S$labels, m2$S$labels[perm,perm])
  if (!is.null(m1[['M']])) {
    expect_equal(m1$M$free, m2$M$free[,perm,drop=FALSE])
    expect_equal(m1$M$values, m2$M$values[,perm,drop=FALSE])
    expect_equal(m1$M$labels, m2$M$labels[,perm,drop=FALSE])
  }
  expect_equal(length(m1$algebra), length(m2$algebra))
  if (length(m1$algebra)) {
    expect_setequal(names(m1$algebra), names(m2$algebra))
    for (a1 in names(m1$algebra)) {
      expect_equal(deparse(m1[[a1]]$formula), deparse(m2[[a1]]$formula))
    }
  }
  expect_equal(length(m1$constraints), length(m2$constraints))
  if (length(m1$constraints)) {
    c1 <- sort(unname(sapply(m1$constraints, function(x) deparse(x$formula))))
    c2 <- sort(unname(sapply(m2$constraints, function(x) deparse(x$formula))))
    expect_equal(c1, c2)
  }
}

roundtrip("y~x")
roundtrip("y~1
          x~1")
roundtrip("y~~x")
roundtrip("y =~ a + b + c")
roundtrip("y =~ a + b + c
          x =~ d + e + f
          x~y")
roundtrip("y ~ x1 + 2.4*x2 + x3")
roundtrip("x1 ~ x3_loading*x3")
roundtrip("y ~ x1 + 2.4*x2; s =~ 1*y12 + 2*y13 + 3*y14")
roundtrip("L =~ X1 + X2; L ~ Y")
roundtrip("spatial =~ visual   + cubes    + flags
       verbal  =~ paragrap + sentence + wordm
       speed   =~ addition + counting + straight")
roundtrip(" # Moderated mediation
 gnt ~ a*cb
 INT ~ b1*gnt + b2*cn + b3*cngn + c*cb
 
 indirect := a*b1
 direct := c
 
 ab3 := a * b3
 loCN := a * b1 + ab3 * -0.5
 hiCN := a * b1 + ab3 * 0.5")
roundtrip("x1~b1*x2; B1_sq := b1^2")
roundtrip("
	y ~ b1*x1 + b2*x2 + b3*x3
	# constraints
	b1 == (b2 + b3)^2
	b1 > exp(b2 + b3)")
roundtrip("e1~~n1; e2~~n2; e2+n2 ~ e1; n2 ~ n1")

if (0) {
  #debugging
  modelStr <- "
	y ~ b1*x1 + b2*x2 + b3*x3
	# constraints
	b1 == (b2 + b3)^2
	b1 > exp(b2 + b3)"
  m1 <- umxLav2RAM(modelStr, autoRun=FALSE, printTab=FALSE, lavaanMode="lavaan")
  lavaan::lavaanify(modelStr, ngroups = 1, group.equal = NULL,
                    std.lv = FALSE, auto.fix.first = TRUE, fixed.x = FALSE)
  debug(umxRAM2Lav)
  debug(roundtrip)
}
