ncol = 4 # typically rank 1:9
n = ncol^2
values = matrix(round(rnorm(n),2), nrow = ncol, byrow = T)
labs = paste0("r", rep(1:ncol, each = ncol), "c", rep(1:ncol, times = ncol))
labels = matrix(labs, nrow = ncol, byrow = T)
free = matrix(rbinom(n= n, size = 1, prob = .5), nrow = ncol, byrow = T)

myObj = list(values = values, labels = labels, free = free)


# use formattable, htmlwidgets, and htmltools
library(formattable)
library(htmltools)
library(htmlwidgets)

# purrr could ease some of this pain
#  but wanted to avoid dependencies
formattable(
  matrix(
    formatter(
      "span",
      "data-toggle" = "tooltip",
      # use the label and free to add a simple title
      #  this can be infinitely styled and refined
      "title" = mapply(
        function(value,label,free) {
          paste0(label)
        },
        myObj$values, myObj$label, myObj$free
      ),
      # color the background of cells based on free
      "style" = mapply(
        function(value,free) {
          if(free==1) color = "pink"
          if(free==0) color = "white"
          paste0("display:block; background:",color,";")
        },
        myObj$values, myObj$free
      ),
      # values will be the text in the cells
      x~x
    )(myObj$values),
    # put back in a matrix of ncol=4
    ncol=4
  )
)

from =  c("mpg", "cyl", "disp", "hp")
forms =  "lat"

umxPath(from, forms = forms, hasMeans = TRUE)

a = unlist(umxPath(unique.bivariate = from))
b = unlist(umxPath(from, to = forms, firstAt = 1))
c = mxPath(from = forms, arrows = 2, free = FALSE, values = 0)
d = mxPath(from = "one", to = forms, free = FALSE, values = 0)
e = mxPath(from = from, arrows = 2, free = TRUE, values = 1, labels = labels, lbound = 0, ubound = ubound)
f = mxPath(from = "one", to = from, free = TRUE, values = 0, labels = labels, lbound = lbound, ubound = ubound)

m1 <- umxRAM("B", data = mtcars,
		return(list(a, b, c, d, e, f))

)


umxPath( c("mpg", "cyl", "disp", "hp"), forms = "lat", hasMeans = TRUE)

Error: Nice try, you need to add 'mpg' and 'cyl' to either manifestVars or latentVars before you can use them in a path.

In addition: Warning message:
In is.na(object) : is.na() applied to non-(list or vector) of type 'NULL'

names(mtcars)