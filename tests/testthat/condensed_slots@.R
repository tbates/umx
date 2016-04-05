library(umx)
options('mxCondenseMatrixSlots'= TRUE)
a = mxMatrix(name="a", "Full", 3,3, values=1:9)
# Code with @ accessor will fail to do the expected thing,
# but program moves on without error
a$labels
#      [,1] [,2] [,3]
# [1,] NA   NA   NA
# [2,] NA   NA   NA
# [3,] NA   NA   NA
a@labels
#      [,1]
# [1,] NA

# $ accessors work properly expand if necessary on get and set
a$labels[2,3] = "bob"
a$labels

a = mxMatrix(name="a", "Full", 3,3, values=1:9)
# @ fails with error
# Error in `[<-`(`*tmp*`, 2, 3, value = "bob") : subscript out of bounds 
a@labels[2,3] = "bob"