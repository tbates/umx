HS.model <- '  visual =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, 
           data = HolzingerSwineford1939, 
           group = "school")

summary(fit)

To convert lavaan to OpenMx
1. replace "=~" with "->"
2. add the black-box elements "visual <-> visual"
HS.model <- " visual  -> x1@1 + x2 + x3
              textual -> x4@1 + x5 + x6
              speed   -> x7@1 + x8 + x9
			  # Added silently by lavaan (also the @1 above)
			  visual <-> visual 
			  textual<-> textual
			  speed  <-> speed
			  x1     <-> x1
			  x2     <-> x2
			  x3     <-> x3
			  x4     <-> x4
			  x5     <-> x5
			  x6     <-> x6
              x7     <-> x7
			  x8     <-> x8
 			  x9     <-> x9
"
fit <- cfa(HS.model, 
           data = HolzingerSwineford1939, 
           group = "school")

summary(fit)