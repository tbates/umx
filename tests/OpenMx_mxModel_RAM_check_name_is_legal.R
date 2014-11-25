require(OpenMx)
omxCheckError(mxModel("A", type = "RAM"), "The name 'A' is not valid for a type='RAM' model:
 Pick something other than  reserved names  'A', 'S', 'F', and 'M'") # should error.

omxCheckError(mxModel("S", type = "RAM"), "The name 'S' is not valid for a type='RAM' model:
 Pick something other than  reserved names  'A', 'S', 'F', and 'M'") # should error.

omxCheckError(mxModel("M", type = "RAM"), "The name 'M' is not valid for a type='RAM' model:
 Pick something other than  reserved names  'A', 'S', 'F', and 'M'") # should error.

omxCheckError(mxModel("F", type = "RAM"), "The name 'F' is not valid for a type='RAM' model:
 Pick something other than  reserved names  'A', 'S', 'F', and 'M'") # should error.
