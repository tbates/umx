<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Text formatting}
-->




# Text formatting reference sheet

Within roxygen tags, you use `.Rd` syntax to format text. This vignette shows you examples of the most important commands. The full details are described in [R extensions](http://cran.r-project.org/doc/manuals/R-exts.html#Marking-text).

## Character formatting

* `\emph{italics}`

* `\strong{bold}`
\bold{Build and Run a Model}

\code{\link{umxLabel}}                Add labels to all parameters of a model

\code{\link{umxStart}}                Set sensible start values

\code{\link{umxRun}}                  Run a model, adding saturated models if necessary, optionally adds labels and starts

\code{\link{umxLatent}}               Create a latent variable, taking care of reflective/formative distinctions automagically

\code{\link{umxSaturated}}            Create saturated and independence models so that reporting works. (called from umxRun for you)

\bold{Get reports of model results}

\code{\link{umxSummary}}             Compact report of model fit

\code{\link{umxPlot}}                Create a Path Diagram from your model!

\code{\link{umxCompare}}             Compare one or more models

\code{\link{umxCI}}                  Add and report FIML Confidence Intervals for a model.

\code{\link{extractAIC.MxModel}}     Report the AIC for a model.



\bold{Updating & Modifying}

\code{\link{umxReRun}}                Re-run a model with changes

\code{\link{umxEquate}}               Equate parameters in a model by label

\code{\link{umxDrop1}}                Compute a table of fits based on dropping single paths

\code{\link{umxAdd1}}                 Compute a table of fits based on adding paths to the model

\code{\link{umxMI}}                   Compute modification indices for RAM models

\code{\link{umxGetParameters}}        A regex enabled version of \code{\link{omxGetParameters}}

\bold{High-level reporting}

\code{\link{umxUnexplainedCausalNexus}} Compute the change in an outcome variable that flows from a change in an input



\bold{Utilities}

\code{\link{umxUpdateOpenMx}}         umxUpdateOpenMx

\code{\link{umxStandardizeModel}}     Standardize a RAM Model

\code{\link{umxReportTime}}           Show how long a model took to run

\code{\link{umxHetCor}}               Calculate HeteroChoric correlations

\code{\link{umxJiggle}}               Jiggle values in a list

\code{\link{umx_has_CIs}}             Utility function to ask a model if it has CIs

\code{\link{umx_is_MxModel}}            Utility function to test if an object is an OpenMx model

\code{\link{umx_is_RAM}}           Utility function to test if an object is an OpenMx RAM model
 
\code{\link{umx_is_Ordinal}}         Utility function to test if an object is an ordinal variable

\code{\link{umx_is_cov}}              Test if data is a covariance matrix

\code{\link{umx_find_object}}         Find an OpenMx (or other) object by name

\code{\link{umx_grep_labels}}         Find labels (SPSS-style) which match a regular expression. Useful.

\code{\link{umxCov2cor}}              Convert a covariance matrix to a correlation matrix, guaranteeing the upper and lower triangles are exact equal


\code{\link{umx_reorder}}             Reorder the variables in a covariance matrix (including dropping variables)

\code{\link{umxLower2full}}          Take a lower matrix and populate the upper triangle

\bold{Non-SEM helpers}

\code{\link{umx_swap_a_block}}        Swap places with one block of data to another

\code{\link{umx_move_file}}       Flexibly move files on the filesystem

\code{\link{umx_rename_file}}      Flexibly rename files on the filesystem

\code{\link{umx_greater_than}}        A version of %>% which excludes NA

\code{\link{umx_less_than}}           A version of %<% which excludes NA

\code{\link{umx_cor}}                 Report correlations and their p-values

\code{\link{print.dataframe}}         Print dataframe in readable form by supressing NAs, rounding numbers, and presenting zeros as "0"

\code{\link{umxAnovaReport}}          Format an anova model into an APA-style report

\code{\link{Stouffer.test}}           Run a Stouffer.test (meta-analytic p-value)

\code{\link{umx_APA_pval}}            Utility function to format a p-value for APA (three decimals, "<.001" if under that)

\code{\link{umx_round}}               Control rounding in ways of more use to scientists than computers

\code{\link{umxDescriptives}}         Utility function to generate participant section for an APA-style article.

\code{\link{umx_pp33.Rd}}             Reproducibility function: Use non-central distribution: likelihood of a result given the effect was true
