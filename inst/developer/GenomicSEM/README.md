# Manual GenomicSEM / GSEM developer checks

These scripts are **not** part of `tests/testthat` and are **not** run on CRAN or GitHub Actions.

## Why separate?

- Comparing to **GenomicSEM** requires installing that package from GitHub and often a **pinned older lavaan** (current lavaan S3/`fitted` behaviour breaks GenomicSEM’s stack).
- That install is slow/flaky on CI and is not needed to validate umx’s own OpenMx WLS rails.
- umx unit coverage of GSEM helpers/models can live in `tests/testthat` without GenomicSEM; keep **cross-package** parity here.

## When to run

On a maintainer machine, after installing:

```r
# pin as needed for GenomicSEM compatibility (version will drift — check their README)
# remotes::install_version("lavaan", version = "0.6-17")
# remotes::install_github("GenomicSEM/GenomicSEM")
source("inst/developer/GenomicSEM/test_GSEM_umxGSEM_on_Psych_LDSC.R")
# ... or other test_GSEM_*.R scripts in this folder
```

Toy SNP subsets (`SCZ_subset.txt`, etc.) are for local `umxGSEM_sumstats` / `umxGSEM_GWAS` smoke only.

## Caching note

If you ever re-enable GenomicSEM on CI, use `r-lib/actions/setup-r-dependencies` caching (default) **and** pin both GenomicSEM and lavaan versions explicitly — do not install “latest lavaan + latest GenomicSEM” unpinned.
