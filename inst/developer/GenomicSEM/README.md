# Manual GenomicSEM / GSEM developer checks

These scripts are **not** part of `tests/testthat` and are **not** run on CRAN or GitHub Actions.

## Why separate?

- Comparing to **GenomicSEM** requires installing that package from GitHub and often a **pinned older lavaan** (current lavaan S3/`fitted` behaviour breaks GenomicSEM’s stack).
- That install is slow/flaky on CI and is not needed to validate umx’s own OpenMx WLS rails.
- umx unit coverage of GSEM helpers/models can live in `tests/testthat` without GenomicSEM; keep **cross-package** parity here.

## CI goldens (no GenomicSEM required)

P0 regression tests live in `tests/testthat/test_GSEM_goldens.R` with fixtures under
`tests/testthat/fixtures/gsem/`:

| Fixture | Role |
|---------|------|
| `commonfactor_psych_golden.rds` | Psych_LDSC DWLS CF coefs + fit |
| `expand_snp_golden.rds` | `xmu_gsem_expand_snp` S/V algebra |
| `gwas_toy_snps.rds` | 10-SNP sumstats table |
| `gwas_golden.csv` | expected `est`/`se`/`status`/`se_source` |

Copies also under `_snaps/` for human inspection.

## Optional GenomicSEM parity

```r
# pin lavaan if needed for GenomicSEM
# remotes::install_github("GenomicSEM/GenomicSEM")
Sys.setenv(UMX_TEST_GENOMICSEM = "1")
# optional: Sys.setenv(UMX_WRITE_GENOMICSEM_SNAP = "1")
testthat::test_file("inst/developer/GenomicSEM/test_GSEM_optional_GenomicSEM_parity.R")
```

## When to run other scripts

```r
source("inst/developer/GenomicSEM/test_GSEM_umxGSEM_on_Psych_LDSC.R")
# ... or other test_GSEM_*.R scripts in this folder
```


Toy SNP subsets (`SCZ_subset.txt`, etc.) are for local `umxGSEM_sumstats` / `umxGSEM_GWAS` smoke only.

## Caching note

If you ever re-enable GenomicSEM on CI, use `r-lib/actions/setup-r-dependencies` caching (default) **and** pin both GenomicSEM and lavaan versions explicitly — do not install “latest lavaan + latest GenomicSEM” unpinned.
