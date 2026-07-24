# Plan: `mx_make` flight-check, hardening, Rd tutorial

## Goal

Make `umx::mx_make()` the single, bulletproof entry point so that:

1. Local R always runs **this** OpenMx source tree (R + C++ DLL), not a stale CRAN/library path copy.
2. `mx_make("win")` always packages **that same tree** for win-builder.
3. Rd/`roxygen` for OpenMx is one documented command (not ad-hoc `devtools::document` that can muck DESCRIPTION/man).
4. Rd documents a short **flight checklist** (local install → verify → win-builder).

## Flight check (current behaviour — no code yet)

| Intent | Command today | What it actually does | Gap |
|--------|---------------|------------------------|-----|
| Run **latest source** in R | `mx_make()` / `mx_make("install")` | `make install` → `R CMD INSTALL` of `pkg` (default `~/bin/OpenMx`) into the active library | OK if `pkg` is correct; **must restart R** (no `load_all`). No post-install print of `find.package` / mtime / git SHA. |
| CRAN-like install (no NPSOL) | `mx_make("cran-install")` | `make cran-install` | OK |
| Rebuild man pages | `mx_make("Rd")` | `devtools::document(pkgPath)` then **falls through** into `switch()` → **`"Unhandled mx_make target: Rd"`** | **Bug.** Wrong error string (`"win"`). Wrong tool for OpenMx: Makefile says regenerate with `sh util/rox` / `make roxygen` (compile_dll + roxygenize rd only). |
| Win-builder | `mx_make("win")` | `devtools::check_win_devel(pkg = pkgPath)` | Packages **disk tree at `pkg`**, not the installed library. Correct for “send our code”. Does **not** require prior `install`. Uncommitted files are included unless `.Rbuildignore`d. No dirty-tree warning. |
| Binary from releases | `mx_make("GenomicMx")` | `install.OpenMx("GenomicMx")` | Different product (prebuilt), not local source. |
| Verify engine | (manual) | `xmu_openmx_engine_status()` only checks GenomicMx **capability**, not “is this my git tree?” | No `find.package` / source SHA compare. |

### Critical mental model (why last night failed)

- **Installed library** (`find.package("OpenMx")`) = what `library(OpenMx)` runs. Only updated by `mx_make("install")` / `cran-install` / `GenomicMx` / CRAN install.
- **Win-builder tarball** = snapshot of **`pkg` source path** at submit time. Independent of what is installed in R.
- Roxygen mess: OpenMx tracks `man/*.Rd` in git; official regen is `make roxygen` → `util/rox` (not umx-style `devtools::document` alone). Mixing that with DESCRIPTION prep can leave the tree odd; `make install` also runs `util/prep` and `git checkout DESCRIPTION` after install.

### Your flight checklist (after this plan lands)

```r
library(umx)

# 1) Refresh man from roxygen (OpenMx util/rox), if you edited R/ docs
mx_make("Rd")

# 2) Compile + install THIS tree into R's library
mx_make()                 # or mx_make("install")

# 3) New R session (or detach), then verify identity
library(OpenMx)
packageVersion("OpenMx")
find.package("OpenMx")    # must be the library you just installed into
# optional: mtime of libs/OpenMx.so should be ~now

# 4) Send the same source tree to win-builder
mx_make("win")            # check_win_devel on pkg= ~/bin/OpenMx
```

Order for win only (if you do not need local DLL): `Rd` (if docs changed) → commit if you care about clean git → `mx_make("win")`. Install is **not** required for win-builder.

## Proposed code changes (only `R/misc_and_utility.R` + regenerate `man/mx_make.Rd`)

### 1. Fix `what == "Rd"`

- Run OpenMx’s path: `xmu_mx_make_run(pkgPath, "roxygen", ...)` **or** `system2("sh", c("./util/rox"), ...)` from `pkgPath` (prefer Makefile target `roxygen` for consistency).
- **`return(invisible(...))`** so it never falls into `switch`.
- Fix copy-paste error message (`"Rd"` needs devtools/roxygen2, not `"win"`).
- If `make roxygen` is heavy, document that; still prefer it over bare `devtools::document` for OpenMx.

### 2. After successful `install` / `cran-install`

Print a short identity block:

- `find.package("OpenMx", quiet=TRUE)` (may still be old session if package loaded — say so).
- Path of source `pkgPath` + `git rev-parse --short HEAD` if `.git` exists.
- Reminder: restart R, then `packageVersion("OpenMx"); find.package("OpenMx")`.

No new helper file; inline messages only (or tiny internal if already similar).

### 3. `what == "win"` hardening

- Message: “Submitting **source tree** at `pkgPath` (not the installed binary).”
- If git available and dirty: `message` warning (do not stop unless user wants `stopOnDirty` later — default warn only).
- Optional: list Version from DESCRIPTION so the email is identifiable.
- Keep `devtools::check_win_devel(pkg = pkgPath)`.

### 4. Rd tutorial section (roxygen `@description` / `@details`)

Add a **Flight checklist** section in prose:

- Update Rd: `mx_make("Rd")`
- Install local engine: `mx_make()` then restart R; verify `find.package` / version
- Win-builder: `mx_make("win")` (source tree; commit first if you want git-clean tarball)
- Contrast: `mx_make("GenomicMx")` = prebuilt release binary, not local WIP
- Explicit: never use `devtools::load_all` for OpenMx; never assume CRAN OpenMx is your tree

### 5. Help target list

Include `Rd` in `--help` message (currently omitted).

### 6. Out of scope

- Changing OpenMx Makefile / `util/rox`
- Auto-restart of R
- Requiring clean git before win
- umx_make changes
- Re-running win-builder as part of this task

## Names (existing only)

- `mx_make`, `xmu_mx_make_run` — no new exported names.
- No new helpers unless a 3-line git dirty check is inlined.

## Verification

1. Static: `Rd` branch returns; help lists `Rd`.
2. Manual (user): `mx_make("Rd")` exits 0 on OpenMx tree (or documents known roxygen noise).
3. Docs: `?mx_make` shows flight checklist after `devtools::document` on umx (or `umx_make` document).

## Approval gate

No file edits until explicit **Proceed**.
