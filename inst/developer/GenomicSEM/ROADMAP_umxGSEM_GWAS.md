# Roadmap: `umxGSEM_GWAS` (for review, not execution)

Status as of 2026-07-11: **R scaffold works** after SNP-in-S + sandwich-SE fixes.  
Goal: correctness vs GenomicSEM tutorials, then speed toward genome-wide SNP loops.

---

## 0. Where we are

| Piece                  | State                                                                      |
|:-----------------------|:---------------------------------------------------------------------------|
| `umxGSEM_sumstats()`   | Toy SCZ/BIP/MDD + 1000G subset; continuous-scale betas                     |
| `umxGSEM()` structural | DWLS on LDSC S/V; param/SE match `commonfactor` (Psych_LDSC)               |
| `umxGSEM_GWAS()`       | R loop: expand S/V → `umxGSEM` → `mxRun` → extract F1~SNP                  |
| OpenMx                 | `imxMunge` / `imxLDSC`; `GsemImpliedStats` Jacobian; **no** SNP stream yet |
| Bottleneck             | Full model rebuild + `mxRun` **per SNP** (seconds × millions)              |

**Do not** treat current SE/Z/P as golden until tutorial parity tests exist. Some SNPs still hit bad optim status; sandwich SE is a fallback when OpenMx omits SEs.

---

## 1. Phase A — Correctness & robust testing (do first)

### A1. Golden tests vs GenomicSEM / workshop output

**Goal:** Prove we estimate the same SNP→factor effects (and SEs where defined) as the tutorial stack on a **small, fixed** SNP set.




| Test                      | Input                            | Compare                                                                        |
|:--------------------------|:---------------------------------|:-------------------------------------------------------------------------------|
| Structural (already soft) | Psych_LDSC 5-trait CF            | loadings/SEs to printed precision                                              |
| GWAS smoke (expand)       | 3-trait block + ~20–100 toy SNPs | `est` vs GenomicSEM `commonfactorGWAS` (or workshop table) when available      |
| GWAS unit                 | Hand-built S/V expand for 1 SNP  | `S_Full`/`V_Full` blocks match hand formula (varSNP, β·varSNP, GC SE formulas) |
| Regression                | Same snps + seed                 | stable `est` vector; no constant-est bug regression                            |

**Practical goldens without full GenomicSEM CI:**

1. **Pinned snapshot** in `inst/developer/GenomicSEM/_snaps/` (CSV of SNP, est, se from a known good run on the toy files).  
2. Optional **local-only** test: if `GenomicSEM` + pinned lavaan present, run their GWAS on the same SNPs and `expect_equal(..., tolerance=…)`.  
3. **Expand-only** tests (no optim): pure R checks on `xmu_gsem_expand_snp` vs algebra — always CI-safe.

**Workshop caveats to encode in tests/docs (not as “truth” for fit indices):**

- Their CFI>.95 / SRMR<.05 / AIC stories ≠ our continuous-WLS guidance.  
- Golden tests should target **est/SE for SNP path**, not CFI.

### A2. Hardening the R scaffold (cheap, high value)

- **Convergence:** record `status`; NA SE when status ∉ {0,1} and SE numerically absurd (partially done).  
- **Starts:** warm-start free params from previous SNP (same structure) to cut iterations and reduce status=4.  
- **Identification:** document unit-loading vs unit-variance for GWAS (GenomicSEM-style unit loading on first trait).  
- **Quiet API:** suppress latent-creation / plot noise in the SNP loop (options already partially used).  
- **Output:** optional columns `se_source` (`openmx` | `sandwich` | `NA`), `n_iter`.

### A3. Tutorial parity protocol (maintainer checklist)

```text
1. Same traits, same LDSC S/V/I (Psych_LDSC 3-trait or workshop object).
2. Same sumstats files + ref + se.logit flags.
3. Same maxSNPs / SNP list order.
4. Compare: est, se (tol), Z, P; list SNPs with status≠0 on either side.
5. Archive CSV under inst/developer/GenomicSEM/_snaps/.
```

---

## 2. Phase B — Parallel R (your sketch) — intermediate speed

### Idea

Chunk SNPs into `nCores` blocks → each worker runs the current expand + `mxRun` loop → `rbind` results.

```text
SNPs ──split──► [chunk_1] … [chunk_n]
                    │ mxRun × n_i
                    ▼
                 results_i ──rbind──► gwas table
```

| Option | Mechanism | Pros | Cons |
|--------|-----------|------|------|
| **B1. `parallel::mclapply` (fork)** | macOS/Linux | Simple; shares memory pages | **No Windows**; OpenMx + forks can be flaky (threads, BLAS) |
| **B2. `parallel::parLapply` / PSOCK** | All platforms | Safer isolation | Serialize model template + chunks; higher overhead |
| **B3. `future` + `furrr`** | Portable API | Nice UX; plan backend later | Extra dependency (Suggests) |
| **B4. Single process, BLAS threads=1, OpenMP outer** | Later C++ | Best long-term | Not this phase |

**Recommendation for B:** **B2 (PSOCK) or B3 with `multisession`**, not `mclapply`, unless we document “Unix only” and force `OMP_NUM_THREADS=1` / `mxOption` cores=1 per worker.

**Chunk size:** aim ~500–5000 SNPs/chunk so process startup is amortized but a failed chunk is recoverable.

**Correctness:** each worker must rebuild or deep-copy models; never share a live MxModel across processes. Expand S/V **inside** the worker from row slices of the SNP table.

**Expected gain:** ~nCores× if optim dominates; less if expand/R overhead dominates. Still O(nSNP) full optims — not genome-wide friendly alone.

---

## 3. Phase C — `mxComputeLoop` / data streaming (OpenMx-integrated R)

### Your idea, refined

Hold **one** MxModel (structure free map fixed). Loop:

1. Inject new `observedStats` (cov, useWeight, asymCov) for SNP *i*  
2. Optionally reset starts  
3. `mxRun` / compute plan step  
4. Store free estimates / SEs  

`mxComputeLoop` + related loaders (`mxComputeLoadMatrix`, `mxComputeLoadData`, …) are designed for **repeated runs with varying data**. Worth prototyping on a **tiny** model before committing.

| Option | Sketch | Pros | Cons |
|--------|--------|------|------|
| **C1. R loop + `mxModel(model, newData)` + `mxRun`** | Minimal change from today | Easy; debuggable | Still rebuilds data object; may re-init more than needed |
| **C2. `mxComputeLoop` around GD + report** | OpenMx-native repetition | Less R overhead; checkpointing | Need to learn how to swap summary WLS `observedStats` each iter; docs sparse for this use |
| **C3. Flatten once: free vector θ, only update data matrices in C** | Closest to endgame | Fast | Requires OpenMx API work (Phase D) |

**Prototype C2 questions to answer tomorrow:**

1. Can `observedStats` be updated each loop without full model reparse?  
2. Does WLS recompute Jacobian each time (needed for SE) cheaply via `GsemImpliedStats`?  
3. Can we report only the SNP-path free parameter each iter?  
4. Interaction with `tryHard` / failed SNPs (skip vs continue loop)?

**Rationale:** C1 is a stepping stone; C2 is worth 1–2 days spike; if blocked, fall back to B + push D.

---

## 4. Phase D — OpenMx rear end (long-term, real GWAS scale)

Aligned with OpenMx `roadmap.md` / release notes:

> Flatten one RAM GSEM model once; OpenMP over SNP blocks; expand LDSC S/V with SNP-trait covs; in-thread DWLS; reassemble table to R.

| Option | What changes in OpenMx | umx role |
|--------|------------------------|----------|
| **D1. `.Call` expand+fit batch** | C++: for each SNP, build S/V blocks, call shared DWLS / Newton on free map, sandwich SE from stored Δ pattern | `umxGSEM_GWAS` becomes thin: munge table → call → data.frame |
| **D2. Hybrid: fix structural free params, only free SNP path (+ maybe residual SNP)** | Faster linear/small optim per SNP | API flag `freezeStructure=TRUE` |
| **D3. Analytic / GLS closed form for F1~SNP given fixed measurement model** | Fastest if assumptions hold | May diverge from GenomicSEM “full re-estimate” |

**Suggested OpenMx modifications to discuss:**

1. **Stable WLS SE path** when Hessian eigenvalues fail but Jacobian + W + V exist (export sandwich SE to `output$standardErrors`).  
2. **Public “replace observedStats”** helper that invalidates only data-dependent caches.  
3. **`imxGsemExpandSnp(S, V, I, beta, se, maf, GC)`** in C++ (shared with R for tests).  
4. **Batch API:** `imxGsemGWAS(modelTemplate, snpMatrix, …)` returning matrix of est/se/status.  
5. Document **vech pair names** (`imxGsemVechNames`) as the contract for V.

**D2 note:** GenomicSEM often re-estimates the full model per SNP; freezing structure is a product choice (speed vs parity). Offer both once D1 works.

---

## 5. Comparison of “how to parallelize” (decision matrix)

| Approach | Effort | Speed (1e6 SNPs) | Parity risk | Platform |
|----------|--------|------------------|-------------|----------|
| A only (serial R, correct) | Low | Unusable genome-wide | Low | All |
| B chunk `parLapply` | Low–med | ~cores× hours→hours | Low if pure | All |
| C `mxComputeLoop` | Med | Better constants | Med | All |
| D OpenMP C++ | High | Minutes (target) | Med (test hard) | All |
| B then D | Recommended path | Good interim + endgame | Controlled | All |

**Recommended sequence:**

1. **A** — goldens + expand tests + no constant-est regression  
2. **B** — optional `nCores` on `umxGSEM_GWAS` (PSOCK), default 1  
3. **C** — spike `mxComputeLoop` (time-box 1 day); adopt only if clear win over B  
4. **D** — OpenMx batch backend; umx keeps API  

Your chunk + multi-`mxRun` idea is **exactly B**, and is the right **interim** before D. `mxComputeLoop` is **C**, not a replacement for OpenMP over millions of SNPs, but may cut per-SNP overhead inside each R worker.

---

## 6. API sketch (stable surface)

```r
umxGSEM_GWAS(
  covstruc, SNPs,
  model = NULL,           # default CF + F1 ~ SNP
  traits = NULL,
  estimation = "DWLS",
  GC = c("standard", "conserv", "none"),
  maxSNPs = NULL,
  nCores = 1L,            # Phase B
  tryHard = "no",         # keep light in loop; "yes" only on failure?
  backend = c("R", "OpenMx"),  # Phase D
  quiet = TRUE
)
```

Return always a data.frame: SNP, CHR, BP, MAF, A1, A2, est, se, Z, P, status, fail, warning, (+ se_source).

---

## 7. Risks & open questions

1. **χ² vs GenomicSEM Q** — they often use η′V⁻¹η at DWLS θ̂; we report r′W_diag r. Parity tests should focus on **SNP path est/SE**, not model χ².  
2. **V expand block-diagonal** — confirm against GenomicSEM source for off-blocks (SNP–trait vs trait–trait).  
3. **OpenMx + parallel** — set threads=1 per worker; avoid nested OpenMP.  
4. **Memory** — don’t deep-copy full SNP table nCores times if avoidable (file-backed / index ranges).  
5. **Windows** — no fork; design B for PSOCK first.

---

## 8. Suggested morning agenda (pick one track)

| Priority | Task | Done when |
|----------|------|-----------|
| **P0** | Freeze goldens: expand unit tests + 10-SNP snapshot of `est` | **Done:** `tests/testthat/test_GSEM_goldens.R` + `fixtures/gsem/`; optional GenomicSEM parity script |
| **P1** | Optional: local GenomicSEM GWAS compare script in `inst/developer/` | Documented tol + snap |
| **P2** | Design note: confirm V_Full layout vs GenomicSEM | Comment + test |
| **P3a** | Implement `nCores` PSOCK chunking | 4× on 1e4 SNPs laptop |
| **P3b** | Time-boxed `mxComputeLoop` spike | Go/no-go memo |
| **P4** | OpenMx issue/PR list for D1–D3 | Agreed with OpenMx side |

---

## 9. Out of scope for this roadmap

- Replacing LDSC with something else  
- Full 1KG reference packaging in umx  
- Workshop CFI cutoffs as package defaults  
- GPU (your GPU can keep sleeping)

---

## 10. Competitive / ecosystem note: GenomicSEM PR #140 (Rust)

Source: [GenomicSEM/GenomicSEM#140](https://github.com/GenomicSEM/GenomicSEM/pull/140)  
(May 2026 draft experiment — `lavaanrust` via extendr; strict no silent lavaan fallback)

### What they built

| Layer                 | Content                                                                                               |
|:----------------------|:------------------------------------------------------------------------------------------------------|
| Compatibility surface | `sem_rust()`, `commonfactor_rust()`, `commonfactorGWAS_rust()`, `usermodel_rust()`, `userGWAS_rust()` |
| Generic path          | DWLS **RAM compiler/evaluator** for a GenomicSEM-relevant lavaan subset                               |
| Hot path              | **Native kernels** for one-factor CF / GWAS (reuse compiled plan)                                     |
| GWAS options          | `fix_measurement = TRUE/FALSE`, `Q_SNP`, `parallel`, `std.lv`, …                                      |
| Parallel              | Worker plumbing on supported GWAS wrappers                                                            |

### Benchmarks they report (1k SNPs × 5 traits, remote 16-CPU)

| Workflow                | lavaan seq | rust seq | lavaan@16 | rust@16    |
|:------------------------|:-----------|:---------|:----------|:-----------|
| `commonfactorGWAS`      | ~167 s     | ~6.3 s   | ~18 s     | **~1.5 s** |
| `userGWAS` fix_meas + Q | ~62 s      | ~5.4 s   | ~14 s     | **~2.3 s** |
| `userGWAS` free meas    | ~88 s      | ~6.6 s   | ~15 s     | **~2.3 s** |

Rough order of magnitude if linear: **~1–3 s / 1e3 SNPs parallel** → **~15–40 min / 1e6 SNPs** at that hardware — or better if they have specialized one-factor analytic kernels beyond the 1k table. User-quoted **~2 s / 1e6 serial** would imply a **much** tighter specialized path (locked measurement + near-analytic SNP solve), not full re-optim per SNP; treat as aspirational/special-case until we verify against PR code paths.

### Implications for us

1. **Product space is moving** — full-per-SNP SEM re-estimation is no longer the competitive frontier; **compile once / freeze measurement / reuse plan** is.  
2. **Our roadmap Phase D2 is validated** by their API: `fix_measurement=TRUE` is first-class.  
3. **Language choice differs** — they replace lavaan with **Rust+extendr**; we already have **OpenMx C++ / Stan Jacobian / OpenMP / imxLDSC**. Don’t chase lavaanrust; chase the **same math** (locked Λ,Θ; free SNP→factor; DWLS sandwich) inside OpenMx.  
4. **Parity target shifts** — golden tests should eventually include both:
   - classic lavaan GenomicSEM (slow, legacy), and  
   - rust-backed `*_rust()` when available (fast, new default for them).  
5. **R-only chunked `mxRun` (Phase B)** becomes a **bridge**, not the endgame — their serial rust already beats lavaan×16 workers in their table.  
6. **Differentiation for umx/OpenMx:**
   - umxPath / MxModel input (not only lavaan strings)  
   - modern WLS rails + SB nested tests for **structural** GSEM  
   - munge/LDSC already in C++  
   - optional **full re-estimate** mode when users distrust fixed measurement  
   - integration with OpenMx ecosystem (CI, plot, compare)

### Morning decision add-on

| Choice                                            | If we believe PR #140 lands                                                  |
|:--------------------------------------------------|:-----------------------------------------------------------------------------|
| **Skip** heavy investment in B as the main story  | Still OK for correctness loop on hundreds of SNPs                            |
| **Prioritize D2 analytic/fixed-measurement GWAS** | Match their speed class with OpenMx C++                                      |
| **Keep full optim path**                          | As `fix_measurement=FALSE` parity / diagnostics                              |
| **Watch** `lavaanrust` scope                      | Generic RAM DWLS in rust ≈ what `GsemImpliedStats` + free map already aim at |

---

*Written for morning review. No Phase B–D implementation in this pass. §10 added after PR #140 review.*
