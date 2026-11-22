## Destination choice with the destination wage instrumented by the Bartik shock (control function), cluster bootstrap over destinations.
args <- commandArgs(trailingOnly = TRUE); IN <- args[1]; POPF <- args[2]; OUT <- args[3]; BK <- args[4]; B <- as.integer(args[5])
source(file.path(IN, "mig_build.R"))
bk <- read_csv(BK, col_types = cols(.default = "c")); names(bk) <- c("s", "d", "bartik")
bk <- bk %>% transmute(dest = paste0(s, "-", d), bartik = as.numeric(bartik)) %>% mutate(bartik_z = (bartik - mean(bartik)) / sd(bartik))
gE <- merge(gE, as.data.table(bk), by = "dest"); gL <- merge(gL, as.data.table(bk), by = "dest")
gE[, `:=`(survey = "nss64", heat = heat32)]; gL[, `:=`(survey = "plfs21", heat = heat32)]
pool <- rbind(gE, gL, fill = TRUE)
for (x in c("gE", "gL", "pool")) { y <- get(x); y[, `:=`(origin_s = paste(survey, origin), dstate_s = paste(survey, dstate), dkey = paste(survey, dest))]; assign(x, y) }
set.seed(20261009)
## one estimate: returns OLS and control-function coefficients and the value of 30 days above 32 C
est_once <- function(g, controls, dstateFE, inst = "bartik_z") {
  dl <- unique(g[, unique(c("dkey", "survey", "dstate_s", "lwage", inst, "heat", "log_urban_pop", all.vars(as.formula(paste("~ 0", controls))))), with = FALSE])
  fs_rhs <- paste0(inst, " + heat + log_urban_pop", controls)
  fsf <- as.formula(paste("lwage ~", fs_rhs, if (dstateFE) "| dstate_s" else "| survey"))
  fs <- feols(fsf, dl, notes = FALSE, warn = FALSE)
  dl$vhat <- dl$lwage - predict(fs, newdata = dl)
  g2 <- merge(g, dl[, .(dkey, vhat)], by = "dkey")
  fe <- paste("origin_s", if (dstateFE) "+ dstate_s" else "")
  rhs <- paste0("lwage + heat + log_dist + same_state + log_urban_pop", controls)
  m_ols <- fepois(as.formula(paste("N ~", rhs, "|", fe)), g2, notes = FALSE, warn = FALSE)
  m_cf  <- fepois(as.formula(paste("N ~", rhs, "+ vhat |", fe)), g2, notes = FALSE, warn = FALSE)
  c(fs_b = unname(coef(fs)[inst]), fs_t = unname(coef(fs)[inst] / se(fs, vcov = "hetero")[inst]),
    ols_w = unname(coef(m_ols)["lwage"]), ols_h = unname(coef(m_ols)["heat"]), ols_v = unname(30 * coef(m_ols)["heat"] / coef(m_ols)["lwage"]),
    cf_w = unname(coef(m_cf)["lwage"]), cf_h = unname(coef(m_cf)["heat"]), cf_vhat = unname(coef(m_cf)["vhat"]),
    cf_v = unname(30 * coef(m_cf)["heat"] / coef(m_cf)["lwage"]))
}
run_iv <- function(g0, label, controls = ctl, dstateFE = FALSE, inst = "bartik_z") {
  vars <- unique(c("N", "lwage", inst, "heat", "log_dist", "same_state", "log_urban_pop", all.vars(as.formula(paste("~ 0", controls)))))
  g <- g0[complete.cases(g0[, ..vars])]
  pt <- est_once(g, controls, dstateFE, inst)
  keys <- unique(g$dest); setkey(g, dest)
  one_rep <- function(b) {
    set.seed(1000 + b); setFixest_nthreads(1); data.table::setDTthreads(1)
    draw <- sample(keys, length(keys), replace = TRUE)
    gb <- rbindlist(lapply(seq_along(draw), function(k) { x <- g[.(draw[k])]; x[, dkey := paste(dkey, k)]; x }))
    r <- try(est_once(gb, controls, dstateFE, inst), silent = TRUE); if (inherits(r, "try-error")) rep(NA, length(pt)) else r
  }
  bs <- do.call(rbind, parallel::mclapply(seq_len(B), one_rep, mc.cores = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "4"))))
  colnames(bs) <- names(pt)
  se <- apply(bs, 2, sd, na.rm = TRUE)
  q <- function(v) quantile(bs[, v], c(.025, .975), na.rm = TRUE)
  cat(sprintf("\n--- %s | migrants %d, destinations %d | first stage: %s %.3f (t = %.1f, F = %.1f)\n", label, sum(g$N), uniqueN(g$dest), inst, pt["fs_b"], pt["fs_t"], pt["fs_t"]^2))
  cat(sprintf("   Not instrumented: wage %6.3f (%5.3f) | heat %8.5f (%7.5f) | value of 30 days >32C %6.3f (%5.3f)\n", pt["ols_w"], se["ols_w"], pt["ols_h"], se["ols_h"], pt["ols_v"], se["ols_v"]))
  cat(sprintf("   Control function: wage %6.3f (%5.3f) | heat %8.5f (%7.5f) | value %6.3f (%5.3f), 95%% bootstrap interval [%6.3f, %6.3f] | residual %6.3f (%5.3f)\n",
      pt["cf_w"], se["cf_w"], pt["cf_h"], se["cf_h"], pt["cf_v"], se["cf_v"], q("cf_v")[1], q("cf_v")[2], pt["cf_vhat"], se["cf_vhat"]))
  data.frame(spec = label, n_mig = sum(g$N), n_dest = uniqueN(g$dest), t(pt), t(setNames(se, paste0(names(se), "_se"))),
             cf_v_lo = q("cf_v")[1], cf_v_hi = q("cf_v")[2], B_ok = sum(complete.cases(bs)))
}
## other survey's wage estimate for the same district as the instrument (independent sampling noise)
gL[, alt_wage := wage_eus]; gE[, alt_wage := wage_plfs]
pool <- rbind(gE, gL, fill = TRUE); pool[, `:=`(origin_s = paste(survey, origin), dstate_s = paste(survey, dstate), dkey = paste(survey, dest))]
res <- rbind(
  run_iv(gL, "PLFS21 wage instrumented by NSS68 wage: no controls", controls = "", inst = "alt_wage"),
  run_iv(gL, "PLFS21 wage instrumented by NSS68 wage: + controls", inst = "alt_wage"),
  run_iv(gL, "PLFS21 wage instrumented by NSS68 wage: + controls + dest-state FE", dstateFE = TRUE, inst = "alt_wage"),
  run_iv(gE, "NSS64 (NSS68 wage) instrumented by PLFS wage: + controls", inst = "alt_wage"),
  run_iv(pool, "Pooled, each wage instrumented by the other survey: + controls", inst = "alt_wage"),
  run_iv(pool, "Pooled, each wage instrumented by the other survey: + controls + dest-state FE", dstateFE = TRUE, inst = "alt_wage"))
write_csv(res, file.path(OUT, "mig_choice_iv_altwage_results.csv"))
