suppressMessages({library(dplyr); library(fixest, lib.loc=Sys.getenv("R_LIBS_USER"))})
W <- commandArgs(trailingOnly=TRUE)[1]; options(width=200)
R <- readRDS(file.path(W, "root_cause_data.rds"))
A <- R$alt %>% rename(pop_w = pop11.x) %>% mutate(urban_share = ifelse(urban_share > 1, NA, urban_share), pm25 = ifelse(pm25 <= 0, NA, pm25),
  log_area = log(Shape_Area), Q_e = 0.2*rent_hedonic - wage_eus, Q_l = 0.2*rent76_hed - wage_plfs)
blocks <- c(base = "", geography = "+ coast_dist + Unsuitable_Slope_Share + Internal_Water_Share + log_area",
            all = "+ coast_dist + Unsuitable_Slope_Share + Internal_Water_Share + log_area + log_urban_pop + urban_share + ec_emp_pc + manuf_share + hired_share + ag_share + lit + log(pm25)")
for (y in c("Q_e","Q_l")) {
  per <- ifelse(y == "Q_e", "e", "l"); vv <- paste0(c("c26_","c32_","c38_"), per)
  dA <- A[complete.cases(A[, c(y, "coast_dist","log_urban_pop","urban_share","ec_emp_pc","manuf_share","hired_share","ag_share","lit","pm25")]), ]
  for (bn in names(blocks)) {
    m <- feols(as.formula(paste0(y, " ~ ", paste(vv, collapse=" + "), " ", blocks[[bn]], " | state")), dA, weights=~pop_w, vcov="hetero", notes=FALSE)
    L <- rbind(c(1,0,0), c(1,1,0), c(1,1,1)); b <- coef(m)[vv]; V <- vcov(m)[vv,vv]; e <- 30*drop(L %*% b); se <- 30*sqrt(diag(L %*% V %*% t(L)))
    cat(sprintf("%-4s N=%3d %-10s | 30 days 26-32: %6.3f (%5.3f)  32-38: %6.3f (%5.3f)  >38: %6.3f (%5.3f)\n", y, nrow(dA), bn, e[1], se[1], e[2], se[2], e[3], se[3]))
  }
}
