## Alternative measures: tenant hedonic rents with dwelling size (NSS 65, NSS 76), HCES actual rent (520), actual wages (Stata EUS/PLFS FEs)
W <- commandArgs(trailingOnly=TRUE)[1]
source(file.path(W, "rep_03_functions.R")); suppressMessages(library(haven))
options(width=200, digits=4)
S <- readRDS(file.path(W, "samples.rds")); U <- readRDS(file.path(W, "utci.rds"))
hdc <- S$hces_distcodes
plfs2pc11 <- hdc %>% filter(!is.na(plfscode)) %>% transmute(plfs4=sprintf("%04d", plfscode), pc11_sd_id) %>% distinct(plfs4, .keep_all=TRUE)
fe_tab <- function(fit, nm) { f <- fixef(fit)$pc11_sd_id; setNames(data.frame(names(f), as.numeric(f)), c("pc11_sd_id", nm)) }
min_cell <- function(df, k=10) { ok <- df %>% count(pc11_sd_id) %>% filter(n>=k) %>% pull(pc11_sd_id); df %>% filter(pc11_sd_id %in% ok) }

## ---- Early rent, NSS 65 ----
h <- S$hm_all %>% filter(rent > 0) %>% mutate(lrent=log(rent), larea=log(pmax(total_area,10)), rooms=living_rooms + coalesce(other_rooms,0),
        dtype=factor(dwelling_type), kit=factor(kitchen_type), bath=factor(bathroom), vent=factor(ventilation), ten=factor(tenure))
cat("NSS65 urban renters:", nrow(h), "\n")
## (a) James-style: per-capita rent, piped+latrine, formal(pucca+elec)
a <- feols(log(rent/hhsize) ~ piped_water + own_latrine | pc11_sd_id, data=h %>% filter(pucca_floor==1, pucca_walls==1, pucca_roof==1, elec==1), weights=~Wgt_combined, notes=FALSE)
## (b) market tenants only (drop employer quarters), dwelling rent, same structure controls, no size
hm <- min_cell(h %>% filter(tenure %in% c("4","5")))
b <- feols(lrent ~ pucca_walls + pucca_floor + pucca_roof + piped_water + own_latrine + elec + kit + bath + dtype | pc11_sd_id, data=hm, weights=~Wgt_combined, notes=FALSE)
## (c) + size and ventilation and contract type
c <- feols(lrent ~ larea + rooms + pucca_walls + pucca_floor + pucca_roof + piped_water + own_latrine + elec + kit + bath + dtype + vent + ten | pc11_sd_id, data=hm, weights=~Wgt_combined, notes=FALSE)
etable(b, c, drop="kit|bath|dtype|vent", digits=3) |> print()
E <- fe_tab(a,"rent_james") %>% inner_join(fe_tab(b,"rent_nosize"), by="pc11_sd_id") %>% inner_join(fe_tab(c,"rent_hedonic"), by="pc11_sd_id")
cat(sprintf("NSS65 district rent FEs (n=%d): corr(james, hedonic)=%.2f  corr(nosize, hedonic)=%.2f  sd: james %.2f nosize %.2f hedonic %.2f\n",
    nrow(E), cor(E$rent_james,E$rent_hedonic), cor(E$rent_nosize,E$rent_hedonic), sd(E$rent_james), sd(E$rent_nosize), sd(E$rent_hedonic)))

## ---- Late rent, HCES item 520 (hired dwellings, all urban HH types) ----
L <- S$hces_all_urban_h %>% filter(dwellingtype==2, !is.na(arent), arent>0) %>% mutate(lrent=log(arent + coalesce(mwater,0)))
L <- min_cell(L)
d520 <- feols(lrent ~ pucca_walls + pucca_floor + pucca_roof + cooking_fuel + lighting_source + piped_water + own_latrine | pc11_sd_id, data=L, weights=~mult, notes=FALSE)
L539 <- S$hces_all_urban_h %>% filter(mrent > 0)
d539 <- feols(loghc ~ pucca_walls + pucca_floor + pucca_roof + cooking_fuel + lighting_source + piped_water + own_latrine | pc11_sd_id, data=L539, weights=~mult, notes=FALSE)
cat("HCES hired dwellings with actual rent:", nobs(d520), "in", length(fixef(d520)$pc11_sd_id), "districts\n")

## ---- Late rent, NSS 76 tenant hedonic ----
R76 <- "/Users/tanmaydevi/Library/CloudStorage/Box-Box/Urban Economics India/Data/RawData/Housing Conditions 2018"
l3 <- read_dta(file.path(R76,"R76120L03-Household characteristics.dta"), col_select=c(HHID, Sector, Tenurial_status_dwelling, area_type_dwelling_located, HH_size, Total_Monthly_expenditure, Multiplier, DistrictCode))
l7 <- read_dta(file.path(R76,"R76120L07-Particulars of the dwelling of the households living in houses.dta"),
               col_select=c(HHID, Type_dwelling, No_living_rooms, No_other_rooms, total_area, Ventilation, Kitchen_type, Floor_type, Wall_type, Roof_type, montly_rent))
l5 <- read_dta(file.path(R76,"R76120L05-Particulars of living facilities- drinking water, bathroom, sanitation, etc.dta"), col_select=c(HHID, source_drinking_water, Access_latrine, Acces_bathroom))
l6 <- read_dta(file.path(R76,"R76120L06-Housing characteristics and micro environment of the households living in houses.dta"), col_select=c(HHID, Struct_condition, wheth_electricity, drainage_system, No_floor))
n76 <- l3 %>% inner_join(l7, by="HHID") %>% inner_join(l5, by="HHID") %>% inner_join(l6, by="HHID") %>%
  filter(Sector=="2", Tenurial_status_dwelling %in% c("4","5"), montly_rent > 0) %>%
  left_join(plfs2pc11, by=c("DistrictCode"="plfs4")) 
cat("NSS76 urban market tenants:", nrow(n76), " mapped to pc11:", sum(!is.na(n76$pc11_sd_id)), "\n")
n76 <- n76 %>% filter(!is.na(pc11_sd_id)) %>% mutate(lrent=log(montly_rent), larea=log(pmax(total_area,10)), rooms=as.numeric(No_living_rooms)+coalesce(as.numeric(No_other_rooms),0),
   pucca_w=as.numeric(Wall_type) %in% 5:9, pucca_r=as.numeric(Roof_type) %in% 5:9, pucca_f=as.numeric(Floor_type) %in% 3:9,
   piped=source_drinking_water %in% c("01","02","03"), excl_latrine=Access_latrine=="1", excl_bath=Acces_bathroom=="01") %>% min_cell()
n76a <- feols(lrent ~ larea + rooms + pucca_w + pucca_r + pucca_f + piped + excl_latrine + excl_bath + factor(Type_dwelling) + factor(Ventilation) + factor(Kitchen_type) +
              factor(Struct_condition) + factor(wheth_electricity) + factor(drainage_system) + factor(Tenurial_status_dwelling) | pc11_sd_id, data=n76, weights=~Multiplier, notes=FALSE)
n76b <- feols(lrent ~ pucca_w + pucca_r + pucca_f + piped + excl_latrine | pc11_sd_id, data=n76, weights=~Multiplier, notes=FALSE)
cat(sprintf("NSS76 hedonic: N=%d, districts=%d, coef log area=%.3f (se %.3f), rooms=%.3f\n", nobs(n76a), length(fixef(n76a)$pc11_sd_id), coef(n76a)["larea"], se(n76a)["larea"], coef(n76a)["rooms"]))
Lt <- fe_tab(d520,"rent520") %>% full_join(fe_tab(d539,"rent539"), by="pc11_sd_id") %>% full_join(fe_tab(n76a,"rent76_hed"), by="pc11_sd_id") %>% full_join(fe_tab(n76b,"rent76_nosize"), by="pc11_sd_id")
cc <- function(x,y) { ok <- complete.cases(x,y); sprintf("%.2f (n=%d)", cor(x[ok],y[ok]), sum(ok)) }
cat("Late rent FE correlations: 520 vs 539:", cc(Lt$rent520,Lt$rent539), " | NSS76 hedonic vs 520:", cc(Lt$rent76_hed,Lt$rent520), " | NSS76 hedonic vs 539:", cc(Lt$rent76_hed,Lt$rent539),
    " | NSS76 nosize vs hedonic:", cc(Lt$rent76_nosize, Lt$rent76_hed), "\n")
cat("Early NSS65 hedonic vs late NSS76 hedonic (persistence):", cc(E$rent_hedonic[match(Lt$pc11_sd_id, E$pc11_sd_id)], Lt$rent76_hed), "\n")
cat("Early NSS65 hedonic vs late 539 (what pipeline pairs):", cc(E$rent_hedonic[match(Lt$pc11_sd_id, E$pc11_sd_id)], Lt$rent539), "\n")

## ---- Actual wages from the Stata pipeline ----
B <- "/Users/tanmaydevi/Library/CloudStorage/Box-Box/Urban Economics India/Code/Stata"
we <- read_dta(file.path(B,"Albouy_Replication_Early/output/data/wage_diff_nss68_eus.dta")) %>% mutate(plfs4=sprintf("%04d", as.integer(district_id_2011))) %>% inner_join(plfs2pc11, by="plfs4") %>% select(pc11_sd_id, wage_eus=fe_wage_nss68_eus, n_eus=cell_n) %>%
  group_by(pc11_sd_id) %>% summarise(wage_eus = weighted.mean(wage_eus, n_eus), n_eus = sum(n_eus), .groups = "drop")
wl <- read_dta(file.path(B,"Albouy_Replication/output/data/wage_diff_plfs_pooled_matched.dta")) %>% mutate(plfs4=sprintf("%04d", as.integer(district_id_2011))) %>% inner_join(plfs2pc11, by="plfs4") %>% select(pc11_sd_id, wage_plfs=fe_wage_plfs_matched, n_plfs=cell_n) %>%
  group_by(pc11_sd_id) %>% summarise(wage_plfs = weighted.mean(wage_plfs, n_plfs), n_plfs = sum(n_plfs), .groups = "drop")   # post-2011 child districts -> 2011 parent
cons_e <- fe_tab(feols(logepc ~ poly(Age,2) + male + educ + hindu + scst | pc11_sd_id, data=S$nss_all_urban, weights=~Combined_multiplier, notes=FALSE), "cons_early")
cons_l <- fe_tab(feols(logepc ~ poly(age,2) + male + edu + hindu + scstbc | pc11_sd_id, data=S$hces_all_urban_h, weights=~mult, notes=FALSE), "cons_late")
cat("Wage vs consumption FE correlations: early EUS vs NSS68 consumption:", cc(we$wage_eus, cons_e$cons_early[match(we$pc11_sd_id, cons_e$pc11_sd_id)]),
    " | late PLFS vs HCES consumption:", cc(wl$wage_plfs, cons_l$cons_late[match(wl$pc11_sd_id, cons_l$pc11_sd_id)]), "\n")

## ---- Assemble district panel with clean heat measures ----
ue <- as.data.frame(U$early)[,c("pc11_sd_id","c26","c32","c38","mean_utci")]; names(ue)[-1] <- paste0(names(ue)[-1],"_e")
ul <- as.data.frame(U$late)[,c("pc11_sd_id","c26","c32","c38","mean_utci")];  names(ul)[-1] <- paste0(names(ul)[-1],"_l")
D <- ue %>% inner_join(ul, by="pc11_sd_id") %>% left_join(E, by="pc11_sd_id") %>% left_join(Lt, by="pc11_sd_id") %>% left_join(we, by="pc11_sd_id") %>% left_join(wl, by="pc11_sd_id") %>%
  left_join(cons_e, by="pc11_sd_id") %>% left_join(cons_l, by="pc11_sd_id") %>% left_join(pop %>% transmute(pc11_sd_id, pop11=pop_11_proj, pop23p=pop_23_proj, pop23w=pop_23_wp), by="pc11_sd_id") %>%
  left_join(instr %>% select(pc11_sd_id, coast_dist, Unsuitable_Slope_Share, Internal_Water_Share, Shape_Area), by="pc11_sd_id") %>% mutate(state=substr(pc11_sd_id,1,2))
saveRDS(D, file.path(W, "alt_panel.rds"))
grad <- function(y, per="l", extra="") {
  f <- as.formula(paste0(y, " ~ c26_", per, " + c32_", per, " + c38_", per, extra, " | state"))
  m <- feols(f, D, weights=~pop11, vcov="hetero", notes=FALSE); b <- coef(m)[1:3]; V <- vcov(m)[1:3,1:3]
  Lm <- rbind(c(1,0,0), c(1,1,0), c(1,1,1)); est <- Lm %*% b; se <- sqrt(diag(Lm %*% V %*% t(Lm)))
  sprintf("%-14s n=%3d | per-day effect 26-32: %8.5f (%.5f)  32-38: %8.5f (%.5f)  >38: %8.5f (%.5f)", y, nobs(m), est[1], se[1], est[2], se[2], est[3], se[3])
}
cat("\nHeat gradients of each district differential (state FE, pop-weighted; cumulative per-day effect by band, robust se)\n")
for (y in c("cons_early","wage_eus","rent_james","rent_nosize","rent_hedonic")) cat(grad(y, "e"), "\n")
for (y in c("cons_late","wage_plfs","rent539","rent520","rent76_nosize","rent76_hed")) cat(grad(y, "l"), "\n")
cat("\nSame, adding geography controls (coast distance, slope share, water share, log area):\n")
gx <- " + coast_dist + Unsuitable_Slope_Share + Internal_Water_Share + log(Shape_Area)"
for (y in c("wage_eus","rent_hedonic")) cat(grad(y, "e", gx), "\n")
for (y in c("wage_plfs","rent76_hed","rent520")) cat(grad(y, "l", gx), "\n")
