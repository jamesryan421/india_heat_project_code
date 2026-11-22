"""Figures 2 to 5 for the heat project summary note.

Usage: python3 -I make_figs.py <figdata dir> <destination_attributes.csv> <theta_results.csv> <mig_choice_iv_altwage_results.csv>
       <mig_choice_iv_asi_results.csv> <output dir>
"""
import sys
import numpy as np
import pandas as pd
import geopandas as gpd
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.colors import TwoSlopeNorm

FD, DEST, THETA, ALTW, ASI, OUT = sys.argv[1:7]

mpl.rcParams.update({
    "font.family": "Palatino", "font.size": 9, "axes.labelsize": 9, "axes.titlesize": 9.5,
    "xtick.labelsize": 8, "ytick.labelsize": 8, "legend.fontsize": 8, "axes.spines.top": False,
    "axes.spines.right": False, "axes.linewidth": 0.6, "xtick.major.width": 0.6, "ytick.major.width": 0.6,
    "pdf.fonttype": 42, "savefig.bbox": "tight", "savefig.pad_inches": 0.03,
})
BLUE, LIGHT, RED, GRAY, BAND, BROWN = "#1f4e79", "#c6d7ea", "#b03a2e", "#7f7f7f", "#f6dcb5", "#8a5a12"
DEG = "°"


def ci(ax, y, est, se, color=BLUE, hollow=False):
    """Horizontal point with a 95 percent interval."""
    ax.plot([est - 1.96 * se, est + 1.96 * se], [y, y], color=color, lw=1.2, solid_capstyle="butt")
    ax.plot(est, y, "o", ms=4.5, color=color, mfc="white" if hollow else color, mew=1.1)


# ---------------- Figure 2: the wage gap shrinks as the measure and controls improve; rents do not move ----------------
steps = ["Consumption per person\n(pipeline)", "+ household size", "Actual PLFS wages,\nNSS 76 tenant rents",
         "+ geography", "+ city size, economy,\nliteracy, PM2.5"]
wage = [(-6.8, 1.8), (-5.7, 1.7), (-4.1, 1.8), (-2.6, 1.9), (-2.3, 2.1)]
rent = [(-3.8, 3.2), None, (-0.1, 4.9), (-0.7, 5.7), (-1.1, 5.9)]
qol = [(6.0, 1.7), (5.0, 1.5), (4.1, 2.6), (3.9, 2.8), (2.3, 3.1)]
fig, axes = plt.subplots(1, 3, figsize=(6.3, 2.45), sharey=True, gridspec_kw={"wspace": 0.12})
ys = np.arange(len(steps))[::-1]
for ax, vals, title in zip(axes, [wage, rent, qol], ["Wage", "Rent", "Quality of life (0.2 rent $-$ wage)"]):
    ax.axvline(0, color=GRAY, lw=0.6, ls="--")
    for y, v in zip(ys, vals):
        if v is not None:
            ci(ax, y, *v)
    ax.set_title(title)
    ax.grid(axis="x", color="#e6e6e6", lw=0.5)
    ax.set_axisbelow(True)
    ax.tick_params(axis="y", length=0)
axes[0].set_xlim(-11, 3); axes[1].set_xlim(-14, 12); axes[2].set_xlim(-5, 11)
axes[0].set_yticks(ys); axes[0].set_yticklabels(steps)
axes[1].text(-3.8, ys[1], "unchanged", color=GRAY, fontsize=7.5, va="center", ha="center", style="italic")
fig.text(0.6, -0.05, f"Effect of 30 more days above 38{DEG}C within states, percent, with 95% interval", ha="center", fontsize=9)
fig.savefig(f"{OUT}/fig2_wage_steps.pdf")
plt.close(fig)

# ---------------- Figure 3: hot days by district, and within-state deviations ----------------
dist = gpd.read_file(f"{FD}/districts.geojson")
states = gpd.read_file(f"{FD}/states.geojson")
d = pd.read_csv(DEST, usecols=["pc11_sd_id", "c32_1222"])
d = d[~d.pc11_sd_id.str.endswith("-000")]
d["state"] = d.pc11_sd_id.str[:2]
d["dev"] = d.c32_1222 - d.groupby("state").c32_1222.transform("mean")
share = 1 - (d.dev ** 2).sum() / ((d.c32_1222 - d.c32_1222.mean()) ** 2).sum()
g = dist.merge(d, on="pc11_sd_id", how="left")
fig, axes = plt.subplots(1, 2, figsize=(6.3, 3.75), gridspec_kw={"wspace": 0.08})
panels = [("c32_1222", f"(a) Days a year above 32{DEG}C, 2012–22", "YlOrRd", None),
          ("dev", "(b) Difference from the state average", "RdBu_r", TwoSlopeNorm(vcenter=0, vmin=-120, vmax=120))]
for ax, (col, title, cmap, norm) in zip(axes, panels):
    g.plot(ax=ax, color="#dddddd", lw=0)
    g.dropna(subset=[col]).plot(column=col, ax=ax, cmap=cmap, norm=norm, lw=0.05, edgecolor="white",
                                vmin=None if norm else 0, vmax=None if norm else 330)
    states.boundary.plot(ax=ax, color="#333333", lw=0.35)
    ax.set_xlim(67.5, 97.8); ax.set_ylim(6.5, 37.5)
    ax.set_axis_off(); ax.set_title(title, pad=4)
    sm = mpl.cm.ScalarMappable(cmap=cmap, norm=norm if norm else mpl.colors.Normalize(0, 330))
    cb = fig.colorbar(sm, ax=ax, orientation="horizontal", fraction=0.045, pad=0.02, aspect=32)
    cb.outline.set_linewidth(0.4); cb.ax.tick_params(labelsize=7.5, width=0.5, length=2)
    cb.set_label("days", fontsize=7.5, labelpad=1)
fig.savefig(f"{OUT}/fig3_heat_map.pdf")
plt.close(fig)
print(f"Figure 3: between-state share of variance in days above 32 C (2012-22): {share:.3f}; districts mapped {g.c32_1222.notna().sum()}")

# ---------------- Figure 5: the cost of heat depends on theta ----------------
tg = pd.read_csv(f"{FD}/theta_grid_heat_value.csv")
cur = tg[(tg["sample"] == "pool") & (tg["type"] == "fixed") & (tg.theta <= 32.4)].sort_values("theta")
free = tg[(tg["sample"] == "pool") & (tg["type"] == "free")].iloc[0]
alt = pd.read_csv(ALTW)
alt = alt[alt.spec.str.startswith("Pooled, each wage instrumented by the other survey: + controls") & ~alt.spec.str.contains("dest-state")].iloc[0]
asi = pd.read_csv(ASI)
asi = asi[asi.spec.str.startswith("A. Pooled")].iloc[0]
fig, ax = plt.subplots(figsize=(6.3, 3.1))
ax.axvspan(11.7, 32.4, color=BAND, lw=0, zorder=0)
ax.axhline(0, color=GRAY, lw=0.6, ls="--", zorder=1)
ax.fill_between(cur.theta, -100 * cur.value30_hi, -100 * cur.value30_lo, color=LIGHT, lw=0, zorder=2)
ax.plot(cur.theta, -100 * cur.value30, color=BLUE, lw=1.6, zorder=3)
ax.errorbar(free.theta, -100 * free.value30, yerr=[[100 * (free.value30_hi - free.value30)], [100 * (free.value30 - free.value30_lo)]],
            fmt="o", color=RED, ms=5, lw=1.2, capsize=0, zorder=4)
ax.annotate("District wage as measured\n(wage coefficient estimated at 0.65)", (free.theta, -100 * free.value30), xytext=(1.05, 16.5),
            fontsize=8, color=RED, arrowprops=dict(arrowstyle="-", color=RED, lw=0.6))
for row in [alt, asi]:
    ax.errorbar(row.cf_w, -100 * row.cf_v, yerr=[[100 * (row.cf_v_hi - row.cf_v)], [100 * (row.cf_v - row.cf_v_lo)]],
                fmt="s", color=GRAY, ms=4.5, lw=1.1, capsize=0, zorder=4)
    ax.annotate("", (row.cf_w, -100 * row.cf_v_hi - 0.1), xytext=(4.0, -4.6), arrowprops=dict(arrowstyle="-", color=GRAY, lw=0.5))
ax.text(4.0, -4.7, "Wage instrumented by the other survey's\nwage (5.8) or the ASI factory wage (11.7)", fontsize=7.5, color="#555555",
        ha="center", va="top")
for th in (12.9, 17.2):
    ax.axvline(th, color="#c98a2b", lw=0.8, zorder=1)
ax.text(12.9, 20.6, "12.9 ", fontsize=7.5, color=BROWN, ha="right", va="top")
ax.text(17.2, 20.6, " 17.2", fontsize=7.5, color=BROWN, ha="left", va="top")
ax.text(20.5, -4.7, "Our $\\theta$ and its\n95% interval", fontsize=7.5, color=BROWN, ha="center", va="top")
ax.text(2.25, 5.2, "$\\theta$ fixed at each value\n(line and 95% band)", fontsize=7.5, color=BLUE)
ax.set_xscale("log")
ax.set_xlim(0.45, 36); ax.set_ylim(-7.5, 22)
ax.set_xticks([0.5, 1, 2, 5, 10, 20, 30]); ax.set_xticklabels(["0.5", "1", "2", "5", "10", "20", "30"])
ax.set_xlabel("Migrants' response to pay, $\\theta$ (log scale)")
ax.set_ylabel("Cost of 30 more hot days\n(percent of income)")
top = ax.secondary_xaxis("top")
top.set_xticks([12.5, 27.6]); top.set_xticklabels(["Indonesia", "US"], fontsize=7.5, color=GRAY)
top.tick_params(length=3, color=GRAY)
top.spines["top"].set_visible(False)
top.xaxis.set_minor_locator(mpl.ticker.NullLocator())
fig.savefig(f"{OUT}/fig5_theta_cost.pdf")
plt.close(fig)

# ---------------- Figure 4: the Bryan-Morten relationship and theta across specifications ----------------
B = pd.read_csv(f"{FD}/bm_binscatter.csv")
slope = pd.read_csv(f"{FD}/bm_binscatter_slope.csv").iloc[0]
T = pd.read_csv(THETA)
rows = [("X1 movers excl. return migrants, both surveys", "Both surveys (preferred)"),
        ("X2 movers excl. return migrants, NSS 64", "NSS 64 only"),
        ("X3 movers excl. return migrants, PLFS 2020-21", "PLFS 2020–21 only"),
        ("X4 movers excl. return migrants, moved for work", "Moved for work"),
        ("A1 movers, both surveys", "Including return migrants"),
        ("X5 all wage earners incl. natives, excl. return migrants", "Including non-movers")]
fig, (a1, a2) = plt.subplots(1, 2, figsize=(6.3, 2.9), gridspec_kw={"width_ratios": [1, 1.15], "wspace": 0.55})
xs = np.linspace(B.x.min() - 0.1, B.x.max() + 0.1, 50)
a1.axhline(0, color="#e6e6e6", lw=0.5); a1.axvline(0, color="#e6e6e6", lw=0.5)
a1.plot(xs, slope.slope * xs, color=BLUE, lw=1.2)
a1.plot(B.x, B.y, "o", color=BLUE, ms=4)
a1.text(-2.6, -0.17, f"Slope {slope.slope:.3f}, so $\\theta$ = {slope.theta:.1f}\n{int(slope.n):,} movers, 20 bins", fontsize=7.5, color=BLUE)
a1.set_xlabel("Log share of the origin state's men\nliving in the district (net of effects)")
a1.set_ylabel("Log daily wage (net of effects)")
a1.set_title("(a) Movers earn more where few\nfrom their state go", loc="left")
yl = np.arange(len(rows))[::-1]
for x, lab in [(12.5, "Indonesia"), (27.6, "US")]:
    a2.axvline(x, color=GRAY, lw=0.7, ls="--")
    a2.text(x, len(rows) - 0.35, lab, fontsize=7.5, color=GRAY, ha="center", va="bottom")
for y, (spec, lab) in zip(yl, rows):
    ivm = "IV: ldist" if ((T.spec == spec) & (T.method == "IV: ldist")).any() else "IV: inter + ldist_i"
    for method, off, hollow in [("OLS", 0.13, True), (ivm, -0.13, False)]:
        r = T[(T.spec == spec) & (T.method == method)].iloc[0]
        hi = min(r.theta_hi, 95) if np.isfinite(r.theta_hi) else 95
        a2.plot([r.theta_lo, hi], [y + off, y + off], color=BLUE, lw=1.1, solid_capstyle="butt")
        if not np.isfinite(r.theta_hi) or r.theta_hi > 95:
            a2.annotate("", xy=(100, y + off), xytext=(88, y + off), arrowprops=dict(arrowstyle="->", color=BLUE, lw=0.9))
        a2.plot(r.theta, y + off, "o", ms=4, color=BLUE, mfc="white" if hollow else BLUE, mew=1.0)
a2.set_xscale("log"); a2.set_xlim(6, 100)
a2.set_xticks([6, 10, 20, 50, 100]); a2.set_xticklabels(["6", "10", "20", "50", "100"])
a2.set_yticks(yl); a2.set_yticklabels([r[1] for r in rows]); a2.tick_params(axis="y", length=0)
a2.set_ylim(-0.6, len(rows) - 0.1)
a2.set_xlabel("$\\theta$ with 95% interval (log scale)")
a2.set_title("(b) $\\theta$ across samples", loc="left")
a2.plot([], [], "o", color=BLUE, mfc="white", ms=4, label="Without instrument")
a2.plot([], [], "o", color=BLUE, ms=4, label="Distance instrument")
a2.legend(loc="lower left", frameon=False, handletextpad=0.3, borderaxespad=0.1)
fig.savefig(f"{OUT}/fig4_bryan_morten.pdf")
plt.close(fig)
print("done")
