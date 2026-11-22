"""Figure 5 of the updated summary note (15 Oct 2026): the cost of heat at each value of the flow elasticity eta.

Same design as the 11 October Figure 5 (heat_project_summary_figures/make_figs.py); only the data and labels change.
Line and band: Census 2011 counts of men who moved for work in the last 0-4 years, eta fixed at each value
(census_grid_dense.csv). Red point: eta estimated with the district wage as measured. Gray squares: the wage
instrumented by the other random half of each district's earners (both directions) and by the other survey's wage
(census_iv.csv, origin fixed effects). Shaded band: the earnings-based theta (Bryan-Morten) and its 95% interval.

Usage: python3 -I make_fig5.py <census_grid_dense.csv> <census_iv.csv> <output dir>
"""
import sys
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt

GRID, IV, OUT = sys.argv[1:4]
mpl.rcParams.update({
    "font.family": "Palatino", "font.size": 9, "axes.labelsize": 9, "axes.titlesize": 9.5,
    "xtick.labelsize": 8, "ytick.labelsize": 8, "legend.fontsize": 8, "axes.spines.top": False,
    "axes.spines.right": False, "axes.linewidth": 0.6, "xtick.major.width": 0.6, "ytick.major.width": 0.6,
    "pdf.fonttype": 42, "savefig.bbox": "tight", "savefig.pad_inches": 0.03,
})
BLUE, LIGHT, RED, GRAY, BAND, BROWN = "#1f4e79", "#c6d7ea", "#b03a2e", "#7f7f7f", "#f6dcb5", "#8a5a12"

g = pd.read_csv(GRID)
free = g[g.theta == "free"].iloc[0]
cur = g[~g.theta.isin(["free", "0"])].copy()
cur["eta"] = cur.theta.astype(float)
cur = cur.sort_values("eta")
iv = pd.read_csv(IV)
iv = iv[(iv.fe == "o") & iv.instrument.isin(["split", "split_rev", "plfs"])]

fig, ax = plt.subplots(figsize=(6.3, 3.1))
ax.axvspan(11.7, 32.4, color=BAND, lw=0, zorder=0)
ax.axhline(0, color=GRAY, lw=0.6, ls="--", zorder=1)
ax.fill_between(cur.eta, 100 * cur.lo, 100 * cur.hi, color=LIGHT, lw=0, zorder=2)
ax.plot(cur.eta, 100 * cur.cost30, color=BLUE, lw=1.6, zorder=3)
ax.errorbar(free.theta_hat, 100 * free.cost30, yerr=[[100 * (free.cost30 - free.lo)], [100 * (free.hi - free.cost30)]],
            fmt="o", color=RED, ms=5, lw=1.2, capsize=0, zorder=4)
ax.annotate("District wage as measured\n($\\eta$ estimated at 1.8)", (free.theta_hat, 100 * free.cost30), xytext=(0.5, 17.0),
            fontsize=8, color=RED, arrowprops=dict(arrowstyle="-", color=RED, lw=0.6))
for _, r in iv.iterrows():
    ax.errorbar(r.eta, 100 * r.cost30, yerr=[[100 * (r.cost30 - r.cost_lo)], [100 * (r.cost_hi - r.cost30)]],
                fmt="s", color=GRAY, ms=4.5, lw=1.1, capsize=0, zorder=4)
    ax.annotate("", (r.eta, 100 * r.cost_lo - 0.1), xytext=(2.6, -4.6), arrowprops=dict(arrowstyle="-", color=GRAY, lw=0.5))
ax.text(2.6, -5.0, "Wage instrumented: other half of each district's\nearners (2.4, 2.9) or the other survey's wage (3.7)",
        fontsize=7.5, color="#555555", ha="center", va="top")
for th in (12.9, 17.2):
    ax.axvline(th, color="#c98a2b", lw=0.8, zorder=1)
ax.text(12.9, 23.6, "12.9 ", fontsize=7.5, color=BROWN, ha="right", va="top")
ax.text(17.2, 23.6, " 17.2", fontsize=7.5, color=BROWN, ha="left", va="top")
ax.text(20.5, 20.0, "Earnings-based $\\theta$\nand its 95% interval:\nan upper bound on $\\eta$", fontsize=7.5, color=BROWN, ha="center", va="top")
ax.text(5.3, 6.5, "$\\eta$ fixed at each value\n(line and 95% band)", fontsize=7.5, color=BLUE)
ax.set_xscale("log")
ax.set_xlim(0.45, 36); ax.set_ylim(-7.5, 25)
ax.set_xticks([0.5, 1, 2, 5, 10, 20, 30]); ax.set_xticklabels(["0.5", "1", "2", "5", "10", "20", "30"])
ax.set_xlabel("Migrants' response to pay, $\\eta$ (log scale)")
ax.set_ylabel("Cost of 30 more hot days\n(percent of income)")
top = ax.secondary_xaxis("top")
top.set_xticks([12.5, 27.6]); top.set_xticklabels(["Indonesia", "US"], fontsize=7.5, color=GRAY)
top.tick_params(length=3, color=GRAY)
top.spines["top"].set_visible(False)
top.xaxis.set_minor_locator(mpl.ticker.NullLocator())
fig.savefig(f"{OUT}/fig5_eta_cost.pdf")
fig.savefig(f"{OUT}/fig5_eta_cost_check.png", dpi=110)
plt.close(fig)
print("cost at estimated eta:", round(100 * free.cost30, 2), "| IV:", [(r.instrument, round(r.eta, 2), round(100 * r.cost30, 2)) for _, r in iv.iterrows()])
