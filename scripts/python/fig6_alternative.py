"""
Figure 6 – DDM parameter-space topography  (alternative 2-panel layout)
One panel per outcome (response proportion · RT), spanning the full drift-rate
range. Incongruent-trial arrows are solid; congruent-trial arrows are dashed.
"""

import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
from matplotlib.patches import FancyArrowPatch

# ── Style ─────────────────────────────────────────────────────────────────────
plt.rcParams.update({
    "font.family":     "sans-serif",
    "font.sans-serif": ["Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"],
    "font.size":        12,
    "axes.titlesize":   14,
    "axes.labelsize":   13,
    "legend.fontsize":  12,
    "figure.titlesize": 15,
    "figure.dpi":       600,
})

# ── Palette ───────────────────────────────────────────────────────────────────
cmap_acc = LinearSegmentedColormap.from_list('acc', ['#c8c8c8', '#D55E00'])
cmap_rt  = LinearSegmentedColormap.from_list('rt',  ['#2171B5', '#c8c8c8'])

CONDITIONS = ["Experience", "Demands"]
COLORS     = ["#f0f0f0", "#4a4a4a"]   # white=Experience, dark grey=Demands

# ── Analytical DDM ────────────────────────────────────────────────────────────
def ddm_analytical(v_abs, a, z=0.5, ndt=1.0, gamma=0.05):
    """
    Closed-form DDM accuracy and mean RT for asymmetric starting point.

    Parameters
    ----------
    v_abs  : absolute drift rate (positive scalar or array)
    a      : boundary separation
    z      : relative starting point as fraction of a (0.5 = unbiased)
    ndt    : non-decision time (s)
    gamma  : contaminant mixture weight (lapse rate)
    """
    v_abs = np.maximum(v_abs, 1e-8)
    z_abs = z * a

    exp_neg2va  = np.exp(-2.0 * v_abs * a)
    exp_neg2vz  = np.exp(-2.0 * v_abs * z_abs)
    p_upper_ddm = (1.0 - exp_neg2vz) / (1.0 - exp_neg2va + 1e-300)
    acc         = (1.0 - gamma) * p_upper_ddm + gamma * 0.5

    dt = (a / (2.0 * v_abs)) * np.tanh(v_abs * a / 2.0)
    rt = dt + ndt

    return acc, rt


def run_sweep(v_values, a_values, z, ndt):
    V, A    = np.meshgrid(v_values, a_values)
    acc, rt = ddm_analytical(V, A, z=z, ndt=ndt)
    return (pd.DataFrame(acc, index=a_values, columns=v_values),
            pd.DataFrame(rt,  index=a_values, columns=v_values))


# ── Parameter grid ────────────────────────────────────────────────────────────
N        = 1000
Z_MEAN   = 0.530
NDT      = 1.25
a_values = np.linspace(2.5, 3.5, N)
v_full   = np.linspace(0.00, 1.20, N)   # full range: inc (0–0.6) + con (0.6–1.2)

acc_full, rt_full = run_sweep(v_full, a_values, z=Z_MEAN, ndt=NDT)

acc_vmin, acc_vmax = 0.50, 1.00
rt_vmin  = rt_full.values.min()
rt_vmax  = rt_full.values.max()

# ── Empirical arrows  (v_start, v_end, a_start, a_end) ───────────────────────
arrows_inc = {
    "Experience": (0.154,  0.2595, 3.13, 2.969),
    "Demands":    (0.1285, 0.3065, 3.07, 2.951),
}
arrows_con = {
    "Experience": (0.956,  1.0475, 3.13, 2.969),
    "Demands":    (0.8915, 1.0155, 3.07, 2.951),
}
label_offsets_inc = {
    "Experience": ( 0.03, -0.13),
    "Demands":    ( 0.01,  0.05),
}
label_offsets_con = {
    "Experience": (-0.09,  0.24),
    "Demands":    (-0.04, -0.07),
}

# ── Figure ────────────────────────────────────────────────────────────────────
fig, axes = plt.subplots(
    2, 1, figsize=(12, 8),
    gridspec_kw={"hspace": 0.08},
)

panel_configs = [
    (axes[0], acc_full, "acc", cmap_acc, acc_vmin, acc_vmax, "Response proportion", "A"),
    (axes[1], rt_full,  "rt",  cmap_rt,  rt_vmin,  rt_vmax,  "Mean RT (s)",         "B"),
]

for ax, piv, kind, cmap, vmin, vmax, cbar_label, letter in panel_configs:
    V, A = np.meshgrid(piv.columns.values, piv.index.values)
    Z    = piv.values

    im = ax.imshow(
        Z, origin="lower", aspect="auto",
        extent=[v_full.min(), v_full.max(), a_values.min(), a_values.max()],
        cmap=cmap, vmin=vmin, vmax=vmax, interpolation="bilinear",
    )

    # Contours
    if kind == "acc":
        levels = sorted(set(np.arange(0.5, 0.95, 0.1).tolist() + [0.95, 0.97, 0.98, 0.99]))
        c = ax.contour(V, A, Z, levels=levels,
                       colors="white", linestyles="solid", linewidths=1.0, alpha=0.6)
        ax.clabel(c, inline=True, fontsize=11, fmt="%.2f")
    else:
        rt_levels = np.arange(np.ceil(rt_vmin * 5) / 5, rt_vmax, 0.2)
        c = ax.contour(V, A, Z, levels=rt_levels,
                       colors="white", linestyles="solid", linewidths=1.0, alpha=0.6)
        ax.clabel(c, inline=True, fontsize=11, fmt="%.1fs")

    ax.set_xticks([])
    ax.set_yticks([])
    ax.set_ylabel("Threshold  ($a$)")
    if ax is axes[1]:
        ax.set_xlabel("Drift rate ($|v|$)")
    else:
        ax.set_xticklabels([])

    # Panel letter
    ax.text(-0.06, 1.08, letter, transform=ax.transAxes,
            fontsize=18, fontweight="bold", va="top", color="#111")

    # Colorbar
    cbar = fig.colorbar(im, ax=ax, shrink=0.80, pad=0.02)
    cbar.set_label(cbar_label, fontsize=12)

    # Arrows ── incongruent (solid, labelled) and congruent (dashed, unlabelled)
    for cond, color in zip(CONDITIONS, COLORS):

        # Incongruent — solid annotate arrow + label
        v_s, v_e, a_s, a_e = arrows_inc[cond]
        ax.annotate(
            "", xy=(v_e, a_e), xytext=(v_s, a_s),
            arrowprops=dict(arrowstyle="->", color=color, lw=1.5, mutation_scale=14),
            zorder=2,
        )
        ax.plot(v_s, a_s, "o", color=color, ms=8,
                markerfacecolor="white", markeredgewidth=1.5, zorder=3)
        dx, dy = label_offsets_inc[cond]
        ax.text(v_e + dx, a_e + dy, cond,
                color=color, fontsize=13, va="center", fontweight="bold")

        # Congruent — dashed FancyArrowPatch, no duplicate label
        v_s, v_e, a_s, a_e = arrows_con[cond]
        patch = FancyArrowPatch(
            (v_s, a_s), (v_e, a_e),
            arrowstyle="->", linestyle="dashed",
            color=color, lw=1.5, mutation_scale=14,
            transform=ax.transData, zorder=2,
        )
        ax.add_patch(patch)
        ax.plot(v_s, a_s, "o", color=color, ms=8,
                markerfacecolor="white", markeredgewidth=1.5, zorder=3)

# ── Save ──────────────────────────────────────────────────────────────────────
plt.tight_layout()

out_path = os.path.abspath(
    os.path.join(os.path.dirname(__file__), "..", "..", "figures", "Figure6_alternative.png")
)
os.makedirs(os.path.dirname(out_path), exist_ok=True)
plt.savefig(out_path, dpi=300, bbox_inches="tight")
print(f"Saved → {out_path}")
plt.show()
