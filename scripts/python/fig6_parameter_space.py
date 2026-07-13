"""
Figure 6 – DDM parameter-space topography
"""

import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# ---------------------------------------------------------------------------
# Style  —  Helvetica Neue / Arial (Nature figure convention: sans-serif)
# ---------------------------------------------------------------------------
plt.rcParams.update({
    "font.family":       "sans-serif",
    "font.sans-serif":   ["Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"],
    "font.size":         12,
    "axes.titlesize":    14,
    "axes.labelsize":    13,
    "legend.fontsize":   12,
    "figure.titlesize":  15,
    "figure.dpi":        600,
})

# ---------------------------------------------------------------------------
# Analytical DDM  —  asymmetric starting point z (fraction of a)
# ---------------------------------------------------------------------------
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

    Notes
    -----
    With z != 0.5 the probability of hitting the upper boundary is:

        P(upper) = [1 - exp(-2 v z_abs)] / [1 - exp(-2 v a)]

    where z_abs = z * a.  Mean decision time conditioned on the response is
    more complex; here we use the unconditional mean DT (Ratcliff 1978):

        E[DT] = (a/2v) * tanh(v*a/2)   corrected for starting-point shift:
              = (1/v) * [ z_abs - a * P(upper) * (1 - exp(-2v*(a-z_abs))) /
                                                  (1 - exp(-2v*a)) ]

    For simplicity (and because z is close to 0.5) we use the standard
    unconditional approximation + a small bias correction term.
    """
    v_abs  = np.maximum(v_abs, 1e-8)
    z_abs  = z * a                          # absolute starting point

    # --- accuracy ---
    exp_neg2va  = np.exp(-2.0 * v_abs * a)
    exp_neg2vz  = np.exp(-2.0 * v_abs * z_abs)
    p_upper_ddm = (1.0 - exp_neg2vz) / (1.0 - exp_neg2va + 1e-300)
    acc         = (1.0 - gamma) * p_upper_ddm + gamma * 0.5

    # --- mean RT ---
    # For z close to 0.5, the symmetric tanh approximation is accurate;
    # the full asymmetric formula introduces numerical issues for large a*v.
    dt = (a / (2.0 * v_abs)) * np.tanh(v_abs * a / 2.0)
    rt = dt + ndt

    return acc, rt


def run_sweep(v_values, a_values, z=0.53, ndt=1.0):
    V, A        = np.meshgrid(v_values, a_values)
    acc, rt     = ddm_analytical(V, A, z=z, ndt=ndt)
    acc_pivot   = pd.DataFrame(acc, index=a_values, columns=v_values)
    rt_pivot    = pd.DataFrame(rt,  index=a_values, columns=v_values)
    return acc_pivot, rt_pivot


# ---------------------------------------------------------------------------
# Parameter grid
# ---------------------------------------------------------------------------
N = 1000
a_values = np.linspace(2.5, 3.5, N)
v_inc    = np.linspace(0.00, 0.60, N)   # incongruent: |v_text − v_purpose|
v_con    = np.linspace(0.60, 1.20, N)   # congruent:   |v_text + v_purpose|
Z_MEAN = 0.530
NDT    = 1.25 

acc_inc, rt_inc = run_sweep(v_inc, a_values, z=Z_MEAN, ndt=NDT)
acc_con, rt_con = run_sweep(v_con, a_values, z=Z_MEAN, ndt=NDT)

# Shared colour scales
acc_vmin, acc_vmax = 0.50, 1.00
rt_vmin  = min(rt_inc.values.min(), rt_con.values.min())
rt_vmax  = max(rt_inc.values.max(), rt_con.values.max())

# ---------------------------------------------------------------------------
# Empirical arrows  (v_start, v_end, a_start, a_end)
# ---------------------------------------------------------------------------
CONDITIONS = ["Experience", "Demands"]

COLORS_LIGHT_BG = ["#f0f0f0", "#4a4a4a"]
COLORS_DARK_BG  = ["#f0f0f0", "#4a4a4a"]

arrows_inc = {
    "Experience": (0.154,  0.2595, 3.13, 2.969),
    "Demands":   (0.1285, 0.3065, 3.07, 2.951),
}
arrows_con = {
    "Experience": (0.956,  1.0475, 3.13, 2.969),
    "Demands":   (0.8915, 1.0155, 3.07, 2.951),
}

label_offsets_inc = {
    "Experience": ( 0.03, -0.13),
    "Demands":   ( 0.01,  0.05),
}
label_offsets_con = {
    "Experience": (-0.09,  0.24),
    "Demands":   (-0.04, -0.07),
}

# ---------------------------------------------------------------------------
# Plot
# ---------------------------------------------------------------------------
fig, axes = plt.subplots(
    2, 2, figsize=(12, 9),
    gridspec_kw={"wspace": 0.08, "hspace": 0.04},
)

from matplotlib.colors import LinearSegmentedColormap

cmap_acc = LinearSegmentedColormap.from_list('acc', ['#c8c8c8', '#D55E00'])
cmap_rt  = LinearSegmentedColormap.from_list('rt',  ['#2171B5', '#c8c8c8'])

panel_configs = [
    (axes[0, 0], acc_inc, v_inc, arrows_inc, cmap_acc, acc_vmin, acc_vmax, "Response proportion", "Textualism (incongruent trials)", COLORS_DARK_BG),
    (axes[0, 1], acc_con, v_con, arrows_con, cmap_acc, acc_vmin, acc_vmax, "Response proportion", "Accuracy (congruent trials)",     COLORS_LIGHT_BG),
    (axes[1, 0], rt_inc,  v_inc, arrows_inc, cmap_rt,  rt_vmin,  rt_vmax,  "Mean RT (s)",         " ",                              COLORS_DARK_BG),
    (axes[1, 1], rt_con,  v_con, arrows_con, cmap_rt,  rt_vmin,  rt_vmax,  "Mean RT (s)",         " ",                              COLORS_LIGHT_BG),
]

row_ims = {}

for idx, (ax, piv, v_vals, arrows, cmap, vmin, vmax, cbar_label, title, colors) in enumerate(panel_configs):
    V, A = np.meshgrid(piv.columns.values, piv.index.values)
    Z    = piv.values

    im = ax.imshow(
        Z, origin="lower", aspect="auto",
        extent=[v_vals.min(), v_vals.max(), a_values.min(), a_values.max()],
        cmap=cmap, vmin=vmin, vmax=vmax, interpolation="bilinear",
    )
    row = 0 if idx < 2 else 1
    row_ims[row] = (im, cbar_label)

    ax.set_title(title, fontsize=14, pad=6)
    ax.set_xticks([])
    ax.set_yticks([])

    if idx >= 2:
        xlabel = (
            "Drift rate (" + r"$|v_{\rm text} - v_{\rm purpose}|$" + ")"
            if idx % 2 == 0
            else "Drift rate (" + r"$|v_{\rm text} + v_{\rm purpose}|$" + ")"
        )
        ax.set_xlabel(xlabel)
        if idx == 3:
            ticks = ax.get_xticks()
            ax.set_xticks(ticks[1:])
    else:
        ax.set_xticklabels([])

    if idx % 2 == 0:
        ax.set_ylabel("Threshold  ($a$)")
    else:
        ax.set_yticklabels([])

    if idx < 2:
        levels = sorted(set(np.arange(0.5, 0.95, 0.1).tolist() + [0.95, 0.97, 0.98, 0.99]))
        c = ax.contour(V, A, Z, levels=levels,
                       colors="white", linestyles="solid", linewidths=1.0, alpha=0.6)
        ax.clabel(c, inline=True, fontsize=11, fmt="%.2f")
    else:
        rt_levels = np.arange(np.ceil(rt_vmin * 5) / 5, rt_vmax, 0.2)
        c = ax.contour(V, A, Z, levels=rt_levels,
                       colors="white", linestyles="solid", linewidths=1.0, alpha=0.6)
        ax.clabel(c, inline=True, fontsize=11, fmt="%.1fs")

    label_offsets = label_offsets_inc if idx % 2 == 0 else label_offsets_con
    for cond, color in zip(CONDITIONS, colors):
        v_s, v_e, a_s, a_e = arrows[cond]
        ax.annotate(
            "", xy=(v_e, a_e), xytext=(v_s, a_s),
            arrowprops=dict(arrowstyle="->", color=color, lw=1.5, mutation_scale=14),
            zorder=2,
        )
        ax.plot(v_s, a_s, "o", color=color, ms=8,
                markerfacecolor="white", markeredgewidth=1.5, zorder=3)
        dx, dy = label_offsets[cond]
        ax.text(v_e + dx, a_e + dy, cond,
                color=color, fontsize=13, va="center", fontweight="bold")

# ---------------------------------------------------------------------------
# One colourbar per row
# ---------------------------------------------------------------------------
for row, (im, label) in row_ims.items():
    cbar = fig.colorbar(im, ax=axes[row, :], shrink=0.80, pad=0.02)
    cbar.set_label(label, fontsize=12)


for i, ax in enumerate(axes[0, :]):
    ax.text(-0.06, 1.1, chr(65 + i), transform=ax.transAxes,
            fontsize=18, fontweight="bold", va="top", color="#111")
# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------
plt.tight_layout()

out_path = os.path.abspath(
    os.path.join(os.path.dirname(__file__), "..", "..", "figures", "Figure6.png")
)
os.makedirs(os.path.dirname(out_path), exist_ok=True)
plt.savefig(out_path, dpi=300, bbox_inches="tight")
print(f"Saved → {out_path}")
plt.show()