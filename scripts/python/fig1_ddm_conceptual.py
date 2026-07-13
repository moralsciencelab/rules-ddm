import numpy as np
import matplotlib.pyplot as plt


pre_a = "#999999"   # fast / pre-conflict
post_a = "#2171B5" 
pre_v = "#999999"
post_v = "#D55E00" 


plt.rcParams.update({
    "font.family": "sans-serif",
    "font.sans-serif": ['Helvetica Neue', 'Helvetica', 'Arial', 'sans-serif'],
    "axes.titlesize": 17,
    "axes.titleweight": "bold",
    "axes.labelsize": 15,
    "axes.labelweight": "regular",
    "xtick.labelsize": 10,
    "ytick.labelsize": 10,
    "legend.fontsize": 10,
    "axes.linewidth": 1,
})

# -----------------------------
# Simulation helper
# -----------------------------
def simulate_ddm(v, a, noise=2.0, dt=0.01, n_steps=10000):
    """Simulate one drift diffusion trajectory, truncated at boundary crossing."""
    evidence = 0
    trajectory = [evidence]
    for i in range(n_steps):
        evidence += v * dt + np.random.normal(0, noise * np.sqrt(dt))
        trajectory.append(evidence)
        if abs(evidence) >= a:  # stop when a boundary is hit
            break
    trajectory = np.array(trajectory)
    trajectory[trajectory > a] = a
    trajectory[trajectory < -a] = -a
    return trajectory

def simulate_many(v, a, n_traj=6, dt=0.01):
    """
    Simulate multiple DDM trajectories and store decision outcome and RT.
    
    Returns:
        trajectories : list of np.arrays
        outcomes     : list of +1/-1 for upper/lower boundary
        rts          : list of reaction times (in same units as dt)
        p_upper      : proportion of upper boundary hits
        p_lower      : proportion of lower boundary hits
    """
    trajectories = []
    outcomes = []
    rts = []
    
    for _ in range(n_traj):
        tr = simulate_ddm(v, a, dt=dt)
        trajectories.append(tr)
        outcome = np.sign(tr[-1])
        outcomes.append(outcome)
        rt = (len(tr) - 1) * dt  # first step is t=0
        rts.append(rt)
        
    p_upper = np.mean(np.array(outcomes) == 1)
    p_lower = 1 - p_upper
    
    return trajectories, outcomes, rts, p_upper, p_lower



def truncated_walk(v, a, dt=0.01, noise=1.0, n_steps=1000):
    """Generate a deterministic drift + small noise trajectory, aligned with simulate_ddm."""
    evidence = np.zeros(n_steps)
    for i in range(1, n_steps):
        evidence[i] = evidence[i-1] + v*dt + np.random.normal(0, noise*np.sqrt(dt))
        if abs(evidence[i]) >= a:
            evidence[i:] = np.nan
            break
    t = np.arange(n_steps) * dt
    return t, evidence

# -----------------------------
# Parameters
# -----------------------------

params = {
    "urgency_high":  {"v": 0.35, "a": 1.5, "color": pre_v, "color_a": pre_a, "label": "fast"},
    "urgency_low":   {"v": 0.6, "a": 3, "color": post_v, "color_a": post_a, "label": "accurate"},
    "conflict_pre":  {"v": 0.6, "a": 3, "color": pre_v, "color_a": pre_a, "label": "pre-"},
    "conflict_post": {"v": 0.9, "a": 2, "color": post_v, "color_a": post_a,  "label": "post-"},
}

PANEL_PAIRS = [
    ("urgency_high", "urgency_low"),
    ("conflict_pre", "conflict_post"),
]
titles = [
    "Deliberation (Δv > 0,  Δa > 0)",
    "Adaptation (Δv > 0, Δa < 0)",
]
PANEL_LABELS = ["A", "B"]   # uppercase panel letters for the manuscript

# -----------------------------
# Figure setup
# -----------------------------

fig, axes = plt.subplots(1, 2, figsize=(12, 5), sharey=True)
plt.subplots_adjust(wspace=0.15)

# -----------------------------
# Left panel – Conflict adaptation
# -----------------------------
for i, (cond1, cond2) in enumerate([("urgency_high", "urgency_low"),
                                    ("conflict_pre", "conflict_post")]):
    ax = axes[i]
    ax.axhline(0, color="black", ls="--", lw=1)
    
    ax.text(-0.04, 1.07, chr(65 + i), transform=ax.transAxes,
            fontsize=20, fontweight="bold", va="top", color="#222")

    for cond in [cond1, cond2]:
        p = params[cond]
        trajectories, outcomes, p_upper, p_lower, rts = simulate_many(p["v"], p["a"], n_traj=10)
        # plot trajectories
        for tr in trajectories:
            t = np.arange(len(tr)) * 0.01
            ax.plot(t, tr, color=p["color"], alpha=0.4, lw=.6)

        # boundaries
        twalk_x, twalk_y = truncated_walk(p["v"], p["a"], 0.01, noise = .1)
        ax.plot(twalk_x, twalk_y, color=p["color"], linestyle='-', lw=3, label=p["label"])
        ax.axhline(p["a"], color=p["color_a"], ls="--", lw=1.5)
        ax.axhline(-p["a"], color=p["color_a"], ls="--", lw=1.5)
        # histogram bars (horizontal, proportional)
        # annotate
       # ax.text(0.6, p["a"] - 0.35, f"v = {p['v']}", color=p["color"], fontsize=9)
       # ax.text(0.6, -p["a"] + 0.25, f"a = {p['a']}", color=p["color"], fontsize=9)
    ax.set_title(titles[i], fontsize=18, weight="bold")
    ax.set_xlabel("Time", fontsize = 18)
    ax.set_xlim(0, 8)
    ax.set_ylim(-3.2, 3.2)
    ax.set_yticks([])
    ax.set_xticks([])
    ax.set_ylabel("Evidence accumulation" if i == 0 else "" , fontsize = 14)

# -----------------------------
# Legends and style
# -----------------------------
axes[0].text(
    1, 0.2 - params["urgency_high"]["a"], 
    params["urgency_high"]["label"],
    color=params["urgency_high"]["color_a"],
    fontsize=17,weight="bold",
    ha="center", va="bottom",
    # transform=axes[0].transAxes
)

axes[0].text(
    7, 0.2 - params["urgency_low"]["a"],
    params["urgency_low"]["label"],
    color=params["urgency_low"]["color_a"],
    fontsize=17, weight="bold",
    ha="center", va="bottom",
    # transform=axes[0].transAxes
)

axes[1].text(
    1, 0.2 - params["conflict_pre"]["a"], 
    params["conflict_pre"]["label"],
    color=params["conflict_pre"]["color_a"],
    fontsize=17, weight="bold",
    ha="center", va="bottom",
    # transform=axes[0].transAxes
)

axes[1].text(
    7, 0.2 - params["conflict_post"]["a"],
    params["conflict_post"]["label"],
    color=params["conflict_post"]["color_a"],
    fontsize=17, weight="bold",
    ha="center", va="bottom",
    # transform=axes[0].transAxes
)

axes[0].annotate(
    "", 
    xy=(0, params["urgency_high"]["a"]),       # top arrowhead
    xytext=(0, -params["urgency_high"]["a"]),   # bottom arrowhead
    arrowprops=dict(
        arrowstyle="<->",        # double-headed
        color=params["urgency_high"]["color_a"],
        lw=2,
    )
)

axes[1].annotate(
    "", 
    xy=(0, params["conflict_pre"]["a"]),       # top arrowhead
    xytext=(0, -params["conflict_pre"]["a"]),   # bottom arrowhead
    arrowprops=dict(
        arrowstyle="<->",        # double-headed
        color=params["conflict_pre"]["color_a"],
        lw=2,
    )
)

axes[0].annotate(
    "", 
    xy=(8, params["urgency_low"]["a"]),       # top arrowhead
    xytext=(8, -params["urgency_low"]["a"]),   # bottom arrowhead
    arrowprops=dict(
        arrowstyle="<->",        # double-headed
        color=params["urgency_low"]["color_a"],
        lw=2,
    )
)

axes[1].annotate(
    "", 
    xy=(8, params["conflict_post"]["a"]),       # top arrowhead
    xytext=(8, -params["conflict_post"]["a"]),   # bottom arrowhead
    arrowprops=dict(
        arrowstyle="<->",        # double-headed
        color=params["conflict_post"]["color_a"],
        lw=2,
    )
)


for ax in axes:
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.spines["left"].set_visible(False)
    ax.spines["bottom"].set_visible(False)

plt.tight_layout()

out_path = os.path.abspath(
    os.path.join(os.path.dirname(__file__), "..", "..", "figures", "Figure1.png")
)
os.makedirs(os.path.dirname(out_path), exist_ok=True)
plt.savefig(out_path, dpi=300, bbox_inches="tight")
print(f"Saved → {out_path}")
plt.show()
