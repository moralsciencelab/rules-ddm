# Textualism as an effort-accuracy trade-off in rule enforcement

**Authors:** Ivar R. Hannikainen, Neele Engelmann, Carlos González-García, María Ruz

**Abstract:** People often enforce rules according to their letter even when doing so
undermines their deeper spirit. Across multiple experiments, we show that dilemmas
pitting a rule's letter against its spirit elicit cognitive conflict, and examine whether
their textualist resolution reflects improved deliberation or adaptation to cognitive
demands. Contrary to the deliberation model, inducing response caution did not increase
letter-based verdicts. Meanwhile, cognitive demands — even when imposed through unrelated
interference tasks — did. Drift diffusion modeling indicated that this shift enabled a
less effortful resolution of rule enforcement dilemmas through faster accumulation of
textual evidence and reductions in the evidence threshold. We propose that textualism
emerges as an adaptation to the cognitive labor of rule enforcement, reducing effort
while preserving accuracy on low-conflict cases.

**Preregistration:** https://researchbox.org/3227&PEER_REVIEW_passcode=AHEJAU

**OSF repository:** https://osf.io/v35xj/

---

## Repository structure

```
.
├── data/
│   ├── exp1a/                  ← Experiment 1a (violation judgments)
│   ├── exp1b/                  ← Experiment 1b (compliance judgments)
│   ├── exp1c/                  ← Experiment 1c (punishment judgments)
│   ├── exp2/                   ← Experiment 2 (cross-task correlations: Rules + Stroop + Flanker)
│   ├── exp3a/                  ← Experiment 3a (speed vs. accuracy instructions)
│   ├── exp3b/                  ← Experiment 3b (congruency proportion manipulation)
│   ├── exp4a/                  ← Experiment 4a (task experience vs. executive demands, between-subjects)
│   ├── exp4b/                  ← Experiment 4b (task experience vs. executive demands, pre-to-post)
│   ├── rules_allstudies.csv    ← combined dataset across all studies
│   ├── codingsheet.csv         ← stimulus coding for Experiments 1–3 (text/purpose violation flags)
│   ├── stimulus_coding.csv     ← stimulus coding for Experiments 4a–4b (case labels: v/c/o/u)
│   ├── demographics_allstudies.csv
│   └── codebook.Rmd            ← variable documentation for rules_allstudies.csv
│
├── scripts/
│   ├── r/                      ← behavioral analyses and figure generation (R)
│   │   ├── study1.R            ← Experiments 1a–c
│   │   ├── study2.R            ← Experiment 2
│   │   ├── study3.R            ← Experiments 3a–b
│   │   ├── study4a.R           ← Experiment 4a
│   │   ├── study4b.R           ← Experiment 4b
│   │   ├── mega-analysis.R     ← trial-by-trial adaptation analysis (Studies 1a–3b)
│   │   ├── figure2.R           ← Figure 2 (RT histograms + DDM illustration)
│   │   └── exp*/               ← per-experiment wrangling and cleaning scripts
│   ├── python/                 ← DDM figure scripts
│   │   ├── main_ddm.ipynb      ← main HDDM models (Studies 1, 3, 4)
│   │   ├── crosstask.ipynb     ← cross-task DDM (Study 2)
│   │   ├── bifactor.ipynb      ← bifactor model (Study 2)
│   │   ├── fig6_parameter_space.py
│   │   └── fig1_ddm_conceptual.py
│   └── ddm/
│       └── hddm_neele.ipynb    ← additional DDM analyses
│
├── results/
│   ├── ddm/                    ← HDDM model outputs (one subfolder per model)
│   └── *.csv                   ← summary statistics and model coefficient tables
│
├── figures/                    ← final figure files (manuscript + supplementary)
│   ├── Figure1.png – Figure6.png
│   ├── SuppFigure1.png, SuppFigure2.png
│   └── exp*/                   ← intermediate figure outputs per experiment
│
├── materials/                  ← jsPsych experiment code and stimuli
│   ├── experiment1a.html – experiment4b.html
│   ├── rules.js                ← stimulus presentation logic
│   ├── rules_congruency.js     ← congruency-proportion variant
│   └── irb/                    ← ethics approval documents
│
└── manuscript/                 ← LaTeX source
    ├── textualism-ddm.tex
    ├── textualism-ddm.bib
    ├── sn-jnl.cls              ← Springer Nature journal class
    └── sn-nature.bst           ← bibliography style
```

Each experiment subfolder under `data/` contains:

- `raw_batch*.csv` — anonymized Prolific downloads
- `clean.csv` — cleaned data used by analysis scripts
- `demographics.csv` — age, gender, and other participant variables

---

## Reproducing the analyses

### Dependencies

**R** (behavioral analyses): `tidyverse`, `lme4`, `lmerTest`, `emmeans`, `lavaan`, `broom`, `patchwork`, `ggplot2`, `ggpubr`, `magick`

**Python** (drift diffusion modeling): requires the `pyHDDM` conda environment:

```bash
conda create -n pyHDDM python=3.6
conda activate pyHDDM
conda install pymc=2.3.8 -c conda-forge
conda install pandas patsy seaborn
pip install cython hddm
conda install jupyter
```

### Steps

1. **Behavioral analyses** — run scripts in `scripts/r/` from the repo root (`.Rproj` sets the working directory). Order: `study1.R` → `study2.R` → `study3.R` → `study4a.R` → `study4b.R`. The mega-analysis script (`mega-analysis.R`) requires studies 1–3 to have been run first.

2. **DDM modeling** — activate the `pyHDDM` environment and run notebooks in `scripts/python/` with the working directory set to `data/`. Key notebooks: `main_ddm.ipynb` (Studies 1, 3, 4), `crosstask.ipynb` and `bifactor.ipynb` (Study 2). Model outputs are saved to `results/ddm/`.

3. **Figures** — run `scripts/r/figure2.R` for Figure 2, and `scripts/python/fig6_parameter_space.py` and `fig1_ddm_conceptual.py` for Figures 1 and 6. Remaining figures are generated within the per-study R scripts.

---

## Citation

> Hannikainen, I. R., Engelmann, N., González-García, C., & Ruz, M. (in preparation). Textualism as an effort-accuracy trade-off in rule enforcement.

## License

Data and analysis code are released under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
Experiment materials are available for reuse with attribution.
