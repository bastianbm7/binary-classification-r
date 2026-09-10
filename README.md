# Binary Classification in R

Freelance project applying and comparing multiple binary classification algorithms in R on a survey-style dataset, including EDA, model training, and full performance evaluation (ROC/AUC, learning curves).

## What it does

`Proyect2.r` runs the full pipeline standalone, end to end:

- **Setup & EDA**: auto-installs any missing required packages, loads the survey data from `GUIA-1.xlsx`, profiles it (structure, unique values per categorical column, missing values), recodes the target variable (`RESULTADO`) to 0/1, and produces pie charts, a pairs plot, and bar plots (`ggplot2`, `gridExtra`).
- **Preprocessing**: scales numeric features and reduces them with PCA (keeping the first 15 components) before modeling.
- **Modeling**: trains and compares several classifiers — **Logistic Regression, XGBoost, SVM, a neural network (nnet), and Random Forest (randomForestSRC)** — using `caret` for evaluation and `pROC` for ROC/AUC.
- **Evaluation artifacts**: exported plots for the ROC curve, learning rate/curve, pairs plot, bar plot, and class distribution pie chart.

## Tech stack

R — `tidyverse`, `caret`, `xgboost`, `e1071`, `nnet`, `randomForestSRC`, `pROC`, `ggplot2`, `gridExtra`, `openxlsx`.

## Project structure

```
Proyect2.r          # Full pipeline: setup, EDA, PCA, modeling, evaluation
GUIA-1.xlsx         # Source survey dataset
rocCurve.jpg, learningRate.jpg, pairsPlot.jpg / pairs_plot.png, barpPlot.jpg, pieChart.jpg
```

## How to run

Open `Proyect2.r` in RStudio (or run with `Rscript`) and run it top to bottom — it's self-contained. It calls a `verificar_paquetes()` helper at the top that automatically installs any of the required packages that aren't already present, so a fresh R environment is enough to get started. Update the `setwd('PATH')` call at the top of the script to point at this project's folder before running (the path to `GUIA-1.xlsx` inside the script is already relative).

## Notes from a static review

R isn't available in the environment used to verify this portfolio, so this script could not be executed end-to-end — the review below is from reading the code, not from a live run.

- Fixed: `setwd()` at the top was hardcoded to an absolute path on the original author's machine — changed to a placeholder (`setwd('PATH')`) so it's obvious this needs to be set per-environment. `GUIA-1.xlsx` is already read via a relative path.
- `modelPipeline()` takes `varNum`/`varCat` parameters but never actually uses them — it reads `varNumericas_scaled` and `varCategoricas` from the global environment instead. It still works because those globals exist by the time the function is called, but the parameters are dead code and the argument order at the call sites doesn't match what the parameter names imply.
- Each classifier function uses a different, hand-picked probability threshold (0.31, 0.15, 0.1, 0.005, 0.3) instead of the conventional 0.5 — likely tuned for this dataset's class balance, but worth a comment explaining why if this is reused elsewhere.
