# 🧠 DRIM 2025 – Risk Dynamics Modelling Project

This repository contains the full workflow for the **DRIM 2025 Challenge**, from raw data cleaning to macro-level analysis of Probability of Default (PD) dynamics.  
The project is organized into **four parallel R modules**, so each member can work independently while contributing to a single analytical pipeline.

---

## 👥 Team structure

| Member | Folder | Role | Input | Output |
|:------:|:----------------------------|:------------------------------|:------------------------------------|:-----------------------------------|
| **A** | `A_Data_Features` | Data cleaning & PD feature engineering | `00_data_raw/DRIM_VF.txt` | `output_features.parquet` |
| **B** | `B_Descriptive_Analysis` | Aggregation & visualization | `output_features.parquet` | `output_agg.csv`, plots |
| **C** | `C_Clustering_Segmentation` | Risk segmentation (clustering) | `output_agg.csv` | `output_clusters.csv`, cluster plots |
| **D** | `D_Dependencies_Macro` | Dependencies & macro modelling | `output_clusters.csv`, `macro_data.xlsx` | `output_model_results.csv`, macro plots |

---

## 🔄 Workflow logic

All members work **in parallel** after the cleaned dataset (`output_features.parquet`) is created by A.

```text
[Raw TXT]
   │
   ▼
(A) Cleaning & Features → output_features.parquet
   │
   ├──→ (B) Aggregation_Descriptive → output_agg.csv
   ├──→ (C) Clustering_Segmentation → output_clusters.csv
   └──→ (D) Dependencies_Macro → output_model_results.csv
