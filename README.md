# DRIM2025_Project

# DRIM 2025 Project – Team Workflow Guide

## 👥 Team structure
The project is divided into 4 parallel modules so each member can work independently.

| Member | Folder | Role | Input | Output | Objective |
|---------|---------|------|--------|----------|------------|
| **A** | `A_FeatureEngineering` | Data cleaning & feature creation | `00_data_raw/DRIM_VF.txt` | `output_features.parquet` | Import raw txt, clean data, de-annualize PD, compute slope/curvature |
| **B** | `B_Aggregation_Descriptives` | Aggregation & visualization | `output_features.parquet` | `output_agg.csv` + plots | Aggregate by country/sector, compute monthly averages and gaps, visualize trends |
| **C** | `C_Clustering` | Segmentation analysis | `output_agg.csv` | `output_clusters.csv` + plots | Cluster countries/sectors by PD behaviour (k-means or hierarchical) |
| **D** | `D_Macro_Link_Report` | Macro link & final report | `output_clusters.csv`, `macro_data.xlsx` | `Final_Report.Rmd` or PPT | Link clusters to macro variables and build final storyline |

---

## 🔄 Workflow logic
Only one dependency: **A’s output** (`output_features.parquet`)  
Once it's ready, **B, C, and D** can all work *in parallel*.
