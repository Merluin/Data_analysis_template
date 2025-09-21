# 🧠 PsychoPy Behavioral Data Analysis Template

This R project provides a clean and modular template for analyzing behavioral data collected using [PsychoPy](https://www.psychopy.org/). It is designed to aggregate `.csv` files exported from PsychoPy, clean and structure them, and compute key performance and signal detection metrics.

> ✅ Click the **“Use this template”** button above to create your own analysis-ready repository!

---

## 📦 Features

- 🔄 Automatic concatenation of `.csv` files
- 🧼 Data cleaning and formatting (e.g., participant ID, gender)
- 🧠 Signal Detection Theory (SDT) metrics (`d'`, `c`)
- ⏱️ Reaction time (RT) summaries
- ✅ Accuracy summaries
- 📊 Outlier detection
- 📁 Data export to `.RData` or `.xlsx`

---

## 📂 Project Structure
```bash
AnalysisRproj/
├── DESCRIPTION             # R package metadata
├── R/                       # All core analysis functions
├── data/                    # Cleaned datasets saved as .RData
├── original_data/           # Raw .csv files from PsychoPy
├── scripts/                 # Custom analysis scripts (e.g., per experiment)
├── LICENSE                  # MIT License
├── README.md                # You’re here!
└── Data_analysis.Rproj      # RStudio project file
```
---

## 🚀 Getting Started

### 1. Clone or use this template

Click the green **“Use this template”** button to copy this repo to your own GitHub account.

Or clone it manually:

```bash
git clone https://github.com/YOUR_USERNAME/AnalysisRproj.git
```
2. Open the project

Open Data_analysis.Rproj in RStudio.

3. Load dependencies

Install required packages if you don’t already have them:
```bash
install.packages(c("dplyr", "tidyr", "readxl", "openxlsx", "ggplot2", "afex", "emmeans", "effectsize", "devtools"))
```
4. Run the analysis
 write your analysis and keep the scripts in scripts folder
 
📄 License

This project is licensed under the MIT License.

© 2025 Thomas Quettier

⸻

🤝 Contributions

This repository is a template. You are welcome to fork it, adapt it, and contribute improvements via pull requests.
(Collaborators) Please precede your commit's message with one or more of the following:

BF: bug fix
FF: feature fix. This is for fixes to code that hasn’t been released
RF: refactoring
NF: new feature
ENH: enhancement (improvement to existing code)
DOC: for all kinds of documentation-related commits
TEST: for adding or changing tests

⸻

🧠 Acknowledgements
- [PsychoPy](https://www.psychopy.org) for stimulus presentation  
- [R and the tidyverse](https://cran.r-project.org) for data processing  
- [My website](https://tcjq.eu)
