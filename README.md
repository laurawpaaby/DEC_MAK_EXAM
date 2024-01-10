# Decision Making Exam: Cognitive Modeling of The Pizza Game 🍕
__Contributors:__ [Daniel Blumenkranz](https://github.com/daniblu) & [Laura Wulff Paaby](https://github.com/laurawpaaby)

The current repository contains the code for our exam project for the course [Decision making](https://kursuskatalog.au.dk/en/course/119712/Decision-making) at the Cognitive Science MSc 2023, Aarhus University.

## Setup
Make sure JAGS is installed on your system. This can be achieved by entering the following in the terminal
```bash
sudo apt install jags
```

Open the Rproject ``DEC_MAK_EXAM.Rproj``. Enter the following in the R console to set up a project-specific package environment
```R
renv::restore()
```

## Usage
To conduct a parameter recovery analysis testing our Bayesian model implementation of the cognitive model, the following can be entered in the terminal
```bash
Rscript src/subject_recovery.R # subject-level parameter recovery
Rscript src/group_recovery.R # group parameter mean recovery
Rscript src/group_diff_recovery.R # group parameter means difference recovery
```

The user is not enabled to replicate our estimation of group parameter means and group parameter mean differences since this requires the Pizza Game data which we are prohibited from making available in the repository. Real parameter estimation, though, would have been achieved by the following
```bash
Rscript src/group_mean_estimation.R
Rscript src/group_diff_estimation.R
```

## Repository overview
```
.
├── .gitignore
├── .Rprofile                                   <--- script related to environment setup
├── DEC_MAK_EXAM.Rproj
├── jags_output/                                <--- folder containing BUGS objects and data frames containing MPD values for plotting
├── plots/                                      <--- folder containing all plots produced by the scripts in src/
├── README.md
├── renv/                                       <--- folder for storing project environment packages after using renv::restore()
├── renv.lock                                   <--- list of packages automatically added to environment by renv::restore()
└── src/
    ├── Simulations.Rmd                         <--- messy markdown for experimenting with distributions
    ├── group_diff_estimation.R
    ├── group_diff_model_no_reparam.txt         <--- unused Bayesian model
    ├── group_diff_model.txt
    ├── group_diff_recovery.R
    ├── group_mean_estimation.R
    ├── group_model.txt
    ├── group_recovery.R
    ├── plot_functions.R                         <--- collection of all plotting functions utilized across the scripts
    ├── simulation_functions.R                   <--- collection of all data simulation functions utilized across the scripts (mainly for recovery)
    ├── subject_model_norm.txt                   <--- unused Bayesian model
    ├── subject_model.txt
    └── subject_recovery.R
```
