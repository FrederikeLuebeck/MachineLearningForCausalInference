# Machine Learning for Causal Inference: Estimating Heterogeneous Treatment Effects with Causal Forests

This Repository contains my Bachelor's thesis (PDF) along with the codes of the simulations I conducted, as well as the code of the analysis of the empirical data. The codes are organized in the same order as they appear in the thesis.

## Code of Simulations

### Function Setup

The script named *00_FunctionSetup.R* initializes all functions used for the simulations. This includes the data generating processes, the estimators and the parallelization function.

### Strong Treatment Effect Heterogeneity

These simulations are to explore the ability of Causal Forests to detect treatment effect heterogeneity. In my thesis, this corresponds to the simulations on data drawn according to Design 1 and the results are presented in Table 1 as well as Figures 3,4,5.

### Confounding

In this simulations, I examine whether confounding is correctly addressed by local centering. Therefore, I compare Causal Forests with and without this technique. The results can be seen in Table 2 and Figure 6.

### Heterogneity & Confounding

This simulation incorporates both, heterogeneity and confounding. Data is drawn according to Design 3. The result can be seen in Table 3.

### Concept Analysis: Honesty

In this simulation, the implications of honesty are investigated. I compare honest Causal Forests to their adaptive (not honest) analog. Results are shown in Figure 7.

## Code of Field Experiment Analysis

This Folder named *05_FieldExperiment* contains the data of the field experiment analyzed in this thesis as well as my code. It is organized in the same way as explained in my thesis.
