# Machine Learning for Causal Inference: Estimating Heterogeneous Treatment Effects with Causal Forests

There is a growing interest in evaluating the causal effect of a treatment beyond its average
effect. Understanding how a treatment varies among sub-populations is a key question across
research fields ranging from medicine to policy. This understanding is important to enable
personalized treatments or well targeted interventions. Due to the large amount of data that is
constantly produced, there is a demand for methods that automatically detect heterogeneity
in treatment effects. Since machine learning has shown to perform well on similar tasks, it
is promising to apply machine learning based methods to causal inference. However, most
methods are not directly applicable and need to be tailored to the task of estimating treatment
effects.
This work analyzes the recently introduced Causal Forests in the framework of Generalized Random Forests of Athey et al. (2019), which is based on the popular Random Forest
algorithm of Breiman (2001). The theoretical part of this thesis provides a comprehensive
introduction into tree-based methods and their application to estimating treatment effects.
On simulated data, I demonstrate that Causal Forests are able to automatically detect strong
treatment effect heterogeneity. Further, I show that the challenge of confounding is effectively
addressed by the technique of local centering. Investigating the implications of the underlying
concept of honesty, I find that Causal Forests successfully resist overfitting to noise. Lastly,
I apply the Causal Forest algorithm to data from a field experiment in political science from
Broockman and Kalla (2016), who show that brief but high-quality canvassing can markedly
reduce prejudice against transgender people. Overall, the results using Causal Forests confirm the findings of the authors. Only small variation in treatment effects, but no statistically
significant heterogeneity is found. Simultaneously, an approach to make best use of Causal
Forests is demonstrated in order to enable researchers outside the field of statistics to take
advantage of this machine learning method in the setting of estimating treatment effects.


This Repository contains my Bachelor's thesis along with the codes of the simulations I conducted, as well as the code of the analysis of the empirical data. The codes are organized in the same order as they appear in the thesis.

## Code of Simulations

- Function Steup: The script named *00_FunctionSetup.R* initializes all functions used for the simulations. This includes the data generating processes, the estimators and the parallelization function.

- Strong Treatment Effect Heterogeneity: These simulations are to explore the ability of Causal Forests to detect treatment effect heterogeneity. In my thesis, this corresponds to the simulations on data drawn according to Design 1 and the results are presented in Table 1 as well as Figures 3,4,5.

- Confounding: In these simulations, I examine whether confounding is correctly addressed by local centering. Therefore, I compare Causal Forests with and without this technique. The results can be seen in Table 2 and Figure 6.

- Heterogneity & Confounding: This simulation incorporates both, heterogeneity and confounding. Data is drawn according to Design 3. The result can be seen in Table 3.

- Concept Analysis - Honesty: In this simulation, the implications of honesty are investigated. I compare honest Causal Forests to their adaptive (not honest) analog. Results are shown in Figure 7.

## Code of Field Experiment Analysis

The Folder named *05_FieldExperiment* contains the data of the field experiment analyzed in this thesis as well as my code. It is organized in the same way as explained in my thesis.
