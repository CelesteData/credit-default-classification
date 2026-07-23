#Predictive Risk Analytics: Consumer Classification &amp; Forecasting

Project Description:

Designed and evaluated a predictive machine learning model in R to forecast consumer default risk. The objective of this project was to identify the optimal threshold for risk detection by balancing model complexity with generalizability, ensuring high-risk accounts are flagged without overly penalizing stable consumers.

Key Technical Contributions:

Data Preprocessing: Handled missing values, separated target labels from features, and engineered a custom mathematical function to apply min-max scaling for numeric normalization.

Predictive Modeling: Built a k-Nearest Neighbors (k-NN) classification algorithm using the caret and gmodels packages to evaluate historical risk data.

Algorithm Tuning: Conducted an 80/20 train-test split and evaluated multiple algorithmic thresholds to explicitly diagnose and prevent model overfitting.

Performance Optimization: Extracted confusion matrix metrics to analyze the trade-off between Precision and Recall, ultimately optimizing the model to prioritize risk mitigation (minimizing false negatives).

Data Visualization: Utilized dplyr, tidyr, and ggplot2 to reshape evaluation metrics and build comparative visual dashboards of the model’s performance.
