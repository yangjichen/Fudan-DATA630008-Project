## DATA630008 - Semiparametric Modeling 
## Final Exam – Spring 2019

*Due to the confidentiality agreements, I will not provide raw data and final result. Only algorithms and techniques used for data mining are shown.*

Guide: This is a take-home final exam. This exam contains two parts: real data analyses and theoretical derivation.

- For Part 1, the real data analyses, you are assigned to group A - D and need to collaborate with your group members. Each group is required to write a statistical report and present it to the class on June 22nd. (The presenter should be different from the writer.) The written report together with executable program code is due by 11 pm. of June 22nd.
- For Part 2, the theoretical derivation, you are required to work individually and derive the properties of Cox model with current status data. The written answer is due by 11pm. of June 24th.

#### 1 Evaluating Risk of Car Dealers

A car company were interested in evaluation the risk of their dealers. The risk evaluation of the dealers were recorded in the dataset. The potential risk factors from two sources were provided: inside accounting  and outside commercial database. The goal is to use these potential risk factors to predict the risk of the dealers. There are outliers and missing values in the dataset, which should be treated in the following analysis. Please randomly split your data into 60% training and 40% testing datasets to evaluate performance of different models.

1. First remove the dealers in the class of middle risk, and only use the ones with low or high risk. Build a logistic regression to select the risk factors and do risk prediction. Please specify how you treat the missing data and outliers.

2. Alternative to a parametric logistic regression, k-nearest neighbors (KNN) and classification and regression tree (CART) are common nonparametric models for classification and often perform better in classification problems. Please try the “cart” function in the “rpart” library and “ctree” in the “partykit” library to implement such a tree-based classification and predict the risk of car dealers. (For example, see chapter 9.2 of “Elements of Statistical Learning” by HTF.) Did you change how you treat missing data from part (a)?

3. In reality, the percentage of high risk dealers is very low, only about 3% 7%. Based on the suggestion from “Elements of Statistical Learning” by HTF, one should consider BOOSTING for better identifying rare events. Actually the classical boosting method ‘AdaBoost” (by Breiman 1998) can be viewed as semiparametric additive model minimizing exponential loss function. Please try the gradient boosting model for boosting with trees in the “xgboost” R library, and predict the risk of dealers. Please compare all three classification models.

4. Please extend part (c) to include middle risk dealers. What is the performance of the three class classification?

    ####2 The details Part 2 can be found in `part2.pdf`





