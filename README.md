### Banking Default Prediction and the Class Imbalance Problem
## Supplementary Material to data analysis project executed during summer term 2019 at the University of Konstanz.


While in the common textbook classification scenario classes are assumed to be more or less balanced, real-world applications often struggle with a *highly imbalanced distribution of the target variable*. The very intention often is the prediction of interesting minority cases, as in this case few failing banks among a vast majority of successfully performing banks. I performed an experiment on how the performance of a bagging technique, a gradient boosting machine, can be improved by drastically oversampling the minority class. This was done using the *Synthetic Minority Oversampling Technique (SMOTE)* which creates artificial observations on random points of the linear combinations of a minority observation and its nearest minority neighbors.

*Tools:* Partly done in *R* with the help of Max Kuhn's machine learning package *caret* and partly in *Python*.
