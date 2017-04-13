# #Following is not a code but analysis of Cross Validation.
# 
# > dl_model_1
# Model Details:
#   ==============
#   
#   H2OBinomialModel: deeplearning
# Model ID:  DeepLearning_model_R_1491399139235_3153 
# Status of Neuron Layers: predicting Outcome, 2-class classification, bernoulli distribution, CrossEntropy loss, 433,002 weights/biases, 5.0 MB, 1,898,189 training samples, mini-batch size 1
# layer units      type dropout       l1       l2 mean_rate rate_rms momentum mean_weight
# 1     1  1961     Input  0.00 %                                                          
# 2     2   200 Rectifier  0.00 % 0.000000 0.000000  0.087287 0.234865 0.000000   -0.000073
# 3     3   200 Rectifier  0.00 % 0.000000 0.000000  0.155572 0.256949 0.000000   -0.026576
# 4     4     2   Softmax         0.000000 0.000000  0.007730 0.003318 0.000000   -0.007230
# weight_rms mean_bias bias_rms
# 1                              
# 2   0.031232 -0.225796 0.384649
# 3   0.077172  0.761039 0.184639
# 4   0.351254 -0.509729 0.107903
# 
# 
# H2OBinomialMetrics: deeplearning
# ** Reported on training data. **
#   ** Metrics reported on temporary training frame with 9915 samples **
#   
#   MSE:  0.2473464
# RMSE:  0.4973394
# LogLoss:  0.6878262
# Mean Per-Class Error:  0.4999075
# AUC:  0.5295338
# Gini:  0.05906751
# 
# Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#   0    1    Error        Rate
# 0      1 5407 0.999815  =5407/5408
# 1      0 4507 0.000000     =0/4507
# Totals 1 9914 0.545335  =5407/9915
# 
# Maximum Metrics: Maximum metrics at their respective thresholds
# metric threshold    value idx
# 1                       max f1  0.344389 0.625061 398
# 2                       max f2  0.344389 0.806492 398
# 3                 max f0point5  0.429582 0.510373 339
# 4                 max accuracy  0.525474 0.551286  75
# 5                max precision  0.594921 1.000000   0
# 6                   max recall  0.344389 1.000000 398
# 7              max specificity  0.594921 1.000000   0
# 8             max absolute_mcc  0.493988 0.049736 166
# 9   max min_per_class_accuracy  0.446191 0.375638 272
# 10 max mean_per_class_accuracy  0.477191 0.521105 210
# 
# Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
# 
# 
# > glm_model_1
# Model Details:
#   ==============
#   
#   H2OBinomialModel: glm
# Model ID:  GLM_model_R_1491399139235_3166 
# GLM Model: summary
# family  link                                regularization number_of_predictors_total
# 1 binomial logit Elastic Net (alpha = 0.5, lambda = 1.939E-5 )                       1958
# number_of_active_predictors number_of_iterations training_frame
# 1                        1030                    3 train.complete
# 
# Coefficients: glm coefficients
# names coefficients standardized_coefficients
# 1     Intercept    -0.129182                 -0.140586
# 2    Stock_ID.1     0.000000                  0.000000
# 3   Stock_ID.10     0.103157                  0.103157
# 4  Stock_ID.100     0.000000                  0.000000
# 5 Stock_ID.1000     0.172038                  0.172038
# 
# ---
#   names coefficients standardized_coefficients
# 1954          Volume_Size.low    -0.015905                 -0.015905
# 1955       Volume_Size.medium    -0.029351                 -0.029351
# 1956  MACD_3510_Crossover.Buy    -0.069241                 -0.069241
# 1957 MACD_3510_Crossover.Sell     0.045819                  0.045819
# 1958           Trend_Strength    -0.015211                 -0.019706
# 1959                MACD_3510     0.280038                  0.023708
# 
# H2OBinomialMetrics: glm
# ** Reported on training data. **
#   
#   MSE:  0.242605
# RMSE:  0.4925495
# LogLoss:  0.6775036
# Mean Per-Class Error:  0.4755938
# AUC:  0.5707104
# Gini:  0.1414208
# R^2:  0.02098043
# Null Deviance:  961954.9
# Residual Deviance:  946248.9
# AIC:  948310.9
# 
# Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#   0      1    Error            Rate
# 0      27749 354143 0.927338  =354143/381892
# 1       7547 308896 0.023849    =7547/316443
# Totals 35296 663039 0.517932  =361690/698335
# 
# Maximum Metrics: Maximum metrics at their respective thresholds
# metric threshold    value idx
# 1                       max f1  0.346967 0.630733 286
# 2                       max f2  0.235288 0.806992 348
# 3                 max f0point5  0.428558 0.526973 206
# 4                 max accuracy  0.484317 0.557389  98
# 5                max precision  0.607727 0.750000   0
# 6                   max recall  0.077898 1.000000 398
# 7              max specificity  0.607727 0.999995   0
# 8             max absolute_mcc  0.414445 0.117027 224
# 9   max min_per_class_accuracy  0.468119 0.541250 135
# 10 max mean_per_class_accuracy  0.451899 0.547897 163
# 
# Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
# 
# H2OBinomialMetrics: glm
# ** Reported on cross-validation data. **
#   ** 3-fold cross-validation on training data (Metrics computed for combined holdout predictions) **
#   
#   MSE:  0.2437162
# RMSE:  0.4936762
# LogLoss:  0.679873
# Mean Per-Class Error:  0.4771898
# AUC:  0.5570376
# Gini:  0.1140752
# R^2:  0.01649638
# Null Deviance:  961956.6
# Residual Deviance:  949558.2
# AIC:  951000.2
# 
# Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#   0      1    Error            Rate
# 0      25725 356167 0.932638  =356167/381892
# 1       6880 309563 0.021742    =6880/316443
# Totals 32605 665730 0.519875  =363047/698335
# 
# Maximum Metrics: Maximum metrics at their respective thresholds
# metric threshold    value idx
# 1                       max f1  0.350986 0.630363 284
# 2                       max f2  0.232193 0.806859 353
# 3                 max f0point5  0.417644 0.523510 221
# 4                 max accuracy  0.493177 0.549294  82
# 5                max precision  0.597651 0.535714   2
# 6                   max recall  0.093098 1.000000 397
# 7              max specificity  0.629943 0.999995   0
# 8             max absolute_mcc  0.369337 0.108503 269
# 9   max min_per_class_accuracy  0.465848 0.534573 137
# 10 max mean_per_class_accuracy  0.443964 0.539903 176
# 
# Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
# Cross-Validation Metrics Summary: 
#   mean           sd cv_1_valid cv_2_valid cv_3_valid
# accuracy   0.4797714  7.881821E-5 0.47975266 0.47991633 0.47964525
# auc        0.5570722  7.838784E-4 0.55697733  0.5557645  0.5584749
# err       0.52022856  7.881821E-5 0.52024734 0.52008367 0.52035475
# err_count   121098.0    246.86906   121407.0   120610.0   121277.0
# f0point5   0.5194057 3.2258508E-4 0.51981324  0.5187688 0.51963514
# 
# ---
#   mean          sd  cv_1_valid  cv_2_valid  cv_3_valid
# precision           0.4648489 2.730615E-4  0.46522087  0.46431664  0.46500918
# r2                0.016493835  2.22221E-4 0.016448282 0.016133739 0.016899481
# recall              0.9790115 9.507113E-4  0.97964984  0.97714114   0.9802434
# residual_deviance    316519.4   613.55524   317380.47   315331.72   316846.03
# rmse                0.4936762 4.125186E-5   0.4937477  0.49367598   0.4936048
# specificity        0.06609014 0.001711169  0.06445539 0.069511354  0.06430366
# > gbmF_model_1
# Model Details:
#   ==============
#   
#   H2OBinomialModel: gbm
# Model ID:  GBM_model_R_1491399139235_3128 
# Model Summary: 
#   number_of_trees number_of_internal_trees model_size_in_bytes min_depth max_depth
# 1            5000                     5000            71731725         5         5
# mean_depth min_leaves max_leaves mean_leaves
# 1    5.00000         32         32    32.00000
# 
# 
# H2OBinomialMetrics: gbm
# ** Reported on training data. **
#   
#   MSE:  0.2438319
# RMSE:  0.4937934
# LogLoss:  0.6805649
# Mean Per-Class Error:  0.4751728
# AUC:  0.5835564
# Gini:  0.1671128
# 
# Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#   0      1    Error            Rate
# 0      27274 354618 0.928582  =354618/381892
# 1       6887 309556 0.021764    =6887/316443
# Totals 34161 664174 0.517667  =361505/698335
# 
# Maximum Metrics: Maximum metrics at their respective thresholds
# metric threshold    value idx
# 1                       max f1  0.396514 0.631349 321
# 2                       max f2  0.341102 0.807116 373
# 3                 max f0point5  0.442560 0.529906 212
# 4                 max accuracy  0.469483 0.563655  97
# 5                max precision  0.497360 0.600251   1
# 6                   max recall  0.293711 1.000000 399
# 7              max specificity  0.497912 0.995931   0
# 8             max absolute_mcc  0.433068 0.123422 245
# 9   max min_per_class_accuracy  0.459148 0.557424 142
# 10 max mean_per_class_accuracy  0.455429 0.558594 160
# 
# Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
