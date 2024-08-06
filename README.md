# Knowledge Mining (EPPS 6323)

## Course Overview

This course, taught by Dr. Karl Ho at The University of Texas at Dallas, introduces key data mining and machine learning methods. The focus is on predictive analytics using various modeling techniques such as regression, classification, decision trees, and neural networks.

## Final Project

### Predicting Interstate Affinity Using Machine Learning

**Authors:** Alden Felix, Wei-chen Huang, Jim Pan, Samuel Adelusi  
**Date:** May 3, 2023

**Description:** This study examines the use of machine learning to predict generalized relationships between states, using the United States as the base of reference. International relations were measured using an aggregate affinity score derived from event data, with various socioeconomic and demographic factors used as predictive variables. Multiple models, including a regression and classification random forest and a deep learning model, were built and evaluated for their accuracy in predicting interstate affinity.

**Key Components:**
1. **Event Data:** 
   - Integrated Crisis Early Warning System (ICEWS) data from January 1, 1995, to April 11, 2023.
   - Affinity score computed from the intensity found in event data.
2. **Socioeconomic Data:**
   - World Bank World Development Indicators
   - International Monetary Fund
   - Freedom House
3. **Methods:**
   - Random Forest Regression and Classification
   - Deep Learning (TensorFlow and Keras)

**Main Results:**
- Random Forest Regression: R2 = 0.27
- Random Forest Classification: R2 = 0.43
- Deep Learning Model Accuracy: 0.76 after hyperparameter tuning

**Tools:** Python, TensorFlow, Keras, Random Forest, SHAP Library

[Final Paper](https://github.com/Jimpan0612/Knowledge-Mining-EPPS-6323-/blob/main/Final%20Paper.pdf) | [Final Presentation](https://github.com/Jimpan0612/Knowledge-Mining-EPPS-6323-/blob/main/Final%20Presentation.pdf)
