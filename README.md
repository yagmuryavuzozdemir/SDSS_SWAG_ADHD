# SDSS_SWAG_ADHD

This repository consists of the code and data for the analysis done in our paper called "A Multi-Model Framework
to Explore ADHD Diagnosis from Neuroimaging Data". 

ADHD is a common neurodevelopmental disorder in children, often diagnosed subjectively. Detecting ADHD objectively using neuroimaging data has been challenging due to complex diagnostic processes, high feature counts, and imperfect data measurements. Reliable neuroimaging biomarkers for ADHD detection have been difficult to find. The Sparse Wrapper Algorithm (SWAG) is proposed as a solution, combining screening and wrapper approaches to create low-dimensional models with good predictive power. This project aims to use SWAG to maintain accuracy while highlighting important brain regions for ADHD identification. SWAG generates simple yet equally performing models, revealing key feature combinations and interactions. Using SWAG, the relevance of frontal and temporal lobes for ADHD detection is confirmed, showing consistent results across different learning mechanisms like logistic regression and support vector machines. SWAG's models are smaller and often perform better than their original versions, providing valuable population-level insights. 

"train_data.csv" and "test_data.csv" is the train and test datasets that is used in our application. They consist the fMRI signals of the individuals with/without ADHD. The information about data collection can be found in the paper in more detail. We thank our author Dr. Gopikrishna Deshpande for providing and giving insight about the data. Since the names of the features is written in a different form, we provide another excel file called "ADHD_CC200_ROI_labels.xlsx".

The code for the analysis can be found in the files named as "adhd_logistic.R", "logreg.R", "test_errors.R" and "network.R". These files include the codes used to analyse the SWAG results as well as the tables and the network. One can find the information about the correkations of the features in "correlations.csv" and "corr of most selected.xlsx"

 

 
