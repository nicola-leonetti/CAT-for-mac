# Purpose   : menu construction in Windows OS. Basically each menu item is
#             associated with a single file script locate in the subdirectory 
#             \myscript. Functions called by the script are located in the
#             subdirectory \mylib and loaded at section start-up.
# Script    : graphical menus on the R-GUI application
# Author    : Gruppo di Chemiometria 
#            (Divisione di Chimica Analitica della Società Chimica Italiana)
# Programmer: GMP
# Version   : 5.0
# Licence   : GPL2.1
#
#
# loading required libraries 
Sys.setenv(GTK_PATH='/GTK')
library(utils)         # menu addition
library(chemometrics)  # chemiometric functions
suppressWarnings(suppressPackageStartupMessages(library("gdata")))
library(gdata)         # needed for data transformation
library(tcltk2)        # table editor
suppressWarnings(suppressPackageStartupMessages(library("stringr")))
library(stringr)       # string manipulation
library(gplots)        # color scale
library(RGtk2)         # graphical interface
library(RGtk2Extras)   # graphical tools
library(rggobi)        # exploratory analysis
# find the base directory where functions and scripts necessary are located 
setwd(base.dir)
#setwd('Perl.5.16/perl/bin/')
#installXLSXsupport(perl='perl.exe',verbose=TRUE)
setwd(working.dir)
if(exists('DB'))rm(DB)
# Package Variable Initialization (the others are initialized in .Rprofile)
previous.name<-'A'

################################################################################
#
# Menu Data Handling
#
################################################################################

winMenuAdd("Data Handling");
winMenuAddItem("Data Handling","Help",
'source(paste(script.dir,"DH_Help.r",sep="/"))');
winMenuAddItem("Data Handling/Load", "CSV",
'source(paste(script.dir,"DH_load_CSV.r",sep="/"))');
winMenuAddItem("Data Handling/Load", "TXT",
'source(paste(script.dir,"DH_load_TXT.r",sep="/"))');
winMenuAddItem("Data Handling/Load", "XLS/XLSX",
'source(paste(script.dir,"DH_load_XLS.r",sep="/"))');
winMenuAddItem("Data Handling/Load", "Copy and Paste",
'source(paste(script.dir,"DH_load_copypaste.r",sep="/"))');
winMenuAddItem("Data Handling", "Workspace Management",
'source(paste(script.dir,"DH_workspace_management.r",sep="/"))');
winMenuAddItem("Data Handling/Data Set", "Row",
'source(paste(script.dir,"DH_dataset_row.r",sep="/"))');
winMenuAddItem("Data Handling/Data Set", "Column",
'source(paste(script.dir,"DH_dataset_column.r",sep="/"))');
winMenuAddItem("Data Handling/Data Set", "Check replicates",
'source(paste(script.dir,"DH_check_replicates.r",sep="/"))');
winMenuAddItem("Data Handling/Data Set", "Randomize rows",
'source(paste(script.dir,"DH_randomize.r",sep="/"))');
winMenuAddItem("Data Handling/Export", "CSV",
'source(paste(script.dir,"DH_export_CSV.r",sep="/"))');
winMenuAddItem("Data Handling/Export", "TXT",
'source(paste(script.dir,"DH_export_TXT.r",sep="/"))');

################################################################################
#
# Menu Plot Editing
#
################################################################################

winMenuAdd("Plot Editing");
winMenuAddItem("Plot Editing","Help",
'source(paste(script.dir,"PL_Help.r",sep="/"))');
winMenuAddItem("Plot Editing","Close all plots",
'source(paste(script.dir,"PL_close_plots.r",sep="/"))');
winMenuAddItem("Plot Editing","2D plot editing",
'source(paste(script.dir,"PL_magnify.r",sep="/"))');
winMenuAddItem("Plot Editing","Contour plot editing",
'source(paste(script.dir,"PL_contourplot_editing.r",sep="/"))');
winMenuAddItem("Plot Editing","Isocurve levels definition",
'source(paste(script.dir,"PL_contourplot_levels.r",sep="/"))');
winMenuAddItem("Plot Editing","3D plot single viewpoint",
'source(paste(script.dir,"PL_3D_single_viewpoint.r",sep="/"))');
winMenuAddItem("Plot Editing","3D plot rotation",
'source(paste(script.dir,"PL_3D_rotation.r",sep="/"))');


################################################################################
#
# Menu Exploration
#
################################################################################

winMenuAdd("Exploration");
winMenuAddItem("Exploration","Help",
'source(paste(script.dir,"EX_Help.r",sep="/"))');
winMenuAddItem("Exploration", "Load Dataset",
'source(paste(script.dir,"EX_load_dataset.r",sep="/"))');
winMenuAddItem("Exploration", "Save Picture",
'source(paste(script.dir,"EX_save_picture.r",sep="/"))');

################################################################################
#
# Menu Univariate
#
################################################################################

winMenuAdd("Univariate");
winMenuAddItem("Univariate","Help",
'source(paste(script.dir,"UN_Help.r",sep="/"))');
winMenuAddItem("Univariate","Summary",
'source(paste(script.dir,"UN_summary.r",sep="/"))');
winMenuAddItem("Univariate/Average","Arithmetic",
'source(paste(script.dir,"UN_average_arithmetic.r",sep="/"))');
winMenuAddItem("Univariate/Average","Geometric",
'source(paste(script.dir,"UN_average_geometric.r",sep="/"))');
winMenuAddItem("Univariate/Average","Median",
'source(paste(script.dir,"UN_average_median.r",sep="/"))');
winMenuAddItem("Univariate/Dispersion","Standard Deviation",
'source(paste(script.dir,"UN_dispersion_sd.r",sep="/"))');
winMenuAddItem("Univariate/Dispersion","Coeff. of Variation (RSD)"
,'source(paste(script.dir,"UN_dispersion_RSD.r",sep="/"))');
winMenuAddItem("Univariate/Dispersion/Robust Estimators","InterQuartile Range (IQR)",
'source(paste(script.dir,"UN_dispersion_robust_IQR.r",sep="/"))');
winMenuAddItem("Univariate/Dispersion/Robust Estimators","Median Absolute Deviation (MAD)",
'source(paste(script.dir,"UN_dispersion_robust_MAD.r",sep="/"))');
winMenuAddItem("Univariate/Dispersion/Robust Estimators","Robust Coeff. of Variation",
'source(paste(script.dir,"UN_dispersion_robust_RobustCV.r",sep="/"))');
winMenuAddItem("Univariate/Plots","Box and Whiskers Plot",
'source(paste(script.dir,"UN_plot_boxplot.r",sep="/"))');
winMenuAddItem("Univariate/Plots","Histogram",
'source(paste(script.dir,"UN_plot_hist.r",sep="/"))');
winMenuAddItem("Univariate/Plots","Probability Density Plot",
'source(paste(script.dir,"UN_plot_density.r",sep="/"))');
winMenuAddItem("Univariate/Plots","Scatter Plot",
'source(paste(script.dir,"UN_plot_stripchart.r",sep="/"))');
winMenuAddItem("Univariate/Plots","EDA Plot",
'source(paste(script.dir,"UN_plot_edaplot.r",sep="/"))');
winMenuAddItem("Univariate/Plots","Y plot",
'source(paste(script.dir,"UN_plot_Y.r",sep="/"))');
winMenuAddItem("Univariate/Plots","Row profile",
'source(paste(script.dir,"UN_profile.r",sep="/"))');

################################################################################
#
# Menu Bivariate
#
################################################################################

winMenuAdd("Bivariate");
winMenuAddItem("Bivariate","Help",
'source(paste(script.dir,"BI_Help.r",sep="/"))');
winMenuAddItem("Bivariate","Covariance",
'source(paste(script.dir,"BI_covariance.r",sep="/"))');
winMenuAddItem("Bivariate","Correlation",
'source(paste(script.dir,"BI_correlation.r",sep="/"))');
winMenuAddItem("Bivariate","Robust Correlation (MCD)",
'source(paste(script.dir,"BI_robust_mcd.r",sep="/"))');
winMenuAddItem("Bivariate/Special","Spearman",
'source(paste(script.dir,"BI_spearman.r",sep="/"))');
winMenuAddItem("Bivariate/Special","Kendall",
'source(paste(script.dir,"BI_kendall.r",sep="/"))');
winMenuAddItem("Bivariate/Plots","Pairs",
'source(paste(script.dir,"BI_plot_pairs.r",sep="/"))');
winMenuAddItem("Bivariate/Plots","X vs. Y",
'source(paste(script.dir,"BI_plot_XY.r",sep="/"))');
winMenuAddItem("Bivariate/Plots","Correlation Matrix",
'source(paste(script.dir,"BI_plot_cor.r",sep="/"))');

################################################################################
#
# Menu Transformations
#
################################################################################

winMenuAdd("Transformations");
winMenuAddItem("Transformations","Help",
'source(paste(script.dir,"TR_Help.r",sep="/"))');
winMenuAddItem("Transformations/Row","Sum 100",
'source(paste(script.dir,"TR_row_sum100.r",sep="/"))');
winMenuAddItem("Transformations/Row","Autoscaling (SNV)",
'source(paste(script.dir,"TR_row_autoscale.r",sep="/"))');
winMenuAddItem("Transformations/Row","Binning",
'source(paste(script.dir,"TR_row_binning.r",sep="/"))');
winMenuAddItem("Transformations/Row", "First Derivative",
'source(paste(script.dir,"TR_row_der_first.r",sep="/"))');
winMenuAddItem("Transformations/Row","Second Derivative",
'source(paste(script.dir,"TR_row_der_second.r",sep="/"))');
winMenuAddItem("Transformations/Row","Moving Average",
'source(paste(script.dir,"TR_moving_average_row.r",sep="/"))');
winMenuAddItem("Transformations/Row","Savitzky-Golay",
'source(paste(script.dir,"TR_row_savitzkygolay.r",sep="/"))');
winMenuAddItem("Transformations/Column","Sum 100",
'source(paste(script.dir,"TR_column_sum100.r",sep="/"))');
winMenuAddItem("Transformations/Column","Max 100",
'source(paste(script.dir,"TR_column_max100.r",sep="/"))');
winMenuAddItem("Transformations/Column","Length 1",
'source(paste(script.dir,"TR_column_length1.r",sep="/"))');
winMenuAddItem("Transformations/Column"," 0 - +1",
'source(paste(script.dir,"TR_column_01.r",sep="/"))');
winMenuAddItem("Transformations/Column"," -1 - +1",
'source(paste(script.dir,"TR_column_11.r",sep="/"))');
winMenuAddItem("Transformations/Column","Log",
'source(paste(script.dir,"TR_column_log.r",sep="/"))');
winMenuAddItem("Transformations/Column","Centering",
'source(paste(script.dir,"TR_column_centering.r",sep="/"))');
winMenuAddItem("Transformations/Column","Scaling",
'source(paste(script.dir,"TR_column_scaling.r",sep="/"))');
winMenuAddItem("Transformations/Column","Autoscaling",
'source(paste(script.dir,"TR_column_autoscale.r",sep="/"))');
winMenuAddItem("Transformations/Column","Block Scaling",
'source(paste(script.dir,"TR_column_block.r",sep="/"))');
winMenuAddItem("Transformations/Column","Robust Autoscaling",
'source(paste(script.dir,"TR_column_rubust_centerscale.r",sep="/"))');
#winMenuAddItem("Transformations/Matrix by column", "-", "");
winMenuAddItem("Transformations/Column", "First Derivative",
'source(paste(script.dir,"TR_column_der_first.r",sep="/"))');
winMenuAddItem("Transformations/Column","Second Derivative",
'source(paste(script.dir,"TR_column_der_second.r",sep="/"))');
winMenuAddItem("Transformations/Column","Moving Average",
'source(paste(script.dir,"TR_moving_average_col.r",sep="/"))');
winMenuAddItem("Transformations","Global centering",
'source(paste(script.dir,"TR_global_centering.r",sep="/"))');

################################################################################
#
# Menu Distances
#
################################################################################

winMenuAdd("Distances");
winMenuAddItem("Distances","Help",
'source(paste(script.dir,"DST_Help.r",sep="/"))');
winMenuAddItem("Distances","Euclidean",
'source(paste(script.dir,"DST_euclidean.r",sep="/"))');
winMenuAddItem("Distances","Mahalanobis",
'source(paste(script.dir,"DST_mahalanobis.r",sep="/"))');
winMenuAddItem("Distances","Mahalanobis MCD",
'source(paste(script.dir,"DST_mahalanobis_MCD.r",sep="/"))');
winMenuAddItem("Distances","Manhattan",
'source(paste(script.dir,"DST_manhattan.r",sep="/"))');
winMenuAddItem("Distances","Maximum",
'source(paste(script.dir,"DST_maximum.r",sep="/"))');

################################################################################
#
# Menu PCA
#
################################################################################

winMenuAdd("PCA");
winMenuAddItem("PCA","Help",
'source(paste(script.dir,"PCA_Help.r",sep="/"))');
winMenuAddItem("PCA/Model Computation","PCA",
'source(paste(script.dir,"PCA_model_PCA.r",sep="/"))');
winMenuAddItem("PCA/Model Computation","Varimax",
'source(paste(script.dir,"PCA_model_varimax.r",sep="/"))');
winMenuAddItem("PCA/Model Computation","Randomization test",
'source(paste(script.dir,"PCA_model_PCA_rnd.r",sep="/"))');
winMenuAddItem("PCA","Missing Data Reconstruction",
'source(paste(script.dir,"PCA_data_reconstruction.r",sep="/"))');
winMenuAddItem("PCA", "-", "");
winMenuAddItem("PCA","Extract and export",
'source(paste(script.dir,"PCA_extract.r",sep="/"))');
winMenuAddItem("PCA/Plots","-","");
winMenuAddItem("PCA/Plots/Variance","Scree Plot",'
source(paste(script.dir,"PCA_variance_plot.r",sep="/"))');
winMenuAddItem("PCA/Plots/Variance","Cumulative",'
source(paste(script.dir,"PCA_cumulative_var_plot.r",sep="/"))');
winMenuAddItem("PCA/Plots/Variance","Variance of each Variable explained",
'source(paste(script.dir,"PCA_explained_variance_variable.r",sep="/"))');
winMenuAddItem("PCA/Plots/Loadings","Scatter/Line",
'source(paste(script.dir,"PCA_loading_plot_scatter.r",sep="/"))');
winMenuAddItem("PCA/Plots/Loadings","Bar",
'source(paste(script.dir,"PCA_loading_plot_bar.r",sep="/"))');
winMenuAddItem("PCA/Plots/Scores","2D Score Plot",
'source(paste(script.dir,"PCA_score_plot.r",sep="/"))');
winMenuAddItem("PCA/Plots/Scores","3D Score Plot",
'source(paste(script.dir,"PCA_score3D.r",sep="/"))');
winMenuAddItem("PCA/Plots","Biplot",
'source(paste(script.dir,"PCA_biplot.r",sep="/"))');
winMenuAddItem("PCA/Diagnostics","T^2 and Q",
'source(paste(script.dir,"PCA_diagnostic_plot_t2q.r",sep="/"))');
winMenuAddItem("PCA/Diagnostics","T^2 vs. Q (Influence Plot)",
'source(paste(script.dir,"PCA_diagnostic_plot_t2vsq.r",sep="/"))');
winMenuAddItem("PCA/Diagnostics","T^2 Contribution Plot",
'source(paste(script.dir,"PCA_diagnostic_T2_cont_plot.r",sep="/"))');
winMenuAddItem("PCA/Diagnostics","Q Contribution Plot",
'source(paste(script.dir,"PCA_diagnostic_Q_cont_plot.r",sep="/"))');
winMenuAddItem("PCA/External Data Set","Projection on the training set",
'source(paste(script.dir,"PCA_projection_training_set.r",sep="/"))');
winMenuAddItem("PCA/External Data Set","T2 and Q",
'source(paste(script.dir,"PCA_t2q_Dataset.r",sep="/"))');
winMenuAddItem("PCA/External Data Set","T^2 vs. Q (Influence Plot)",
'source(paste(script.dir,"PCA_t2vsq_Dataset.r",sep="/"))');
winMenuAddItem("PCA/External Data Set","T^2 Contribution Plot",
'source(paste(script.dir,"PCA_cont_plot_T2_Dataset.r",sep="/"))');
winMenuAddItem("PCA/External Data Set","Q Contribution Plot",
'source(paste(script.dir,"PCA_cont_plot_Q_Dataset.r",sep="/"))');


################################################################################
#
# Menu 3W PCA
#
################################################################################

winMenuAdd("3W-PCA");
winMenuAddItem("3W-PCA","Help",
'source(paste(script.dir,"3W_Help.r",sep="/"))');
winMenuAddItem("3W-PCA","Model Computation (Tucker3)",
'source(paste(script.dir,"3W_model.r",sep="/"))');
winMenuAddItem("3W-PCA", "-", "");
winMenuAddItem("3W-PCA","Extract and export",
'source(paste(script.dir,"3W_extract.r",sep="/"))');
winMenuAddItem("3W-PCA/Loading Plots","Objects",
'source(paste(script.dir,"3W_plot_objects.r",sep="/"))');
winMenuAddItem("3W-PCA/Loading Plots","Variables",
'source(paste(script.dir,"3W_plot_variables.r",sep="/"))');
winMenuAddItem("3W-PCA/Loading Plots","Conditions",
'source(paste(script.dir,"3W_plot_conditions.r",sep="/"))');
winMenuAddItem("3W-PCA/Loading Plots","Triplot",
'source(paste(script.dir,"3W_plot_triplot.r",sep="/"))');
winMenuAddItem("3W-PCA/Residuals (RMSE)","Objects",
'source(paste(script.dir,"3W_plot_rmse_objects.r",sep="/"))');
winMenuAddItem("3W-PCA/Residuals (RMSE)","Variables",
'source(paste(script.dir,"3W_plot_rmse_variables.r",sep="/"))');
winMenuAddItem("3W-PCA/Residuals (RMSE)","Conditions",
'source(paste(script.dir,"3W_plot_rmse_conditions.r",sep="/"))');

################################################################################
#
# Menu MLR-DOE
#
################################################################################

winMenuAdd("MLR-DOE");
winMenuAddItem("MLR-DOE","Help",
'source(paste(script.dir,"DOE_Help.r",sep="/"))');
winMenuAddItem("MLR-DOE","Grid creation",'
source(paste(script.dir,"DOE_candidate_points.r",sep="/"))');
winMenuAddItem("MLR-DOE","Coding of qualitative variables",'
source(paste(script.dir,"DOE_coding_qualit.r",sep="/"))');
winMenuAddItem("MLR-DOE","Mixture constraints setting",'
source(paste(script.dir,"DOE_constraint_definition.r",sep="/"))');
winMenuAddItem("MLR-DOE", "-", "");
winMenuAddItem("MLR-DOE","Extract and export",
'source(paste(script.dir,"DOE_extract.r",sep="/"))');
winMenuAddItem("MLR-DOE/D-Optimal Design","","");
winMenuAddItem("MLR-DOE/D-Optimal Design/Independent var.","D-Optimal",
'source(paste(script.dir,"DOE_doptimal.r",sep="/"))');
winMenuAddItem("MLR-DOE/D-Optimal Design/Independent var.","by Addition",
'source(paste(script.dir,"DOE_doptadd.r",sep="/"))');
winMenuAddItem("MLR-DOE/D-Optimal Design/Mixture","D-Optimal",
'source(paste(script.dir,"DOE_doptimal_mixt.r",sep="/"))');
winMenuAddItem("MLR-DOE/D-Optimal Design/Mixture","by Addition",
'source(paste(script.dir,"DOE_doptadd_mixt.r",sep="/"))');
winMenuAddItem("MLR-DOE/D-Optimal Design","Subset extraction",
'source(paste(script.dir,"DOE_subset_extraction.r",sep="/"))');
winMenuAddItem("MLR-DOE/Model Computation","Independent variables",
'source(paste(script.dir,"DOE_model_computation.r",sep="/"))');
winMenuAddItem("MLR-DOE/Model Computation","Mixture",
'source(paste(script.dir,"DOE_model_computation_mixt.r",sep="/"))');
winMenuAddItem("MLR-DOE/Plots","Coefficients",'
source(paste(script.dir,"DOE_coefficients.r",sep="/"))');
winMenuAddItem("MLR-DOE/Plots","Experimental vs. Fitted",
'source(paste(script.dir,"DOE_experimental_fitted.r",sep="/"))');
winMenuAddItem("MLR-DOE/Plots","Residuals in Fitting",'
source(paste(script.dir,"DOE_residuals_fitting.r",sep="/"))');
winMenuAddItem("MLR-DOE/Plots","Experimental vs. CV Predicted",
'source(paste(script.dir,"DOE_experimental_predicted.r",sep="/"))');
winMenuAddItem("MLR-DOE/Plots","Residuals in CV",
'source(paste(script.dir,"DOE_CVresiduals_experimental.r",sep="/"))');
winMenuAddItem("MLR-DOE/Leverage Surface","Independent variables",
'source(paste(script.dir,"DOE_leverage_surface.r",sep="/"))');
winMenuAddItem("MLR-DOE/Leverage Surface","Mixture no constraints",
'source(paste(script.dir,"DOE_leverage_noconstr.r",sep="/"))');
winMenuAddItem("MLR-DOE/Leverage Surface","Mixture with constraints",
'source(paste(script.dir,"DOE_leverage_constr.r",sep="/"))');
winMenuAddItem("MLR-DOE/Prediction Confidence Interval Surface","Independent variables",
'source(paste(script.dir,"DOE_CI_surface.r",sep="/"))');
winMenuAddItem("MLR-DOE/Prediction Confidence Interval Surface","Mixture no constraints",
'source(paste(script.dir,"DOE_CI_surface_noconstr.r",sep="/"))');
winMenuAddItem("MLR-DOE/Prediction Confidence Interval Surface","Mixture with constraints",
'source(paste(script.dir,"DOE_CI_surface_constr.r",sep="/"))');
winMenuAddItem("MLR-DOE/Experimental Confidence Interval Surface","Independent variables",
'source(paste(script.dir,"DOE_CI_surface_exp.r",sep="/"))');
winMenuAddItem("MLR-DOE/Experimental Confidence Interval Surface","Mixture no constraints",
'source(paste(script.dir,"DOE_CI_surface_exp_noconstr.r",sep="/"))');
winMenuAddItem("MLR-DOE/Experimental Confidence Interval Surface","Mixture with constraints",
'source(paste(script.dir,"DOE_CI_surface_exp_constr.r",sep="/"))');
winMenuAddItem("MLR-DOE/Response Surface","Independent variables",
'source(paste(script.dir,"DOE_response_surface.r",sep="/"))');
winMenuAddItem("MLR-DOE/Response Surface","Mixture no constraints",
'source(paste(script.dir,"DOE_response_noconstr.r",sep="/"))');
winMenuAddItem("MLR-DOE/Response Surface","Mixture with constraints",
'source(paste(script.dir,"DOE_response_constr.r",sep="/"))');
winMenuAddItem("MLR-DOE/Prediction","Independent variables",
'source(paste(script.dir,"DOE_prediction.r",sep="/"))');
winMenuAddItem("MLR-DOE/Prediction","Mixture",
'source(paste(script.dir,"DOE_prediction_mixt.r",sep="/"))');
winMenuAddItem("MLR-DOE","Pareto front",
'source(paste(script.dir,"DOE_Pareto.r",sep="/"))');

################################################################################
#
# Menu Calibration
#
################################################################################

winMenuAdd("Calibration");
winMenuAddItem("Calibration","Help",
'source(paste(script.dir,"CAL_Help.r",sep="/"))');
winMenuAddItem("Calibration/Model Computation","-","");
winMenuAddItem("Calibration/Model Computation/PCR","single CV",
'source(paste(script.dir,"CAL_model_computation_PCR.r",sep="/"))');
winMenuAddItem("Calibration/Model Computation/PCR","repeated CV",
'source(paste(script.dir,"CAL_model_computation_PCR_rnd.r",sep="/"))');
winMenuAddItem("Calibration/Model Computation/PCR","permutation test",
'source(paste(script.dir,"CAL_permutation_test_PCR.r",sep="/"))');
winMenuAddItem("Calibration/Model Computation/PLS1","single CV",
'source(paste(script.dir,"CAL_model_computation_PLS1.r",sep="/"))');
winMenuAddItem("Calibration/Model Computation/PLS1","repeated CV",
'source(paste(script.dir,"CAL_model_computation_PLS_rnd.r",sep="/"))');
winMenuAddItem("Calibration/Model Computation/PLS1","permutation test",
'source(paste(script.dir,"CAL_permutation_test_PLS1.r",sep="/"))');
winMenuAddItem("Calibration","-","");
winMenuAddItem("Calibration","Extract and export",
'source(paste(script.dir,"CAL_extract.r",sep="/"))');
winMenuAddItem("Calibration","Experimental vs.Calculated",
'source(paste(script.dir,"CAL_experimental_calculated.r",sep="/"))');
winMenuAddItem("Calibration","Residuals",
'source(paste(script.dir,"CAL_residuals.r",sep="/"))');
winMenuAddItem("Calibration","Scores",
'source(paste(script.dir,"CAL_scores.r",sep="/"))');
winMenuAddItem("Calibration","x-Loadings",
'source(paste(script.dir,"CAL_xloadings.r",sep="/"))');
winMenuAddItem("Calibration","BiPlot",
'source(paste(script.dir,"CAL_biplot.r",sep="/"))');
winMenuAddItem("Calibration","Coefficients",
'source(paste(script.dir,"CAL_coefficients.r",sep="/"))');
winMenuAddItem("Calibration","Prediction",
'source(paste(script.dir,"CAL_prediction.r",sep="/"))');

################################################################################
#
# Menu Classification
#
################################################################################

winMenuAdd("Classification");
winMenuAddItem("Classification","Help",
'source(paste(script.dir,"CL_Help.r",sep="/"))');
winMenuAddItem("Classification/Model Computation","KNN",
'source(paste(script.dir,"CL_method_KNN.r",sep="/"))');
winMenuAddItem("Classification/Model Computation/LDA","single CV",
'source(paste(script.dir,"CL_method_LDA.r",sep="/"))');
winMenuAddItem("Classification/Model Computation/LDA","repeated CV",
'source(paste(script.dir,"CL_method_LDA_rnd.r",sep="/"))');
winMenuAddItem("Classification/Model Computation/LDA","permutation test",
'source(paste(script.dir,"CL_permutation_test_LDA.r",sep="/"))');
winMenuAddItem("Classification/Model Computation/QDA","single CV",
'source(paste(script.dir,"CL_method_QDA.r",sep="/"))');
winMenuAddItem("Classification/Model Computation/QDA","repeated CV",
'source(paste(script.dir,"CL_method_QDA_rnd.r",sep="/"))');
winMenuAddItem("Classification/Model Computation/QDA","permutation test",
'source(paste(script.dir,"CL_permutation_test_QDA.r",sep="/"))');
winMenuAddItem("Classification", "-", "");
winMenuAddItem("Classification","Extract and export",
'source(paste(script.dir,"CL_extract.r",sep="/"))');
winMenuAddItem("Classification/Prediction","KNN",
'source(paste(script.dir,"CL_prediction_KNN.r",sep="/"))');
winMenuAddItem("Classification/Prediction","LDA",
'source(paste(script.dir,"CL_prediction_LDA.r",sep="/"))');
winMenuAddItem("Classification/Prediction","QDA",
'source(paste(script.dir,"CL_prediction_QDA.r",sep="/"))');
winMenuAddItem("Classification", "-", "");
winMenuAddItem("Classification/Plots","-","")
winMenuAddItem("Classification/Plots/Model (CV)","k neighbours (object)",
'source(paste(script.dir,"CL_plot_KNN.r",sep="/"))');
winMenuAddItem("Classification/Plots/Model (CV)","Mahalanobis Distance",
'source(paste(script.dir,"CL_plot_mahalanobis.r",sep="/"))');
winMenuAddItem("Classification/Plots/Model (CV)","Mahalanobis Distance (category)",
'source(paste(script.dir,"CL_plot_mahalanobis_cat.r",sep="/"))');
winMenuAddItem("Classification/Plots/Model (CV)","Mahalanobis Distance (object)",
'source(paste(script.dir,"CL_plot_mahalanobis_obj.r",sep="/"))');
winMenuAddItem("Classification/Plots/Model (CV)","object plot (repeated CV)",
'source(paste(script.dir,"CL_plot_rnd.r",sep="/"))');
winMenuAddItem("Classification/Plots/Prediction","k neighbours (object)",
'source(paste(script.dir,"CL_pre_KNN.r",sep="/"))');
winMenuAddItem("Classification/Plots/Prediction","Mahalanobis Distance",
'source(paste(script.dir,"CL_pre_mahalanobis.r",sep="/"))');
winMenuAddItem("Classification/Plots/Prediction","Mahalanobis Distance (category)",
'source(paste(script.dir,"CL_pre_mahalanobis_cat.r",sep="/"))');
winMenuAddItem("Classification/Plots/Prediction","Mahalanobis Distance (object)",
'source(paste(script.dir,"CL_pre_mahalanobis_obj.r",sep="/"))');

################################################################################
#
# Menu video tutorials
#
################################################################################

winMenuAdd("Video tutorials");
winMenuAddItem("Video tutorials","Download and Installation",
'browseURL("https://www.youtube.com/watch?v=fPJsFJMobAI&list=PLJ6gnfjVL40FSj9MVGwJocbYS-8h7a-ym")')
winMenuAddItem("Video tutorials","Data loading",
'browseURL("https://www.youtube.com/watch?v=lTULWu4VOIo&list=PLJ6gnfjVL40FSj9MVGwJocbYS-8h7a-ym&index=2")')
winMenuAddItem("Video tutorials","Data handling",
'browseURL("https://www.youtube.com/watch?v=X2vopiTvRe4&list=PLJ6gnfjVL40FSj9MVGwJocbYS-8h7a-ym&index=3")')
winMenuAddItem("Video tutorials","PCA",
'browseURL("https://www.youtube.com/watch?v=8riq9OH2GVo&list=PLJ6gnfjVL40FSj9MVGwJocbYS-8h7a-ym&index=7")')
winMenuAddItem("Video tutorials","Varimax",
'browseURL("https://www.youtube.com/watch?v=k3XY2Etatz8&list=PLJ6gnfjVL40FSj9MVGwJocbYS-8h7a-ym&index=8")')
winMenuAddItem("Video tutorials","Full Factorial Design",
'browseURL("https://www.youtube.com/watch?v=aWSURJr54bM&list=PLJ6gnfjVL40FSj9MVGwJocbYS-8h7a-ym&index=4")')
winMenuAddItem("Video tutorials","MLR model computation",
'browseURL("https://www.youtube.com/watch?v=Mktqsiue840&list=PLJ6gnfjVL40Exmy4-_v7A_X9tutWogcE0&index=2&t=19s")')
winMenuAddItem("Video tutorials","Response surface",
'browseURL("https://www.youtube.com/watch?v=1XhET9GJ4bg&list=PLJ6gnfjVL40Exmy4-_v7A_X9tutWogcE0&index=3")')
winMenuAddItem("Video tutorials","PCA on the responses",
'browseURL("https://www.youtube.com/watch?v=cRiFsKScJw4&list=PLJ6gnfjVL40Exmy4-_v7A_X9tutWogcE0")')
winMenuAddItem("Video tutorials","Multicriteria Decision making: test point generation",
'browseURL("https://www.youtube.com/watch?v=YflMPq_GvIE&list=PLJ6gnfjVL40Exmy4-_v7A_X9tutWogcE0&index=4")')
winMenuAddItem("Video tutorials","Multicriteria Decision Making: predictions",
'browseURL("https://www.youtube.com/watch?v=UIZDF40XlhM&list=PLJ6gnfjVL40Exmy4-_v7A_X9tutWogcE0&index=5")')
winMenuAddItem("Video tutorials","D-optimal Design",
'browseURL("https://www.youtube.com/watch?v=7vaIZh4wjPw&list=PLJ6gnfjVL40Exmy4-_v7A_X9tutWogcE0&index=6")')
winMenuAddItem("Video tutorials","Mixture Design with constraints",
'browseURL("https://www.youtube.com/watch?v=nDn-P_MkoEM&list=PLJ6gnfjVL40FSj9MVGwJocbYS-8h7a-ym&index=5")')
winMenuAddItem("Video tutorials","Mixture-Process Design",
'browseURL("https://www.youtube.com/watch?v=jxevWOIqVgw&list=PLJ6gnfjVL40FSj9MVGwJocbYS-8h7a-ym&index=6")')


################################################################################
#
# Menu About
#
################################################################################

winMenuAdd("About");
winMenuAddItem("About","Credits",
'source(paste(script.dir,"Credits.r",sep="/"))');

winMenuAdd("Version October 9 2026");