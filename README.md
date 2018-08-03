# Chen_etal_2018
Codes and data for Bayesian RN model and multi-species occupancy model with random effects

Based on work from: Cheng Chen, Ruichang Quan, Guanghong Cao, Hongpei Yang, A. Cole Burton, Michael Meitner, Jedediah Brodie. 2018. Do local perceptions about law enforcement and community outreach affect conservation outcomes? Comparing protected area management and mammal diversity in a biodiversity hotspot in southwestern China. Under review.

## **Description:**
This repository includes R codes, Jags codes and data for 
1) Nichol-Royle N-mixture abundance model (RN model) with site random effects in Bayesian framework.
2) Multi-species occupancy model with site random effects in Bayesian framework.

## **Data:**
All_widedata – Detection history of all 22 species in wide format 

RN_model_final –
-	STDED.DEC.sitecov.csv – All standardized site covariates. 
-	Dec.data.commonplamcivet.csv - Detection history and site covariate for common palm civet
-	Dec.data.maskedpalmcivet.csv - Detection history and site covariate for masked palm civet
-	Dec.data.muntjac.csv - Detection history and site covariate for muntjac
-	Dec.data.wildboar.csv - Detection history and site covariate for wild boar

Park_level_cov.csv – Original-scale of reserve level covariates

MSOM_sitecov.csv - Original-scale site covariate for multi-species occupancy model 

## **R:**

MSOM_parkeffect.R - R code to run the multi-species occupancy model

MSOM_ post_analysis.R - R code to do post-modelling, plots, checking the results of multi-species occupancy model

RNmodel_parkEff_commonpalmcivet.R - R code to run the RN model of common palm civet

RNmodel_parkEff_ maskedpalmcivet.R - R code to run the RN model of masked palm civet

RNmodel_parkEff_ muntjac.R - R code to run the RN model of muntjac

RNmodel_parkEff_ wildboar.R - R code to run the RN model of wild boar

RNmodel_plot_final.R - R code to plot publish quality beta coefficient plot

model_6_var_parkeff.txt - Jags model file of multispecies occupancy model 

model_parkEff.txt – jags model file of RN model 


