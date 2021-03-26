# Paraclaravis project
 An integrated approach to assessing extinction likelihood in a Critically Endangered tropical bird
 
Lees AC, Devenish C, Areta JI, Barros de Araújo C, Keller C, Phalan B, Silveira LF. 2021. Assessing the extinction probability of the Purple-winged Ground Dove, an enigmatic bamboo specialist. Frontiers in Ecology and Evolution 9. 

https://www.frontiersin.org/articles/10.3389/fevo.2021.624959/abstract


## SDM scripts 
Scripts show the data preparations, modelling area preparation and SDM methods (from Breinman et al). They require downloading external data, eg from range maps from IUCN red list (permission required) and predictor layers. They are intended to show analysis steps.
* s1_occurrence_pts_v1.r
* s2_predictors_v1.r
* s3_model_ESM_v3.r

## Extinction risk scripts
The extinction risk model follows Thompson et al (2017). Scripts here follow those in supp material of the paper. The first script prepares eBird absence data for estimating study area surveyed in passive surveys.
* ebird_absences.r
* thompson_functions.r (collection of functions from supp info of Thompson paper)
* thompsons_method_with_specimen_data.r
* thompsons_method_with_study_data.r
* final plot.r (final extinction risk plot)

See paper for complete refs.

