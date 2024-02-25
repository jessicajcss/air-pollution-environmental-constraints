---
title: "Step_by_Step"
author: "Jessica C. dos Santos-Silva"
date: "05/04/2022"
---


*Supplementary material:*

**A new strategy for risk assessment of PM2.5-bound elements by considering the influence of wind regimes**. 

DOS SANTOS-SILVA, J. C. et al. A new strategy for risk assessment of PM2.5-bound elements by considering the influence of wind regimes. Science of The Total Environment, v.872, p. 162131, 2023.
| https://doi.org/10.1016/j.scitotenv.2023.162131

Dataset associated with this material can be found at DOI: 10.17632/szgtbzwpy8.3

Authors: 
Jéssica Caroline dos Santos Silva^1^, Sanja Potgieter-Vermaak^2,3^, Sandra Helena Westrupp Medeiros^4^, Luiz Vitor da Silva^4^, Danielli Ventura Ferreira^4^, Camila Ariele Bufato Moreira^5^, Priscila Caroline de Souza Zorzenão^6^, Theotonio Pauliquevis^7^, Ana Flávia Locateli Godoi^5^, Rodrigo Augusto Ferreira de Souza^8^, Carlos Itsuo Yamamoto^6^, Ricardo Henrique Moreton Godoi^1,5*^


<p style="font-family: times, serif; font-size:8pt; font-style:italic">
1-	Postgraduate Program in Water Resources and Environmental Engineering, Federal University of Paraná, Curitiba, Paraná, Brazil;
2-	Ecology & Environment Research Centre, Department of Natural Science, Manchester Metropolitan University, Manchester M1 5GD, United Kingdom;
3-	Molecular Science Institute, University of the Witwatersrand, Johannesburg, South Africa;
4-	Department of Environmental and Sanitary Engineering, University of the Region of Joinville, Joinville, Santa Catarina, Brazil;
5-	Department of Environmental Engineering, Federal University of Paraná, Curitiba, Paraná, Brazil;
6-	Department of Chemical Engineering, Federal University of Paraná, Curitiba, Paraná, Brazil;
7-	Department of Environmental Sciences, Federal University of São Paulo, Diadema, São Paulo, Brazil;
8-	School of Technology, Amazonas State University, Manaus, Amazonas, Brazil.
* Corresponding author. E -mail: rhmgodoi@ufpr.br. Postal address: Rua Francisco H. dos Santos, 210, Jardim das Américas, Curitiba, Paraná, Brazil.
</p>


First, run the script **00-basic_functions.R**.

To column descriptions and variable units, see **README_Column_description.txt**.



## 2.3.	Meteorological database

For missing data, homogenization and interpolation were done using the **Climatol package in R** (Azorin-Molina et al., 2019; Guijarro, 2019). 

See script **01-meteo_data.R**

REFERENCES:

Azorin-Molina, C., Guijarro, J.A., McVicar, T.R., Trewin, B.C., Frost, A.J., Chen, D., 2019. An approach to homogenize daily peak wind gusts: An application to the Australian series. International Journal of Climatology 39, 2260–2277. https://doi.org/10.1002/joc.5949

GRANGE, S. Averaging wind speeds and directions. October, p. 12, 2014.
Guijarro, J.A., 2019. climatol: Climate Tools (Series Homogenization and   Derived Products). R package version 3.1.2.




## 2.4.	Horizontal flow characterization

The horizontal flow conditions at the measurement point are determined by calculating and comparing discrete integral quantities such as the ‘wind run’ (S), which represents a measure of the total distance the parcel travelled in that time, and the ‘recirculation factor’ (R). 

See script **01-meteo_data_wind-circulation.R** for dataset preparation.



In order to characterize horizontal air transport in the study area, the methodology of Allwine and Whiteman (1994) adapted with Russo et al. (2018) approach was applied to determine critical values of dispersion from the historical dataset available for the period from 2012 to 2021. Following this approach, indices were set by calculating the integral quantities for eight meteorological stations,  from which a broad set of CTIs was then determined for the municipality airshed.

See script **02-horizontal_flow_characterization.R** for dataset preparation, CTIs calculation and horizontal wind flow conditions characterization.

REFERENCES:

Allwine, K.J., Whiteman, C.D., 1994. Single-station integral measures of atmospheric stagnation, recirculation and ventilation. Atmospheric Environment 28, 713–721. https://doi.org/10.1016/1352-2310(94)90048-5

Russo, A., Gouveia, C.M., Soares, P.M.M., Cardoso, R.M., Mendes, M.T., Trigo, R.M., 2018. The unprecedented 2014 Legionnaires’ disease outbreak in Portugal: atmospheric driving mechanisms. Int J Biometeorol 62, 1167–1179. https://doi.org/10.1007/s00484-018-1520-8


## 2.5. Trajectory analysis

96-h back trajectories for each sampling site were calculated using the HYSPLIT model integrated to **openair R package** to get an insight into the origin and transport pathway of the air masses arriving near receptor level (10 m asl) in these sites. 


See script **02-trajectory-analysis.R** for dataset preparation.

REFERENCES:

https://bookdown.org/david_carslaw/openair

Carslaw, D.C., Ropkins, K., 2012. openair — An R package for air quality data analysis. Environmental Modelling & Software 27–28, 52–61. https://doi.org/10.1016/j.envsoft.2011.09.008




## 2.6.	Risk assessment

### Ecological risk assessment 

See script **03-risk_assessment-ecological.R** for dataset preparation and ecological risk calculations.


REFERENCES:

Alves, C.A., Vicente, E.D., Vicente, A.M.P., Rienda, I.C., Tomé, M., Querol, X., Amato, F., 2020. Loadings, chemical patterns and risks of inhalable road dust particles in an Atlantic city in the north of Portugal. Science of The Total Environment 737, 139596. https://doi.org/10.1016/j.scitotenv.2020.139596

Bai, L., He, Z., Ni, S., Chen, W., Li, N., Sun, S., 2019. Investigation of PM2.5 absorbed with heavy metal elements, source apportionment and their health impacts in residential houses in the North-east region of China. Sustainable Cities and Society 51, 101690. https://doi.org/10.1016/j.scs.2019.101690

Barbieri, M., 2016. The Importance of Enrichment Factor (EF) and Geoaccumulation Index (Igeo) to Evaluate the Soil Contamination. J Geol Geophys 5. https://doi.org/10.4172/2381-8719.1000237

Censi, P., Cibella, F., Falcone, E.E., Cuttitta, G., Saiano, F., Inguaggiato, C., Latteo, V., 2017. Rare earths and trace elements contents in leaves: A new indicator of the composition of atmospheric dust. Chemosphere 169, 342–350. https://doi.org/10.1016/j.chemosphere.2016.11.085

Chen, H., Chen, Zhibiao, Chen, Zhiqiang, Ou, X., Chen, J., 2020. Calculation of Toxicity Coefficient of Potential Ecological Risk Assessment of Rare Earth Elements. Bull Environ Contam Toxicol 104, 582–587. https://doi.org/10.1007/s00128-020-02840-x

Douay, F., Pelfrêne, A., Planque, J., Fourrier, H., Richard, A., Roussel, H., Girondelot, B., 2013. Assessment of potential health risk for inhabitants living near a former lead smelter. Part 1: metal concentrations in soils, agricultural crops, and homegrown vegetables. Environ Monit Assess 185, 3665–3680. https://doi.org/10.1007/s10661-012-2818-3

Egbueri, J.C., 2020. Groundwater quality assessment using pollution index of groundwater (PIG), ecological risk index (ERI) and hierarchical cluster analysis (HCA): A case study. Groundwater for Sustainable Development 10, 100292. https://doi.org/10.1016/j.gsd.2019.100292

Gujre, N., Mitra, S., Soni, A., Agnihotri, R., Rangan, L., Rene, E.R., Sharma, M.P., 2021. Speciation, contamination, ecological and human health risks assessment of heavy metals in soils dumped with municipal solid wastes. Chemosphere 262, 128013. https://doi.org/10.1016/j.chemosphere.2020.128013

Li, H., Shi, A., Zhang, X., 2015. Particle size distribution and characteristics of heavy metals in road-deposited sediments from Beijing Olympic Park. Journal of Environmental Sciences 32, 228–237. https://doi.org/10.1016/j.jes.2014.11.014

Liu, B., Xu, M., Wang, J., Wang, Z., Zhao, L., 2021. Ecological risk assessment and heavy metal contamination in the surface sediments of Haizhou Bay, China. Marine Pollution Bulletin 163, 111954. https://doi.org/10.1016/j.marpolbul.2020.111954

Mason, B., 1966. Principles of geochemistry.

Müeller, G., 1969. Index of geoaccumulation in sediments of the Rhine River 108–118.
Wei, X., Gao, B., Wang, P., Zhou, H., Lu, J., 2015. Pollution characteristics and health risk assessment of heavy metals in street dusts from different functional areas in Beijing, China. Ecotoxicology and Environmental Safety 112, 186–192. https://doi.org/10.1016/j.ecoenv.2014.11.005

Williams, J.A., Antoine, J., 2020. Evaluation of the elemental pollution status of Jamaican surface sediments using enrichment factor, geoaccumulation index, ecological risk and potential ecological risk index. Marine Pollution Bulletin 157, 111288. https://doi.org/10.1016/j.marpolbul.2020.111288

Zhang, X., Eto, Y., Aikawa, M., 2021. Risk assessment and management of PM2.5-bound heavy metals in the urban area of Kitakyushu, Japan. Science of The Total Environment 795, 148748. https://doi.org/10.1016/j.scitotenv.2021.148748

Zhi, M., Zhang, X., Zhang, K., Ussher, S.J., Lv, W., Li, J., Gao, J., Luo, Y., Meng, F., 2021. The characteristics of atmospheric particles and metal elements during winter in Beijing: Size distribution, source analysis, and environmental risk assessment. Ecotoxicology and Environmental Safety 211, 111937. https://doi.org/10.1016/j.ecoenv.2021.111937




### Health risk assessment


See script **03-risk_assessment-health.R** for dataset preparation and health risk calculations.


REFERENCES:

Behrooz, R.D., Kaskaoutis, D.G., Grivas, G., Mihalopoulos, N., 2021. Human health risk assessment for toxic elements in the extreme ambient dust conditions observed in Sistan, Iran. Chemosphere 262, 127835. https://doi.org/10.1016/j.chemosphere.2020.127835

Hu, X., Zhang, Y., Ding, Z., Wang, T., Lian, H., Sun, Y., Wu, J., 2012. Bioaccessibility and health risk of arsenic and heavy metals (Cd, Co, Cr, Cu, Ni, Pb, Zn and Mn) in TSP and PM2.5 in Nanjing, China. Atmospheric Environment 57, 146–152. https://doi.org/10.1016/j.atmosenv.2012.04.056

Roy, D., Singh, G., Seo, Y.-C., 2019. Carcinogenic and non-carcinogenic risks from PM10-and PM2.5-Bound metals in a critically polluted coal mining area. Atmospheric Pollution Research 10, 1964–1975. https://doi.org/10.1016/j.apr.2019.09.002

US EPA, 1989. Risk Assessment Guidance for Superfund (RAGS): Part A (Reports and Assessments).

US EPA, O., 2015a. Exposure Assessment Tools by Routes [WWW Document]. URL https://www.epa.gov/expobox/exposure-assessment-tools-routes (accessed 3.3.22).

US EPA, O., 2015b. Regional Screening Levels (RSLs) - Generic Tables [WWW Document]. URL https://www.epa.gov/risk/regional-screening-levels-rsls-generic-tables (accessed 3.18.22).

Zhang, X., Eto, Y., Aikawa, M., 2021. Risk assessment and management of PM2.5-bound heavy metals in the urban area of Kitakyushu, Japan. Science of The Total Environment 795, 148748. https://doi.org/10.1016/j.scitotenv.2021.148748
