# longitudional-analysis-NSF

This script includes multiple functions to test the longitudinal NSF data.

## Files:
* longitudional-quant-tests.r: This automates the following tasks:
1. Reads all data and formats it for analysis
2. Runs a total of 36 Shapiro-Wilk tests (9 measures and 4 conditions) to test the normality of each sample prior to paired analysis. Note, that because these results indicated that a subset of the samples did not have a normal distribution, all additional methods were conducted with non-parametric stats.
3. Runs 36 Wilcoxon Signed Rank tests to test the pre and post raw data for each condition and measurement. 
4. Descriptive statistics are calculated for each of the Wilcoxon Signed Rank tests, to provide more context surrounding the median and IQRs.
5. If the results of the Wilcoxon Signed Rank tests are significant (alpha=0.05), then a boxplot is created to visualize the distributions of the pre and post responses.
6. Finally, a Mann Whitney U Test is calculated on the difference scores of different conditions, in order to indicate whether one condition is significantly more 'effective' than another.

## Requirements:
* R with libraries dplyr, plyr, tidyr, broom, PairedData, ggpubr, extrafont installed.
* seniors.csv: Contains all raw pre and senior data (ommitted for participants' privacy)
* seniors-difference.csv: Contains the difference of the pre and senior scores based on the composite measures.(ommitted for participants' privacy)

## Outputs:
* Boxplots for each of the significant Wilcoxon Signed Rank test results.

## Notes:
Please do not upload any participant data to Github. 