# religion-and-income
In this paper, we investigate whether religious affiliation causes an increase in annual income.
In this paper, we investigate whether religious affiliation causes an increase in annual income.
Furthermore, the persistent hypothesis by Karl Marx, Max Weber and others that a Protestant 
affiliation relative to Catholic affiliation leads to higher annual income will be tested. Our 
findings suggest that religious affiliation does not contribute to higher annual earnings and that
Protestants do not report higher income than Catholics. On the contrary: Being Catholic has a significant
and substantially positive ($5,640) effect on income whereas being Protestant had a negative ($3,149), 
though significantly weak, effect on income. 

* Open Dataset
use data/gss0012, clear


* ===========================================================================
* =                                  1st Draft     							=
* ===========================================================================

* delete all observations if year is not 2012
drop if year != 2012

* drop income (wrong income data variable)
drop income

* rename variable rincom06 "income"
ren rincom06 income

* rename variable relig "religion"
ren relig religion


* Subsetting
* ----------

* Check missing values
misstable pat income sex race religion educ age, freq

/* finalize the dataset by deleting observations with missing values in 
our selection of variables. The final count is the actual sample size that we
will analyze at later stages */
drop if mi(income, age, sex, race, religion, educ)

* summarize variables with valid observations
su income age sex race educ religion


* Data Manipulation
* -----------------

* ======================
* = DEPENDENT VARIABLE =
* ======================

/* recode income from ordinal into quantitative variable. Thereby, the middle
value (rounded) of the respective interval is assigned to each observation */
recode income (1 = 500 "$500") (2 = 2000 "$2000") (3 = 3500 "$3500") ///
(4 = 4500 "$4500") (5 = 5500 "$5500") (6 = 6500 "$6500") (7 = 7500 "$7500") ///
(8 = 9000 "$9000") (9 = 11250 "$11250") (10 = 13750 "$13750") (11 = 16250 ///
"$16250") (12 = 18750 "$18750") (13 = 21250 "$21250") (14 = 23750 "$23750") ///
(15 = 27500 "$27500") (16 = 32500 "$32500") (17 = 37500 "$37500") (18 = ///
45000 "$45000") (19 = 55000 "$55000") (20 = 67500 "$67500") (21 = 82500 ///
"$82500") (22 = 100000 "$100000") (23 = 120000 "$120000") (24 = 140000 ///
"$140000") (25 = 160000 "$160000"), gen(income_quan)

* label new variable "income_cat" appropriately
la var income_quan "income recoded"

* table of categorized income
tab income_quan


* =========================
* = INDEPENDENT VARIABLES =
* =========================


* categorize religion into different religious groups
recode religion (1 = 1 "protestant") (2 = 2 "catholic") (10/11 = 3 "other christian") ///
(4 = 4 "none") (3 = 5 "other") (5/9 = 5 "other") (12/13 = 5 "other") (else = .), ///
gen(religion_cat)

* label new variable "religion_cat" appropriately
la var religion_cat "religion categorized"

* table of categorized religion
tab religion_cat


* Recoding to dummies
* -------------------

recode religion_cat (1 = 1 "protestant") (2/max = 0 "non-protestant"), ///
gen(religion_protestant)
recode religion_cat (2 = 1 "catholic") (1 3/max = 0 "non-catholic"), ///
gen(religion_catholic) 
recode religion_cat (3 = 1 "other christian") (1/2 4/max = 0 ///
"non-other christian"), gen(religion_otherchrist) 
recode religion_cat (4 = 1 "none") (1/3 5 = 0 "non-none"), ///
gen(religion_none)
recode religion_cat (5 = 1 "other") (1/4 = 0 "non-other"), ///
gen(religion_other)

* recode binary variable for sex (male = 1; female = 0)
recode sex (1 = 1 "male") (2 = 0 "female"), gen (sex_bi)

* create binary variable for race (white vs. non-white)
recode race (1 = 1 "white") (2/max = 0 "non-white"), gen(race_bi)


* Confidence intervals
* --------------------

* 95% confidence inverval of income distribution by religion
ci income_quan if religion_cat == 1
ci income_quan if religion_cat == 2
ci income_quan if religion_cat == 3
ci income_quan if religion_cat == 4
ci income_quan if religion_cat == 5


* Visualization
* -------------

* histogramm of income according to each religious group
hist income_quan, by(religion_cat) percent xtitle("Income Intervals") ///
name(hist_income_religion, replace)

* mean of income divided according to religion
gr dot income_quan, over(religion_cat) ///
ytitle("Distribution of Income by Religious Affiliation") ///
name(income_religion, replace)


* ================
* = DISTRIBUTION =
* ================


* Summary Statistics
* ------------------

su income_quan, d
tabstat income_quan, s(p25 median p75 iqr)


* Visualization
* -------------


* Histogram with normal distribution superimposed
hist income_quan, percent normal

/* Show that income is deviating from the normal distribution in its central 
values */
pnorm income_quan 


* Show that income is deviating from the normal distribution in the tail values
qnorm income_quan 

* Kernel density
kdensity income_quan, normal /// 
title(" Probability density function of household income distribution")

* Box plots
gr hbox income_quan,name(box_plot_income, replace)


* =============
* = NORMALITY =
* =============


* Visual assessment
hist income, bin(15) normal kdensity kdenopts(lp(dash) lc(black) bw(1.5)) ///
	note("Normal distribution (solid red) and kernel density (dashed black).") ///
	name(income_quan, replace)

symplot income_quan, ti("Symmetry plot") ///
	name(income_sym, replace)

qnorm income_quan, ti("Normal quantile plot") ///
	name(income_qnorm, replace)

su income_quan, d


* Variable transformation
* -----------------------

/* A technique used to approach normality with a continuous variable consists
in 'transforming' the variable with a mathematical operator that modifies
its basic unit of measurement. We learnt that the distribution of income for
its standard unit measurement is not normal, but perhaps the distribution
of the same values is closer to normality if we take a different measure. */

* The -gladder- command visualizes several common transformations all at once.
gladder income_quan, ///
	name(gladder, replace)

/* The logarithm transformation appears to approximate a normal distribution.
We transform the variable accordingly. */
gen logincome = ln(income_quan)
la var logincome "Income (log units)"

* Looking at skewness and kurtosis for the logged variable
tabstat income_quan logincome, s(n sk kurtosis min max) c(s)

* The log-income  histogram shows some improvement towards normality.
hist logincome, normal ///
    name(logincome, replace)


* Comparison Plot
* ---------------

/* Running the same graphs with a few options to combine them allows a quick
visual comparison of the transformation. */

* Part 1/4.
hist income_quan, norm xti("") ysc(off) ti("Untransformed (metric)") bin(21) ///
	name(income1, replace)

* Part 2/4.
gr hbox income_quan, fysize(25) ///
	name(income2, replace)

* Part 3/4.
hist logincome, norm xti("") ysc(off) ti("Transformed (logged)") bin(21) ///
	name(income3, replace)

* Part 4/4.
gr hbox logincome, fysize(25) ///
	name(income4, replace)

* Final combined graph.
gr combine income1 income3 income2 income4, imargin(small) ysize(3) col(2) ///
	name(income_comparison, replace)

* Drop individual pieces.
gr drop income1 income2 income3 income4
gr di income_comparison


* ===========================================================================
* =                                  2nd Draft     							=
* ===========================================================================


* =====================
* = ASSOCIATION TESTS =
* =====================


/* Association tests or test of correlation are used to check if there is a
relationship between two categorical variables and the strenght of this 
relationship. */

/* to run the tests you will first have to generate dummy variables for 
religius categories */
tab religion_cat, gen(m)
su m1
su m2
su m3
su m4
su m5


* ======================
* = SIGNIFICANCE TESTS =
* ======================


/* ttest for variables 
The t-test is an analysis of two population means . Statistical examinination 
is used to detrmine how these means are from each other. This test compares the 
various religious categories in terms of inome in dummies */
ttest income_quan,by(religion_protestant)
ttest income_quan,by(religion_catholic)
ttest income_quan,by(religion_otherchrist)
ttest income_quan,by(religion_none)
ttest income_quan,by(religion_other)

 
/* The Chi-square test is intended to test how likely it is that an observed 
distribution is due to chance. It is also called a "goodness of fit" statistic, 
because it measures  how well the observed distribution of data fits with the 
distribution that is expected if the variables are independent. */
tab income_quan religion_protestant, exp chi2
tab income_quan religion_catholic, exp chi2
tab income_quan religion_otherchrist, exp chi2
tab income_quan religion_none, exp chi2
tab income_quan religion_other, exp chi2


* ================
* = CORRELATIONS =
* ================

pwcorr income_quan educ, obs sig
pwcorr income_quan age, obs sig
pwcorr income_quan race_bi, obs sig
pwcorr income_quan sex_bi, obs sig
pwcorr income_quan religion_protestant, obs sig
pwcorr income_quan religion_catholic, obs sig
pwcorr income_quan religion_otherchrist, obs sig
pwcorr income_quan religion_none, obs sig
pwcorr income_quan religion_other, obs sig
pwcorr income_quan educ age race_bi sex_bi, star(.05)
pwcorr income_quan educ age race_bi sex_bi religion_protestant religion_catholic ///
religion_otherchrist religion_none religion_other, star(.05)


* Visualization
* -------------

* scatterplot without dummies
gr mat income_quan educ age, half name(gr_matrix_nonbinary, replace)

* scatterplot for religious subgroups
gr mat income_quan educ age if religion_protestant == 1, half ///
name(gr_matrix_religion_protestant, replace)
gr mat income_quan educ age if religion_catholic == 1, half ///
name(gr_matrix_religion_catholic, replace)
gr mat income_quan educ age if religion_otherchrist == 1, half ///
name(gr_matrix_religion_otherchrist, replace)
gr mat income_quan educ age if religion_none == 1, half ///
name(gr_matrix_religion_none, replace)
gr mat income_quan educ age if religion_other == 1, half ///
name(gr_matrix_religion_other, replace)


*smoothed fit income, education
lowess income_quan educ, name(income_educ_lowess, replace)

*smoothed fit income, age
lowess income_quan age, name(income_educ_lowess, replace)


* =====================
* = REGRESSION MODELS =
* =====================


* Simple linear regressions
* -------------------------
 
* Binary regressions
reg income_quan religion_protestant
reg income_quan religion_catholic
reg income_quan religion_otherchrist
reg income_quan religion_none
reg income_quan religion_other

* Logarithmic binary regressions
reg logincome religion_protestant
reg logincome religion_catholic
reg logincome religion_otherchrist
reg logincome religion_none
reg logincome religion_other


* ===========================================================================
* =                                  3rd Draft     							=
* ===========================================================================


* Multiple linear regressions
* ---------------------------

reg income_quan sex_bi educ age race_bi religion_protestant
reg income_quan sex_bi educ age race_bi religion_catholic
reg income_quan sex_bi educ age race_bi religion_otherchrist
reg income_quan sex_bi educ age race_bi religion_none
reg income_quan sex_bi educ age race_bi religion_other

* Logarithmic multiple linear regressions
reg logincome sex_bi educ age race_bi religion_protestant
reg logincome sex_bi educ age race_bi religion_catholic
reg logincome sex_bi educ age race_bi religion_otherchrist
reg logincome sex_bi educ age race_bi religion_none
reg logincome sex_bi educ age race_bi religion_other

/* With standardised ('beta') coefficients (only for regressions with 
statistically significant (on 5% level) results for our variable of interest 
(religion) */
reg income_quan sex_bi educ age race_bi religion_catholic, beta
reg logincome sex_bi educ age race_bi religion_none, beta


* ==========================
* = REGRESSION DIAGNOSTICS =
* ==========================

* Rerun regression model for estimating the effect of being Catholic
reg logincome sex_bi educ age race_bi religion_catholic

* Store fitted (predicted) values
cap drop yhat
predict yhat


* (1) Standardized residuals
* --------------------------

* Store the unstandardized (metric) residuals.
cap drop r
predict r, resid

* Assess the normality of residuals.
kdensity r, norm legend(off) ti("Normality of Residuals Catholic Regression") ///
    name(diag_kdens, replace)
    
* Homoskedasticity of the residuals versus fitted values (DV).!!!!!
rvfplot, yline(0) ms(i) mlab(religion_catholic) name(diag_rvf, replace)

* Store the standardized residuals.
cap drop rsta
predict rsta, rsta

* Identify outliers beyond 2 standard deviation units.
sc rsta yhat, yline(-2 2) || sc rsta yhat if abs(rsta) > 2, ///
    ylab(-3(1)3) mlab(religion_catholic) legend(lab(2 "Outliers")) ///
    name(diag_rsta, replace)

* (2) Heteroskedasticity
* ----------------------

* Homoskedasticity of the residuals vs. education
sc r educ, ///
	yline(0) mlab(religion_catholic) legend(lab(2 "Outliers")) ///
	name(diag_edu1, replace)

* LOWESS curve illustrating departure form homogenous variace for education
lowess rsta educ, bw(.5) yline(0) ///
	name(diag_edu2, replace)
	
* Breusch-Pagan test
estat hettest, rhs
estat hettest sex_bi
estat hettest age
estat hettest race_bi
estat hettest educ
estat hettest religion_catholic

* Huber-White regression with robust standard errors
reg logincome sex_bi educ age race_bi religion_catholic, vce(robust)


* (3) Variance inflation and interaction terms
* --------------------------------------------

* no 'kitchen sink' model
vif

* creating an interaction term to account for the variance that is explained
* by education and age within each other
gen educXage = educ * age
la var educXage "Education * Age"

* Regression model
reg logincome sex_bi educ age race_bi religion_catholic

* Regression model with an interaction term, standardised
reg logincome sex_bi educ age race_bi religion_catholic educXage, b


* Regression model with race and religion
reg income race_bi religion_catholic race_bi##religion_catholic

* Margins
margins i.race_bi i.religion_catholic i.race_bi#i.religion_catholic

* Marginplot
marginsplot, name(Marginsplot, replace)



