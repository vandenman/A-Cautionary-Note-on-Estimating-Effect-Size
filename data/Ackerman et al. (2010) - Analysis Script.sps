
RECODE ES1 ES2 ES3 ES4 ES5 ES6 ES7 ES8 (0=SYSMIS).
EXECUTE.

COMPUTE DV=MEAN(ES1,ES2,ES5,ES6,ES7,ES8).
EXECUTE.

*Selecting only non-excluded cases

USE ALL.
COMPUTE filter_$=(Exc = 0).
VARIABLE LABELS filter_$ 'Exc = 0 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


* T Test for only Round 1
* First, filter only cases with Collection = 1

USE ALL.
COMPUTE filter_$=(Exc = 0 & Collection = 1).
VARIABLE LABELS filter_$ 'Exc = 0 & Collection = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST GROUPS=Condition('H' 'L')
  /MISSING=ANALYSIS
  /VARIABLES=DV
  /CRITERIA=CI(.95).

T-TEST GROUPS=Condition('H' 'L')
  /MISSING=ANALYSIS
  /VARIABLES=ES3 ES4
  /CRITERIA=CI(.95).



*T Test for all the data
*Removing the filter with only data collection for round 1, but still excluding the exclusion cases

USE ALL.
COMPUTE filter_$=(Exc = 0).
VARIABLE LABELS filter_$ 'Exc = 0 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST GROUPS=Condition('H' 'L')
  /MISSING=ANALYSIS
  /VARIABLES=DV
  /CRITERIA=CI(.95).


T-TEST GROUPS=Condition('H' 'L')
  /MISSING=ANALYSIS
  /VARIABLES=ES3 ES4
  /CRITERIA=CI(.95).








