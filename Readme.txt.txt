These files can be used to replicate the tables, figures and results from the article. 

The file "A numerical example.R" corresponds to the Monte-Carlo simulation in section 3.4.
It can be run independently and doesn't interact (read or write) files on your PC, 
so it is not necessary to specify paths. Parameters can be changed at will. 
They are currently set to correspond with the example presented in the article.

The empirical treatment is split between Stata files where data is read in and formatted,
fitted values are computed and graphs are plotted, and an R file ("Empirical analysis.R") where
the calibration is done. These files will specify "YOURPATH" to indicate where your personal
save location is. we recommend using the folder structure as specified with separate folders
for input and output, but any other folder structure can be accomodated by changing the paths
in the header of the do-files accordingly. You can find all instances where the path needs to be modified
by searching for "YOURPATH" (ctrl+f).

File "00 Read in raw data" reads in the raw data as described in the article 
(it is publicly available for download). 

"01 Estimation and correlation tables" produces, among other things, a csv.-file of fitted 
values that serves as input to "Empirical analysis.R", so it has to be run first. 
"Empirical analysis.R" produces a file "percentage figures.csv" that is used by "02 Figures.do",
so you need to run it before producing the figures. A recommended order of running the files 
is therefore:

1. A numerical example.R
2. 00 Read in raw data
3. 01 Estimation and correlation tables
4. Empirical analysis
5. 02 Figures

ph

