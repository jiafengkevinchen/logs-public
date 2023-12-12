
# Replication files for "Logs with Zeros? Some Problems and Solutions" by Chen and Roth (QJE) 

## Overview

The code and data in this replication package reproduces all tables and figures in the paper, including the analysis of 10 recent papers in the *AER* using arcsinh(Y) in Section 2.3, and the empirical illustrations in Section 5. All of the data used in the paper come from publicly-available replication packages on the AEA webpage.

## Data Availability and Provenance Statements

As mentioned above, all of the data used in the paper are from replication packages downloaded from the AEA website.

### Statement about Rights

- [x] I certify that the author(s) of the manuscript have legitimate access to and permission to use the data used in this manuscript. 
- [x] I certify that the author(s) of the manuscript have documented permission to redistribute/publish the data contained within this replication package.

### License for Data

<p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/"><a property="dct:title" rel="cc:attributionURL" href="https://github.com/jiafengkevinchen/logunique-reps">Replication code/data for Logs with Zeros? Some Problems and Solutions</a> by <span property="cc:attributionName">Jiafeng Chen and Jonathan Roth</span> is licensed under <a href="http://creativecommons.org/licenses/by/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">CC BY 4.0<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1"><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1"></a></p>



### Summary of Availability

- [x] All data **are** publicly available.
- [ ] Some data **cannot be made** publicly available.
- [ ] **No data can be made** publicly available.

### Details on each Data Source

The `_output/` directory contains the data from the 10 papers replicated in Section 2.3. The replication packages were are all downloaded from the AEA website for the relevant papers in the AER, and the code was lightly modified to save the data used for the relevant specification. The replication packages for the 10 papers are contained in `replications.zip`.  

The `sequeira_2016/` directory contains the replication package from Sequeira (2016), also downloaded from the AEA website, which is used in Section 5.



## Computational requirements

The replication code uses a combination of R and Stata. Package installation is handled in the scripts `r-package-setup.R` and `stata-package-setup.do`.  


The code was last run on a **2019 Macbook Pro with 64GB of RAM running MacOS Version 11.6**, using **StataSE 17** and **R 4.3.1**. Approximate runtime is 1 hour for the main script, and an additional 1 hour if re-running the original replication files.  



## Description of programs/code

- The bash script `main.sh` runs all of the programs to reproduce the analysis. It calls the following programs:
	- `r-package-setup.R` and `stata-package-setup.do` install the packages needed in R and Stata
	- `recalculate-with-rescaled-outcomes.do` runs the analyses in the 10 papers studied in Section 2.3 and assesses sensisitvity to units. This script calls the files titled `paper1.do`,...,`paper10.do`, which run the code from the original papers.
	- `summary-tables.R` produces the tables and figures in Section 2.3 based on the analysis of the 10 papers
	- `carranza-application.R`, `sequeira-2016-application.R`, and `berkouwer-dean-application.R` produce the results for the three applications in Section 5 
- The bash script `replications/run_all.sh` re-runs all of the code from the 10 replication packages used for the analysis in Section 2.3. (This is not necessary to run to reproduce the main results, as the results of the replications are already saved in the folder `_output/`)   
 	

## Instructions to Replicators

- The bash script `main.sh` should run all of the code needed to reproduce the files in the paper on Unix-based systems such as Mac or Linux (or in PowerShell in Windows). 
	- Before running the `main.sh` script, ensure that Stata and R are added to the PATH. For example, if the Stata application is located in `/Applications/Stata/StataMP.app/Contents/MacOS/` run `export PATH=$PATH:/Applications/Stata/StataMP.app/Contents/MacOS/` in the Terminal.
	- The `main.sh` script assumes that `stataME` is installed. If the user has a different vintage of Stata, e.g. `stataSE`, simply replace all occurrences of `stataME` with `stataSE` in `main.sh`.
	- Users who do not wish to run `main.sh` can also sequentially run all of the R and Stata files listed in `main.sh`
- The bash script `main.sh` takes the output from the original replication packages stored in the `_output/` directory as given. Users who wish to reproduce this output from the original replication files can unzip `replications.zip` and then run the bash script `replications/run_all.sh`. 
	- As with `main.sh`, please ensure that Stata/R are added to the path, and that the script references the appropriate vintage of Stata.
	- Running `replications/run_all.sh` will produce output to `replications/_output`. We copied the output from there to the folder `_output/` in the main replication directory, which is referenced in the main code above.     



## List of tables and programs


The provided code reproduces:

- [x] All numbers provided in text in the paper
- [ ] All tables and figures in the paper
- [ ] Selected tables and figures in the paper, as explained and justified below.

Below is a list of the tables and figures in the main text, and where they are reproduced. (Table 2 contains no data and thus is not listed).

| Figure/Table #    | Program                  | Line Number | Output file                      | Note                            |
|-------------------|--------------------------|-------------|----------------------------------|---------------------------------|
| Table 1           | summary-tables.R    |   47          | tables/change-from-a100-table.tex                 ||
| Table 3           | carranza-application.R| 37          | tables/carranza-asinh-results.tex                       ||
| Table 4          | carranza-application.R           | 162            |  Tables/carranza-poisson.tex                                |          |
| Table 5          | carranza-application.R      |      118       | tables/carranza-lee-bounds.tex                      ||
| Table 6          | sequeira-2016-application.R      | 255            | tables/sequeira-2016-poisson.tex            |       |
| Table 7          | sequeira-2016-application.R      | 297            | tables/sequeira-2016-log0-x.tex            |       |
| Figure 1          | summary-tables.R      | 101            | figures/change-from-a100-actual-vs-predicted.eps            |       |
| Figure 2          | sequeira-2016-application.R      | 312            | figures/sequeira-density.eps            |       |

