### An Extended de Bruijn Graph for Feature Engineering Over Biological Sequential Data

_Mert Onur Cakiroglu, HASAN KURBAN, Parichit Sharma, Elham Khorasani Buxton, M. Oguzhan Kulekci, Maryam Raeeszadeh-Sarmazdeh, Mehmet Dalkilic_

This repository contains the software code and data used in the paper titled - "An Extended de Bruijn Graph for Feature Engineering Over Biological Sequential Data". The code is organized into collection of R scripts and instructions for replicating the results are as follows:

**Note:** 

- __Package dependencies__ The software has been tested with R version 4.2.2 and dependency installation is managed by the software itself. However, using newer version of R may require the users to manually install some of the package dependencies needed by the pipeline. In worst case, if the some of the packages have not been updated for newer R versions then those packages (and the models provided by them) need to be manually removed from the list of models to avoid unexpected failures.

- __Expected runtime__ Training all classification models (experiments sections of the manuscript) takes ~5 hours and the actual time may vary depending on the hardware resources of the system on which the program is ran. 


- Following commands should be executed in the terminal/shell/command prompt.

**Clone the repo**

```
git clone 
```



=======
# de Bruijn Graph



**Source code:** The Python implementation of the de Bruijn Graph (dBG) is available inside the `dBG` folder.

**Accessing the Data:** A fasta file to generate the GAR motifs is located in `data/rg-rgg.fasta`.

**Installing the dependencies:** Installing the NetworkX and Biopython libraries is required to run this implementation.
```bash
pip install networkx biopython
```

**How to run the program:** To reproduce the results, the approximate de Bruijn graph implementation (`debruijn.py`) should be run. The parameters in this implementation are set to $k=5$, `weight_threshold` = 0.5, and `similarity_const` ($\kappa$) = 0.5 (for approximate results). To get the results without approximation, `similarity_const` ($\kappa$) parameter has to be set to $0$ in line 194. The substitution matrix to be employed for the approximation can be specified using the `sub` parameter which can be defined in line 193 (BLOSUM80, BLOSUM90, PAM30, etc.).

**Output:** The resulting motifs will be saved in a CSV file named `out.csv` in the same directory by default.



# Training with dBG Features

Change the directory

```bash
cd TIMP_Classification/ClassificationPipeline
```

Run the main script on command line. It is advised to run it as a background job to prevent interference due to system sleeping and killing the job.

Run on Debrujin extracted motifs

```bash
nohup Rscript classificationPipeline.R no dbg_output 308_full.csv TRUE > dbg_job.log 2>&1 &
```

Run on BLAST aligned motifs

```bash
nohup Rscript classificationPipeline.R no blast_output blast.csv TRUE > BLAST_job.log 2>&1 &
```

Run on randomly sampled motifs (use classificationPipeline_alt.R)

```bash
nohup Rscript classificationPipeline_alt.R no rand_out rand100_avg.csv TRUE > rand_job.log 2>&1 &
```
**Output:** All the output data will be generated as CSV files in the output folder specified on the command line. Evaluation on validation folds are reported in the file `train_results.csv`, evaluation on the test data are reported inside the file `test_results.csv`. Additionally, the output plots for top models and all models will also be generated in the corresponding output folder. Note that the name of file containing the plots will remain same across the runs, however, each plot will be generated in its own output folder.
