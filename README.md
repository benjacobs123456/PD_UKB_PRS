This repository contains the main code used for the analysis presented in our paper at: https://www.biorxiv.org/content/10.1101/2020.02.15.950733v1.article-info

It is not an exhaustive list of all the scripts used but contains the main scripts used for the analysis
and especially the bits that will be useful for replicating the analysis

There were 3 main parts to this:
1) Extracting UKB data
--> extract_ukb_pheno_stage4.sh
2) Making the PRS
--> make_PD_PRS_stagex (run in order)
3) The bulk of the analysis (case-control, PRS performance, predict-PD performance)
--> ukb_case_control_PD.sh
--> example_bootstrap.sh (an example of how we bootstrapped the estimates of interaction)

All of these analyses were run on the Queen Mary University of London HPC, which runs on a job scheduler system.

A lot of these analyses were run in chunks (either by chromosome or subsets of the phenotype data) to speed up performance.

Let me know if you spot any mistakes or anything is unclear.
Thanks
Ben Jacobs
18-12-2019
