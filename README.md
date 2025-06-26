# Overview
This repository contains the code for three distinct projects, all in some way related to the A Year In Metascience 2025  report https://www.gov.uk/government/publications/a-year-in-metascience-2025:
1) An allocation program used in the AI Metascience Fellowships call 2025 where distributed peer review (DPR) was used.
2) An analysis of agreement in reviewer scores depending on the number of reviewers.
3) Partial Randomisation: Simulations of proposals and their assessment 
in funding decision meetings, to attempt to help funders 
understand better how the degree of bias 
and the extent of randomisation can affect the 
quality of decision-making and its outcomes.

# DPR Allocation program

Distributed Peer Review (DPR) is a peer review system whereby the applicants to a funding opportunity are themselves reviewers. In this system, there are no external expert reviewers. A core step of this system is therefore matching applicants to reviewers.

In the specific funding opportunity that DPR was used for in this instance (AI Metascience Fellowships (https://www.ukri.org/opportunity/ukri-metascience-ai-early-career-fellowships/), the aim was to have each application reviewed 8-10 times and for each applicant to review 8-10 times. It was a priority to minimise "bad faith" interaction with the system, and this was pursued by:
- Creating two funding pools with equal funding available, each comprised of half the applicants.
- Assigning each applicant to ten reviewers if possible, ensuring that reciprocal matches and matches with the same institutions are impossible.

A schematic overview of how the program works can be seen in Figure 1 below.

![DPR_Allocation_Diagram_100225](https://github.com/user-attachments/assets/bd7fd0b5-e52a-40c3-b75d-34ac526f21db)
*Figure 1, DPR Allocation Program overview

The scripts involved are:
* scripts/main.R is the main workhorse
* scripts/QAloop.R and scripts/qualityassurance.R, two small scripts used purely for quality assuring
* All scripts in functions/, which are loaded in via main.R.



While the real data used as input cannot be shared publicly, a mock dataframe row can be viewed in the load_input_data280425DPR.R script

The output from this program is a table (csv) with each applicant as a row and each reviewer as a column. 

# Consistency Analysis
This analysis can be read about in the A Year In Metascience report (linked above). 
The code for this analysis is found in ConsistencyAnalysis/

# Partial Randomisation Simulations

The code for this analysis is found in PartialRandomisation/



