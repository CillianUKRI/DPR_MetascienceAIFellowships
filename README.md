# Overview
This repository contains the code for three distinct projects, all in some way related to the A Year In Metascience 2025  report https://www.gov.uk/government/publications/a-year-in-metascience-2025:
1) An allocation program used in the AI Metascience Fellowships call 2025 where distributed peer review (DPR) was used.
2) An analysis of agreement in reviewer scores depending on the number of reviewers.
3) <Alex>

DPR Allocation program

Distributed Peer Review (DPR) is a peer review system whereby the applicants to a funding opportunity are themselves reviewers. In this system, there are no external expert reviewers. A core step of this system is therefore matching applicants to reviewers.

In the specific funding opportunity that DPR was used for in this instance (AI Metascience Fellowships (https://www.ukri.org/opportunity/ukri-metascience-ai-early-career-fellowships/), the aim was to have each application reviewed 8-10 times and for each applicant to review 8-10 times. A goal was to minimise "bad faith" interaction with the system, and this was pursued by:
Creating two funding pools with equal funding available, each comprised of half the applicants.
Assigning each applicant to ten reviewers if possible, ensuring that reciprocal matches and matches with the same institutions are impossible.

A schematic overview of how the program works can be seen in Figure 1 below.

The scripts involved are:
scripts/main.R is the main workhorse
scripts/QAloop.R and scripts/qualityassurance.R, two small scripts used purely for quality assuring
All scripts in functions/, which are loaded in via main.R.

![DPR_Allocation_Diagram_100225](https://github.com/user-attachments/assets/bd7fd0b5-e52a-40c3-b75d-34ac526f21db)
*Figure 1, DPR Allocation Program overview


