# <em>In silico</em> drug repositioning
Traditional drug discovery and development are often time-consuming and high risk [[```1```](https://github.com/Akmazad/Drug-Repositioning/blob/master/Writeup/Literatures/2019%20%5BBioinformatics%5D%20deepDR%20a%20network-based%20deep%20learning%20approach%20to%20in%20silico%20drug%20repositioning.pdf)]. Repurposing/repositioning of approved drugs offers a relatively low-cost and high-efficiency approach toward rapid development of efficacious treatments. [[```1```](https://github.com/Akmazad/Drug-Repositioning/blob/master/Writeup/Literatures/2019%20%5BBioinformatics%5D%20deepDR%20a%20network-based%20deep%20learning%20approach%20to%20in%20silico%20drug%20repositioning.pdf)]. <em>In silico</em> approaches may offer repositioning a candidate drug of low side-effect in the place of a drug with high side-effect, even though the direct associations between the candidate drugs and the diseases-of-the interest are absent in the literature gold-standard data.

In the quest for a list of state-of-the-art <em>In silico</em> methods, we've been conducting a manual literature survey using the search keyword, "Drug Repositioning" in the websites for Nature and Bioinformatics journals which yielded 42 articles so far ![published 2004-2019](https://github.com/Akmazad/Drug-Repositioning/blob/master/Writeup/Literatures/count%20of%20publication.png). 


# Resources
The set of resources that have been used in these literature are listed below:
## Drug-Target Network
### Source:
- DrugBank
- Therapeutic Target Database (TTD)
- PharmKB
- ChEMBL
- BindingDB
- IUPHAR/BPS
## Drug-drug interaction:
### Source:
- DrugBank
## Drug-SideEffect Net:
### Source:
- MetaADEDB
- CTD
- SIDER
- OFFSIDES
## Structure similarity net:
### Source:
- SMILES from DrugBank
- PubChem
## Target sequence similarith net:
### Source:
- Uniprot database for Protein sequence
## Pathway similarity net:
### Source:
- KEGG
- GO terms (BP, CC, MF)
## Clinical similarity (drug-disease)
### Source:
- ATC (Anatomical Therapeutics chemical classification) from DrugBank
- ICD10 (International classification of disease 10)
- ClinicalTrials.gov
## Disease MESH terms
### Source:
- OMIM database
## GE profiles
### Source:
- CMap (7000 GE profiles from Human cell-lines, that are treated with various small-molecules)
- 

# Literatures
|Study|Problem Formulation|Data|Methodology|Validation|Conclusion|
|---|---|---|---|---|---|
|<em>deep</em>DR|Predict Drug-Disease association|<ul><li>Drug-Disease annotation</li><li>Drug-side-effect network</li><li>Drug-target net</li><li>Drug-drug similarity net</li></ul>|<ul><li>Random-walk based representation of each network</li><li>Network fusion via Multi-modal deep autoencoder to get high-quality drug features</li><li>Infer drug-disease association score via cVAE (Collective Variational Auto-encoder</li></ul>|<ul><li>Data: ClinicalTrials.gov</li><li>Metrics: AUROC, AUPR</li><li>Methods: <ul><li>DISNet (Inductive Matrix Completion-based)</li><li>KBMF (Kernelized Bayesian Matrix Factorization)</li><li>RWR (Random-walk with restart)</li><li>Katz (Graph-based method)</li><li>RF (Random forest)</li><li>SVM</li></ul></li></ul>|List of drugs with high prediction scores with a certain disease are considered as candidate drugs for repositioning| 
## Few points to ponder

# DrugRepo: Our approach

## What we can do more

