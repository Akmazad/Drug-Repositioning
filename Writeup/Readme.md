# <em>In silico</em> drug repositioning
Traditional drug discovery and development are often time-consuming and high risk [[```1```](https://github.com/Akmazad/Drug-Repositioning/blob/master/Writeup/Literatures/2019%20%5BBioinformatics%5D%20deepDR%20a%20network-based%20deep%20learning%20approach%20to%20in%20silico%20drug%20repositioning.pdf)]. Repurposing/repositioning of approved drugs offers a relatively low-cost and high-efficiency approach toward rapid development of efficacious treatments. [[```1```](https://github.com/Akmazad/Drug-Repositioning/blob/master/Writeup/Literatures/2019%20%5BBioinformatics%5D%20deepDR%20a%20network-based%20deep%20learning%20approach%20to%20in%20silico%20drug%20repositioning.pdf)]. <em>In silico</em> approaches may offer repositioning a candidate drug of low side-effect in the place of a drug with high side-effect, even though the direct associations between the candidate drugs and the diseases-of-the interest are absent in the literature gold-standard data.

In the quest for a list of state-of-the-art <em>In silico</em> methods, we've been conducting a manual literature survey using the search keyword, "Drug Repositioning" in the websites for Nature and Bioinformatics journals which yielded 42 articles so far ![published 2004-2019](https://github.com/Akmazad/Drug-Repositioning/blob/master/Writeup/Literatures/count%20of%20publication.png). 

# Resources
The set of resources that have been used in these literature are listed below:

| Type |Source|
|---|---|
|Drug-Target Network|<ul><li>DrugBank</li><li>Therapeutic Target Database (TTD)</li><li>PharmKB</li><li>ChEMBL</li><li>BindingDB</li><li>IUPHAR/BPS</li></ul>|
|Drug-drug interaction|<ul><li>DrugBank</li></ul>|
|Drug-SideEffect Net|<ul><li>MetaADEDB</li><li>CTD</li><li>SIDER</li><li>OFFSIDES</li></ul>|
|Structure similarity net|<ul><li>SMILES from DrugBank</li><li>PubChem</li></ul>|
|Target sequence similarith net|<ul><li>Uniprot database for Protein sequence</li></ul>|
|Pathway similarity net|<ul><li>KEGG</li><li>GO terms (BP, CC, MF)</li></ul>|
|Clinical similarity (drug-disease)|<ul><li>ATC (Anatomical Therapeutics chemical classification) from DrugBank</li><li>ICD10 (International classification of disease 10)</li><li>ClinicalTrials.gov</li></ul>|
|Disease MESH terms|<ul><li>OMIM database</li></ul>|
|GE profiles|<ul><li>CMap (7000 GE profiles from Human cell-lines, that are treated with various small-molecules)</li></ul>|

# Literatures
|Problem Formulation|Study|Data|Methodology|Validation|Conclusion|
|---|---|---|---|---|---|
|Predict Drug-Disease association|[```<em>deep</em>DR```](https://github.com/Akmazad/Drug-Repositioning/blob/master/Writeup/Literatures/2019%20%5BBioinformatics%5D%20deepDR%20a%20network-based%20deep%20learning%20approach%20to%20in%20silico%20drug%20repositioning.pdf)|<ul><li>Drug-Disease annotation</li><li>Drug-side-effect network</li><li>Drug-target net</li><li>Drug-drug similarity net</li></ul>|<ul><li>Random-walk based representation of each network</li><li>Network fusion via Multi-modal deep autoencoder to get high-quality drug features</li><li>Infer drug-disease association score via cVAE (Collective Variational Auto-encoder</li></ul>|<ul><li>Data: ClinicalTrials.gov</li><li>Metrics: AUROC, AUPR</li><li>Methods: <ul><li>DISNet (Inductive Matrix Completion-based)</li><li>KBMF (Kernelized Bayesian Matrix Factorization)</li><li>RWR (Random-walk with restart)</li><li>Katz (Graph-based method)</li><li>RF (Random forest)</li><li>SVM</li></ul></li></ul>|List of drugs with high prediction scores with a certain disease are considered as candidate drugs for repositioning|
||[```DisDrugPred```](https://github.com/Akmazad/Drug-Repositioning/blob/master/Writeup/Literatures/2019%20%5BBioinfo%5D%20Drug%20repositioning%20through%20integration%20of%20prior%20knowledge%20and%20projections%20of%20drugs%20and%20diseases.pdf)|<ul><li>Drug-Disease annotation</li><li>Drug-drug similarity net:<ul><li>Chemical structure</li><li>Target similarity</li><li>Side-effect</li><li>Pathway similarity</li></ul></li></ul>|Non-negative matrix factorization|<ul><li>Metrics: TPR/FPR, AUROC, AUPR</li><li>Methods: <ul><li>TL-HGBI (Inormation flow based)</li><li>MBiRW (Random walk based)</li><li>LRSSL (Optimization based)</li><li>SCMFDD (Constrained Matrix factorization)</li></ul></li></ul>|Same as <em>deep</em>DR|
## Few points to ponder

# DrugRepo: Our approach

## What we can do more

