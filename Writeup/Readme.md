# <em>In silico</em> drug repositioning
Traditional drug discovery and development are often time-consuming and high risk [[```1```](https://github.com/Akmazad/Drug-Repositioning/blob/master/Writeup/Literatures/2019%20%5BBioinformatics%5D%20deepDR%20a%20network-based%20deep%20learning%20approach%20to%20in%20silico%20drug%20repositioning.pdf)]. Repurposing/repositioning of approved drugs offers a relatively low-cost and high-efficiency approach toward rapid development of efficacious treatments. [[```1```](https://github.com/Akmazad/Drug-Repositioning/blob/master/Writeup/Literatures/2019%20%5BBioinformatics%5D%20deepDR%20a%20network-based%20deep%20learning%20approach%20to%20in%20silico%20drug%20repositioning.pdf)]. <em>In silico</em> approaches may offer the repositioning of a low side-effect candidate drug in the place of a drug with high side-effect, even though the direct associations between the candidate drugs and the diseases of the query drug are absent in the literature or gold-standard data.

In the quest for a list of state-of-the-art <em>In silico</em> methods, we've been conducting a manual literature survey using the search keyword, "Drug Repositioning" in the Nature and Bioinformatics journals, which yielded 42 articles so far ![published 2004-2019](https://github.com/Akmazad/Drug-Repositioning/blob/master/Writeup/Literatures/count%20of%20publication.png). 

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
|GE profiles|<ul><li>CMap (7000 GE profiles from Human cell-lines, that are treated with various small-molecules)</li><li>TCGA</li></ul>|

# Literatures
|Problem Formulation|Study|Data|Methodology|Validation|Conclusion|
|---|---|---|---|---|---|
|Predict Drug-Disease association|[```deepDR```](https://github.com/Akmazad/Drug-Repositioning/blob/master/Writeup/Literatures/2019%20%5BBioinformatics%5D%20deepDR%20a%20network-based%20deep%20learning%20approach%20to%20in%20silico%20drug%20repositioning.pdf) </br> 2019|<ul><li>Drug-Disease annotation</li><li>Drug-side-effect network</li><li>Drug-target net</li><li>Drug-drug similarity net</li></ul>|<ul><li>Random-walk based representation of each network</li><li>Network fusion via Multi-modal deep autoencoder to get high-quality drug features</li><li>Infer drug-disease association score via cVAE (Collective Variational Auto-encoder</li></ul>|<ul><li>Data: ClinicalTrials.gov</li><li>Metrics: AUROC, AUPR</li><li>Methods: <ul><li>DISNet (Inductive Matrix Completion-based)</li><li>KBMF (Kernelized Bayesian Matrix Factorization)</li><li>RWR (Random-walk with restart)</li><li>Katz (Graph-based method)</li><li>RF (Random forest)</li><li>SVM</li></ul></li><li>Case-study of selected Disease and describe their drug association in literature</li></ul>|List of drugs with high prediction scores with a certain disease are considered as candidate drugs for repositioning|
||[```DisDrugPred```](https://github.com/Akmazad/Drug-Repositioning/blob/master/Writeup/Literatures/2019%20%5BBioinfo%5D%20Drug%20repositioning%20through%20integration%20of%20prior%20knowledge%20and%20projections%20of%20drugs%20and%20diseases.pdf)|<ul><li>Drug-Disease annotation</li><li>Drug-drug similarity net:<ul><li>Chemical structure</li><li>Target similarity</li><li>Side-effect</li><li>Pathway similarity</li></ul></li></ul>|Non-negative matrix factorization|<ul><li>Metrics: TPR/FPR, AUROC, AUPR</li><li>Methods: <ul><li>TL-HGBI (Inormation flow based)</li><li>MBiRW (Random walk based)</li><li>LRSSL (Optimization based)</li><li>SCMFDD (Constrained Matrix factorization)</li></ul></li><li>Case-study of selected Disease: Same as deepDR</li></ul>|Same as <em>deep</em>DR|
||[```BNNR```](https://github.com/Akmazad/Drug-Repositioning/blob/master/Writeup/Literatures/2019%20%5BBioinfo%5D%20Drug%20repositioning%20based%20on%20bounded%20nuclear%20norm%20regularization.pdf)|<ul><li>Drug-Disease annotation</li><li>Drug-drug Chemical structure similarity net</li></ul>|Bounded neuclear norm regularization|<ul><li>Metrics: TPR/FPR, AUROC, AUPR</li><li>Methods: <ul><li>HGBI (Inormation flow based)</li><li>MBiRW (Random walk based)</li><li>DrugNet (Propagation flow based)</li><li>DRRS (Matrix completion of SVT algorithm)</li></ul></li></ul>|Same as <em>deep</em>DR|
||[```Nafiseh et al```](https://github.com/Akmazad/Drug-Repositioning/blob/master/Writeup/Literatures/2019%20%5BBioinfo%5D%20A%20new%20computational%20drug%20repurposing%20method%20using%20established%20disease%E2%80%93drug%20pair%20knowledge.pdf)|<ul><li>Drug-Disease annotation</li><li>GE data from human cell-lines</li><li>GE data for disease (breast cancer, RA, and IPF)</li></ul>|Combination of LLE (locally linear embedding) and DML (Distance metrics learning)|<ul><li>Metrics: AUC</li><li>Methods: <ul><li>PCA-ED</li><li>LLE-ED</li><li>PCA-DML</li><li>LLE-DML</li></ul></li></ul>|Same as <em>deep</em>DR|
|Hypothesis-driven approach|[```drugPredict```](https://github.com/Akmazad/Drug-Repositioning/blob/master/Writeup/Literatures/2018%20%5BOncogene%5D%20Using%20a%20novel%20computational%20drug-repositioning%20approach%20(DrugPredict)%20to%20rapidly%20identify%20potent%20drug%20candidates%20for%20cancer%20treatment.pdf)|<ul><li>OC GE data from TCGA</li><li>Drug-disease association</li></ul>|<ul><li>Construct GE-specific phenotype profile</li><li>Construct Drug-specific phenotype profile</li><li>Prioritize drugs based on similarities between those two profiles</li></ul>|<ul><li>Metrics: TPR</li><li>Methods: <ul><li>PCA-ED</li><li>LLE-ED</li><li>PCA-DML</li><li>LLE-DML</li></ul></li></ul>|Same as <em>deep</em>DR|


## Few points to ponder
<ul><li>Methods that predict drug-disease association through the incorporation of known drug-disease pairs (e.g. deepDR, DisDrugPred BNNR, etc) are likely to find similar drugswith same as class as query drug  (i.e. associated with similar disease) , correct?</li><li>In deepDR, drug-side-effect matrix is used to construct Drug-side-effects’ similarity measure, and then use that in their analysis, which seems unreasonable; candidate drugs with similar side-effect should be avoided, right?</li></ul>

# DrugRepo: Our approach
 - Datasets: <ul><li>DrugBank drug information</li><li>KEGG pathways</li></ul>
 - Method: 
    + For a query drug, DrugRepo prioritizes candidate drugs based on their combined ranks in all three similarity matrices.
    + DrugRepo also uses drug-drug similarity measures in order to find candidate drugs (```D_j```) for a query drug (```D_i```). We've compiled following three similarity matrices so far.
       - Chemical structure similairy
       - Target protein sequence similarity
       - Pathway similarity
    + Each similarity matrices are sorted in descending order of their similarity score.
    + A [```shiny app```](http://vafaeelab.com/drugrepositioning.html) for this project is under construction 

## What we can do more
- We can add the "Relative side-effect" of query-candidate drug-pairs as another feature in the ranking of candidate drugs, which can be defined as below. The idea is, lower value of (Query-vs-Candidate) relative side-effect should be higher in the ranking, <em>vice versa</em>.
