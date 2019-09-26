## Drugbank XML parsing
This [```drugs_xml_new.R```](https://github.com/Akmazad/Drug-Repositioning/blob/master/scripts/drugs_xml_new.R) code is used to parse the Drugbank XML file to extract drug meta-data and store in a data frame. This code should take ~ 6GB of RAM and 2.3hrs running time. Raijin can be used for this task using [```drugs_xml_new.sh```](https://github.com/Akmazad/Drug-Repositioning/blob/master/scripts/drugs_xml_new.sh).

## Find protein sequence similarity
| Data | Code |
| --- | --- |
| [```protein.fasta```](https://github.com/Akmazad/Drug-Repositioning/tree/master/data) | [```multiple_jobs.sh```](https://github.com/Akmazad/Drug-Repositioning/blob/master/scripts/multiple_jobs.sh), [```parallel_target_Sim_Raijin.sh```](https://github.com/Akmazad/Drug-Repositioning/blob/master/scripts/parallel_target_Sim_Raijin.sh), [```parallel_target_Sim_Raijin.R```](https://github.com/Akmazad/Drug-Repositioning/blob/master/scripts/parallel_target_Sim_Raijin.R), [```combineFiles.R```](https://github.com/Akmazad/Drug-Repositioning/blob/master/scripts/combineFiles.R) |

### STEP-1:
Run [```multiple_jobs.sh```](https://github.com/Akmazad/Drug-Repositioning/blob/master/scripts/multiple_jobs.sh), which is wrapper around [```parallel_target_Sim_Raijin.sh```](https://github.com/Akmazad/Drug-Repositioning/blob/master/scripts/parallel_target_Sim_Raijin.sh) PBS script for running 100 rows (processed by [```parallel_target_Sim_Raijin.R```](https://github.com/Akmazad/Drug-Repositioning/blob/master/scripts/parallel_target_Sim_Raijin.R)) in parallel on Raijin. All the parallel processes should be finished by 3hrs at max. Note, within the [```parallel_target_Sim_Raijin.R```](https://github.com/Akmazad/Drug-Repositioning/blob/master/scripts/parallel_target_Sim_Raijin.R) file, each row were further parallelized (using ```%dopar%``` function in ```doparallel``` package), where the number of node clusters used is 122.

#### output:
Upon finishing each process will generate output files named, ```protSim_[startRow]_[endRow].csv```. Save these files in a local directory, say: ```C:\\Users\\Azad\\OneDrive - UNSW\\Vafaee Lab\\Projects\\Deep Brain\\Training 2\\protSims\\```.

### STEP-2:
Run [```combineFiles.R```](https://github.com/Akmazad/Drug-Repositioning/blob/master/scripts/combineFiles.R) file to combine all the files within a given (hard-coded) into a single one, named, say: ```protSim_Combined.csv```. 

Note: This file should be considered as a matrix, indexed by the protein indices in the [```protein.fasta```](https://github.com/Akmazad/Drug-Repositioning/tree/master/data).
