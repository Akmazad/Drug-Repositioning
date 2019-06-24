## Drugbank XML parsing

## Find protein sequence similarity
| Data | Code |
| --- | --- |
| [```protein.fasta```](https://github.com/Akmazad/Drug-Repositioning/tree/master/data) | [```multiple_jobs.sh```](https://github.com/Akmazad/Drug-Repositioning/blob/master/scripts/multiple_jobs.sh), [```parallel_target_Sim_Raijin.sh```](https://github.com/Akmazad/Drug-Repositioning/blob/master/scripts/parallel_target_Sim_Raijin.sh), [```parallel_target_Sim_Raijin.R```](https://github.com/Akmazad/Drug-Repositioning/blob/master/scripts/parallel_target_Sim_Raijin.R), [```combineFiles.R```](https://github.com/Akmazad/Drug-Repositioning/blob/master/scripts/combineFiles.R) |

### STEP-1:
Run [```multiple_jobs.sh```](https://github.com/Akmazad/Drug-Repositioning/blob/master/scripts/multiple_jobs.sh), which is wrapper around [```parallel_target_Sim_Raijin.sh```](https://github.com/Akmazad/Drug-Repositioning/blob/master/scripts/parallel_target_Sim_Raijin.sh) PBS script for running 100 rows (processed by [```parallel_target_Sim_Raijin.R```](https://github.com/Akmazad/Drug-Repositioning/blob/master/scripts/parallel_target_Sim_Raijin.R) in parallel on Raijin. All the parallel processes should be finished by 3hrs at max.
#### output:
Opon finishing each process will generate output files named, ```protSim_[startRow]_[endRow].csv```. Save these files in a local directory, say: ```C:\\Users\\Azad\\OneDrive - UNSW\\Vafaee Lab\\Projects\\Deep Brain\\Training 2\\protSims\\```.

### STEP-2:
Run [```combineFiles.R```](https://github.com/Akmazad/Drug-Repositioning/blob/master/scripts/combineFiles.R) file to combine all the files within a given (hard-coded) into a single one, named, say: ```protSim_Combined.csv```. 

Note: This file should be considered as a matrix, indexed by the protein indices in the [```protein.fasta```](https://github.com/Akmazad/Drug-Repositioning/tree/master/data).
