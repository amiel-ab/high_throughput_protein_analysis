
README


Script correlation :main script

extraction of the number of pixels that make up the size of the islands on the protein surface
count of pixels and number of islands
 data_cluster clean was used to remove all class 0 pixels; it is no longer necessary to use it for new files that do not contain class 0 pixels
Note that here the max variable does not relate to abundance but to the size of the largest island.

Abundance VT script: For the thesaurus match there are two possibilities: either run for the median or run the match for the max values.
 Do not use both pieces of script at the same time as they depend on the same variables (IDPAXDB).

Script_abundancediff :script used to make the difference between 2 files then the script was originally used to apply a difference between files that seemed redundant (peptide atlas).
This script was remodelled to apply a difference between the final abundance file (‘median of the interval’) and the whole-intergrated file.



NB: The abundance data comes from 17 files (including the integrated, which is a file that weights the different experiments):
15 were retained because I averaged two redundants and removed the integrated.


