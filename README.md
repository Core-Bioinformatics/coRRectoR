# coRRectoR: co-variation based, Robust and Reproducible method for correcting batch effects using a functional perspective
developed and maintained by Cameron Crawford, Miguel Larraz, Eleanor Williams, Irina Mohorianu

Bulk sequencing is state-of-the-art for exploring a wide-range of biological questions. Often not all required aspects for an in-depth characterisation of a biological process are available in one experiment, making it necessary to integrate quantifications from different sources; also, for larger projects, the samples require structuring in several batches. Current approaches for batch-correction rely solely on aligning the distributions of expression levels, regardless of the identity of expressed genes or the variation of amplitude for individual transcripts. The recent, significant sequencing advances facilitate deeper sequencing and more refined quantification of signal, increasing the dynamic range of expression levels but also introducing noise/variation in the system. 

To embed the functional interpretation of signal into the pre-processing of the data, we propose an inference of Gene Regulatory Networks on the batches to be integrated, followed by a batch correction based on aligning the functional interactions. This approach relies on the comparable aspects across batches and accommodates the signal specific to individual samples (both technical [noise], and biological [patient characteristics]). To link the data-analysis to its interpretation, and facilitate interactive, exploratory tasks and the sharing of easily-accessible information, we integrated coRRectoR with the bulkAnalyseR pipeline (accepted in Briefings in Bioinformatics).

Live app example: https://bioinf.stemcells.cam.ac.uk/shiny/coRRectoR_ab6s5HVclEDzVgrjp4wJ/

If you are using components of this package in published research please cite the following papers along with the *bulkAnalyseR* manuscript:

* **Comprehensive Shiny App for bulk data** Moutsopoulos, I., Williams EC, Mohorianu I (2022). bulkAnalyseR: An accessible, interactive pipeline for analysing and sharing bulk multi-modal sequencing data 
* **Noise removal:** Moutsopoulos, I. et al. (2021). noisyR: enhancing biological signal in sequencing datasets by characterizing random technical noise. Nucleic Acids Research, 49(14),e83–e83.
* **Gene regulatory network inference:** Huynh-Thu, V. A. et al. (2010). Inferring regulatory networks from expression data using tree-based methods. PloS one, 5(9), e12776.
