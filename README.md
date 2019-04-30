# A Graphical User Interface for accessing and modeling the Cancer Genomics Data of MSKCC.

[![BuildStatus](http://www.bioconductor.org/shields/build/devel/bioc/canceR.svg?label=bioC)](https://bioconductor.org/checkResults/devel/bioc-LATEST/canceR/)
[![Linux/Mac Travis Build Status](https://img.shields.io/travis/kmezhoud/canceR/master.svg?label=Mac%20OSX%20%26%20Linux)](https://travis-ci.org/kmezhoud/canceR)
[![AppVeyor Build Status](https://img.shields.io/appveyor/ci/kmezhoud/canceR/master.svg?label=Windows)](https://ci.appveyor.com/project/kmezhoud/canceR)

[![Travis-CI Build Status](https://travis-ci.org/kmezhoud/canceR.svg?branch=master)](https://travis-ci.org/kmezhoud/canceR)
[![releaseVersion](https://img.shields.io/badge/release%20version-1.6.00-green.svg?style=flat)](https://bioconductor.org/packages/canceR) [![develVersion](https://img.shields.io/badge/devel%20version-1.7.04-green.svg?style=flat)](https://github.com/canceR)
[![Bioc](http://www.bioconductor.org/shields/years-in-bioc/canceR.svg)](https://www.bioconductor.org/packages/devel/bioc/html/canceR.html#since)
[![total](https://img.shields.io/badge/downloads-1759/total-blue.svg?style=flat)](https://bioconductor.org/packages/stats/bioc/canceR)


The package is user friendly interface based on the cgdsr and other modeling packages to explore, compare, and analyse all available Cancer Data (Clinical data, Gene Mutation, Gene Methylation, Gene Expression, Protein Phosphorylation, Copy Number Alteration) hosted by the Computational Biology Center at Memorial-Sloan-Kettering Cancer Center (MSKCC).


<img src="./vignettes/image/Circos.png">


Circular layout is an interesting way to integrate multi-omics heterogeneous  and big data of cancer disease in the same plot. The goal is to make easy the interpretation and the exploring of the relationships between cancers, genes or dimensions.
There are two R packages (RCircos and Circlize) available for Circular layout but its use remain laborious and needs computational skills.
CanceR package implements "getCircos" function to facilitate the visualization of cancer disease using Circos style. User needs only to select cancers and check which dimensions will be plotted.

User can select a simple gene list from files or examples or can focus on gene list from gene sets selected from MSigDB.
In the following case I  focus my exploring to two gene lists corresponding to gene set involved in DNA repair and Response to oxidative stress. canceR package allows user to select any gene sets from MSigDB without gene duplication.

I selected mRNA (0.3 threshold), CNA (0.85), Met HM450 (0.3), Mutation (Frequency 101). Only genes with significant rates are plotted with corresponding colors. For example Gold for mutation, Orange (Methylation), Green (CNA), red (mRNA expression).

I selected 5 cancers represented with 5 sectors. In the same sector there are 7 tracks (layouts) corresponding respectively (out : in) to: Gene List, Gene Sets, mRNA, CNA, Methylation, Mutation

<img src="./vignettes/image/dialogCircos.png">