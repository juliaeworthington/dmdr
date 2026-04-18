# DMDr : An R package for the Dynamic Mode Decomposition Algorithm

This package implements the SVD-based dynamic mode decomposition (DMD) algorithm on snapshot data matrices. It also provides functions for plotting the resulting modes and dynamics.

### Installation

This package can be downloaded directly from the Github repository using the devtools package.

`install.packages('remotes')`

`library(remotes)`

`devtools::install_github('juliaeworthington/dmdr')`

### Tutorials
A basic tutorial of the available functions is available in the tutorials folder. Do not run the tutorial in Google Colab as the `dmd()` function produces erroneous values. RStudio and CSEL do not have this issue.

### Notes

This package depends on the following packages: ggplot2, ggforce, patchwork, and pracma.


### References
- Demo, Tezzele, Rozza. PyDMD: Python Dynamic Mode Decomposition. Journal of Open Source Software, 2018. [[DOI]](https://doi.org/10.21105/joss.00530)[[bibitem]](https://github.com/PyDMD/PyDMD/blob/master/readme/refs/Demo2018.bib)
- Ichinaga, Andreuzzi, Demo, Tezzele, Lapo, Rozza, Brunton, Kutz. PyDMD: A Python Package for Robust Dynamic Mode Decomposition. Journal of Machine Learning Research, 2024. [[DOI]](http://jmlr.org/papers/v25/24-0739.html)[[bibitem]](https://github.com/PyDMD/PyDMD/blob/master/readme/refs/Ichinaga2024.bib)[[arXiv]](https://doi.org/10.48550/arXiv.2402.07463)
- Peter J. Schmid. 2022. Dynamic Mode Decomposition and Its Variants. Annual Review Fluid Mechanics. 54:225-254.[[DOI]](https://doi.org/10.1146/annurev-fluid-030121-015835)
- Williams, Kevrekidis, Rowley. A data–driven approximation of the koopman operator: Extending dynamic mode decomposition. Journal of Nonlinear Science, 2015.[[arXiv]](https://arxiv.org/abs/1408.4408)

*This is my first time creating an R package (or any package for that matter) and using Github. Please let me know if you have any suggestions to improve DMDr!*
