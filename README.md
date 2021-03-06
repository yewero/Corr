# Corr
A new cell similarity measure based on cell-pair differentiability correlation.  
Note: The original codes were written using MATLAB by Dr. Haiyan Huang's Lab and Dr. Luonan Chen's Lab.  
For convenience, the codes were rewritten using R.

## Usage
### Input:
`expr_mat`, an expression matrix (row: cell; column: gene).
### Correlation calculation:
`source("Corr.r")`  
`corMatrix <- Corr(expr_mat, ncpu = 2)` # For small dataset  
`corMatrix <- Corr_quick(expr_mat, ncpu = 2)` # For large dataset
### Output:
`corMatrix`, a correlation matrix.

## New feature
Multipe-CPU analysis is supported.

## Credit
Jiang, H., Sohn, L.L., Huang, H., and Chen, L. (2018). Single cell clustering based on cell-pair differentiability correlation and variance analysis. *Bioinformatics* 34, 3684-3694. [[Press](https://academic.oup.com/bioinformatics/article/34/21/3684/4996592)]
