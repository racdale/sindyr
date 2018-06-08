
## sindyr: Tools for Sparse Identification of Nonlinear Dynamics with R

`sindyr` is an R library for inferring ordinary differential equations from raw data, based on SINDy by Brunton et al. (2016). The library also extends this method in several ways, including a sliding window tool that generates descriptive measures of a system from SINDy.

If you use the code, toss us a shout out by citing:

> Dale, R. & Bhat, H. S. (submitted). Equations of mind: Data science for inferring ODEs for socio-cognitive systems.

Under "dale-bhat-materials," we show several example uses of `sindyr`, based on the manuscript cited above. For a quick start, consider perusing the reconstruction of the Lorenz attractor's equations using `sindyr` [here](https://htmlpreview.github.com/?https://github.com/racdale/sindyr/blob/master/dale-bhat-materials/Figure_3.html). 

### Installation

`install.packages('devtools')`

`devtools::install_github('racdale/sindyr')`

### Notes

sindyr works with R >= 3.4

sindyr depends on the following packages: arrangements, matrixStats, and igraph.

### References:

> Brunton, S. L., Proctor, J. L., & Kutz, J. N. (2016). Discovering governing equations from data by sparse identification of nonlinear dynamical systems. Proceedings of the National Academy of Sciences, 113(15), 3932-3937.
