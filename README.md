upset.hp diagram is an extension of UpSet technique to  and is used to visualize the object ofrdacca.hp,glmm.hp,gam.hp,and phylolm.hp (Lai et al. 2022a,2022b,2023,2024; Liu et al. 2023). The matrix layout enables the effective representation of relative importance of predictors, such as the unique effects and common effects in VP, as well as additional summary statistics or individual effects in HP. upset.hp diagram could, in principle, allow visualization of any number of predictor variables or groups of predictor variables. But considering the interpretability of data, we would like to recommend that the number of predictors (or groups of predictors) no more than 7.


library(devtools)

install_github('laijiangshan/upset.hp',build_vignettes = TRUE)

#If you haven't had devtools installed, please install it by typing "install.packages("devtools")" in R console. 
#use this packages, please cite the papers:
Lai, J.S., Tang, J., Li, T.Y., et al., 2024. Evaluating the relative importance of predictors in Generalized Additive Models using the gam.hp R package. Plant Diversity. 46(4), 542--546.
Lai, J.S., Zhu, W.J., Cui, D.F., et al., 2023. Extension of the glmm.hp package to zero-inflated generalized linear mixed models and multiple regression. J. Plant Ecol. 16(6), rtad038.
Lai, J.S., Zou, Y., Zhang, J.L., et al., 2022a. Generalizing hierarchical and variation partitioning in multiple regression and canonical analyses using the rdacca.hp R package. Methods Ecol. Evol. 13(4), 782–788.
Lai, J.S., Zou, Y., Zhang, S., et al., 2022b. glmm.hp: an R package for computing individual effect of predictors in generalized linear mixed models. J. Plant Ecol. 15(6), 1302–1307.
#Please feel free to send an email to the package maintainer Dr. Jiangshan Lai (lai@njfu.edu.cn) if you have any question or comments about this package.
