# Project 4: Algorithm implementation and evaluation: Collaborative Filtering

### [Project Description](doc/project4_desc.md)

Term: Fall 2019

+ Team #
+ Projec title: Project 4: Collaborative Filtering
+ Team members
	+ Cai, Lingyi lc3352@columbia.edu
	+ Chen, Xiwen xc2463@columbia.edu
	+ Dai, Tong td2602@columbia.edu
	+ Wu, Wenyue ww2501@columbia.edu
	
+ Project summary: In this project, we explored different matrix factorization methods for recommender system and compared a pair of algorithms of post processing. The goal of matrix factorization is to match consumers with most appropriate products. We used alternating least squares to minimize RMSE with temporal dynamics regularization, updating each of the parameters one by one in each iteration. The parameters used in the final fit came from cross validation. Lastly, we implemented and compared two postprocessing methods, which are Postprocessing SVD with KNN and Postprocessing SVD with kernel ridge regression. For evaluation part, we used root mean squared error (RMSE) for the test data set.
+ Please see [testing_report_group9.Rmd](https://github.com/TZstatsADS/fall2019-project4-sec1-grp9/blob/master/doc/testing_report_group9.Rmd) in /doc for final report.


	
**Contribution statement**: [default] All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement. 

+ WW worked on Matrix Factorization functions (ALS with Temporal Dynamics) and helped in editing of final reports.
+ LC and XC worked together. They take charge of comparison of postprocessing SVD with KNN and Postprocessing SVD with kernel ridge regression, debugging the implement of matrix factorization methods and writing the report and github.
+ DT. Prepare presentation slides and presenting key insights that team consolidated.

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.


P2 vs P3 given A3+R3

1. Alternating Least Aquares: A3--Alternating Least Squares to minimize object function 3, paper 4. 
2. Regularization Terms: R3--temporal dynamics, paper 5.
3. Postprocessing: P2--Postprocessing SVD with KNN, P3--Postprocessing SVD with kernel ridge regression, paper 2. 
