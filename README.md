# Project 4: Algorithm implementation and evaluation: Collaborative Filtering

### [Project Description](doc/project4_desc.md)

Term: Fall 2019

+ Team #
+ Projec title: Lorem ipsum dolor sit amet
+ Team members
	+ Cai, Lingyi lc3352@columbia.edu
	+ Chen, Xiwen xc2463@columbia.edu
	+ Dai, Tong td2602@columbia.edu
	+ Wu, Wenyue ww2501@columbia.edu
	
+ Project summary: In this project, we explored different matrix factorization methods for recommender system and compared a pair of algorithms of post processing. The goal of matrix factorization is to match consumers with most appropriate products. We use alternating least squares to minimize object function 3 in paper 4. First, we initialize matrix q by assigning the average rating for that movie as the first row, and small random numbers for the remaining entries. Next we fix p, solve q by minimizeing the objective function. Then we fix p, solve q by minimizing the objective funciton similarly. After that, we use temporal dynamics as regularization terms.
	
**Contribution statement**: [default] All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement. 

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
