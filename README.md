# highermath
**I got some starter code for some apps from Professor Paul Bamberg (Classic Math with a Modern User Interface) and the rest of the code is by Adi Lanka.**
**NOTE: First chunk is Cryptocurrency data analysis project
After are some interactive proofs and quizes.  **

**R Shiny Interactive Apps**

https://johnstuckey7.shinyapps.io/FinalQuiz/?_ga=2.40111714.1723400139.1649384582-1626263570.1649384582

I chose to apply some of the data analysis tools we learned in this class to analyze the price data of 3 different cryptocurrencies to look for patterns that were useful to someone who was trading in these markets. I was able to test out theories I have had for a few years in each of the apps I made. While I wasn't able to prove any of these theories, I was able to make the case that the signals I was looking at were much weaker than I thought.
First, it’s important to address the obvious question to put this crypto project in perspective: What is the utility of cryptocurrency and what problems is the technology aiming to solve? Please go through the Quiz App first to get a quick background on the relevant cryptocurrencies.

https://johnstuckey7.shinyapps.io/CryptoPermuted/?_ga=2.230885983.1723400139.1649384582-1626263570.1649384582
The point of this test is to see if there is a significant difference between 2 data sets. I partitioned the data into Weekends (fri through sun) and Weekdays (rest of the days). The question posed was whether there was a difference between trading on weekdays or weekends. I know that Friday night in the eastern time zone is the lowest trade volume of the week and recently there was a big crash because there wasn’t enough liquidity for the market to efficiently respond to big changes. Cryptocurrencies are unique in that they are equities markets that trade all over the world 24 hours a day. This test failed to reject the null hypothesis and there is no evidence that there is a meaningful difference between weekdays and weekends. Once we conduct the test, the p value shows that there is ~53% chance that we got data like this. Usually you set the alpha before the test to avoid bias and set it around 1-10%. This failed to find a significant difference but this tool could be used to test other effects, such as times of the day for a more detailed data set

https://johnstuckey7.shinyapps.io/CryptoAnalysis/?_ga=2.230885983.1723400139.1649384582-1626263570.1649384582
The Fourier Analysis is useful to identify periods in data. This is difficult to see in crypto because the prices go up so quickly. We want to see smaller scale increases and decreases. This would be more effective on data with shorter time scales, like 1-2 months then you could see if something periodic was happening weekly for example. If the prices decline over the month, then shoot up on a new month some of the coefficients will be very high and you can focus on that period in more detail. In general, Day Traders make money off selling at good times and profiting off the variance in the market. Looking at Solana from the ~300-400 days since start, using more coefficients would allow the Fourier Analysis to pick up on the wringing up and down in the graph. While overall this is a pretty good predictor of the line, you can see in the 230-410 days since start that while the graph tracks pretty well it misses some pretty big dips and this could be very costly for a trader. 

https://johnstuckey7.shinyapps.io/PCAnalysis/?_ga=2.230885983.1723400139.1649384582-1626263570.1649384582
Principal Component Analysis is useful for dimensionality reduction. The PCA will identify the important parts of the price and reduce the dimensions. (Go to year 2019 in ETH choose eigen of 3 toggle to 4) -Looking at this year, the prices are high in the fall and low in spring, winter and summer. The way this could be used is to check seasons on other years to see if there are trends by comparing the eigenvectors for different years. For example, the first eigenvalue is very high, 2nd and 3rd are relatively high with the 3rd value being a decent predictor but not great and the fourth eigenvalue is very good and is about 500x the 5th eigenvalue which is an error of about ⅕ of a percent. This is useful because there are actually 4-5 dimensions that matter here and 365 total before the PCA. There is a useful application to Machine learning here by compression the data you are able to train your neural net on data with 365 dimensions. This is known as the curse of dimensionality leading to an exponential problem which we have successfully reduced to a linear problem. 

https://johnstuckey7.shinyapps.io/CryptoBetaFitter/?_ga=2.262876108.1723400139.1649384582-1626263570.1649384582
I added a column to my data which was the logarithm of the price and tried to fit this data to a beta distribution but this didn’t turn out to be fruitful at all. Clearly the beta mean and beta variance are far off and this is because the distribution was not normalized. I choose to focus on the log price to make the distribution a better fit, though this didn’t end up mattering here. Also I choose the log price because it is a much more accurate measure of happiness and utility of money. Buying power is proportional to wealth, and going from 250k -> 500k can be seen as roughly the same as going from 2.5 million -> 5 million. 

https://johnstuckey7.shinyapps.io/PaintBrushTest/
This tool allows you to zoom in on a part of the graph and use linear regression to find a fitting line. This is interesting in hypothesis testing where you could ask the question if the price changes faster or slower when the volume traded is higher implying the variance goes down and the prices are more stable. The graph is plotted with volume versus change percent. The model failed to reject the null hypothesis that volume traded is correlated to change percent.

End of Cryptocurrency data analysis project
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------

https://johnstuckey7.shinyapps.io/AirMiles3D/
The user can add a new city by specifying its name, latitude, and
longitude. (See DataFrameGuide for appending a new row to a data
frame.)
•City names in the Western Hemisphere are shown in red.
•Routes, instead of being great circles, are straight lines on the Mercator
map.

https://johnstuckey7.shinyapps.io/Arithmetic/?_ga=2.4271859.1723400139.1649384582-1626263570.1649384582

Performs simple arithmetic operations.

https://johnstuckey7.shinyapps.io/PCAnalysis/?_ga=2.230885983.1723400139.1649384582-1626263570.1649384582
The idea is to introduce a new orthonormal basis into the vector space
of data, but this time the goal will be to achieve a very good reconstruction
by using only a small number of basis vectors.  We automatically get an orthogonal basis if we start with a
symmetric matrix and find its eigenvectors, so the first step will be to
convert our dataset into a symmetric matrix. We use the same trick as
with linear regression: multiply the matrix by its transpose.

https://johnstuckey7.shinyapps.io/Fields/?_ga=2.40111714.1723400139.1649384582-1626263570.1649384582
Lets the user add, subtract, multiply and divide the nine
2 ×2 conformal matrices with entries from Z3.

https://johnstuckey7.shinyapps.io/GL2S3/?_ga=2.31727102.1723400139.1649384582-1626263570.1649384582
Uses Z3, with elements named 0, 1, and -1, as the
underlying field.
•In this case there is a normal subgroup H consisting of I and −I.
•If you use determinant 1 only, you will find 12 cosets and generate
only the even permutations in A4
•If you use determinant -1, you will find another12 cosets and generate
the odd permutations in S4

https://johnstuckey7.shinyapps.io/IsoF4/?_ga=2.31727102.1723400139.1649384582-1626263570.1649384582

Demonstrates the isomorphism between SL2(F4)
and A5 as follows:
•Generate two matrices A and B
•Convert them to permutations pA and pB .
•Calculate the product p1 = pApB using a function in permutecalc.R.
•Calculate AB using a function in F4calc.R and convert to permutation
p2.
•Confirm that p1 = p2

https://johnstuckey7.shinyapps.io/Petersen/?_ga=2.227722205.1723400139.1649384582-1626263570.1649384582
A Hamiltonian walk is a walk that includes every vertex of the graph
once and only once. A Hamiltonian cycle is a cycle that includes every
vertex of the graph once and only once. App demonstrates that Petersen’s graph
has a Hamiltonian walk but no Hamiltonian cycle.  
An Eulerian walk is a walk that includes each edge of the graph one
and only once. If it starts and ends at the same vertex, it is (care-
lessly) called an Eulerian cycle, even if it passes through some vertices
multiple times.  App demonstrates Eulerian cycles.

https://johnstuckey7.shinyapps.io/ShortTest/?_ga=2.227722205.1723400139.1649384582-1626263570.1649384582
Short Interactive test for the users

https://johnstuckey7.shinyapps.io/PolyFit/?_ga=2.227722205.1723400139.1649384582-1626263570.1649384582
Takes a vector x of evaluation points and
a vector y of function values at those points and returns the coefficients of
the unique polynomial for which p(xi) = yi.

https://johnstuckey7.shinyapps.io/MLRegression/?_ga=2.227722205.1723400139.1649384582-1626263570.1649384582
Using the simplest polynomial model (first degree), but instead of trying to match the data perfectly we will just
try to minimize the difference between the data and our simple model.

https://johnstuckey7.shinyapps.io/Cereal/?_ga=2.227722205.1723400139.1649384582-1626263570.1649384582
Using Cereal dataset in the base R dataset the app displays and compares the variables using charts and regressions.  

https://johnstuckey7.shinyapps.io/Alelager/?_ga=2.27535868.1723400139.1649384582-1626263570.1649384582
Using Alelager dataset in the base R datasets that has one or more factor columns and two or more numeric columns; 
app lets you carry out permutation tests.

https://johnstuckey7.shinyapps.io/Modular/?_ga=2.262876108.1723400139.1649384582-1626263570.1649384582

Allows the user to select a number n less than 53 that is not
a prime and displays a multiplication table for all the elements [q]n∈ Znwhere
gcd(q, n) = 1.
It also counst the number of elements of each order and display the results
in a table.

https://johnstuckey7.shinyapps.io/IrreduciblePoly/?_ga=2.262876108.1723400139.1649384582-1626263570.1649384582
Tests the ten irreducible polynomials for p = 5, n = 2 to
determine, for each of them, whether x is a generator

https://johnstuckey7.shinyapps.io/NewGroupD6/?_ga=2.240256707.1723400139.1649384582-1626263570.1649384582

Presents the symmetry group of the regular hexagon as permuatations of the numbers 1 thorugh 6.  

https://johnstuckey7.shinyapps.io/D4Biggs/?_ga=2.240256707.1723400139.1649384582-1626263570.1649384582

Presents the symmetry group of the square.  Can use the app to check the axioms: closure, associativity, identity, and inverse.

https://johnstuckey7.shinyapps.io/GroupD6/?_ga=2.266494542.1723400139.1649384582-1626263570.1649384582

Presents the symmetry group of the regular hexagon as permuatations of the numbers 1 thorugh 6.

https://johnstuckey7.shinyapps.io/EigenMat/?_ga=2.266494542.1723400139.1649384582-1626263570.1649384582
Generates a 3 ×3 matrix with
randomly chosen entries from the finite field Z5, calculates its character-
istic polynomial, finds the eigenvalues, then checks the Cayley-Hamilton
theorem.

https://johnstuckey7.shinyapps.io/Permutation/?_ga=2.266494542.1723400139.1649384582-1626263570.1649384582

Using cycle notation, calculate permuations, powers, products and inverses of sets.  This is connected to the five Platonic solids, if you label the corners appropriately you can simulate rotations and reflections.

https://johnstuckey7.shinyapps.io/ThreeQuestions/?_ga=2.266494542.1723400139.1649384582-1626263570.1649384582

Interactive Chess Quiz
