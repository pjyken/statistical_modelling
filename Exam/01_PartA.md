
## A1

We first tackle the specification of an appropriate statistical model for the loan amount (in 1000 €) given to subset of variables in the bank default data set. This subset includes the annual income of customers (in 1000 €), their gender, how old they are (in years) and whether or not they are a new or existing customer of the bank. An assumption of normal error terms is provided as a condition of the outcome model. 

The first step can be either done visually or algorithmically. Visually, we can examine the pairs plots from our selected variables to see if any relationships are immediately present. In Figure 1, we can see that there does not appear to be much of a relationship between the loan amount and any of the variables besides income. 

Figure 2 zooms in on the relationship between loan amount and annual income. The lighting-bolt shape in the plot indicates a non-linear relationship between the two variables. We can see that it would require at least a polynomial of order four (solid line) to adequately represent the relationship. However, due to the  noise in the relationship, only a polynomial of order ten (dashed line) would begin to adequately fit the trough. 

Algorithmically, we can use an order selection method like the type that was found in the previous exercise to determine the correct polynomial. After the correct polynomial is found, the other effects can then be brought in using the `stepAIC()` function to determine if any other variables have a significant contribution to the loan amount using AIC and BIC. However, the best model of this form does not have any other variables than income significant at the 95\% confidence level. 

An alternative to using the high-order polynomials is to make use of splines. Piecewise linear functions are capable of fitting the lighting-bolt relationship between loan amount and annual income. If the two inflection points (38, 40.5) are used as knots, a fit results with the lowest AIC and BIC values results. This can also be done automatically using the `spm()` function which will automatically determine the number and location of the knots. Yet, this will have a poorer fit compared to the piecewise linear function. Additionally, it will have a substantially greater amount of parameters due to the large number of knots used. 

Therefore, the selected final model is the piecewise linear function. 

$$\text{Loan} = \beta_0 + \beta_1\text{Income} + b_1(\text{Income} - 38)_+ + b_2(\text{Income} - 40.5)_+ $$

Where $(a)_+ = max(a, 0)$. As always, model diagnostics should be performed through the plotting of residuals and checking parametric form. In this case, the residual plots affirm the model fit, with no structure left in the plots. 




## A3

On the surface this appears to be a simple logistic regression. However, our goal is a model with interpretability. Therefore, some degree of regularization, if not sparsity is advisable to ensure the final model is interpretable by humans. 

If interpretability were not a consideration, there is a final model that will predict the likelihood of default with a considerably better degree than any of the models presented thus far.


We use crossvalidation to deal with the choice of lambda. 

glm(y ~ x, family=binomial)

