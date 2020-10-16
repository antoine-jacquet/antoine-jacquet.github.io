# Microéconomie 5 – TD1

## Exercice 1 : préférences Cobb-Douglas 

On prend les utilités Cobb-Douglas...

\\[ u^A (x_A) = x_1^A x_2^A \\]

```markdown

  omega1 <- 40									# ressources totales en bien 1
	omega2 <- 30									# ressources totales en bien 2
	alpha <- 1/4									# importance relative du bien 1 pour A
	beta <- 3/4										# importance relative du bien 2 pour A
	UA <- function(x1, x2) x1^alpha * x2^beta		# fonction d'utilité de A
	UB <- function(x1, x2) x1 * x2					# fonction d'utilité de B
 
 ```
