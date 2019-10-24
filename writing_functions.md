Writing Functions
================
Kylie Wheelock Riley
10/24/2019

We’re going to write some functions.

``` r
x = rnorm(n = 30, mean = 4, sd = 2.3)
x_again = rnorm(n = 30, mean = 6, sd = .3)

(x - mean(x)) / sd(x)
```

    ##  [1] -0.767120446  0.109493425 -0.993470503  1.637039863  0.267334741
    ##  [6] -0.977065475  0.438222871  0.709719461  0.533829741 -0.419692475
    ## [11]  1.546684110  0.332624325 -0.761479160 -2.485776741  1.128069748
    ## [16] -0.137851865 -0.106748415  0.932105430  0.799422547  0.553437533
    ## [21]  0.905205442  0.757128408 -0.008541293 -2.241925304  0.581490604
    ## [26] -0.149966223 -0.257816586 -1.680744021 -0.606639531  0.363029790

``` r
(x_again - mean(x_again)) / sd(x_again)
```

    ##  [1]  1.5413954 -0.2961850  0.3204956 -0.2345965 -1.8983946 -0.6887392
    ##  [7] -0.6627062 -0.2415224  1.2161757  0.7926368 -0.3738088 -0.4855096
    ## [13]  0.7093845  0.5329776 -1.0329536 -1.0565157  0.2914637  0.7993727
    ## [19] -0.3082034  0.9409190  0.3336151 -0.9364778  0.2619634 -1.5869527
    ## [25]  1.6348722  2.3231173 -0.6286716 -1.4797904  0.5493942 -0.3367558

Already getting complicated and getting a z score is one of the easiest
things you can do. Can write a function to simplify.

``` r
z_scores = function(x_arg) {
  ##adding conditional formatting so the function only works on the type of data you want
    if (!is.numeric(x_arg)) {
    stop("Argument x should be numeric")
  } else if (length(x_arg) == 1) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
  z = (x_arg - mean(x_arg)) / sd(x_arg)
  z
  
}

z_scores(x)
```

    ##  [1] -0.767120446  0.109493425 -0.993470503  1.637039863  0.267334741
    ##  [6] -0.977065475  0.438222871  0.709719461  0.533829741 -0.419692475
    ## [11]  1.546684110  0.332624325 -0.761479160 -2.485776741  1.128069748
    ## [16] -0.137851865 -0.106748415  0.932105430  0.799422547  0.553437533
    ## [21]  0.905205442  0.757128408 -0.008541293 -2.241925304  0.581490604
    ## [26] -0.149966223 -0.257816586 -1.680744021 -0.606639531  0.363029790

try out the function

``` r
z_scores(x_arg = y)
```

    ## Error in z_scores(x_arg = y): object 'y' not found

``` r
##figure out what y is after class

z_scores(x = 3)
```

    ## Error in z_scores(x = 3): Z scores cannot be computed for length 1 vectors

``` r
#does not work
z_scores("my name is jeff")
```

    ## Error in z_scores("my name is jeff"): Argument x should be numeric

``` r
#does not work
z_scores(x = c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_scores(x = c(TRUE, TRUE, FALSE, TRUE)): Argument x should be numeric

``` r
#does work
z_scores(x = iris)
```

    ## Error in z_scores(x = iris): Argument x should be numeric

``` r
#does not work
```

## Multiple Outputs

``` r
mean_and_sd = function(input_x) {
  
  if (!is.numeric(x_arg)) {
    stop("Argument x should be numeric")
  } else if (length(x_arg) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  tibble(
  mean_input = mean(input_x),
  sd_x = sd(input_x)
  )

  list(
    mean_input = mean(input_x), 
       sd_input = sd(input_x),
       z_score = (input_x - mean(input_x))/(sd(input_x))
  )
}
```

test the function above

## Multiple Inputs

``` r
sim_data = tibble(
  x = rnorm(30, mean = 1, sd = 1),
  y = 2 + 3 * x + rnorm(30, 0, 1)
)

##sim_data %>% 
 ## ggplot(aes(x = x, y = y)) +
 ## geop_point()

ls_fit = lm(y ~ x, data = sim_data)
  
beta0_hat = coef(ls_fit)[1]
##square bracket says give me the first coefficient (not a tidy way to do it)
beta1_hat = coef(ls_fit)[2]
##square bracket says give me the second coefficient
```

``` r
sim_regression = function(n) {
  
sim_data = tibble(
  x = rnorm(30, mean = 1, sd = 1),
  y = 2 + 3 * x + rnorm(30, 0, 1)
)


ls_fit = lm(y ~ x, data = sim_data)
  
beta0_hat = coef(ls_fit)[1]
##square bracket says give me the first coefficient (not a tidy way to do it)
beta1_hat = coef(ls_fit)[2]
##square bracket says give me the second coefficient
}

sim_regression( n = )
```

\#\#Revisiting past examples

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = 
  read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-title") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = 
  dynamite_html %>%
    html_nodes(".review-data:nth-child(4)") %>%
    html_text()

reviews = 
  tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

Now create a function

``` r
read_page_reviews <- function(url) {
  
  h = read_html(url)
  
  review_titles = h %>%
    html_nodes("#cm_cr-review_list .review-title") %>%
    html_text()
  
  review_stars = h %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()
  
  review_text = h %>%
    html_nodes(".review-data:nth-child(4)") %>%
    html_text()
  
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
}
```
