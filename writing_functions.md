writing\_functions
================
rj2543
October 25, 2018

``` r
library(tidyverse)
```

    ## -- Attaching packages ----------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts -------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
set.seed(1)
```

``` r
y = rnorm(25, mean = 5, sd = 3)
z_scores = function(x) {
  z = (x - mean(x)) / sd(x)
  z
}

z_scores(y)
```

    ##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872
    ##  [6] -1.04107494  0.33550276  0.59957343  0.42849461 -0.49894708
    ## [11]  1.41364561  0.23279252 -0.83138529 -2.50852027  1.00648110
    ## [16] -0.22481531 -0.19456260  0.81587675  0.68682298  0.44756609
    ## [21]  0.78971253  0.64568566 -0.09904161 -2.27133861  0.47485186

``` r
z_scores(3)
```

    ## [1] NA

``` r
z_scores("my name is jeff")
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning
    ## NA

    ## Error in x - mean(x): non-numeric argument to binary operator

``` r
z_scores(iris)
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning
    ## NA

    ## Warning in Ops.factor(left, right): '-' not meaningful for factors

    ## Error in is.data.frame(x): (list) object cannot be coerced to type 'double'

``` r
z_scores(sample(c(TRUE, FALSE), 25, replace = TRUE))
```

    ##  [1]  0.6721344 -1.4282857  0.6721344  0.6721344  0.6721344  0.6721344
    ##  [7]  0.6721344 -1.4282857 -1.4282857  0.6721344 -1.4282857  0.6721344
    ## [13]  0.6721344  0.6721344 -1.4282857  0.6721344  0.6721344 -1.4282857
    ## [19]  0.6721344 -1.4282857  0.6721344 -1.4282857  0.6721344  0.6721344
    ## [25]  0.6721344

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
  
  z = mean(x) / sd(x)
  
  z
}
```

``` r
z_scores(3)
```

    ## Error in z_scores(3): Z scores cannot be computed for length 1 vectors

``` r
z_scores("my name is jeff")
```

    ## Error in z_scores("my name is jeff"): Argument x should be numeric

``` r
z_scores(iris)
```

    ## Error in z_scores(iris): Argument x should be numeric

``` r
z_scores(sample(c(TRUE, FALSE), 25, replace = TRUE))
```

    ## Error in z_scores(sample(c(TRUE, FALSE), 25, replace = TRUE)): Argument x should be numeric

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  tibble(
    mean_x = mean(x),
    sd_x = sd(x)
  )
}

mean_and_sd(y)
```

    ## # A tibble: 1 x 2
    ##   mean_x  sd_x
    ##    <dbl> <dbl>
    ## 1   5.51  2.85

``` r
sim_data = tibble(
  x = rnorm(30, mean = 1, sd = 1),
  y = 2 + 3 * x + rnorm(30, 0, 1)
)

ls_fit = lm(y ~ x, data = sim_data)
  
beta0_hat = coef(ls_fit)[1]
beta1_hat = coef(ls_fit)[2]

beta0_hat
```

    ## (Intercept) 
    ##     2.17422

``` r
beta1_hat
```

    ##        x 
    ## 3.019278

``` r
sim_regression = function(n, beta0, beta1) {
  
  sim_data = tibble(
    x = rnorm(n, mean = 1, sd = 1),
    y = beta0 + beta1 * x + rnorm(n, 0, 1)
  )
  
  ls_fit = lm(y ~ x, data = sim_data)
  
  tibble(
    beta0_hat = coef(ls_fit)[1],
    beta1_hat = coef(ls_fit)[2]
  )
}

sim_regression(30, 3, 4)
```

    ## # A tibble: 1 x 2
    ##   beta0_hat beta1_hat
    ##       <dbl>     <dbl>
    ## 1      3.25      3.45

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-title") %>%
  html_text()

review_stars = dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = dynamite_html %>%
    html_nodes(".review-data:nth-child(4)") %>%
    html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)

reviews
```

    ## # A tibble: 10 x 3
    ##    title                   stars       text                               
    ##    <chr>                   <chr>       <chr>                              
    ##  1 "Great \"Odd Ball\" mo~ 5.0 out of~ The dance scene was worth the time~
    ##  2 Nostalgic Stupidity     4.0 out of~ This movie is dumb. I won't lie an~
    ##  3 Happy                   5.0 out of~ Don't know why I lov this movie bu~
    ##  4 Go watch THE ROCK or d~ 2.0 out of~ This movie is horrible. How do so ~
    ##  5 My mom loves it         5.0 out of~ Got this for my mom for mother's d~
    ##  6 Nothing Quite Like It.  5.0 out of~ So much fun watching these listles~
    ##  7 Has pretty sweet bow s~ 5.0 out of~ Well, things are getting pretty se~
    ##  8 Great                   5.0 out of~ Great                              
    ##  9 Fast delivery           5.0 out of~ Bought as gift                     
    ## 10 Lol                     5.0 out of~ Funny

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

read_page_reviews("https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=3")
```

    ## # A tibble: 10 x 3
    ##    title                 stars text                                       
    ##    <chr>                 <dbl> <chr>                                      
    ##  1 Five Stars                5 Book report                                
    ##  2 Haha                      5 "\"Tina come get some ham\""               
    ##  3 Five Stars                5 Everyone loves Napoleon!                   
    ##  4 Five Stars                5 Classic … still watch it.                  
    ##  5 Hilarious                 5 Hilarious                                  
    ##  6 Napoleon the wise         5 Napoleon has conquered my heart, and broug~
    ##  7 Hilarious movie           5 Hilarious movie                            
    ##  8 Loved it.                 5 This movie is the best. I try to get my fr~
    ##  9 Worth it ?                3 It was alright                             
    ## 10 "Funniest \U0001f602~     5 Love this movie!  Go back and watch it at ~

``` r
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
urls = str_c(url_base, 1:5)

dynamite_reviews = bind_rows(
  read_page_reviews(urls[1]),
  read_page_reviews(urls[2]),
  read_page_reviews(urls[3]),
  read_page_reviews(urls[4]),
  read_page_reviews(urls[5])
)

dynamite_reviews
```

    ## # A tibble: 50 x 3
    ##    title                     stars text                                   
    ##    <chr>                     <dbl> <chr>                                  
    ##  1 "Great \"Odd Ball\" movi~     5 The dance scene was worth the time spe~
    ##  2 Nostalgic Stupidity           4 This movie is dumb. I won't lie and sa~
    ##  3 Happy                         5 Don't know why I lov this movie but ido
    ##  4 Go watch THE ROCK or dum~     2 This movie is horrible. How do so many~
    ##  5 My mom loves it               5 Got this for my mom for mother's day, ~
    ##  6 Nothing Quite Like It.        5 So much fun watching these listless pe~
    ##  7 Has pretty sweet bow ski~     5 Well, things are getting pretty seriou~
    ##  8 Great                         5 Great                                  
    ##  9 Fast delivery                 5 Bought as gift                         
    ## 10 Lol                           5 Funny                                  
    ## # ... with 40 more rows

``` r
fellowship_ring = readxl::read_excel("./data/LotR_Words.xlsx", range = "B3:D6") %>%
  mutate(movie = "fellowship_ring")

two_towers = readxl::read_excel("./data/LotR_Words.xlsx", range = "F3:H6") %>%
  mutate(movie = "two_towers")

return_king = readxl::read_excel("./data/LotR_Words.xlsx", range = "J3:L6") %>%
  mutate(movie = "return_king")

lotr_tidy = bind_rows(fellowship_ring, two_towers, return_king) %>%
  janitor::clean_names() %>%
  gather(key = sex, value = words, female:male) %>%
  mutate(race = tolower(race)) %>% 
  select(movie, everything()) 

lotr_tidy
```

    ## # A tibble: 18 x 4
    ##    movie           race   sex    words
    ##    <chr>           <chr>  <chr>  <dbl>
    ##  1 fellowship_ring elf    female  1229
    ##  2 fellowship_ring hobbit female    14
    ##  3 fellowship_ring man    female     0
    ##  4 two_towers      elf    female   331
    ##  5 two_towers      hobbit female     0
    ##  6 two_towers      man    female   401
    ##  7 return_king     elf    female   183
    ##  8 return_king     hobbit female     2
    ##  9 return_king     man    female   268
    ## 10 fellowship_ring elf    male     971
    ## 11 fellowship_ring hobbit male    3644
    ## 12 fellowship_ring man    male    1995
    ## 13 two_towers      elf    male     513
    ## 14 two_towers      hobbit male    2463
    ## 15 two_towers      man    male    3589
    ## 16 return_king     elf    male     510
    ## 17 return_king     hobbit male    2673
    ## 18 return_king     man    male    2459

``` r
lotr = function(dat){
  dat %>% 
  janitor::clean_names() %>% 
  gather(key = sex, value = words, female:male) %>%
  mutate(race = tolower(race)) %>% 
  select(movie, everything()) 
}

bind_rows(lotr(fellowship_ring), lotr(two_towers), lotr(return_king))
```

    ## # A tibble: 18 x 4
    ##    movie           race   sex    words
    ##    <chr>           <chr>  <chr>  <dbl>
    ##  1 fellowship_ring elf    female  1229
    ##  2 fellowship_ring hobbit female    14
    ##  3 fellowship_ring man    female     0
    ##  4 fellowship_ring elf    male     971
    ##  5 fellowship_ring hobbit male    3644
    ##  6 fellowship_ring man    male    1995
    ##  7 two_towers      elf    female   331
    ##  8 two_towers      hobbit female     0
    ##  9 two_towers      man    female   401
    ## 10 two_towers      elf    male     513
    ## 11 two_towers      hobbit male    2463
    ## 12 two_towers      man    male    3589
    ## 13 return_king     elf    female   183
    ## 14 return_king     hobbit female     2
    ## 15 return_king     man    female   268
    ## 16 return_king     elf    male     510
    ## 17 return_king     hobbit male    2673
    ## 18 return_king     man    male    2459

``` r
lotr_load_and_tidy = function(path, range, movie_name) {
  
  df = readxl::read_excel(path, range = range) %>%
    janitor::clean_names() %>%
    gather(key = sex, value = words, female:male) %>%
    mutate(race = tolower(race),
           movie = movie_name)
  
  df
  
}

lotr_tidy = 
  bind_rows(
    lotr_load_and_tidy("./data/LotR_Words.xlsx", "B3:D6", "fellowship_ring"),
    lotr_load_and_tidy("./data/LotR_Words.xlsx", "F3:H6", "two_towers"),
    lotr_load_and_tidy("./data/LotR_Words.xlsx", "J3:L6", "return_king")) %>%
  select(movie, everything()) 

lotr_tidy
```

    ## # A tibble: 18 x 4
    ##    movie           race   sex    words
    ##    <chr>           <chr>  <chr>  <dbl>
    ##  1 fellowship_ring elf    female  1229
    ##  2 fellowship_ring hobbit female    14
    ##  3 fellowship_ring man    female     0
    ##  4 fellowship_ring elf    male     971
    ##  5 fellowship_ring hobbit male    3644
    ##  6 fellowship_ring man    male    1995
    ##  7 two_towers      elf    female   331
    ##  8 two_towers      hobbit female     0
    ##  9 two_towers      man    female   401
    ## 10 two_towers      elf    male     513
    ## 11 two_towers      hobbit male    2463
    ## 12 two_towers      man    male    3589
    ## 13 return_king     elf    female   183
    ## 14 return_king     hobbit female     2
    ## 15 return_king     man    female   268
    ## 16 return_king     elf    male     510
    ## 17 return_king     hobbit male    2673
    ## 18 return_king     man    male    2459

``` r
x = rnorm(25, 0, 1)

my_summary = function(x, summ_func) {
  summ_func(x)
}

my_summary(x, sd)
```

    ## [1] 0.9958465

``` r
my_summary(x, IQR)
```

    ## [1] 1.260585

``` r
my_summary(x, var)
```

    ## [1] 0.9917102

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4
