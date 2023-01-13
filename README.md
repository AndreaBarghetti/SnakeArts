
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SnakeArts

<!-- badges: start -->
<!-- badges: end -->

Make a space

``` r
space <- Space$new(nrows = 30, ncol=50)
```

Start a Snake!

``` r
space$new_snake()
space$show() +snake_palette("E")
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

extend the head in a random direction, as long as the space is available

``` r
space$move_head()
space$show()
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Grow the snake until it’s trapped

``` r
space$grow_snake()
space$show()
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Make another one… and one more The longer the snake, the brighest it
gets

``` r
space$new_snake()
space$grow_snake()
space$show()
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Fill up the whole space

``` r
space$fill_space()
space$show() + snake_palette("E")
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

So pretty!
