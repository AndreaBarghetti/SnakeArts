
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SnakeArts

<!-- badges: start -->
<!-- badges: end -->

Make a space

``` r
space <- Space$new(nrows = 40, ncol=70)
```

Start a Snake!

``` r
space$new_snake()
space$show(linewidth=1) + snake_palette("E")
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

extend the head in a random direction, as long as the space is available

``` r
space$move_head()
space$show(linewidth=1) + snake_palette("E")
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Grow the snake until it’s trapped

``` r
space$grow_snake()
space$show(linewidth=1) + snake_palette("E")
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Make another one… and one more The longer the snake, the brighest it
gets

``` r
space$new_snake()
space$grow_snake()
space$show(linewidth=1) + snake_palette("E")
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Fill up the whole space

``` r
space$fill_space()
space$show(linewidth=1) + snake_palette("E")
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

So pretty!

``` r
space$fill_space()
space$show(linewidth=3) + snake_palette("E")
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
space <- Space$new(50,90)
space$fill_space()
space$show(linewidth=1,
           dotsize=2) + snake_palette("B")
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
