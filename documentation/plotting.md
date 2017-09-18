Plotting functions
================

``` r
  library(ramboseli)
```

A custom color palette
----------------------

Emily and Emily and I talked about how it would be neat to create a custom color palette for plots that is based on dominant hues in photos from Amboseli. I created one such palette based on the photo below, and I added some functions to this package so that you can use it in an R plot.

![](../img/amboseli_small.jpg)<br> <sub><sup>Photo by [Sergey Pesterev](https://unsplash.com/photos/DWXR-nAbxCk?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText)</sup></sub>

By default, there are 11 colors:

``` r
make_palette("div_earthsky")
```

![](plots/PLOTTING-palette-full-1.png)

It's called `div_earthsky` because it's a *diverging* color palette, which is useful when both low and high values are interesting and there is a meaningful, well-defined midpoint in the data. You can make make smaller palettes like this:

``` r
make_palette("div_earthsky", 5)
```

![](plots/PLOTTING-palette-small-1.png)

If you need more than 11 colors, you can use a continuous palette to interpolate between the existing colors:

``` r
pal <- make_palette(name = "div_earthsky", n = 100, type = "continuous")
image(volcano, col = pal)
```

![](plots/PLOTTING-palette-continuous-1.png)

There's also a sequential color palette called `seq_swelling` based on this photo:

![](../img/220px-Baboon_buttocks.jpg)<br>

``` r
make_palette("seq_swelling")
```

![](plots/PLOTTING-palette-swelling-full-1.png)

A sequential color palette called `seq_dryseason` based on this photo:

![](../img/dry.jpg)<br>

``` r
make_palette("seq_dryseason")
```

![](plots/PLOTTING-palette-dryseason_full-1.png)

There's also a sequential color palette called `seq_swelling` based on this photo:

![](../img/wet.jpg)<br>

``` r
make_palette("seq_wetseason")
```

![](plots/PLOTTING-palette-wetseason-full-1.png)
