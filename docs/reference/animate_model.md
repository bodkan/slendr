# Animate the simulated population dynamics

Animate the simulated population dynamics

## Usage

``` r
animate_model(model, file, steps, gif = NULL, width = 800, height = 560)
```

## Arguments

- model:

  Compiled `slendr_model` model object

- file:

  Path to the table of saved individual locations

- steps:

  How many frames should the animation have?

- gif:

  Path to an output GIF file (animation object returned by default)

- width, height:

  Dimensions of the animation in pixels

## Value

If `gif = NULL`, return gganimate animation object. Otherwise a GIF file
is saved and no value is returned.
