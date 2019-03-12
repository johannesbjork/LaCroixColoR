# LaCroixColoR (beta)
LaCroix Color Palettes for R. \
Choose from 20 different LaCroix flavors--quench your thirst!

## Install package

`install.packages("devtools")` \
`devtools::install_github("johannesbjork/LaCroixColoR")`

## For discrete palettes

`lacroix_palette("Pamplemousse", type = "discrete")`

<img src="example_images/Pamplemousse_discrete.jpg" width="300">

`lacroix_palette("PassionFruit", type = "discrete")`

<img src="example_images/PassionFruit_discrete.jpg" width="300">

`lacroix_palette("PeachPear", type = "discrete")`

<img src="example_images/PeachPear_discrete.jpg" width="300">

## For continuous palettes

`lacroix_palette("Pamplemousse", n = 50, type = "continuous")`

<img src="example_images/Pamplemousse_continuous.jpg" width="300">

`lacroix_palette("PassionFruit", n = 50, type = "continuous")`

<img src="example_images/PassionFruit_continuous50.jpg" width="300">

`lacroix_palette("PassionFruit", n = 25, type = "continuous")`

<img src="example_images/PassionFruit_continuous25.png" width="300">

`lacroix_palette("PassionFruit", n = 10, type = "continuous")`

<img src="example_images/PassionFruit_continuous10.png" width="300">

`lacroix_palette("PeachPear", n = 50, type = "continuous")`

<img src="example_images/PeachPear_continuous.jpg" width="300">

## For paired palette

`lacroix_palette(type = "paired")`

<img src="example_images/paired.jpg" width="300">

## Example plots

Plotting maps using `ggmaps`

`lacroix_palette("Pamplemousse", n = 7, type = "continuous")`

<img src="example_images/ca_map_Pamplemousse.jpg" width="300">



Contact: Dave Armitage (dave.armitage@gmail.com) & Johannes Bjork (bjork.johannes@gmail.com)
