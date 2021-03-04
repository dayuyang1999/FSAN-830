library("ggplot2") ##load for plotting

# The "+" is used to add layers to a plot
# For ggplot, we use "+" not "%>%"

ggplot(data = starwars) +
  geom_point(mapping = aes(x = height, y = mass))




###### Simple Plot Variations

ggplot(data = starwars) + 
  geom_point(mapping = 
               aes(x = height, 
                   y = mass, 
                   color = gender))


ggplot(data = starwars) + 
  geom_point(mapping = 
               aes(x = height, y = mass), 
             shape = 15, color = "red")


#### Multiple layers of geoms

ggplot(data = starwars) +  
  geom_point(mapping = aes(x = height, y = mass)) +
  geom_text(mapping = 
              aes(x = height, y = mass, label = name),
            check_overlap = TRUE)


###  avoid the redundancy of mapping x-y positions for every geom 
# by specifying mapping defaults in the initial ggplot() function. 
# Any geom layers will use these defaults unless overridden with an aes() call


# omit the data and mapping argument names 
# by specifying those argument values in the order that the function expects. 
ggplot(starwars, aes(x = height, y = mass)) +  # the first x-y mapping auto become default
  geom_point() +
  geom_text(aes(label = name), check_overlap = TRUE) 




### Other Geoms （see https://www.causact.com/ggplot2-data-visualization-using-the-grammar-of-graphics.html）
# geom_col(), geom_density(), geom_histogram(),geom_segment.

## create mpgDF data frame
mpgDF = mpg %>% 
  group_by(manufacturer) %>%
  summarize(cityMPG = mean(cty),
            hwyMPG = mean(hwy),
            numCarModels = n_distinct(model),
  ) %>%
  filter(numCarModels >= 2)

mpgDF ## view contents of data frame



# geom_col

mpgDF %>% ## use mpgDF as data argument to ggplot()
  ggplot() +
  geom_col(aes(x = manufacturer, y = numCarModels))

# density
mpgDF %>% ## use mpgDF as data argument to ggplot()
  ggplot() +
  geom_density(aes(x = cityMPG))


#  geom_histogram
mpgDF %>% ## use mpgDF as data argument to ggplot()
  ggplot() +
  geom_histogram(aes(x = cityMPG))


# geom_linerange
mpgDF %>% ## use mpgDF as data argument to ggplot()
  ggplot() +
  geom_linerange(aes(x = manufacturer, 
                     ymin = cityMPG, 
                     ymax = hwyMPG))


# mpgDF %>% ## use mpgDF as data argument to ggplot()
ggplot() +
  geom_linerange(aes(x = manufacturer, ymin = cityMPG, ymax = hwyMPG)) +
  coord_flip()
