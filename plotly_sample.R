library(tidyverse)
library(plotly)
library(lubridate)

t = seq(from = ymd_hms("2022-01-01 00:00:00"), to = ymd_hms("2022-02-27 00:00:00"), length.out = 100)
a = runif(100, min = 4.5, max = 5.0)
b = runif(100, min = 400, max = 450)
c = runif(100, min = 2900, max = 3000)

df <- data.frame(t, a, b, c)

df %>% 
  plot_ly() %>% 
  add_lines(x = ~t, y = ~c, name = "rpm") %>%
  add_lines(x = ~t, y = ~b, name = "temp", yaxis = "y2") %>% 
  add_lines(x = ~t, y = ~a, name = "pressure", yaxis = "y3") %>% 
  layout(
    title = list(text = "Line graph using plotly package",x = 0),
    coloraxis = list(colorscale = "YlOrRd"),
    yaxis = list(showline = TRUE, side = "left", 
                 title = list(text = "rpm", standoff = 0), range = list(2500,3000),
                 dtick = 25,
                 tick0 = 2500,
                 tickmode = "linear"),
    yaxis2 = list(showline = TRUE, overlaying = "y", anchor = "free", side = "left",
                  position = 0.1,
                  title = list(text = "temp(degC)", standoff = 50), range = list(350, 450)),
    yaxis3 = list(showline = TRUE, overlaying = "y", anchor = "free", side = "left",
                  title = "press(barg)", range = list(4,6)),
    xaxis = list(showline = TRUE, title = "Time", rangeslider = list(visible = TRUE)),
    margin = list(autoexpand = TRUE, pad = 110, b = 50, l = 150, r = 100),
    showlegend = FALSE,
    legend = list(orientation = "v")
  )
