# color_palette_viewer: list of hex colors -> plot
# takes in a list of colors in hex format, then plots the palette via ggplot's bar chart

color_palette_viewer <- function(colors, title = NULL) {
  fake_data <- data.frame(x = seq(1:length(colors)), y = rep(1, length(colors)))
  
  rgbcols <- col2rgb(colors)
  white_text <- as.factor(ifelse(sqrt(.299 * rgbcols[1,]^2 + .587 * rgbcols[2,]^2 + .114 * rgbcols[3,]^2) < 127.5, "#ffffff", "#000000"))
  fake_data$font_color <- white_text
  print(white_text)
  if (is.null(names(colors))) {
    col_labs <- colors 
  } else {
    col_labs <- paste0(paste0(names(colors), " - "), colors)
  }
  fake_data$labs <- col_labs
  
  ggplot(fake_data, aes(x = x, y = y, fill = x)) + 
    geom_col() + 
    scale_fill_gradientn(colors = colors) +
    ggtitle(label = title) +
    theme_void() +
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          axis.text = element_blank(),
          legend.position = "none",
          plot.title = element_text(hjust = .5)) +
    geom_text(data = fake_data, aes(label = labs, color = as.factor(font_color)), position = position_stack(vjust = 0.5)) +
    coord_flip() +
    scale_color_manual(values=c("#000000", "#ffffff"))
  
  
}



outrun_cols <- c("#FF6C11", "#FF3864", "#2DE2E6", "#261447", "#0D0221", "#023788", "#650089", "#920075", "#F6019D", "#D40078", "#241734", "#2E2157", "#FD3777", "#F706CF", "#FD1D53", "#F9C80E", "#FF4365", "#540D6E", "#791E94", "#541388")

color_palette_viewer(outrun_cols, title = "Outrun Color Palette")