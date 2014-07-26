################################################################################
# Color Palette Main Panel
# Help users find and choose palette options.
################################################################################
output$paletteOptions <- renderPlot({
  # The informative display
  ColBrewTypes = levels(RColorBrewer::brewer.pal.info$category)
  names(ColBrewTypes) <- c("Diverging", "Qualitative", "Sequential")
  par(mfcol = c(1, 3))
  for( i in ColBrewTypes ){
    RColorBrewer::display.brewer.all(type = i)
    title(names(ColBrewTypes)[ColBrewTypes==i])
  }    
})
# Define example dataset once.
palExData <- ggplot2::diamonds[sample(nrow(ggplot2::diamonds), 1000), ]
output$paletteExample <- renderPlot({
  dpal <- qplot(carat, price, data=palExData, colour=clarity, size=I(10),
                main = paste("Example Output,", input$pal_main, "Palette"))
  dpal <- dpal + scale_colour_brewer(palette=input$pal_pal)
  dpal <- dpal + shiny_phyloseq_ggtheme_list[[input$theme_pal]] 
  print(dpal)
})
output$paletteTable <- renderDataTable({
  SupportedPalTab <- RColorBrewer::brewer.pal.info
  SupportedPalTab <- data.frame(Palette=rownames(SupportedPalTab), SupportedPalTab)
  colnames(SupportedPalTab)[2:3] <- c("Max_Colors", "Category")
  return(SupportedPalTab)
})
