#Re-organize data:

plotdata <- wf %>%
  as.data.frame() %>% #normally don't have to do this but because wf is a spatial object this makes things easier
  mutate(year = as.numeric(YEAR_)) %>% #for some reason YEAR_ is stored as a character vector but we want it to be numeric
  group_by(var name you want to group by) %>%
  summarize(
    new_var1 = mean(existing_var1),
    new_var2 = sum(existing_var2),
    new_var3 = n() #counts the number of observations in group
  ) %>%
  dplyr::filter(if you want to filter on something)

#Plot
png(filename = “your-file-name.png”, width = 800, height = 400) 

    #initialize 2-column plot
    par(mfrow = c(1,2)) 

    #plot first column
    plot(x=plotdata$variable_name1, y = plotdata$variable_name2, axes = F, xlab = "", ylab = "", main = "", col = color-name)
    
      #add labels
      mtext(side = 1, text= "text you want", cex = label-scale) #x-axis label
      mtext(side = 2, fill in other arguments) #y-axis label
      mtext(side = 3, fill in other arguments) #main label
      axis(1, fill in other arguments) #add x-axis
      axis(2, fill in other arguments) #add y-axis

    #plot second column
    plot(what you want to plot, axes = F, xlab = "", ylab = "", main = "", col = color-name)
      
      #add labels
      mtext(side = 1, text= "text you want", cex = label-scale) #x-axis label
      mtext(side = 2, fill in other arguments) #y-axis label
      mtext(side = 3, fill in other arguments) #main label
      axis(1, fill in other arguments) #add x-axis
      axis(2, fill in other arguments) #add y-axis
      
      
dev.off() #ends the png call. everything between png() and dev.off() will be written to an external file      
      
      
