check.Author <- function(name){
  status = FALSE
  table <- gs_ls()
  while (table$author != name)
  {
    gs_deauth()
    gs_auth()
    table <- gs_ls()
  }
  if (table$author == name)
  {
    status = TRUE
  }
  
  status
}

extract.data <- function(file.name,sheet.name){
  data.name <- gs_title(file.name)
  data <- gs_read(ss = data.name, ws = sheet.name)
  data
}

#create.Histogram(temp,temp$HEIGHT)
create.Histogram <- function(data.fr, var){
  if (is.character(var) == TRUE){
    R.plot <- ggplot() +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("Not Continuous") 
  }
  else{
    R.plot <- ggplot(data = data.fr, aes(x = var)) + 
      geom_histogram(binwidth= 0.5) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(y = "Count") +
      ggtitle("Survey Results")
  }
  R.plot
}

create.Barplot <- function(variable){
  if (is.character(variable) == TRUE){
    tab <- as.data.frame(table(variable), stringsAsFactors = FALSE)
    R.plot <- ggplot(data = tab, aes(x = tab$var, y = tab$Freq)) + 
      geom_bar(stat = "identity") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(y = "Count", x = "") +
      ggtitle("Survey Results") 
  }
  else{
    R.plot <- ggplot() +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("Not Discrete") 
  }
  R.plot
}

create.PieGraph <- function(var){
  tab <- as.data.frame(table(var), stringsAsFactors = FALSE)
  slices <- tab$Freq
  labels <- tab$var
  pct <- round(slices/sum(slices)*100)
  labels <- paste(labels, pct)
  labels <- paste(labels,"%",sep="")
  pie(slices, labels = labels)
}

create.BoxPlot <- function(data.fr, var){
  if (is.numeric(var) == TRUE){
    R.plot <- ggplot(data = data.fr, aes(x= "", y = var)) + 
      geom_boxplot() +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(y = "Count", x = "") +
      ggtitle("Survey Results")
  }
  else
  {
    R.plot <- ggplot() +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("Not Continuous") 
  }
  
  R.plot
}

create.Plot <- function(data.fr, var, chart.name) {
  if (chart.name == "Histogram"){
    create.Histogram(data.fr, var)
  }
  else if (chart.name == "Pie Chart"){
    create.PieGraph(var)
  }
  else if (chart.name == "Bar Plot"){
    create.Barplot(var)
  }
  else if (chart.name == "Boxplot"){
    create.BoxPlot(data.fr, var)
  }
}

create.Scatterplot <- function(data.fr,var1,var2, chart){
  R.plot <- ggplot(data = data.fr, aes(x = var1, y = var2))+
            geom_point()
  if (chart == "Linear"){
    R.plot <- R.plot + geom_smooth(method='lm',formula=y~x)
  }
  else if (chart == "Quadratic"){
    R.plot <- R.plot + geom_smooth(method = 'lm', formula = y ~ x + I(x^2))
  }
  else if (chart == "Hyperbolic"){
    R.plot <- R.plot + geom_smooth(method = 'lm', formula = y ~ 1/x)
  }
  else if (chart == "Natural Log X"){
    R.plot <- R.plot + geom_smooth(method = 'lm', formula = y ~ log(x))
  }
  else if (chart == "Natural Log Y"){
    R.plot <- R.plot + geom_smooth(method = 'lm', formula = log(y) ~ x)
  }
  R.plot
}

convertMenuItem <- function(mi, tabName) {
  mi$children[[1]]$attribs['data-toggle'] = "tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if (length(mi$attribs$class) > 0 && mi$attribs$class == "treeview") {
    mi$attribs$class = NULL
  }
  mi
}