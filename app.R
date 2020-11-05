library(shinydashboard)

ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "R Assignment 2"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Introduction", tabName = "intro", icon = icon("clipboard")),
                        menuItem("PCA", tabName = "PCA", icon = icon("chart-line")),
                        menuItem("EigenFaces", icon = icon("grin-beam"), tabName = "EigenFaces")
                      )),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "intro",
                                h3("Question 1 :Describe the meaning of the term eigen faces. How does it relate to PCA as studied in this
class? Explain the steps used to undertake eigen faces based face classification"),
                                h4("The Key idea of eigen faces was given by Turk and Pentland in 1991, it assumes that most face images lie on a low -dimensional
                  subspace determined by the first k (k<<<d) directions of maximum variance."),
                                h4("PCA is used to determine the vectors or eigenfaces u1,u2,u3,...uk "),
                                h4("Represent all face images in the dataset as linear combinations of eigenfaces."),
                                h4(" The eigenfaces themselves form a basis set of all images used to construct the covariance matrix. This produces dimension reduction by allowing the smaller set of basis images to represent the original training images. Classification can be achieved by comparing how faces are represented by the basis set."),
                        ),
                        tabItem(tabName = "PCA",
                                h2("PCA Dashboard"),
                                fileInput("file1", "Choose CSV File", accept = ".csv"),
                                checkboxInput("header", "Header", TRUE),
                                checkboxInput("Table", "Show Table", TRUE),
                                textInput("colStart", "Column Range start value", value = "3", width = NULL, placeholder = NULL),
                                textInput("colEnd", "Column Range end value", value = "9", width = NULL, placeholder = NULL),
                                textInput("colLabel", "Select Label Column", value = "1", width = NULL, placeholder = NULL),
                                
                                h4("This table shows a view of the selected coloums from the Uploaded data"),
                                tableOutput("contents"),
                                sliderInput(inputId = "variance",
                                            label = "Select variance",
                                            value = 0.9, min = 0.1, max = 1,step = 0.1),
                                h4("This array shows the percentage of variance of the data in order of the principle components"),
                                tableOutput("Output"),
                                h4("This is the plot of the first two Principle components selected"),
                                plotOutput('plotMy'),
                                h4("This is the plot of the first two Principle components selected based on standard pca implementation"),
                                plotOutput('plotOriginal')
                        ),
                        
                        tabItem(tabName = "EigenFaces",
                                h2("Eigen Faces Dashboard"),
                                
                                h3('First we find the average faces of the dataset'),
                                plotOutput('AverageFaces'),
                                br(),
                                br(),
                                br(),
                                br(),
                                h3('We now apply pca to find the eigen values'),
                                h3('Given below is the chart of the prominent eigen values'),
                                plotOutput('EigenGraph'),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                h3('Once we have the eigen values we can then visualize the Eigen Faces'),
                                sliderInput(inputId = "EFnum",
                                            label = "Select Number of Eigen Faces",
                                            value = 20, min = 4, max = 40,step = 4),
                                plotOutput('EigenVals'),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                h3('We can then use these found values for facial recognition'),
                                sliderInput(inputId = "faceFind",
                                            label = "Select an input to perform facial recognition",
                                            value = 20, min = 1, max = 400),
                                plotOutput('FaceFind'),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                        )
                      ),
                      tags$footer(title="Your footer here", align = "right", style = "
position:absolute;
bottom:0;
width:100%;
height:50px; /* Height of the footer */
color: white;
padding: 10px;
z-index: 1000;")
                    )
)

server <- function(input, output, session) {
  
  output$contents <- renderTable({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    read.csv(file$datapath, header = input$header)
  })
  ######################Code for PCA################
  da<- data.frame()
  output$contents <- renderTable({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    if(input$Table){
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      
      df<- read.csv(file$datapath, header = input$header)
      da<-df[,input$colStart:input$colEnd]
      dc<-head(da,5)
    }
    
  }
  )
  
  my_pca<-function(x,variance_explained=0.999,center=T,scale=T)
  {
    my_pca=list()
    ##Compute the mean of each variable
    if (center)
    {
      my_pca[['center']]=colMeans(x)
    }
    ## Otherwise, we set the mean to 0 
    else
      my_pca[['center']]=rep(0,dim(x)[2])
    ####Compute the standard dev of each variable
    if (scale)
    {
      my_pca[['std']]=apply(x,2,sd)
    }
    ## Otherwise, we set the sd to 0 
    else
      my_pca[['std']]=rep(1,dim(x)[2])
    
    ##Normalization
    ##Centering
    x_std=sweep(x,2,my_pca[['center']])
    
    x_std=x_std%*%diag(1/my_pca[['std']])
    
    ##Cov matrix
    eigen_cov=eigen(crossprod(x_std,x_std))
    #print(eigen_cov)
    ##Computing the cumulative variance values
    my_pca[['cumulative_variance']] =cumsum(eigen_cov[['values']])
    ## Computing the cumulative variance percentage
    calcVar=list()
    it =1
    for(vari in my_pca[['cumulative_variance']] ){
      calcVari = (vari/tail(my_pca[['cumulative_variance']],1))*100
      calcVar[it]<-calcVari
      it<-it+1
      
    }
    print(calcVar)
    my_pca[['Cumi_vari_per']]=calcVar
    ##Number of required components
    my_pca[['n_components']] =sum((my_pca[['cumulative_variance']]/sum(eigen_cov[['values']]))<variance_explained)+1
    ##Selection of the principal components
    my_pca[['transform']] =eigen_cov[['vectors']][,1:my_pca[['n_components']]]
    ##print(my_pca[['transform']])
    attr(my_pca, "class") <- "my_pca"
    
    return(my_pca)
  }
  predict.my_pca<-function(pca,x,..)
  {
    ##Centering
    x_std=sweep(x,2,pca[['center']])
    ##Standardization
    x_std=x_std%*%diag(1/pca[['std']])
    return(x_std%*%pca[['transform']])
  }
  
  library(ggplot2)
  output$plotMy<- renderPlot({
    file <- input$file1
    num<-as.integer(input$colLabel)
    print(num)
    
    if(!is.null(file)){
      daf<- read.csv(file$datapath, header = input$header)
      da<-daf[,input$colStart:input$colEnd]
      pca1=my_pca(as.matrix(da),input$variance,scale=TRUE,center = TRUE)
      projected=predict(pca1,as.matrix(da))
      output$Output <- renderTable({
        
        q<-as.array(pca1[['Cumi_vari_per']])
        head(q,   pca1[['n_components']])
      })
      #print(projected)
      ggplot()+geom_point(aes(x=-projected[,1],y=-projected[,2],color=da[,num]))}
  })
  output$plotOriginal<-renderPlot({
    library(FactoMineR)
    num<-as.integer(input$colLabel)
    file <- input$file1
    if(!is.null(file)){
      df<- read.csv(file$datapath, header = input$header)
      da<-df[,input$colStart:input$colEnd]
      pca_stats= PCA(as.matrix(da))
      print(pca_stats)
      projected_stats=predict(pca_stats,as.matrix(da))$coord[,1:5]
      
      ggplot(data=df)+geom_point(aes(x=projected_stats[,1],y=projected_stats[,2],color=da[,num]))+xlab('PC1')+ylab('PC2')+ggtitle(' dataset projected on the two mains PC (FactomineR)')
    }
  })
  ######################Code for Eigen Faces########
  df=read.csv("./archive/train_faces.csv")
 
  plt_img <- function(x){ image(x, col=grey(seq(0, 1, length=256)))}
  library(RSpectra)
  average_face=colMeans(df)
  AVF=matrix(average_face,nrow=1,byrow=T)
  
  output$AverageFaces<- renderPlot({
    print(plt_img(matrix(average_face,nrow=64,byrow=T))
    )},height = 500, width = 500)
  ###########STEP 2 :Perform PCA to get the EIGEN VECTORS
  
  ###########STEP 2.1 :Scale the matrix
  ScaledVal <- scale(df)
  ###########STEP 2.2 :Calculate Co-variance Matrix
  B <- t(ScaledVal) %*% ScaledVal / (nrow(ScaledVal)-1)
  ###########STEP 2.3 :Calculate Eigen Values and Eigen Vectors
  eigs <- eigs(B, 40, which = "LM")
  eigenvalues <- eigs$values
  eigenvectors <- eigs$vectors
  par(mfrow=c(1,1))
  par(mar=c(2.5,2.5,2.5,2.5))
  y=eigenvalues[1:50]
  ###Graph of the first biggest Eigen values
  output$EigenGraph<- renderPlot({
    plot(1:50, y, type="o", log = "y", 
         main="Magnitude of the 40 biggest eigenvalues", xlab="Eigenvalue #", ylab="Magnitude")
  },height = 500, width = 700)
  ###Displaying the first N Eigen Vectors
  output$EigenVals<- renderPlot({
    
    q=input$EFnum/4
 
    par(mfrow=c(q,4))
    par(mar=c(0.2,0.2,0.2,0.2))
    for (i in 1:input$EFnum){
      plt_img(matrix(as.numeric(eigenvectors[, i]),nrow=64,byrow=T))
    }
  },height = 500, width = 500)
  
  output$FaceFind<- renderPlot({
    PF1 <- data.matrix(df[input$faceFind,]) %*% eigenvectors
    
    # Transform all the traning photos onto eigen space and get the coefficients
    PFall <- data.matrix(df) %*% eigenvectors
    
    # Find the simple difference and multiplied by itself to avoid negative value
    test <- matrix(rep(1,400),nrow=400,byrow=T)
    test_PF1 <- test %*% PF1
    Diff <- PFall-test_PF1
    y <- (rowSums(Diff)*rowSums(Diff))
    
    # Find the minimum number to match the photo in the files
    x=c(1:400)
    newdf=data.frame(cbind(x,y))
    
    the_number = newdf$x[newdf$y == min(newdf$y)]
    
    par(mfrow=c(2,1))
    par(mar=c(1.5,1.5,1.5,1.5))
    barplot(y,main = "Similarity Plot: 0 = Most Similar")
    cat("the minimum number occurs at row = ", the_number)
    plt_img(matrix(as.numeric(df[the_number, ]), nrow=64, byrow=T))
    cat("The photo match the number#",the_number,"photo in the files")
  },height = 700, width = 500)
  
}

shinyApp(ui, server)