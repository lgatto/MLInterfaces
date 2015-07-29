
plspinHcube = function() {
# learner candidates -- bound up with extras for parameters etc.
  learnerTags = c("LDA", "DLDA", "rpart(cp)", "randomForest",
                   "knn1", "nnet(size, decay)") #, "blackboost")
#
  shinyApp(ui = fluidPage(
   fluidRow( column(10, 
      textOutput("title", container=h1)) ),
   #fluidRow( column(2, 
   sidebarLayout(
    sidebarPanel(
#
# 3 boxes for data setup
        p(strong("Data setup for mlbench Hypercube:")),
        div(style="display:inline-block",numericInput(inputId="Npoints", label="# points", value = 
                         800, min=200, max=1200, step=200)), #),
        div(style="display:inline-block",numericInput(inputId="cubedim", label="cube dim.", value = 
                         3, min=2, max=6, step=1)), #),
        div(style="display:inline-block",      numericInput("vertSd", 
                         label="SD around vertex",
                         value = .1, min=.05, max=2, step=.05)),
# box for learner method
        div(style="display:block"),
        div(style="display:inline-block",
             selectInput("learner", label = "Learning method:",
               choices = learnerTags,
               selected = "LDA")),
# box for misclassification rate
        div(style="display:block"),
        div(style="display:inline-block",
             strong(em(textOutput("mcl")))), 
        div(style="display:block"),
        br(),
# axis selections (need dim > 3)
        div(style="display:inline-block",
             numericInput("dim1", label = "pc for x",
               value = 1, min=1, max=8, step=1)), #),
        div(style="display:inline-block",
             numericInput("dim2", label = "pc for y",
               value = 2, min=2, max=8, step=1)), #),
        div(style="display:inline-block",
             numericInput("dim3", label = "pc for z",
               value = 3, min=3, max=8, step=1)), # end sidebarPanel  #),
        br(),
# for rendering options
        div(style="display:inline-block",
              selectInput("ngpts",
                         label="grid edge size   ",
                         choices=as.character(4:20),
                         selected="10")),
        div(style="display:inline-block",
             selectInput("renderer", label = "Rendering method:",
               choices = c("auto", "canvas", "webgl"),
               selected = "webgl")), #),
             numericInput("cp", label = "rpart cp:",
               value = .01, min=.01, max=20, step=.01), #),
# two boxes for tuning
             numericInput("nnsize", label = "nnet size:",
               value = 2, min=1, max=10, step=1), #),
             numericInput("nndecay", label = "nnet decay:",
               value = .01, min=0, max=2, step=.01)), #),
#   fluidRow( column(10, 
       mainPanel(
         scatterplotThreeOutput("spinner", height="600px") ) ) ), 
  server = function(input, output, session) {
    requireNamespace("mlbench")
#
# deal with current selections on learner
#
    mod = reactive({
     lr = switch( input$learner,
            "LDA" = ldaI,
            "DLDA" = dldaI,
            "rpart(cp)" = rpartI,
            "randomForest" = randomForestI,
            "knn1" = knnI(),
            "nnet(size, decay)" = nnetI,
#            "blackboost" = blackboostI,
            "bagging" = baggingI )
     extras = switch( input$learner,
            "LDA" = NULL,
            "DLDA" = NULL,
#            "blackboost" = list(family=Multinomial()),
            "rpart(cp)" = list(cp=input$cp),
            "randomForest" = list(importance=TRUE),
            "knn1" = NULL,
            "nnet(size, decay)" = list(size=input$nnsize, decay=input$nndecay, MaxNWts=10000) )
     predExtras = switch( input$learner,
#            "blackboost" = list(type="class"), 
            "nnet(size, decay)" = list(type="class"), 
            "rpart(cp)" = list(type="class"), NULL )

     wrapper = switch( input$learner,
            LDA = function(x) x$class, force )
     list(learner=lr, extras=extras, predExtras=predExtras, predWrapper=wrapper)
     })
#
# build display
#
     ans = reactive({
#
# build dataframe using selections on data parameters
#
       data = mlbench.hypercube(n=input$Npoints, d=as.numeric(input$cubedim), sd=input$vertSd)
       data = data.frame(cl=data$classes, data$x)
# training indices
       tinds = sample(1:nrow(data),size=floor(nrow(data)/2),replace=FALSE)
# model call -- need to ensure that 'extras' go into ... for projectLearnerToGrid
       argl = c(list(
                formula=cl~., data=data, learnerSchema=mod()$learner, 
                  trainInd = tinds, ngpts=as.numeric(input$ngpts), 
                  predExtras=mod()$predExtras,
                  predWrapper = mod()$predWrapper), mod()$extras )
       argl = argl[ which(sapply(argl, length)>0) ]
       cur = do.call( projectLearnerToGrid, argl )
       cmat = confuMat(cur@fittedLearner)
       ok = sum(diag(cmat))
       err = sum(cmat) - ok
       output$mcl = renderText( paste0("Test miscl. proportion (random half) = ", round(err/ok, 2) ) )
       proj = cur@gridFeatsProjectedToTrainingPCs
       projtest = cur@testFeatsProjectedToTrainingPCs
  
       nclass = length(unique(cur@gridPredictions))
#       thecolors = palette(rainbow(nclass))[cur@gridPredictions]
     #  print(table(cur@gridPredictions))
     #  print(class(cur@gridPredictions))
       thecolors = colorRampPalette(brewer.pal(8,"Set2"))(nclass)[as.numeric(cur@gridPredictions)]
       labs = as.character(cur@gridPredictions)
       obj = scatterplot3js( x = proj[,as.numeric(input$dim1)], 
               y = proj[,as.numeric(input$dim2)], 
               z = proj[,as.numeric(input$dim3)], 
               color=thecolors, renderer=input$renderer, labels=as.character(labs) )
#       obj$points3d( x = projtest[,as.numeric(input$dim1)], 
#               y = projtest[,as.numeric(input$dim2)], 
#               z = projtest[,as.numeric(input$dim3)], 
#               color="black" , size=2)
       list(obj=obj, prmat=projtest[, c(as.numeric(input$dim1), as.numeric(input$dim2), as.numeric(input$dim3))],
                                  testlabs = as.character(cur@testLabels))
      })
      output$spinner = renderScatterplotThree(  (ans()$obj)$points3d(ans()$prmat, labels=ans()$testlabs, color="black" ) )
})
}


