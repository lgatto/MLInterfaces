
planarPlot2 = function (clo, eset, classifLab, ...) 
{
    require(RColorBrewer)
    pal <- brewer.pal("Set2", n = 8)
    ff <- MLInterfaces:::getGrid(eset)
    if (clo@learnerSchema@mlFunName %in% c("nnet", "rpart")) 
        ps <- predict(RObject(clo), newdata = ff, type = "class")
    else if (clo@learnerSchema@mlFunName %in% c("dlda2"))
        ps <- MLInterfaces:::predict.dlda2(RObject(clo), newdata = ff)
    else if (clo@learnerSchema@mlFunName %in% c("ada"))
        ps <- ada:::predict.ada(RObject(clo), newdata = ff, type="vector")
    else if (clo@learnerSchema@mlFunName %in% c("lda"))
        ps <- MASS:::predict.lda(RObject(clo), newdata = ff)
    else if (clo@learnerSchema@mlFunName %in% c("knn2"))
        ps <- MLInterfaces:::predict.knn2(RObject(clo), newdata = ff)
    else if (clo@learnerSchema@mlFunName %in% c("svm2"))
        ps <- e1071:::predict.svm(RObject(clo), newdata = ff)
    else if (clo@learnerSchema@mlFunName == "randomForest") {
        names(ff) <- rownames(exprs(eset))
        ps <- predict(RObject(clo), newdata = ff)
    }
    else ps <- MLInterfaces:::predict(RObject(clo), newdata = ff)
    if (clo@learnerSchema@mlFunName %in% c("lda", "qda")) 
        ps <- ps[[1]]
    plot(ff[, 1], ff[, 2], col = pal[as.numeric(factor(ps))], 
        pch = 19, xlab = names(ff)[1], ylab = names(ff)[2], ...)
    legend(min(ff[, 1]) + 0.2, max(ff[, 2]) - 0.5, legend = unique(ps), 
        col = pal[unique(as.numeric(factor(ps)))], pch = 19, bty="n")
#    legend(min(ff[, 1]) + 0.2, max(ff[, 2]) - 0.5, legend = unique(ps), 
#        col = 1, pch = 1)
}
mlearnWidget = function(eset, infmla) {
  learnerTags = c("LDA", "DLDA", "SLDA", "rpart", "randomForest",
                   "knn1", "nnet(size, decay)", "adaboost")
  valMethTags = c("random half", "NOTEST", "5-fold xval",
                   "10-fold xval", "5f xvalFS topvar(.75)")
 shinyApp(ui = fluidPage(
  fluidRow( column(6, textOutput("title", container=h1)),
            column(2, actionButton("btnSend", "Stop widget"))),
  fluidRow(
   column(2,  selectInput("learner", label = "Learning method:",
               choices = learnerTags,
               selected = "randomForest")),

   column(2,  selectInput("valmeth", label = "Validation method:",
               choices = valMethTags,
               selected="NOTEST")),
   column(2,  numericInput("nfeat", label = "N features (decr. in MAD):", 100, min = 2, max = 10000)),
   column(2,  numericInput("nnetdecayOrCP", label = "decay/cp", .01, min = .001, max = .5)),
   column(1,  numericInput("nnetsize", label = "size (nnet)", 3, min = 1, max = 10)),
   column(2,  numericInput("seed", label = "seed", 31415, min = 100, max = 100000))
          ),
  fluidRow(column(4, textOutput("space")), column(5, textOutput("miscl"))),
  fluidRow(column(4, htmlOutput("summRob")), column(5, tableOutput("confu"))),
  fluidRow(column(6, plotOutput("plotz")), column(6, plotOutput("pplot")))
 ), server= function(input, output, session) {
   mod = reactive({ 
     lr = switch( input$learner,
            "LDA" = ldaI,
            "DLDA" = dldaI,
            "SLDA" = sldaI,
            "rpart" = rpartI,
            "randomForest" = randomForestI,
            "knn1" = knnI(),
            "nnet(size, decay)" = nnetI,
            "adaboost" = adaI, 
            "bagging" = baggingI )
     extras = switch( input$learner,
            "LDA" = NULL,
            "DLDA" = NULL,
            "SLDA" = NULL,
            "rpart" = list(cp=input$nnetdecayOrCP),
            "randomForest" = list(importance=TRUE),
            "knn1" = NULL,
            "nnet(size, decay)" = list(size=input$nnetsize, decay=input$nnetdecayOrCP, MaxNWts=10000) )
     xv = switch(input$valmeth, "random half" = sample(1:ncol(eset), size=ceiling(ncol(eset)/2)),
                      "NOTEST" = xvalSpec("NOTEST"),
                      "5-fold xval"=xvalSpec("LOG", 5, balKfold.xvspec(5)),
                      "10-fold xval"=xvalSpec("LOG", 10, balKfold.xvspec(10)),
                      "5f xvalFS topvar(.75)"=xvalSpec("LOG", 5, balKfold.xvspec(5),
                             fsFun=fs.topVariance(.75)))
     list(learner=lr, xvmeth=xv, extras=extras)
     })
   nf = reactive({ input$nfeat })
   output$space = renderText( "RObject excerpt:  " )
   output$title = renderText("MLearn widget")

totext = function(x) {
    on.exit({
        sink(NULL)
    })
    tf = tempfile()
    sink(tf)
    print(x)
    hwrite(matrix(readLines(tf), nc=1))
}

tize = function (x) 
{
#
# purpose is to form an HTML rendering of part of the
# RObject(mloutput) as printed to console
#
    on.exit({
        sink(NULL)
    })
    tf = tempfile()
    sink(tf)
    print(RObject(x))
    rl = readLines(tf)
    hwrite(matrix(rl[1:min(10, length(rl))], nc=1), byrow=TRUE)
}

   ans = reactive({ 
     argl = c(list(
               formula= infmla,
               data=eset[1:nf(),], .method=mod()$learner, 
               trainInd=mod()$xv ), mod()$extras )
     set.seed(input$seed)
     do.call(MLearn, argl ) })
   output$summRob = renderText( tize(ans() ) )
   output$confu = renderTable({ options(digits=3); 
            if (input$learner == "randomForest") 
                   return(RObject(ans())$confu)
            else if (input$valmeth == "NOTEST") 
                    return(confuMat(ans(), "train"))
            confuMat(ans())})
   output$plotz = renderPlot( {
                   par(las=2, mar=c(4,7,2,2))
                   if (input$learner == "randomForest" & 
                         input$valmeth %in% c("NOTEST", "random half"))
                      plot(getVarImp( ans(), TRUE ), n=10, plat=annotation(eset),
                            toktype = "SYMBOL" )
                   else if (input$learner == "rpart" & input$valmeth == "NOTEST") {
                      par(mfrow=c(1,2))
                      library(rpart)
                      plotcp(RObject( ans() ) )
                      plot(RObject( ans() ) )
                      text(RObject( ans() ) )
                      }
                   #else plot(1,1)
                   } )
   output$pplot = renderPlot( {
          if (nchar(abstract(eset))==0) {
              et2 = eset[c("204685_s_at", "206409_at"),]
               argl = c(list(
               formula= infmla,
               data=et2, .method=mod()$learner, 
               trainInd=sample(1:ncol(et2), size=ceiling(ncol(et2)/2) )), mod()$extras )
             set.seed(input$seed)
             ans2 = do.call(MLearn, argl ) 
              rownames(et2) = paste0("X", featureNames(et2))
              planarPlot2(ans2, et2, "Tissue", main = "TIAM1 vs ATP2B2 (train half)" ) 
              }
          else {
#    PROBEID  SYMBOL
#1 1007_s_at    DDR1
#2 1007_s_at MIR4640
#> AnnotationDbi::select(hgu95av2.db, keys="IFNG", keytype="SYMBOL", columns="PROBEID")
#  SYMBOL   PROBEID
#1   IFNG   1021_at
#2   IFNG 1611_s_at
#3   IFNG  40702_at
              et2 = eset[c("1007_s_at", "1021_at"),]
               argl = c(list(
               formula= infmla,
               data=et2, .method=mod()$learner, 
               trainInd=sample(1:ncol(et2), size=ceiling(ncol(et2)/2)) ), mod()$extras )
             set.seed(input$seed)
             ans2 = do.call(MLearn, argl ) 
              rownames(et2) = paste0("X", featureNames(et2))
              planarPlot2(ans2, et2, "Tissue", main = "IFNG vs DDR1 (train half)" ) 
              }

           })
   output$miscl = renderText({ 
            if (input$learner == "randomForest" & input$valmeth %in% c("NOTEST", "random half")) {
                    mat = RObject(ans())$confu
                    mat = mat[,-ncol(mat)]
                    }
            else if (input$valmeth == "NOTEST") {
                    mat = confuMat(ans(), "train")
                    }
            else mat = confuMat(ans())
            sm = sum(mat)
            off = sm-sum(diag(mat))
            paste("est. miscl. rate = ", round(off/sm,3))
            })
    observe({
                    if(input$btnSend > 0)
                        isolate({
                           stopApp(returnValue=0)
                        })
           })

})}
