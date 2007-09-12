standardMLIConverter = function(obj, data, trainInd) {
# use when the predict method directly returns factor-like predictions
# otherwise, need to define a special converter
   teData = data[-trainInd,]
   trData = data[trainInd,]
   tepr = predict(obj, teData)
   trpr = predict(obj, trData)
   new("classifierOutput", testPredictions=factor(tepr),
       trainPredictions=factor(trpr), RObject=obj)
   }

MLIConverter.knn = function(k=1, l=0) function(obj, data, trainInd) {
   kpn = names(obj$traindat)
   teData = data[-trainInd,kpn] # key distinction -- typical predict methods allow
   trData = data[trainInd,kpn]  # variables in newdata to be superset of those in formula, not knn
   tepr = predict(obj, teData, k, l) # our special predict.knn2
   trpr = predict(obj, trData, k, l)
   new("classifierOutput", testPredictions=factor(tepr), testScores=attr(tepr, "prob"),
       trainPredictions=factor(trpr), RObject=obj)
   }

MLIConverter.dlda = function(obj, data, trainInd) {
   kpn = names(obj$traindat)
   teData = data[-trainInd,kpn] # key distinction -- typical predict methods allow
   trData = data[trainInd,kpn]  # variables in newdata to be superset of those in formula, not knn
   tepr = predict(obj, teData)
   trpr = predict(obj, trData)
   new("classifierOutput", testPredictions=factor(tepr), 
       trainPredictions=factor(trpr), RObject=obj)
   }

MLIConverterPredType.class = function(obj, data, trainInd) {
   teData = data[-trainInd,]
   trData = data[trainInd,]
   tepr = predict(obj, teData, type="class")
   trpr = predict(obj, trData, type="class")
   new("classifierOutput", testPredictions=factor(tepr),
       trainPredictions=factor(trpr), RObject=obj)
   }

MLIConverterListEl.class = function(obj, data, trainInd) {
   teData = data[-trainInd,]
   trData = data[trainInd,]
   tepr = predict(obj, teData)$class
   trpr = predict(obj, trData)$class
   new("classifierOutput", testPredictions=factor(tepr),
       trainPredictions=factor(trpr), RObject=obj)
   }

MLIConverter.svm = function(obj, data, trainInd) { # decision.values parm needed
   teData = data[-trainInd,]
   trData = data[trainInd,]
   tepr = predict(obj, teData, decision.values=FALSE)
   trpr = predict(obj, trData, decision.values=FALSE)
   new("classifierOutput", testPredictions=factor(tepr),
       trainPredictions=factor(trpr), RObject=obj)
   }

MLIConverter.ldaPredMeth = function(method) function(obj, data, trainInd) { # get binding for method to predict
   teData = data[-trainInd,]                                              # at call time
   trData = data[trainInd,]
   tepr = predict(obj, teData, method=method)$class
   trpr = predict(obj, trData, method=method)$class
   new("classifierOutput", testPredictions=factor(tepr),
       trainPredictions=factor(trpr), RObject=obj)
   }

MLIConverter.logistic = function(threshold) function(obj, data, trainInd) {
   teData = data[-trainInd,]
   trData = data[trainInd,]
   tepr = 1*(predict(obj, teData, type="response") > threshold)
   trpr = 1*(predict(obj, trData, type="response") > threshold)
   tesco = predict(obj, teData, type="response")
   new("classifierOutput", testPredictions=factor(tepr), testScores=tesco,
       trainPredictions=factor(trpr), RObject=obj)
   }

MLIConverter.rpart = function(obj, data, trainInd) { # decision.values parm needed
   teData = data[-trainInd,]
   trData = data[trainInd,]
   tepr = predict(obj, teData, type="class")
   trpr = predict(obj, trData, type="class")
   teprob = predict(obj, teData, type="prob")
   new("classifierOutput", testPredictions=factor(tepr), testScores=teprob,
       trainPredictions=factor(trpr), RObject=obj)
   }


MLIConverter.nnet = function(obj, data, trainInd) {
   teData = data[-trainInd,]
   trData = data[trainInd,]
   tepr = predict(obj, teData, type="class")
   trpr = predict(obj, trData, type="class")
   new("classifierOutput", testPredictions=factor(tepr), testScores=predict(obj,teData),
       trainPredictions=factor(trpr), RObject=obj)
   }

MLIConverter.RAB = function(obj, data, trainInd) {
# use when the predict method directly returns factor-like predictions
# otherwise, need to define a special converter
   teData = data[-trainInd,]
   trData = data[trainInd,]
   tepr = predict(obj, newdata=teData)
   trpr = predict(obj, newdata=trData)
   new("classifierOutput", testPredictions=factor(tepr),
       trainPredictions=factor(trpr), RObject=obj)
   }

MLIConverter.naiveBayes = function(obj, data, trainInd) {
   kpn = names(obj$tables)
   teData = data[-trainInd,kpn] # key distinction -- typical predict methods allow
   trData = data[trainInd,kpn]  # variables in newdata to be superset of those in formula, not allowed here
   tepr = predict(obj, teData, type="class")
   trpr = predict(obj, trData, type="class")
   new("classifierOutput", testPredictions=factor(tepr),
       trainPredictions=factor(trpr), RObject=obj)
   }

MLIConverter.selftesting = function(obj, data, trainInd) {
   tepr = predict(obj, data)
   new("classifierOutput", testPredictions=factor(tepr),
       trainPredictions=factor(tepr), RObject=obj)
   }

MLIConverter.slda = function(obj, data, trainInd) { # decision.values parm needed
   teData = data[-trainInd,]
   trData = data[trainInd,]
   tepr = predict(obj, teData)
   trpr = predict(obj, trData)
   teprob = predict(obj, teData, type="prob")
   new("classifierOutput", testPredictions=factor(tepr$class), testScores=tepr$posterior,
       trainPredictions=factor(trpr$class), trainScores=trpr$posterior, RObject=obj)
   }
