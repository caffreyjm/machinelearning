predict_lifts <- function{
## Classify how well an exercise (dumbell curl) was done

	library(caret)
	pml <- read.csv("pml-training.csv")
	pml_test <- read.csv("pml-testing.csv")
	original_number_features <- dim(pml)[2]
	original_number_rows <- dim(pml)[1]
	set.seed(1)

### Cleaning the data
##  To clean the data non zero values (columns) and columns with large numbers of missing values were removed.

	nzv <- nearZeroVar(pml,saveMetrics=TRUE)
	pml_nzv <- pml[,!nzv[,4]]
	pml_nzv_test <- pml_test[,!nzv[,4]]
	work <- is.na(pml_nzv)
	work_list <- (colSums(work)==max(colSums(work)))
	pml_stripped <- pml_nzv[,!work_list]
	pml_stripped_test <- pml_nzv_test[,!work_list]

## The next step was to remove highly correlated variables.

	pml_numeric_cor <- pml_stripped[,7:58]
	pml_cor <- abs(cor(pml_numeric_cor))
	diag(pml_cor)<-0
	highlyCorDescr <- findCorrelation(pml_cor, cutoff = .75)
	pml_filtered <- pml_stripped[,(-(highlyCorDescr+6))]
	pml_filtered_test <- pml_stripped_test[,(-(highlyCorDescr+6))]

## Use principal component analysis to reduce the number of variables

    pcaOutput <- preProcess(pml_filtered[,7:37], method=c("BoxCox","center","scale","pca"))
    pml_train <- predict(pcaOutput,pml_filtered[,7:38])
    pml_test <- predict(pcaOutput,pml_filtered_test[,7:38])

	
### Classification model
	pml_rf <-train(classe~.,data=pml_train,method="rf",
				trControl=trainControl(method="cv",number=5),
                prox=TRUE,allowParallel=TRUE)
	pml_rf_result <- predict(pml_rf,pml_train)
	pml_rf_cm <-confusionMatrix(pml_rf_result,pml_train$classe)		
	
	pml_test_answer <- pml_rf_result <- predict(pml_rf,pml_test)
	
### write answer
	n = length(pml_test_answer)
	for(i in 1:n){
		filename = paste0("problem_id_",i,".txt")
		write.table(pml_test_answer[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
	}

}
