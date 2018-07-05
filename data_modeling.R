# Libraries to execute parallel programming
require( SparkR )
require( foreach )
require( parallel )
require( doParallel )

# Libraries to manipulate data
require( RCurl ) 
require( RODBC )
require( rjson )
require( reshape )
require( jsonlite ) 
require( reshape2 )
require( xlsxjars )

# Libraries to model data
require( XML )
require( xlsx )
require( plyr )
require( RSNNS )
require( dplyr )
require( gdata )
require( stringr )
require( stringi )
require( varhandle )
require( geosphere )
require( monmlp )

# Machine learning packages
require( tm )
require( fpc )
require( AUC )
require( ROCR )
require( caret )
require( RWeka )
require( e1071 )
require( mclust )
require( NbClust )
require( xgboost )
require( devtools )
require( rword2vec )
require( randomForest )

rankList <- list()

lista_xf <- list()
lista_mlp <- list()

score_xf <- list()
score_mlp <- list()

# # Train model function
# trainModel <- function ( dataframe ) {
  
  #rm(list=ls())
  suppressWarnings(library(jsonlite))
  suppressWarnings(library(data.table))
  suppressWarnings(library(RWeka))
  
  ######################################################
  ################# Main Function ######################
  ######################################################
  
  suppressWarnings(library(jsonlite))
  suppressWarnings(library(data.table))
  suppressWarnings(library(tm))
  
  set.seed(1)
  
  is.even <- function(x){ x %% 2 == 0 } 
  
  splitDF <- function ( dataset ) {
    
    ntrain <- floor( nrow( dataset )*0.7 )
    
    train <- dataset[ 1:( ntrain ), ]
    test <- dataset[ ( ntrain + 1 ):nrow( dataset ), ]
    
    list <- list()
    list[[1]] <- train
    list[[2]] <- test
    
    return( list )
    
  }
  
  trainMain <- function ( trainingSet ) {
    
    classes <- list()
    typeList <- list() 
    
    modelList <- list()
    scoreList <- list()
    
    modelListXF <- list()
    scoreListXF <- list()
    
    modelListMLP <- list()
    scoreListMLP <- list()
    
    supModel <- list()
    
    # Parallel configuration ('all cores of the machine')
    cores <- detectCores()
    cl <- makeCluster( cores[1] - 1 )
    registerDoParallel( cl )
    
    # Define 'split' function
    splitDF <- function ( dataset ) {
      
      dataset <- dataset[ sample.int( nrow( dataset ) ), ]
      ntrain <- floor( nrow( dataset )*0.7 )
      
      train <- dataset[ 1:( ntrain ), ]
      test <- dataset[ ( ntrain + 1 ):nrow( dataset ), ]
      
      list <- list()
      list[[1]] <- train
      list[[2]] <- test
      
      return( list )
      
    }
    
    # Create class which holds multiple results for each loop iteration.
    # Each loop iteration populates two properties: $result1 and $result2.
    # For a great tutorial on S3 classes, see: 
    # http://www.cyclismo.org/tutorial/R/s3Classes.html#creating-an-s3-class
    multiResultClass <- function( type=NULL, class=NULL, model=NULL, score=NULL, relev=NULL, ntrain=NULL, cmatrix=NULL )
    {
      
      me <- list(
        
        type = type,
        class = class,
        model = model,
        score = score,
        relev = relev,
        ntrain = ntrain,
        cmatrix = cmatrix
        
      )
      
      ## Set the name for the class
      class(me) <- append( class(me),"multiResultClass" )
      return(me)
      
    }
    
    # # Loop through all records
    # supModel <- foreach ( i = 1:8, .packages = c( "parallel", "doParallel", "foreach", "plyr", "dplyr", "reshape", "reshape2", 
    #                                               "geosphere", "varhandle", "stringr", "stringi", "tm", "fpc", "AUC", "ROCR",
    #                                               "caret", "RWeka", "e1071", "mclust", "NbClust", "xgboost", "devtools",
    #                                               "rword2vec", "randomForest", "RSNNS" ), .verbose = T ) %dopar% {
    
    for ( i in 1:3 ) {
    for ( j in 1:5 ) {                                                  
      
      # Define 'split' function
      splitDF <- function ( dataset ) {
        
        # dataset <- dataset[ sample.int( nrow( dataset ) ), ]
        ntrain <- floor( nrow( dataset )*0.7 )
        
        dataset$test <- FALSE
        dataset$test[ ( ( j-1 )*( nrow( dataset ) / 5 ) + 1 ):( ( j )*( nrow( dataset ) / 5 ) ) ] <- TRUE
        
        test <- subset( dataset, test == TRUE )
        train <- subset( dataset, test == FALSE )
        
        test$test <- NULL
        train$test <- NULL
        
        list <- list()
        list[[1]] <- train
        list[[2]] <- test
        
        return( list )
        
      }
      
      mainList <- splitDF( trainingSet[1:380,] )
      # train <- trainingSet[1:380,]
      train <- mainList[[1]]
      label_train <- train$label
      train$label <- NULL
      train <- cbind( train, label_train )
      names( train )[ ncol( train ) ] <- "label"
      train$label <- as.numeric( train$label )
      train <- subset( train, !is.na( label ) )
      
      test <- mainList[[2]]
      # test <- trainingSet[380:600,]
      # test <- subset( test, !is.na( label ) )
      
      if ( ( length( unique( train$label ) ) > 1 ) & ( length( unique( test$label ) ) > 1 ) ) {
        
        train[] <- lapply( train, as.numeric )
        test[] <- lapply( test, as.numeric )
        
        trainlabel <- train$label
        testlabel <- test$label
        
        train$label <- NULL
        test$label <- NULL
        
        # Remove attributes with unique values
        for ( j in 1:ncol( train ) ) {
          
          if ( ( length( unique( train[,j] ) ) )==1 ) {
            
            names( train )[j] <- paste( names( train )[j], "_REMOVE", sep="" )
            
          }
          
        }
        
        # Rebuild test dataframe
        test_new <- data.frame( test[,1] )
        names( test_new )[1] <- names( test )[1]
        
        for ( j in 1:ncol( test ) ) {
          
          if ( !is.na( match( names( test )[j], names( train ) ) ) ) {
            
            test_temp <- test[,j]
            test_new <- cbind( test_new, test_temp )
            
          }
          
        }
        
        test_new[,1] <- NULL
        test <- test_new
        names( test ) <- names( train )
        
        train <- train[ sort( names( train ) ) ]
        test <- test[ sort( names( test ) ) ]
        
        train <- cbind( train, trainlabel )
        test <- cbind( test, testlabel )
        
        names( train )[ ncol( train ) ] <- "label"
        names( test )[ ncol( test ) ] <- "label"
        
        train[] <- lapply( train, as.numeric )
        test[] <- lapply( test, as.numeric )
        
        label_train <- as.numeric( train$label[ 1:( floor( 0.8*nrow( train ) ) ) ] )
        label_valid <- as.numeric( train$label[ ( ( floor( 0.8*nrow( train ) ) + 1 ) ):( nrow( train ) ) ] )
        
        # validSet
        trainSet <- train[ 1:( floor( 0.8*nrow( train ) ) ), ]
        validSet <- train[ ( ( floor( 0.8*nrow( train ) ) + 1 ) ):( nrow( train ) ), ]
        
        unlabel_train <- trainSet$label
        unlabel_valid <- validSet$label
        trainSet$label <- NULL
        validSet$label <- NULL
        
        if ( ( nrow( trainSet ) > 10 ) & ( nrow( validSet ) > 2 ) & ( nrow( test ) > 2 ) ) {
          
          # Convert data frames to xgb.Dmatrices
          trainSet <- lapply( trainSet, function(x) { x[ is.nan(x) ] <- 0; return(x) } )
          trainSet <- lapply( trainSet, function(x) { x[ is.na(x) ] <- 0; return(x) } )
          x.tr <- as.matrix( data.frame( trainSet ) )
          dtrain <- xgb.DMatrix( x.tr, label = as.numeric( unlabel_train ) )
          dtrain <- na.omit( dtrain )
          
          validSet <- lapply( validSet, function(x) { x[ is.nan(x) ] <- 0; return(x) } )
          validSet <- lapply( validSet, function(x) { x[ is.na(x) ] <- 0; return(x) } )
          x.val <- as.matrix( data.frame( validSet ) )
          dvalid <- xgb.DMatrix( x.val, label = as.numeric( unlabel_valid ) )
          dvalid <- na.omit( dvalid )
          
          test <- lapply( test, function(x) { x[ is.nan(x) ] <- 0; return(x) } )
          test <- lapply( test, function(x) { x[ is.na(x) ] <- 0; return(x) } )
          x.sc <- as.matrix( data.frame( test ) )
          dscore <- xgb.DMatrix( x.sc )
          dscore <- na.omit( dscore )
          test <- data.frame( test )
          
          watchlist = list( valid = dvalid )
          
          bestSoFar = 10000000
          
          count = 0
          
          ss_vector <- c( 0.75, 0.85, 0.95, 1 )
          
          col_sample <- 0.80
          
          depth <- 10 #floor( runif( 1, min = 4, max = 10 ) )
          
          for ( k in 1:length( ss_vector ) ) {
            
            if ( i <= 4 ) {
              
              ss_sample <- ( 1 - 0.05*i )
              eta <- 0.2
              
            } else {
              
              ss_sample <- ( 1 - 0.05*( i - 4 ) )
              eta <- 0.8
              
            }
            
            for ( max_depth in seq( from=10, to=10, by=10 ) ) {
              
              for ( eta in seq( from=eta, to=eta, by=eta ) ) {
                
                count = count + 1
                
                
                if ( i == 1 ) { 
                  
                  col_sample <- 1
                  max_depth <- 8
                  eta <- 1.2
                  
                }  
                
                if ( i == 2 ) { 
                  
                  col_sample <- 1
                  max_depth <- 4
                  eta <- 0.1
                  
                }  
                
                if ( i == 3 ) { 
                  
                  col_sample <- 0.8
                  max_depth <- 8
                  eta <- 1.6
                  
                }
                
                ###Set XGBoost parameters
                xgb.params <- list( booster = "gbtree", objective = "reg:linear",
                                    max_depth = max_depth, eta = eta,
                                    colsample_bytree = col_sample, 
                                    subsample = 1 )
                
                objective <- "reg:linear"
                
                ##Train base learner
                xgbModel <- xgb.train( data = dtrain, label = as.numeric( unlabel_train ), params = xgb.params, eval_metric = 'rmse', maximize = F,
                                       watchlist = watchlist, nrounds = 200, early_stopping_rounds = max( 8/eta, 300 ), verbose = 1, print_every_n = 100 )
                
                print( nrow( data.frame( trainSet ) ) )
                
                # Test if model is applicable
                if ( nrow( data.frame( trainSet ) ) < 10 ) {

                  xgbImportant <- data.frame( attr=character(1),
                                              rate=numeric(1),
                                              stringsAsFactors=FALSE )

                  xgbImportant$rate <- 1

                } else {

                  # Obtain important features
                  xgbImportant <- xgb.importance( colnames( data.frame( trainSet ) ), model = xgbModel )
                  names( xgbImportant )[1] <- "attr"
                  names( xgbImportant )[2] <- "rate"
                  xgbImportant$Cover <- NULL
                  xgbImportant$Frequence <- NULL

                }
                
                # Normalize accuracy
                xgbImportant$rate <- xgbImportant$rate / sum( xgbImportant$rate ) 
                
                # Obtain applied features
                xgbFeatures <- colnames( data.frame( trainSet ) )
                
                if ( xgbModel$best_score < bestSoFar ) {
                  
                  modelXF <- xgbModel
                  relevXF <- xgbImportant
                  xfFeatures <- xgbFeatures
                  
                  # Normalize accuracy
                  relevXF$rate <- relevXF$rate / sum( relevXF$rate )
                  
                }
                
              } 
              
            }  
            
          }
          
          # Create neural network model
          trainSet <- data.frame( trainSet )
          validSet <- data.frame( validSet )
          trainSet <- cbind( trainSet, unlabel_train )
          validSet <- cbind( validSet, unlabel_valid )
          
          names( trainSet )[ ncol( trainSet ) ] <- "label"
          names( validSet )[ ncol( validSet ) ] <- "label"
          
          # Prepare NA/NaN/Inf values
          trainSet <- rbind( trainSet, validSet )
          trainSet <- apply( trainSet, 2, function(x) ifelse( is.finite(x), x, 1 ) )
          trainSet <- apply( trainSet, 2, function(x) ifelse( is.nan(x), 1, x ) )
          trainSet <- data.frame( trainSet )
          
          train <- trainSet
          unlabel_train <- trainSet$label
          train$label <- NULL
          
          if ( i == 1 ) { 
            
            n.ensemble <- 20
            monotone <- 0
            hidden1 <- 6
            
          }
          
          if ( i == 2 ) { 
            
            n.ensemble <- 10
            monotone <- 0
            hidden1 <- 7
            
          }
          
          if ( i == 3 ) { 
            
            n.ensemble <- 10
            monotone <- 0
            hidden1 <- 10
            
          }
          
          # # Obtain neural network model
          # modelMLP <- mlp( train, unlabel_train, size = c(5), maxit = 100,
          #                  initFunc = "Randomize_Weights", initFuncParams = c(-0.1*index_mlp, 0.1*index_mlp),
          #                  learnFunc = learnFunc, learnFuncParams = c(0.2, 0),
          #                  updateFunc = "Topological_Order", updateFuncParams = c(0),
          #                  hiddenActFunc = "Act_Logistic", shufflePatterns = TRUE )
          
          w <- monmlp.fit( x = as.matrix( train ), y = as.matrix( unlabel_train ), hidden1 = hidden1, monotone = monotone,
                           n.ensemble = n.ensemble, bag = TRUE, iter.max = 500,
                           control = list( trace = 0 ) )
          
          unlabel_test <- test$label
          test$label <- NULL
          
          #### XGBOOST ####
          score <- vector( mode="numeric", length=5 )
          
          # Compute real
          real <- as.numeric( unlabel_test )
          
          # Compute xgBoost prediction
          pred_xf <- predict( modelXF, dscore )
          pred_xf <- as.numeric( pred_xf )
          
          # Compute MLP prediction
          pred_mlp <- monmlp.predict( x = as.matrix( test ), weights = w )
          pred_mlp <- as.numeric( pred_mlp )
          
          # Compute xgBoost score
          rank_xf <- ( ( as.numeric( pred_xf ) - as.numeric( real ) )^2 )
          score_xf[[j]] <- ( sum( rank_xf, na.rm = TRUE ) )^(0.5) / length( real ) # mean( rank, na.rm = TRUE )

          # Compute MLP score
          rank_mlp <- ( ( as.numeric( pred_mlp ) - as.numeric( real ) )^2 )
          score_mlp[[j]] <- ( sum( rank_mlp, na.rm = TRUE ) )^(0.5) / length( real ) # mean( rank, na.rm = TRUE )
                   
          # xgBoost 
          print( paste0( "###############  ", as.character( score_xf[[j]] ) ) )
          col_sample_vec <- rep( col_sample, length( pred_xf ) )
          max_depth_vec <- rep( max_depth, length( pred_xf ) )
          eta_vec <- rep( eta, length( pred_xf ) )
          
          rank_table_xf <- data.frame( cbind( as.numeric( pred_xf ), as.numeric( real ), col_sample, max_depth_vec, eta_vec ) )
          names( rank_table_xf )[1] <- "pred"
          names( rank_table_xf )[2] <- "real"
          names( rank_table_xf )[3] <- "col_sample"
          names( rank_table_xf )[4] <- "max_depth"
          names( rank_table_xf )[5] <- "eta"
          
          lista_xf[[i]] <- rank_table_xf
          
          # MLP
          print( paste0( "###############  ", as.character( score_mlp[[j]] ) ) )
          n.ensemble_vec <- rep( n.ensemble, length( pred_mlp ) )
          monotone_vec <- rep( monotone, length( pred_mlp ) )
          hidden1_vec <- rep( hidden1, length( pred_mlp ) )
          
          rank_table_mlp <- data.frame( cbind( as.numeric( pred_mlp ), as.numeric( real ), n.ensemble_vec, monotone_vec, hidden1_vec ) )
          names( rank_table_mlp )[1] <- "pred"
          names( rank_table_mlp )[2] <- "real"
          names( rank_table_mlp )[3] <- "n.ensemble"
          names( rank_table_mlp )[4] <- "monotone"
          names( rank_table_mlp )[5] <- "hidden1"
          
          lista_mlp[[i]] <- rank_table_mlp

          modelListXF[[i]] <- modelXF
          scoreListXF[[i]] <- score_xf[[j]]
          
          modelListMLP[[i]] <- w
          scoreListMLP[[i]] <- score_mlp[[j]]
          
          # Save more accurate model
          
          score_vec <- c( score_xf[[j]], score_mlp[[j]] )
          index_score <- which.max( score_vec )
          
          if ( index_score == 1 ) {
            
            typeList[[i]] <- "XGBoost"
            
            modelList[[i]] <- modelXF
            scoreList[[i]] <- score_xf
            
          }
          
          if ( index_score == 2 ) {
            
            typeList[[i]] <- "Neural Network"
            
            modelList[[i]] <- w
            scoreList[[i]] <- score_mlp
            
          }
          
          modelListXF[[i]] <- modelXF
          scoreListXF[[i]] <- score_xf
          
          modelListMLP[[i]] <- w
          scoreListMLP[[i]] <- score_mlp
          
        } else {
          
          typeList[[i]] <- ""
          modelList[[i]] <- 0
          scoreList[[i]] <- 0
          
          modelListXF[[i]] <- 0
          scoreListXF[[i]] <- 0
          
          modelListMLP[[i]] <- 0
          scoreListMLP[[i]] <- 0
          
        }
        
      } else {
        
        typeList[[i]] <- ""
        modelList[[i]] <- 0
        scoreList[[i]] <- 0
        
        modelListXF[[i]] <- 0
        scoreListXF[[i]] <- 0
        
        modelListMLP[[i]] <- 0
        scoreListMLP[[i]] <- 0
        
      }
      
      supModel_temp <- multiResultClass()
      
      supModel_temp$type  <- typeList[[i]]
      supModel_temp$model <- modelList[[i]]
      supModel_temp$score <- scoreList[[i]]
      
      supModel_temp$modelXF <- modelListXF[[i]]
      supModel_temp$scoreXF <- lista_xf[[i]]
      
      supModel_temp$modelMLP <- modelListMLP[[i]]
      supModel_temp$scoreMLP <- lista_mlp[[i]]
     
      supModel[[i]] <- supModel_temp 
       
      # return( supModel_temp )
      
    }
      
    }  
    
    # Stop cluster
    stopCluster(cl)
    
    return( supModel )
    
  }
  
  ######################################################
  ################## Reading Data ######################
  ######################################################
  
  options(java.parameters = "- Xmx1024m")
  
  gc()
  model <- list()
  
  label_vec <- c( "impedance_re", "impedance_img" )
  
  dataframe <- battery_cycle_total
  
  dataframe <- subset( dataframe, !is.na( impedance_re ) )
  dataframe <- subset( dataframe, !is.na( impedance_img ) )
  
  dataframe <- subset( dataframe, as.numeric( impedance_re ) <= 1 )
  dataframe <- subset( dataframe, as.numeric( impedance_re ) >= -1 )
  
  dataframe <- subset( dataframe, as.numeric( impedance_img ) <= 1 )
  dataframe <- subset( dataframe, as.numeric( impedance_img ) >= -1 )
  
  dataframe$label <- ( as.numeric(dataframe$impedance_re)^2 + as.numeric(dataframe$impedance_img)^2 )^(1/2)
  
  for ( h in 1:length( label_vec ) ) {
    
    label <- label_vec[h]
    
    for ( i in 1:ncol( dataframe ) ) {
      
      if ( names( dataframe )[i] == label_vec[h] ) {
        
        labels <- dataframe[ , i ]
        
      }
      
    }
    
    dataframe$cycle <- NULL
    dataframe$impedance_re <- NULL
    dataframe$impedance_img <- NULL
    
    dataframe <- cbind( dataframe, labels )
    names( dataframe )[ ncol( dataframe ) ] <- "label"
    
    # Parallel configuration ('all cores of the machine')
    cores <- detectCores()
    cl <- makeCluster( cores[1] - 1 )
    registerDoParallel( cl )
    
    # Remove special characters
    names( dataframe ) <- gsub( " ", "", names( dataframe ) )
    names( dataframe ) <- gsub( "[-]", "", names( dataframe ) )
    names( dataframe ) <- gsub( "[+]", "", names( dataframe ) )
    names( dataframe ) <- gsub( "[=]", "", names( dataframe ) )
    names( dataframe ) <- gsub( "[/]", "", names( dataframe ) )
    names( dataframe ) <- gsub( "[~]", "", names( dataframe ) )
    names( dataframe ) <- gsub( "[!]", "", names( dataframe ) )
    names( dataframe ) <- gsub( "[?]", "", names( dataframe ) )
    names( dataframe ) <- gsub( "[%]", "", names( dataframe ) )
    names( dataframe ) <- gsub( "[&]", "", names( dataframe ) )
    names( dataframe ) <- gsub( "[$]", "", names( dataframe ) )
    names( dataframe ) <- gsub( "[%]", "", names( dataframe ) )
    names( dataframe ) <- gsub( "[@]", "", names( dataframe ) )
    names( dataframe ) <- gsub( "[#]", "", names( dataframe ) )  
    names( dataframe ) <- gsub( "[']", "", names( dataframe ) ) 
    names( dataframe ) <- gsub( '["]', "", names( dataframe ) ) 
    names( dataframe ) <- gsub( '[<]', "", names( dataframe ) ) 
    names( dataframe ) <- gsub( '[>]', "", names( dataframe ) ) 
    names( dataframe ) <- gsub( '[,]', "", names( dataframe ) ) 
    names( dataframe ) <- gsub( '[;]', "", names( dataframe ) ) 
    names( dataframe ) <- gsub( '[:]', "", names( dataframe ) ) 
    names( dataframe ) <- gsub( '[)]', "", names( dataframe ) ) 
    names( dataframe ) <- gsub( '[(]', "", names( dataframe ) ) 
    names( dataframe ) <- gsub( '\\.', "", names( dataframe ) ) 
    
    dataframe <- dataframe[ , 2:ncol( dataframe ) ]
    dataframe[] <- lapply( dataframe, as.character )
    dataframe[] <- lapply( dataframe, as.numeric )
    
    dataframe <- subset( dataframe, !is.na( label ) )
    dataframe_temp <- dataframe
    
    if ( nrow( dataframe_temp ) > 10 ) {
      
      # Train model
      trainingSet <- data.frame( dataframe_temp )
      trainingSet$label <- as.numeric( trainingSet$label )
      
      #try({
      
      mainModel <- trainMain( trainingSet )
      
      if ( label_vec[h] == "impedance_re" ) {
        
        model$impedance_re <- mainModel
        
      } else {
        
        model$impedance_img <- mainModel
        
      }
      
    } else {
      
      typeList <- list()
      
      modelList <- list()
      scoreList <- list()
      
      modelListXF <- list()
      scoreListXF <- list()
      
      modelListMLP <- list()
      scoreListMLP <- list()
      
      for ( j in 1:10 ) {
        
        classList[[j]] <- c( "class" )
        
        typeList[[j]] <- ""
        
        modelList[[j]] <- 0
        scoreList[[j]] <- 0
        
        modelListXF[[j]] <- 0
        scoreListXF[[j]] <- 0
        
        modelListMLP[[j]] <- 0
        scoreListMLP[[j]] <- 0
        
      }
      
      mainModel <- list()
      
      mainModel$type <- typeList
      
      mainModel$model <- modelList
      mainModel$score <- scoreList
      
      mainModel$modelXF <- modelListXF
      mainModel$scoreXF <- scoreListXF
      
      mainModel$modelMLP <- modelListMLP
      mainModel$scoreMLP <- scoreListMLP
      
      if ( label_vec[h] == "impedance_re" ) {
        
        model$impedance_re <- mainModel
        
      } else {
        
        model$impedance_img <- mainModel
        
      }
      
    }
    
  }
  
  #save( model, file=paste( directory_model, "/train_", as.character( unique( dataframe$typemajor )[i] ), ".RData", sep="" ) )
  save( model, file=paste( "C:/Users/tdunk/Desktop/master/train.RData", sep="" ) )
  
# }