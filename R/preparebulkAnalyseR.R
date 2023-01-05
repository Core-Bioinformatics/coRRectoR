#' Generate the landing page panel of the shiny app
#' @description These are the UI and server components of the landing page
#' panel of the shiny app. It is a modified version of the bulkAnalyseR landing page.
#' @return The UI and Server components of the shiny module, that can be used
#' within the UI and Server definitions of a shiny app.
#' @name landingPanel
NULL

#' @rdname landingPanelcoRRectoR
#' @export
landingPanelcoRRectoRUI <- function(id, show = TRUE){
  ns <- NS(id)
  
  if(show){
    tabPanel(
      'Home',
      icon = icon("home"),
      fluidRow(
        column(6, 
               shinyLP::jumbotron(
                 "Welcome to bulkAnalyseR!", 
                 "An accessible, interactive pipeline for analysing and sharing bulk sequencing results",
                 button = FALSE
               )),
        column(6,
               shinyLP::jumbotron(
                 "Welcome to coRRectoR!", 
                 "A co-variation based, Robust and Reproducible method for correcting batch effects using a functional perspective",
                 button = FALSE
               ))),
      fluidRow(
        column(6, 
               shinyLP::panel_div(
                 class_type = "danger", 
                 panel_title = "Tutorial",
                 content = HTML(
                   "Vignette describing the different panels in this app and how to use them:
          <br>
          <a href='https://core-bioinformatics.github.io/bulkAnalyseR/articles/bulkAnalyseR.html'>
              https://core-bioinformatics.github.io/bulkAnalyseR/articles/bulkAnalyseR.html
          </a>"
                 )
               )),
        column(6, shinyLP::panel_div(
          class_type = "primary", 
          panel_title = "GitHub",
          content = HTML(
            "Latest stable version, bug reports and feature requests:
          <br>
          <a href='https://github.com/Core-Bioinformatics/coRRectoR'>
              https://github.com/Core-Bioinformatics/coRRectoR
          </a>")
        ))),
      fluidRow(
        column(6, shinyLP::panel_div(
          class_type = "primary", 
          panel_title = "GitHub",
          content = HTML(
            "Latest stable version, bug reports and feature requests:
          <br>
          <a href='https://github.com/Core-Bioinformatics/bulkAnalyseR'>
              https://github.com/Core-Bioinformatics/bulkAnalyseR
          </a>")
        )),
        column(6, shinyLP::panel_div(
          class_type = "warning", 
          panel_title = "Created by",
          content = HTML(
            "Cameron Crawford, Miguel Larraz, Eleanor Williams and Irina Mohorianu"
          )
        ))
      ),
      fluidRow(
        column(6, shinyLP::panel_div(
          class_type = "success", 
          panel_title = "Manuscript",
          content = HTML(
            "For a more detailed overview of the app functionality:
          <br>
          <a href='https://www.biorxiv.org/content/10.1101/2021.12.23.473982v1'>
              https://www.biorxiv.org/content/10.1101/2021.12.23.473982v1
          </a>"
          )
        ))),
      fluidRow(
        column(6, shinyLP::panel_div(
          class_type = "info", 
          panel_title = "Our website",
          content = HTML(
            "Wellcome MRC - Cambridge Stem Cell Institute Core Bioinformatics Group:
          <br>
          <a href='https://www.corebioinf.stemcells.cam.ac.uk/bulkAnalyseR'>
              https://www.corebioinf.stemcells.cam.ac.uk/bulkAnalyseR
          </a>")
        ))
      ),
      fluidRow(
        column(6, shinyLP::panel_div(
          class_type = "warning", 
          panel_title = "Created by",
          content = HTML(
            "Ilias Moutsopoulos, Eleanor Williams, and Irina Mohorianu"
          )
        ))
      )
      
    ) 
  }else{
    NULL
  }
}

#' @rdname landingPanelcoRRectoR
#' @export
landingPanelcoRRectoRServer <- function(id){
  ns <- NS(id)
  
  moduleServer(id, function(input, output, session){
    
  })
}

#' Generate all files required for an autonomous shiny app
#' @description This function takes a corrected expression matrix and creates a minimal
#' bulkAnalyseR app comparing this to the uncorrected expression matrix and a full
#' bulkAnalyseR app on the corrected expression matrix. 
#' @param corrected Expression matrix as output by coRRectoR
#' @param metadata Either one metadata dataframe (combined across batches)
#' or multiple metadata dataframes in a list
#' @param uncorrected An expression matrix or multiple expression matrices for each batch
#' in a list (default:NULL)
#' @param add.batch.column Whether a batch column should be added to the metadata. 
#' This is only possible if metadata is included as a list of separate dataframes 
#' (default:TRUE).
#' @param batch.names Names for batches if add.batch.column==TRUE. If not set, 
#' then the batches will be named 1,2,3,...
#' @param create.comparison.app Whether the bulkAnalyseR app comparing corrected 
#' and uncorrected outputs should be created. This is only possible if uncorrected!=NULL.
#' @param comparison.app.dir Directory where comparison app should be saved if it is created (default: 'comparisonApp').
#' @param comparison.app.title Title for comparison app (default: 'Comparison between uncorrected and corrected outputs')
#' @param corrected.app.dir Directory where corrected app should be saved if it is created (default: 'correctedApp').
#' @param corrected.app.title Title for corrected app (default: 'Batch corrected bulkAnalyseR app')
#' @param organism organism name to be passed on to gprofiler2::gost in corrected app; organism names are constructed by concatenating the first letter of the name and the family name; default is NA - enrichment is not included to ensure compatibility with datasets that have non-standard gene names.
#' @param org.db= Database for annotations to transform ENSEMBL IDs to gene names in both apps; a list of bioconductor packaged databases can be found with BiocManager::available("^org\."); default in NA, in which case the row names of the expression matrix are used directly - it is recommended to provide ENSEMBL IDs if the database for your model organism is available.
#' @param modality Name of the modality for corrected app
#' @param theme Shiny theme to be used in the app (default: 'flatly')
#' @param panels.default Argument to control which of the default panels will be included in the corrected app; default is all, but the enrichment panel will not appear unless organism is also supplied; note that the 'DE' panel is required for 'DEplot', 'DEsummary', 'Enrichment', and 'GRNenrichment'.
#' @export
#' @import shiny
#' @import ggplot2
#' @examples
#' # Defining corrected and uncorreced expression matrices and metadata
#' corrected = matrix(sample(1:100,70),ncol = 7)
#' colnames(corrected)=LETTERS[1:7]
#' uncorrected = matrix(sample(1:100,70),ncol = 7)
#' colnames(uncorrected)=LETTERS[1:7]
#'
#' # Run preparebulkAnalyseR with combined uncorrected expression matrix 
#' # and split metadata
#' meta = list('batch1'=data.frame('id'=LETTERS[1:3],
#'                                 'batch'=rep('old',3),
#'                                 'condition'=c('A','A','B')),
#'             'batch2'=data.frame('id'=LETTERS[4:7],
#'                                 'batch'=rep('new',4),
#'                                   'condition'=c('B','C','B','D')))
#' preparebulkAnalyseR(corrected,
#'                     meta,
#'                     uncorrected,
#'                     comparison.app.dir = paste0(tempdir(), "/comparisonShinyApp"),
#'                     corrected.app.dir = paste0(tempdir(), "/correctedShinyApp"))
#' # clean up tempdir
#' unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE)
#' 
#' Run preparebulkAnalyseR with split uncorrected expression matrix 
#' meta = list('batch1'=data.frame('id'=LETTERS[1:3],
#'                                 'condition'=c('A','A','B')),
#'             'batch2'=data.frame('id'=LETTERS[4:7],
#'                                 'condition'=c('B','C','B','D')))
#' uncorrected.list = list('batch1'=uncorrected[,1:3],
#'                         'batch2'=uncorrected[,4:7])
#' preparebulkAnalyseR(corrected,
#'                     meta,
#'                     uncorrected.list,
#'                     batch.names=c('old','new'),
#'                     create.comparison.app = FALSE,
#'                     create.corrected.app = FALSE)
#'                     
#' # Run preparebulkAnalyseR with no uncorrected expression matrix and
#' # combined metadata table
#' preparebulkAnalyseR(corrected,
#'                     rbind(meta[[1]],meta[[2]]),
#'                     batch.names=c('old','new'),
#'                     create.comparison.app = FALSE,
#'                     create.corrected.app = FALSE)
preparebulkAnalyseR <- function(corrected.output,
                                metadata,
                                uncorrected=NULL,
                                add.batch.column=TRUE,
                                batch.names=NULL,
                                create.comparison.app=TRUE,
                                create.corrected.app=TRUE,
                                comparison.app.dir='comparisonApp',
                                comparison.app.title='Comparison between uncorrected and corrected outputs',
                                corrected.app.dir='correctedApp',
                                corrected.app.title='Batch corrected bulkAnalyseR app',
                                organism=NA, 
                                org.db=NA,
                                modality='RNA',
                                theme="flatly",
                                panels.default=c("Landing", "SampleSelect", "QC", "GRN", "DE", "DEplot", "DEsummary","Enrichment", "GRNenrichment", "Cross", "Patterns")
                                ){
  
  # make combined metadata data.frame
  if (is.data.frame(metadata)){
    overall.metadata <- metadata
  } else if (is.list(metadata)){
      if (is.null(batch.names)){batch.names <- 1:length(metadata)}
    
      #check all the IDs are unique
      ids <- lapply(metadata, "[", 1)
      if (any(duplicated(unlist(ids)))){
        stop("Sample IDs must be unique across all batches. Please rename so each ID is only used once.")
      }
      
      # check metadata columns are the same across batches
      check.columns <- function(df1){
        return(all(colnames(df1)==colnames(metadata[[1]])))
      }
      if (!all(unlist(lapply(metadata,check.columns)))){
        stop("Metadata columns must match across batches. Please check and alter the metadata tables accordingly.")
      }
      
      for (i in 1:length(metadata)){
        rownames(metadata[[i]]) <- metadata[[i]][,1]
      }
      
      add.batch = !('batch' %in% colnames(metadata[[1]]))
      for (i in 1:length(metadata)){
        # add batch column to metadata if add.batch.column and there is no current batch column
        if (add.batch.column & add.batch){
          # add the batch as the second column
          metadata[[i]] <- cbind(metadata[[i]][,1, drop=FALSE],
                                 'batch'=rep(batch.names[i],
                                             nrow(metadata[[i]])),
                                 metadata[[i]][,2:ncol(metadata[[i]]), drop=FALSE])
        }
      }
      
      # put all metadata components together
      overall.metadata = do.call(rbind,metadata)
      rownames(overall.metadata)<-overall.metadata[,1]
      if (!all(overall.metadata$id%in%colnames(corrected.output))){
        stop("Samples in metadata do not match samples in corrected expression matrix. Please check and alter the metadata tables accordingly.")
      }
      # reorder metadata if needed to match expression matrix
      overall.metadata = overall.metadata[colnames(corrected.output),]
  } else {
    stop("Structure of metadata is not recognised. Please provide 1 overall metadata dataframes or a list of separate metadata dataframes per batch")
  }
  
  # create comparison app
  if (create.comparison.app){
    if (is.null(uncorrected)){
      stop("To create the comparison app, you must supply the uncorrected expression matrix. Please either supply this or alter the create.comparison.app parameter. You may supply 1 overall expression matrix or a list of separate expression matrices per batch.")
    }
    if (is.matrix(uncorrected)){
    } 
    else if (is.data.frame(uncorrected)){
      uncorrected = as.matrix(uncorrected)
    } else if (is.list(uncorrected)){
      
      uncorrected.colnames=c()
        for (i in 1:length(uncorrected)){
          uncorrected.colnames = c(uncorrected.colnames,colnames(uncorrected[[i]]))
        }
        uncorrected = do.call(cbind,uncorrected)
        colnames(uncorrected)=uncorrected.colnames
        uncorrected = as.matrix(uncorrected)
    } else {
      stop("Structure of uncorrected is not recognised. Please provide 1 overall expression matrix or a list of separate expression matrices per batch")
    }
    if (!(all(colnames(uncorrected)==colnames(corrected.output)))){
      stop("Columns in corrected and uncorrected matrices do not match. Please check and alter accordingly.")
    }
    
    # create app with 2 sets of tabs
    bulkAnalyseR::generateShinyApp(shiny.dir = comparison.app.dir,
                                   app.title = comparison.app.title,
                                   modality = c('Uncorrected','Batch corrected'),
                                   expression.matrix = list(uncorrected,corrected.output),
                                   metadata = list(overall.metadata,overall.metadata),
                                   organism = list(organism,organism),
                                   org.db = list(org.db,org.db),
                                   theme=theme,panels.default = c('QC','DE','Enrichment'))

    # add home panel
    tx  <- readLines(paste0(comparison.app.dir,'/app.R'))
    tx2  <- gsub(pattern = "footer",replace = "tabPanel(title='Home',icon = icon('home'),landingPanelcoRRectoRUI(id='LandingPanel')), footer", x = tx)
    writeLines(tx2, con=paste0(comparison.app.dir,'/app.R'))
  }
  
  if (create.corrected.app){
    my.app <- bulkAnalyseR::generateShinyApp(shiny.dir = corrected.app.dir,
                                             app.title = corrected.app.title,
                                             modality = modality,
                                             expression.matrix = corrected.output,
                                             metadata = overall.metadata,
                                             organism = organism,
                                             org.db = org.db,
                                             theme=theme,panels.default)
    invisible(my.app)
  } else {
    return(list('corrected'=corrected,'metadata'=overall.metadata))
  }
}
