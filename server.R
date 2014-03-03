library(oro.dicom)
library(oro.nifti)
library(shiny)
library(shinyapps)
library(shinyIncubator)
### make shiny max request size 30MB
options(shiny.maxRequestSize=30*1024^2)
nifti2 = function (img = array(0, dim = rep(1, 4)), dim, datatype = 2, 
    cal.min = NULL, cal.max = NULL, pixdim = NULL, ...) 
{
    if (missing(dim)) {
        if (is.array(img)) {
            dim <- base::dim(img)
        }
        else {
            dim <- c(1, length(img))
        }
    }
    message("nifti1")
    ld <- length(dim)
    x <- rep(1, 8)
    x[1] <- length(dim)
    y <- rep(0, 8)
    for (i in 2:length(x)) {
        x[i] <- ifelse(is.na(dim(img)[i - 1]), 1, dim(img)[i - 
            1])
        y[i] <- ifelse(is.na(dim(img)[i - 1]) || is.null(pixdim), 
            1, pixdim[i])
    }
    message("nifti2")
    if (is.null(cal.max)) {
        cal.max <- as.numeric(max(img, na.rm = TRUE))
    }
    if (is.null(cal.min)) {
        cal.min <- as.numeric(min(img, na.rm = TRUE))
    }
    message("nifti3")    
    switch(as.character(datatype), `2` = bitpix <- 8, `4` = bitpix <- 16, 
        `8` = bitpix <- 32, `16` = bitpix <- 32, `64` = bitpix <- 64, 
        `512` = bitpix <- 16, stop(paste("Data type", datatype, 
            "unsupported.")))
    message("nifti4")
    niftiClass <- "nifti"
    if (getOption("niftiAuditTrail")) {
        niftiClass <- "niftiAuditTrail"
    }
    message("nifti5")
    message(paste0("dim is", dim))
    message(paste0("pixdim ", y))
    message(paste0("niftiClass ", niftiClass))
    message(paste0("headimg ", head(img)))

    obj <- new(niftiClass, .Data = array(img, dim = dim), dim_ = x, 
        pixdim = y, cal_max = cal.max, cal_min = cal.min, datatype = datatype, 
        bitpix = bitpix, ...)
    message("nifti6")
    if (getOption("niftiAuditTrail")) {
        audit.trail(obj) <- niftiAuditTrailCreated(call = match.call())
    }
    message("nifti7")    
    validNIfTI <- getValidity(getClassDef("nifti"))
    validNIfTI(obj)
    message("nifti8")    
    return(obj)
}


dicom2nifti2 = function (dcm, datatype = 4, units = c("mm", "sec"), rescale = FALSE, 
    reslice = TRUE, qform = TRUE, sform = TRUE, DIM = 3, descrip = "SeriesDescription", 
    aux.file = NULL, ...) 
{
    message('m0')
    if (DIM == 2){
    # switch(as.character(DIM), "2" = {
        dcmList <- list(hdr = list(dcm$hdr), img = list(dcm$img))
        img <- create3D(dcmList, ...)
    # }, "3" = {
    } else if (DIM == 3){
      message("in create3D")
        img <- create3D(dcm, ...)
    # }, "4" = {
    } else if (DIM == 4){
        img <- create4D(dcm, ...)
    } else {
      message("dim wrong")
      stop("Dimension parameter 'DIM' incorrectly specified.")
    }
    message('m1')
    if (DIM %in% 3:4 && reslice) {
        img <- swapDimension(img, dcm)
    }
    message('m2')
    nim <- nifti2(img, datatype = datatype)
    message('m3')
    if (is.null(attr(img, "pixdim"))) {
        pixelSpacing <- extractHeader(dcm$hdr, "PixelSpacing", 
            FALSE)
        nim@pixdim[2:3] <- header2matrix(pixelSpacing, 2)[1, 
            ]
        nim@pixdim[4] <- ifelse(nim@dim_[1] > 2, extractHeader(dcm$hdr, 
            "SliceThickness")[1], 1)
    } else {
        nim@pixdim[2:4] <- attr(img, "pixdim")
    }
    message('m4')

    if (!is.null(descrip)) {
        for (i in 1:length(descrip)) {
            if (i == 1) {
                descrip.string <- extractHeader(dcm$hdr, descrip[i], 
                  FALSE)[1]
            } else {
                descrip.string <- paste(descrip.string, extractHeader(dcm$hdr, 
                  descrip[i], FALSE)[1], sep = "; ")
            }
        }
        if (nchar(descrip.string) > 80) {
            warning("Description is greater than 80 characters and will be truncated")
        }
        nim@descrip <- descrip.string
    }
    message('m5')   
    if (!is.null(aux.file)) {
        if (nchar(descrip.string) > 24) {
            warning("aux_file is greater than 24 characters and will be truncated")
        }
        nim@aux_file <- aux.file
    }
    message('m6')        
    if (length(units) == 2) {
        nim@xyzt_units <- oro.nifti::space.time2xyzt(units[1], 
            units[2])
    } else {
        stop("units must be a length = 2 vector")
    }
    message('m7')
    if (qform) {
        nim@qform_code <- 2
        nim@quatern_b <- 0
        nim@quatern_c <- 1
        nim@quatern_d <- 0
        nim@pixdim[1] <- -1
    }
    message('m8')
    if (sform) {
        nim@sform_code <- 2
        nim@srow_x <- pixdim(nim)[2] * c(-1, 0, 0, 0)
        nim@srow_y <- pixdim(nim)[3] * c(0, 1, 0, 0)
        nim@srow_z <- pixdim(nim)[4] * c(0, 0, 1, 0)
    }
    message('m9')
    if (rescale) {
        nim@scl_slope <- extractHeader(dcm$hdr, "RescaleSlope")[1]
        nim@scl_inter <- extractHeader(dcm$hdr, "RescaleIntercept")[1]
    }
    return(nim)
}


shinyServer(function(input, output, session) {
  # output$filetable <- renderTable({
  makenii = reactive({
    rimg = c(0, 100)    
    nii = NULL
    message(sessionInfo())
    if (!is.null(input$files)) {
        # User has not uploaded a file yet
     
        filenames = input$files
        # print(input$files)
        filenames = file.path(filenames$datapath)
        # print(filenames)

        nfiles <- length(filenames)
        nch <- nchar(as.character(nfiles))
        headers <- images <- vector("list", nfiles)
        names(images) <- names(headers) <- filenames
            cat(" ", nfiles, "files to be processed by readDICOM()", 
                fill = TRUE)
                pb = txtProgressBar(min=0, max=1, style=3)
          withProgress(session, min=0, max=nfiles, {            
              setProgress(message="Reading Data")
          for (i in 1:nfiles) {
              setProgress(value =i)
              setTxtProgressBar(pb, i/nfiles)

              dd <<- readDICOMFile(filenames[i])
              images[[i]] <<- dd$img
              headers[[i]] <<- dd$hdr
          }
          })
          message(class(dd))
          dcm = list(hdr = headers, img = images)
          message(head(dcm$hdr[[1]]))
          # print(head(dcm$hdr))

        ### rescaling on my own
        dcmtable = dicomTable(dcm$hdr)
        keepcols = grepl("RescaleIntercept|RescaleSlope|PixelSpacing", 
                         colnames(dcmtable))
        dcmtab = dcmtable[, 
          c("0028-1052-RescaleIntercept", 
            "0028-1053-RescaleSlope", 
            "0028-0030-PixelSpacing"),
          drop=FALSE]
        stopifnot(ncol(dcmtab) == 3)
        colnames(dcmtab) = c("intercept", "slope", "pixelspacing")

        for (iimg in 1:length(dcm$img)){
          inter = as.numeric(dcmtab$intercept[iimg])
          slope = as.numeric(dcmtab$slope[iimg])
          dcm$img[[iimg]] = dcm$img[[iimg]] * slope + inter
          x = dcm$img[[iimg]]
          # if (verbose) print(range(dcm$img[[iimg]]))
          dcm$img[[iimg]][x < -1024] = -1024
          dcm$img[[iimg]][x > 3071] = 3071
      #http://www.medical.siemens.com/siemens/en_GLOBAL/rg_marcom_FBAs/files/brochures/DICOM/ct/DICOM_VA70C.pdf 
          # for 4095 ranges
        }

        ord = order(as.numeric(dcmtable$"0020-0013-InstanceNumber"))
        dcm$hdr = dcm$hdr[ord]
        dcm$img = dcm$img[ord]

        message("conversion")
        nii = dicom2nifti(dcm, rescale=FALSE, reslice=FALSE, 
        descrip = NULL)
        rimg = range(nii, na.rm=TRUE)
        message("Finished converting")
    }
      return(list(nii=nii, rimg=rimg))
  })



	# Partial example
	output$sld_window <- renderUI({
    rimg = makenii()$rimg
    message("slider")
		sliderInput("window", "Range of Data:",
                min = rimg[1], max = rimg[2], value = rimg)
	})  
	

	output$ortho = renderPlot({
    window = input$window
    nii = makenii()$nii
    # print(window)
    # print(nii)
    if (!is.null(nii)){
      message('plot')
      img = nii
      message(paste0("img is ", class(img)))
      print(img)
      img@cal_min = window[1]
      img@cal_max = window[2]

      img[ img < window[1] ] = window[1]
      img[ img >= window[2] ] = window[2]    
  		orthographic(img)
    } else {
        plot(0, 0, xaxt="n", pch="")
      text(0, 0, "Need to upload data")
    }
	})

  output$dlimg <- downloadHandler(
    filename = function() {
      stopifnot(input$niifname!="")
      warning(input$niifname)
      f = paste0(input$niifname, ".nii.gz")
      return(f)
    },
    content = function(file) {
      # print(file)
      writeNIfTI(makenii()$nii, filename = file, 
        gzipped= TRUE)
      file.rename(paste0(file, ".nii.gz"), file)
    }
  )
})