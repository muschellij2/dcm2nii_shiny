library(oro.dicom)
library(oro.nifti)
library(shiny)
library(shinyapps)
library(shinyIncubator)
### make shiny max request size 30MB
options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output, session) {
  # output$filetable <- renderTable({
  makenii = reactive({
    rimg = c(0, 100)    
    nii = NULL
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
          rm(list="dd")
          # message(head(dcm$hdr[[1]]))
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
        x = gc()
        df = data.frame(x)
        message(df)
        message("ran gc")
        nii = dicom2nifti(dcm, rescale=FALSE, 
          reslice=FALSE, sequence= TRUE,
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
                min = rimg[1], max = rimg[2], 
                value = c(max(0, rimg[1]), min(100, rimg[2])))
	})  
	

	output$ortho = renderPlot({
    window = input$window
    nii = makenii()$nii
    # print(window)
    # print(nii)
    if (!is.null(nii)){
      if (input$viewimg) {

        message('plot')
        message(paste0("img is ", class(nii)))
        print(nii)
        nii@cal_min = window[1]
        nii@cal_max = window[2]

        nii[ nii < window[1] ] = window[1]
        nii[ nii >= window[2] ] = window[2]    
          orthographic(nii)
      } else {
        plot(0, 0, xaxt="n", yaxt="n", pch="", xlab="", ylab="")
        text(0, 0, "Click view img if you want to see a display")
      }
    } else {
        plot(0, 0, xaxt="n", yaxt="n", pch="", xlab="", ylab="")
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