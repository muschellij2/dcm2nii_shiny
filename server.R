library(oro.dicom)
library(oro.nifti)
options(shiny.maxRequestSize = -1)
shinyServer(function(input, output) {
  # output$filetable <- renderTable({
  makenii = reactive({
    rimg = c(0, 100)    
    nii = NULL
      if (!is.null(input$files)) {
        # User has not uploaded a file yet
     
        filenames = input$files
        print(input$files)
        filenames = file.path(filenames$datapath)
        print(filenames)

        nfiles <- length(filenames)
        nch <- nchar(as.character(nfiles))
        headers <- images <- vector("list", nfiles)
        names(images) <- names(headers) <- filenames
            cat(" ", nfiles, "files to be processed by readDICOM()", 
                fill = TRUE)
            tpb <- txtProgressBar(min = 0, max = nfiles, style = 3)
        for (i in 1:nfiles) {
            setTxtProgressBar(tpb, i)
            dcm <- rereadDICOMFile(filenames[i])
            images[[i]] <- dcm$img
            headers[[i]] <- dcm$hdr
        }
       dcm = list(hdr = headers, img = images)

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

      nii = dicom2nifti(dcm, rescale=FALSE, reslice=FALSE, 
      descrip = NULL)
      rimg = range(nii, na.rm=TRUE)
      	"Finished converting"
    }
      return(list(nii=nii, rimg=rimg))
  })



	# Partial example
	output$sld_window <- renderUI({
    rimg = makenii()$rimg

		sliderInput("window", "Range of Data:",
                min = rimg[1], max = rimg[2], value = rimg)
	})  
	

	output$ortho = renderPlot({
    window = input$window
    nii = makenii()$nii
    # print(window)
    # print(nii)
    if (!is.null(nii)){
      img = nii
      # print("img is ")
      # print(img)
      img@cal_min = window[1]
      img@cal_max = window[2]

      img[ img < window[1] ] = window[1]
      img[ img >= window[2] ] = window[2]    
  		orthographic(img)
    }
	})

  output$dlimg <- downloadHandler(
    filename = function() {
      stopifnot(input$niifname!="")
      print(input$niifname)
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