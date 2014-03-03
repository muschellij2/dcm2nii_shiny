library(shinyIncubator)
shinyUI(pageWithSidebar(
  headerPanel("DICOM TO NIfTI Converter"),
  sidebarPanel(
    fileInput("files", "File data", multiple=TRUE),
    uiOutput("sld_window"),
    checkboxInput("gzipped", "gzip nii?", TRUE),
    textInput("niifname", "Filename of nii (no .nii on it)"),
    downloadButton('dlimg', 'Download NIfTI')
  ),
  mainPanel(
    progressInit(),
    plotOutput("ortho"),

    p("Code on ", 
      a("GitHub", "https://github.com/muschellij2/dcm2nii_shiny"))
  )
))