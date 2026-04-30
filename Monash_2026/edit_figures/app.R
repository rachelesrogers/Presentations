change_fill <- function(file_contents, new_fill = "#aaaaff") {
  stringr::str_replace_all(file_contents, "fill:#[0-f]{6};", sprintf("fill:%s;", new_fill))
}
figure_information <- read.csv("figure_information.csv")

library(dplyr)
library(magick)
library(rsvg)
library(shiny)
library(colourpicker)

fig_info <- figure_information

ui <- shiny::fluidPage(
  # shiny::titlePanel("Character Customization"),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::h4(shiny::strong("Character Customization")),
      shiny::selectInput("clothes_choice", "Select Outfit:",
                         choices= unique(fig_info[fig_info$Part=="clothes",]$Label)),
      shiny::conditionalPanel(condition="output.vis_head",
                              shiny::selectInput("head_choice", "Select Head:",
                                                 choices= unique(fig_info[fig_info$Part=="head",]$Label))),
      shiny::uiOutput('skinselect'),
      shiny::uiOutput('hairselect'),
      shiny::conditionalPanel(condition= "output.vis_hair2",
                              shiny::uiOutput('hair2select')),
      shiny::conditionalPanel(condition= "output.vis_hair3",
                              shiny::uiOutput('hair3select')),
      shiny::conditionalPanel(condition= "output.vis_eye",
                              shiny::uiOutput('eyeselect')),
      shiny::conditionalPanel(condition= "output.vis_glasses",
                              shiny::uiOutput('glassesselect')),
      shiny::conditionalPanel(condition= "output.vis_shirt",
                              shiny::uiOutput('shirtselect')),
      shiny::conditionalPanel(condition= "output.vis_pants",
                              shiny::uiOutput('pantsselect')),
      shiny::conditionalPanel(condition= "output.vis_suit",
                              shiny::uiOutput('suitselect')),
      shiny::conditionalPanel(condition= "output.vis_tie",
                              shiny::uiOutput('tieselect')),
      shiny::conditionalPanel(condition= "output.vis_skirt",
                              shiny::uiOutput('skirtselect')),
      shiny::conditionalPanel(condition= "output.vis_buttons",
                              shiny::uiOutput('buttonselect')),
      shiny::conditionalPanel(condition= "output.vis_shoes",
                              shiny::uiOutput('shoesselect')),
      shiny::downloadButton("download_svg", "Download Character as SVG"),
      shiny::downloadButton("download_png", "Download Character as PNG")
    ),

    shiny::mainPanel(
      shiny::imageOutput("characterPlot")
    )
  )
)

server <- function(input, output) {

  head_path <- shiny::reactive({
    paste0("figures/", "head",input$head_choice,".svg")
    #paste0("inst/www/head",input$head_choice,".svg")
  })

  body_path <- shiny::reactive({
    paste0("figures/",input$clothes_choice,".svg")
    #paste0("inst/www/",input$clothes_choice,".svg")
  })

  clothes_selection <- shiny::reactive({
    return(fig_info %>% dplyr::filter(Part == "clothes", Label == input$clothes_choice))
  })

  head_selection <- shiny::reactive({
    if (input$clothes_choice != "hazmat"){
      return(fig_info %>% dplyr::filter(Part == "head", Label == input$head_choice))
    } else if (input$clothes_choice == "hazmat"){
      return(fig_info %>% dplyr::filter(Part == "clothes", Label == input$clothes_choice))
    }
  })

  # Visibility of Elements

  output$vis_head <- shiny::reactive({input$clothes_choice != "hazmat"})
  shiny::outputOptions(output, "vis_head", suspendWhenHidden = FALSE)

  output$vis_shirt <- shiny::reactive({'shirt' %in% clothes_selection()$Item})
  shiny::outputOptions(output, "vis_shirt", suspendWhenHidden = FALSE)

  output$vis_pants <- shiny::reactive({'pants' %in% clothes_selection()$Item})
  shiny::outputOptions(output, "vis_pants", suspendWhenHidden = FALSE)

  output$vis_suit <- shiny::reactive({'suit' %in% clothes_selection()$Item})
  shiny::outputOptions(output, "vis_suit", suspendWhenHidden = FALSE)

  output$vis_tie <- shiny::reactive({'tie' %in% clothes_selection()$Item})
  shiny::outputOptions(output, "vis_tie", suspendWhenHidden = FALSE)

  output$vis_skirt <- shiny::reactive({'skirt' %in% clothes_selection()$Item})
  shiny::outputOptions(output, "vis_skirt", suspendWhenHidden = FALSE)

  output$vis_buttons <- shiny::reactive({'buttons' %in% clothes_selection()$Item})
  shiny::outputOptions(output, "vis_buttons", suspendWhenHidden = FALSE)

  output$vis_shoes <- shiny::reactive({'shoes' %in% clothes_selection()$Item})
  shiny::outputOptions(output, "vis_shoes", suspendWhenHidden = FALSE)

  output$vis_hair2 <- shiny::reactive({'hair2' %in% head_selection()$Item})
  shiny::outputOptions(output, "vis_hair2", suspendWhenHidden = FALSE)

  output$vis_hair3 <- shiny::reactive({'hair_lines' %in% head_selection()$Item})
  shiny::outputOptions(output, "vis_hair3", suspendWhenHidden = FALSE)

  output$vis_glasses <- shiny::reactive({'glasses' %in% head_selection()$Item})
  shiny::outputOptions(output, "vis_glasses", suspendWhenHidden = FALSE)

  output$vis_eye <- shiny::reactive({'eye' %in% head_selection()$Item})
  shiny::outputOptions(output, "vis_eye", suspendWhenHidden = FALSE)

  # Default Colors
  question_head <- function(item, label){
    default_item <- shiny::reactive(head_selection()[head_selection()$Item==item,]$Color)
    shiny::renderUI({
      colourpicker::colourInput(item, label, default_item())})
  }

  question_clothes <- function(item, label){
    default_item <- shiny::reactive(clothes_selection()[clothes_selection()$Item==item,]$Color)
    shiny::renderUI({
      colourpicker::colourInput(item, label, default_item())})
  }

  output$eyeselect <- question_head("eye", "Eye Color:")

  output$hairselect <- question_head("hair1", "Hair Color:")

  output$hair2select <- question_head("hair2", "Secondary Hair Color:")

  output$hair3select <- question_head("hair_lines", "Hair Line Color:")

  output$glassesselect <- question_head("glasses", "Glasses Color:")

  output$skinselect <- question_head("skin", "Skin Color:")

  output$shirtselect <- question_clothes("shirt", "Shirt Color:")

  output$pantsselect <- question_clothes("pants", "Pants Color:")

  output$suitselect <- question_clothes("suit", "Suit Color:")

  output$tieselect <- question_clothes("tie", "Tie Color:")

  output$skirtselect <- question_clothes("skirt", "Skirt Color:")

  output$buttonselect <- question_clothes("buttons", "Button Color:")

  output$shoesselect <- question_clothes("shoes", "Shoe Color:")

  image_processing <- shiny::reactive({

    file_body <- as.data.frame(paste(gsub("'","",readLines(body_path())), collapse = ""))

    body_split <-file_body %>% stringr::str_split(">") %>%
      as.data.frame(col.names="svg_file") %>% dplyr::filter(svg_file !="")

    body_split$svg_file <- paste0(body_split$svg_file, ">")

    if (input$clothes_choice != "hazmat"){

      file_head <- as.data.frame(paste(gsub("'","",readLines(head_path())), collapse = ""))

      head_split <-file_head %>% stringr::str_split(">") %>%
        as.data.frame(col.names="svg_file") %>% dplyr::filter(svg_file !="")

      head_split$svg_file <- paste0(head_split$svg_file, ">")

      row_remove <- utils::tail(head_split, -3)

      body_short <- utils::head(body_split, -1)
      combined_split <- rbind(body_short, row_remove) } else {

        combined_split <- body_split
      }

    apply_fill <- function(image, item) {
      finding_row<-mapply(grepl, item, image)
      image[finding_row,] <- change_fill(image[finding_row,], input[[item]])

      image
    }

    combined_split <- apply_fill(combined_split, "skin")

    combined_split <- apply_fill(combined_split, "hair1")

    combined_split <- apply_fill(combined_split, "hair2")

    combined_split <- apply_fill(combined_split, "hair_lines")

    combined_split <- apply_fill(combined_split, "glasses")

    combined_split <- apply_fill(combined_split, "eye")

    combined_split <- apply_fill(combined_split, "shirt")

    combined_split <- apply_fill(combined_split, "pants")

    combined_split <- apply_fill(combined_split, "suit")

    combined_split <- apply_fill(combined_split, "tie")

    combined_split <- apply_fill(combined_split, "skirt")

    combined_split <- apply_fill(combined_split, "buttons")

    combined_split <- apply_fill(combined_split, "shoes")

    file_final_combined<- apply(combined_split,2,paste, collapse="")

    file_final_combined
  })

  image_png <- shiny::reactive({

    combined_magic <- magick::image_read_svg(image_processing(), width=400)

    combined <- magick::image_fill(combined_magic, color = "transparent",
                                   refcolor = "white",
                                   fuzz=30,
                                   point = "+1+1")

    tmpfile <- magick::image_write(combined, tempfile(fileext='png'), format="png")

    list(src = tmpfile, contentType = "image/png", width="50%")
  })

  output$characterPlot <- shiny::renderImage({image_png()}, deleteFile = FALSE)

  output$download_png <- shiny::downloadHandler(
    filename = function() {
      paste0(input$clothes_choice, '.png')
    },
    content = function(file) {
      img <- image_png()$src
      file.copy(img, file)
    })

  output$download_svg <- shiny::downloadHandler(
    filename = function() {
      paste0(input$clothes_choice, '.svg')
    },
    content = function(file) {
      img <- image_processing()
      write(img, file)
    })
}
# Run the application
shinyApp(ui = ui, server = server)
