library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(scales)
library(ggplot2)
library(lubridate)
library(zoo)
library(readxl)
library(tidyr)
library(stringr)

# Function to create menu items
menu_item_creator <- function(name, tabName, icon) {
  menuItem(name, tabName = tabName, icon = icon(icon))
}

# Data Preprocessing
load_and_preprocess_data <- function() {
  merged_dataset <- read.csv("merged_dataset.csv")
  quotations_dataset <- read.csv("quotations_dataset.csv")
  merged_dataset$Perioden <- as.Date(merged_dataset$Perioden)
  
  quotations_dataset <- quotations_dataset %>% 
    mutate(Perioden = as.Date(Perioden, "%Y-%m-%d")) %>% 
    arrange(Perioden)
  
  choices_quotations <- unique(paste0(quotations_dataset$Projectnaam, " - ", quotations_dataset$Maand, " - ", quotations_dataset$Jaar))
  
  return(list(merged_dataset = merged_dataset, quotations_dataset = quotations_dataset, choices_quotations = choices_quotations))
}

# Load data and choices
data_choices <- load_and_preprocess_data()
merged_dataset <- data_choices$merged_dataset
quotations_dataset <- data_choices$quotations_dataset
choices_quotations <- data_choices$choices_quotations

# Building UI
ui <- dashboardPage(
  dashboardHeader(title = "VORM 2050", titleWidth = 300),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menu_item_creator("Quotation Assessment", "select", "file"),
      menu_item_creator("Quotation Recalculation", "herijking", "refresh"),
      menu_item_creator("Current Markets", "huidige_markten", "line-chart"),
      menu_item_creator("PRC Value Composition", "kostenstructuur", "bar-chart")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(
        HTML("
          /* Set the dashboard body color */
          body { background-color: #148cbf; }
          .skin-blue .main-header .logo { background-color: #148cbf; }
          .skin-blue .main-header .navbar { background-color: #148cbf; }
          /* Set the font color */
          .skin-blue .main-header .logo { color: white; }
          .skin-blue .main-header .navbar { color: white; }
        ")
      ),
      tags$style(
        HTML("
          /* Custom styles for the Offerte herijking tab */
          #herijking .box.box-solid.box-info {
            background-color: #f8f9fa;
            border-color: #dee2e6;
          }
          
          #herijking .box.box-solid.box-info .box-header {
            background-color: #f8f9fa;
            border-bottom-color: #dee2e6;
            color: #495057;
          }
          
          #herijking .box.box-solid.box-info .box-body {
            background-color: white;
            color: #495057;
          }
        ")
      )
    ),
    
    tabItems(
      # Offerte beoordeling tab
      tabItem(
        tabName = "select",
        fluidRow(
          box(
            title = "1. Select Quotation to assess",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            selectInput(
              "offertes",
              label = NULL,
              choices = choices_quotations,
              selected = choices_quotations[1],
              width = "100%"
            )
          ),
          box(
            title = "2. Select Quotation Of Similar Building Type",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            selectInput(
              "ijk_offerte",
              label = NULL,
              choices = choices_quotations,
              selected = choices_quotations[1],
              width = "100%"
            )
          )
        ),
        fluidRow(
          box(
            title = "",
            plotOutput("marktindex", height = "300px")
          ),
          box(
            title = "Price Change",
            textOutput("Verschil_BVO")
          ),
          box(
            title = "Market Change",
            textOutput("offerte_percentage")
          ),
          box(
            title = "Unexplained Price Difference",
            textOutput("verschil_markt_index")
          )
        )
      ),
      
      # Offerte herijking tab
      tabItem(
        tabName = "herijking",
        fluidRow(
          column(
            width = 12,
            div(
              class = "box box-info",
              style = "background-color: #f8f9fa; border-color: #dee2e6;",
              selectInput("te_herijken_offerte", "Select Quotation to Recalculate:", choices = paste0(quotations_dataset$Projectnaam, " ", quotations_dataset$Maand, " ", quotations_dataset$Jaar)),
              selectInput("herijk_datum", "Select Recalculation Date:", choices = unique(merged_dataset$Perioden)),
              box(
                title = "Original Price (€/m2 GFA)",
                status = "primary",
                solidHeader = TRUE,
                style = "background-color: #f8f9fa; border-color: #dee2e6; color: #495057;",
                textOutput("eerste_getal")
              ),
              box(
                title = "Recalculated Price (€/m2 GFA)",
                status = "primary",
                solidHeader = TRUE,
                style = "background-color: #f8f9fa; border-color: #dee2e6; color: #495057;",
                textOutput("tweede_getal")
              )
            )
          )
        )
      ),
      
      # Huidige markten tab
      
      tabItem(
        tabName = "huidige_markten",
        fluidRow(
          column(
            width = 12,
            div(
              class = "box box-info",
              fluidRow(
                column(
                  3, style = "margin-right: 20px;", style = "margin-left: 40px;",
                  checkboxGroupInput("components", "Pick Markets:", 
                                     choices = c(
                                       "Equipment (CBS)" = 'Bouwkosten totaal',
                                       "Energy: gas + electricity (CBS)" = 'Energie',
                                       "Fuel (CBS)" = 'Diesel',
                                       "Salary (CBS)" = 'Looncomponent',
                                       "Material (CBS)" = 'Materiaalcomponent',
                                       "Concrete (VORM Bouw)" = 'Giet_beton',
                                       "Reinforcing Steel (Belmetal)" = 'Reinforcing steel',
                                       "Construction Steel (Belmetal)" = 'Construction steel'
                                     ), 
                                     selected = 'Bouwkosten totaal')
                ),
                column(
                  7, align="center", 
                  sliderInput("x_range_huidige", "Pick Period:", min = min(merged_dataset$Perioden), max = max(merged_dataset$Perioden), value = c(min(merged_dataset$Perioden), max(merged_dataset$Perioden)))
                )
              ),
              fluidRow(
                column(width = 12, plotOutput("merged_dataset_plot", height = "600px", width = "100%"))
              )
            )
          )
        )
      ),
      # Offerte herijking tab
      tabItem(
        tabName = "herijking",
        fluidRow(
          column(
            width = 12,
            div(
              class = "box box-info",
              style = "background-color: #f8f9fa; border-color: #dee2e6;",
              selectInput("te_herijken_offerte", "Selecteer te herijken offerte:", choices = paste0(quotations_dataset$Projectnaam, " ", quotations_dataset$Maand, " ", quotations_dataset$Jaar)),
              selectInput("herijk_datum", "Selecteer her-ijk datum:", choices = unique(merged_dataset$Perioden))
            )
          )
        )
      ),
      # Offerte herijking tab
      tabItem(
        tabName = "kostenstructuur",
        fluidRow(
          column(width = 3, align = "center", style = "margin-right: 20px;", plotOutput("cost_prop_bar", height = "500px", width = "350px")),
          column(width = 5, align = "center", style = "margin-left: 60px;", plotOutput("cost_formulas", height = "500px", width = "700px")) #style = "margin-top: 20px;" style = "margin-left: 50px;",
        )
      )
    )
  )
)
# Server
server <- function(input, output) {
  datasets <- load_and_preprocess_data()
  merged_dataset <- datasets$merged_dataset
  quotations_dataset <- datasets$quotations_dataset
  choices_quotations <- datasets$choices_quotations
  
  # Function to calculate the pooled market index
  bereken_marktindex <- function() {
    # Construct formulas for each component
    formulas <- list(
      `Columns and floors` = list(`Giet_beton` = 0.4, `Energie` = 0.05, `Reinforcing steel nets` = 0.25, `Looncomponent` = 0.3),
      `Facades and walls` = list(`Giet_beton` = 0.4, `Energie` = 0.05, `Reinforcing steel bars` = 0.25, `Looncomponent` = 0.3),
      `Transport` = list(`Looncomponent` = 0.35, `Materiaalcomponent` = 0.25, `Diesel` = 0.4),
      `Engineering` = list(`Looncomponent` = 0.9, `Materiaalcomponent` = 0.1),
      `Assembly` = list(`Looncomponent` = 0.85, `Diesel` = 0.1, `Materiaalcomponent` = 0.05),
      `Construction steel` = list(`Energie` = 0.05, `Construction steel` = 0.7, `Looncomponent` = 0.25)
    )
    
    # Calculate the index for each component
    component_indexes <- lapply(names(formulas), function(component) {
      formula <- formulas[[component]]
      component_data <- merged_dataset[merged_dataset$Componenten %in% names(formula), ]
      component_index <- component_data %>% 
        group_by(Perioden) %>% 
        summarise(Index = sum(Index * unlist(formula[Componenten]))) %>%
        mutate(Index = zoo::na.locf(Index))  # Fill NA values with the most recent non-NA value
      component_index$Componenten <- component
      return(component_index)
    })
    
    # Combine all component index data frames
    all_component_indexes <- do.call(rbind, component_indexes)
    
    # Weights for the pooled market index for Prefab gewapend beton
    weights_prefab <- c(
      `kolommen en vloeren` = 0.34069850,
      `gevels en wanden` = 0.30096414,
      `transport` = 0.09980243,
      `engineering` = 0.09046891,
      `montage` = 0.13056721,
      `constructiestaal` = 0.03749881
    )
    
    # Calculate the pooled market index for Prefab gewapend beton
    prefab_index <- all_component_indexes %>% 
      group_by(Perioden) %>% 
      summarise(prefab_index = sum(Index * weights_prefab))
    return(prefab_index)
  }
  
  # Reactive expression to calculate the percentage difference
  offerte_beoordeling <- reactive({
    # Get the selected offerte period
    ijk_offerte_period <- quotations_dataset %>%
      filter(paste0(Projectnaam, " - ", Maand, " - ", Jaar) == input$ijk_offerte) %>%
      pull(Perioden)
    
    # Get the period of the selected offertes
    offertes_periods <- quotations_dataset %>%
      filter(paste0(Projectnaam, " - ", Maand, " - ", Jaar) %in% input$offertes) %>%
      pull(Perioden)
    
    # Get the prefab_index values for the selected periods
    ijk_offerte_index <- bereken_marktindex() %>%
      filter(Perioden == ijk_offerte_period) %>%
      pull(prefab_index)
    offertes_index <- bereken_marktindex() %>%
      filter(Perioden %in% offertes_periods) %>%
      pull(prefab_index)
    
    # Calculate the percentage difference
    percentage_diff <- ((offertes_index - ijk_offerte_index) / ijk_offerte_index) * 100
    
    return(percentage_diff)
  })
  
  output$offerte_percentage <- renderText({
    paste("", round(offerte_beoordeling()), "%")
  })
  
  # Define Verschil_BVO as a reactive expression
  Verschil_BVO <- reactive({
    # Get the price_B2_per_BVO of the ijk_offerte
    ijk_offerte <- quotations_dataset %>%
      filter(paste0(Projectnaam, " - ", Maand, " - ", Jaar) == input$ijk_offerte)
    ijk_offerte_prijs_B2_per_BVO <- ijk_offerte$prijs_B2_per_BVO
    
    # Get the price_B2_per_BVO of the selected offertes
    selected_offertes <- quotations_dataset %>%
      filter(paste0(Projectnaam, " - ", Maand, " - ", Jaar) %in% input$offertes)
    selected_offertes_prijs_B2_per_BVO <- selected_offertes$prijs_B2_per_BVO
    
    # Calculate the difference as a percentage
    difference_percentage <- ((selected_offertes_prijs_B2_per_BVO - ijk_offerte_prijs_B2_per_BVO) / ijk_offerte_prijs_B2_per_BVO) * 100
    
    # Return the difference as a percentage
    return(difference_percentage)
  })
  
  # Modify the output$Verschil_BVO
  output$Verschil_BVO <- renderText({
    verschil <- Verschil_BVO() - offerte_beoordeling()
    verschil_rounded <- round(verschil)
    
    # Display the difference
    paste("", verschil_rounded, "%")
  })
  
  
  # Modify the output$verschil_markt_index
  output$verschil_markt_index <- renderText({
    verschil <- Verschil_BVO() - offerte_beoordeling()
    verschil_rounded <- round(verschil)
    
    # Display the difference
    paste("", verschil_rounded, "%")
  })
  
  output$merged_dataset_plot <- renderPlot({
    
    data <- merged_dataset %>% filter(Componenten %in% input$components)
    
    ggplot(data, aes(x = Perioden, y = Index, color = Componenten, group = Componenten)) +
      geom_line(size=1) +
      labs(x = "Years", y = "Index") +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Set breaks and labels for years
      # scale_color_brewer(palette = "Set1") +
      scale_color_manual(values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), 
                         labels = c("Equipment (CBS)" = "Bouwkosten totaal", "Energy: gas + electricity (CBS)" = "Energie", 
                                    "Fuel (CBS)" = "Diesel", "Salary (CBS)" = "Looncomponent", "Material (CBS)" = "Materiaalcomponent", "Concrete (VORM Bouw)" = "Giet_beton", "Reinforcing Steel, (Belmetal)" = "Reinforcing steel", "Construction Steel (Belmetal)" = "Construction steel"),
                         name = "Markets") +
      coord_cartesian(xlim = input$x_range_huidige) +
      theme_minimal() +
      theme(legend.position = "top",
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12)) +
      labs(title = "Live Markets with Influence on PRC Market Value", # Set the plot title
           color = "Markets") # Set the legend title
  })
  
  # Add a separate code block to display the legend
  output$legend <- renderUI({
    legend_colors <- color_mapping[selected_labels]
    legend_labels <- selected_labels
    
    legend_items <- lapply(1:length(legend_colors), function(i) {
      tags$li(
        class = "circle",
        style = sprintf("background:%s;", legend_colors[i]),
        legend_labels[i]
      )
    })
    
    tags$ul(class = "legend", legend_items)
  })
  
  
  
  
  output$eerste_getal <- renderText({
    selected_offerte <- quotations_dataset %>% 
      filter(paste0(Projectnaam, " ", Maand, " ", Jaar) == input$te_herijken_offerte)
    prijs_B2_per_BVO <- selected_offerte$prijs_B2_per_BVO
    eerste_getal <- prijs_B2_per_BVO
    paste(" ", scales::dollar(eerste_getal, prefix = "€", big.mark = ",", decimal.mark = ".", digits = 2))
  })
  
  output$tweede_getal <- renderText({
    selected_offerte <- quotations_dataset %>% 
      filter(paste0(Projectnaam, " ", Maand, " ", Jaar) == input$te_herijken_offerte)
    prijs_B2_per_BVO <- selected_offerte$prijs_B2_per_BVO
    selected_offerte_index <- bereken_marktindex() %>%
      filter(Perioden == selected_offerte$Perioden) %>%
      pull(prefab_index)
    herijk_datum_index <- bereken_marktindex() %>%
      filter(Perioden == input$herijk_datum) %>%
      pull(prefab_index)
    tweede_getal <- (prijs_B2_per_BVO * herijk_datum_index) / selected_offerte_index
    paste(" ", scales::dollar(tweede_getal, prefix = "€", big.mark = ",", decimal.mark = ".", digits = 2))
  })
  
  # Bereken en toon het verschil in BVO op het "Offerte beoordeling" tabblad
  output$Verschil_BVO <- renderText({
    # Haal de prijs_B2_per_BVO van de ijk_offerte op
    ijk_offerte <- quotations_dataset %>% 
      filter(paste0(Projectnaam, " - ", Maand, " - ", Jaar) == input$ijk_offerte)
    ijk_offerte_prijs_B2_per_BVO <- ijk_offerte$prijs_B2_per_BVO
    
    # Haal de prijs_B2_per_BVO van de geselecteerde offertes op
    geselecteerde_offertes <- quotations_dataset %>% 
      filter(paste0(Projectnaam, " - ", Maand, " - ", Jaar) %in% input$offertes)
    geselecteerde_offertes_prijs_B2_per_BVO <- geselecteerde_offertes$prijs_B2_per_BVO
    
    # Bereken het verschil als percentage
    verschil_percentage <- ((geselecteerde_offertes_prijs_B2_per_BVO - ijk_offerte_prijs_B2_per_BVO) / ijk_offerte_prijs_B2_per_BVO) * 100
    
    # Geef het verschil als percentage terug
    paste0(" ", scales::percent(verschil_percentage/100, big.mark = ",", decimal.mark = ".", digits = 3))
  })
  
  marktindex_data <- reactive({
    # Get the selected offertes periods
    offertes_periods <- quotations_dataset %>%
      filter(paste0(Projectnaam, " - ", Maand, " - ", Jaar) %in% input$offertes) %>%
      pull(Perioden)
    
    # Get the prefab_index values for the selected periods
    offertes_index <- bereken_marktindex() %>%
      filter(Perioden %in% offertes_periods) %>%
      mutate(prefab_index = prefab_index)
    
    offerte_verschil <- offerte_beoordeling()
    
    offertes_index <- offertes_index %>%
      mutate(Verschil = prefab_index - offerte_verschil)
    
    # Get the selected "Calibration Quotation" period
    ijk_offerte_period <- quotations_dataset %>%
      filter(paste0(Projectnaam, " - ", Maand, " - ", Jaar) == input$ijk_offerte) %>%
      pull(Perioden)
    
    # Get the prefab_index value for the selected "Calibration Quotation" period
    ijk_offerte_index <- bereken_marktindex() %>%
      filter(Perioden == ijk_offerte_period) %>%
      pull(prefab_index)
    
    # Create a data frame with the selected "Calibration Quotation" data point
    ijk_offerte_data <- data.frame(Perioden = ijk_offerte_period, prefab_index = ijk_offerte_index, Verschil = NA)
    
    # Combine the selected "Calibration Quotation" data point with the other data points
    all_data <- rbind(offertes_index, ijk_offerte_data)
    
    return(all_data)
  })
  
  output$marktindex <- renderPlot({
    data <- bereken_marktindex()
    data$Perioden <- as.Date(data$Perioden)
    
    # Get the selected Calibration Quotation period
    ijk_offerte_period <- quotations_dataset %>%
      filter(paste0(Projectnaam, " - ", Maand, " - ", Jaar) == input$ijk_offerte) %>%
      pull(Perioden)
    
    # Filter the marktindex_data to keep only unique Perioden values
    marktindex_data_unique <- marktindex_data() %>%
      distinct(Perioden, .keep_all = TRUE)
    
    # Remove the Calibration Quotation period from marktindex_data_unique if it exists
    marktindex_data_unique <- marktindex_data_unique %>%
      filter(Perioden != ijk_offerte_period)
    ggplot(data, aes(x = Perioden, y = prefab_index)) +
      geom_line(aes(color = "PRC Cost Index"), size = 1) +
      geom_ribbon(aes(ymin = prefab_index * 0.9, ymax = prefab_index * 1.1), alpha = 0.1, fill = rgb(28/255, 84/255, 148/255)) +
      geom_point(data = marktindex_data_unique, aes(y = prefab_index + (Verschil_BVO() - offerte_beoordeling()), color = "Quotation to Assess"), size = 4)+
      geom_point(data = data[data$Perioden == ijk_offerte_period, ], aes(y = prefab_index, color = "Calibration Quotation"), size = 4) +  # Increase the size to 5
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      theme_minimal(base_size = 14) +
      theme(
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(colour = "black"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 12),  # Increase the font size for legend labels
        legend.title = element_blank()  # Remove the legend title
      ) +
      guides(color = guide_legend(override.aes = list(size = 3))) +
      labs(x = "Years", y = "PRC Index", title = "PRC Cost Index Over Time") +
      scale_color_manual(values = c("Calibration Quotation" = "black", "Quotation to Assess" = "red", "PRC Cost Index" = rgb(28/255, 84/255, 148/255)))
  })
  
  
  
  output$cost_prop_bar <- renderPlot({
    
    file_path <- "Kosten_overzicht.xlsx"
    cost_proportions_dataset <- read_excel(file_path, sheet = 1)
    
    # Select the relevant columns for calculation
    selected_columns <- c("kolommen en vloeren", "gevels en wanden", "transport", "engineering", "montage", "staalconstructies", "totaal")
    
    # Subset the data frame to include only the selected columns
    selected_data <- cost_proportions_dataset[, selected_columns]
    
    # Calculate the sum of all proportions except "risico/winst en ak"
    sum_without_risico <- sum(selected_data[, -7], na.rm = TRUE)
    
    # Calculate the average percentage for each column compared to "totaal" (excluding "risico/winst en ak")
    percentage <- colSums(selected_data[, -7], na.rm = TRUE) / sum_without_risico * 100
    
    # Create a new data frame with column names and average percentages
    cost_proportions_dataset <- data.frame(Column = selected_columns[-7], Percentage = percentage)
    
    prop_colors <- c("kolommen en vloeren" = "darkslateblue",
                     "gevels en wanden" = "burlywood4",
                     "transport" = "darkolivegreen",
                     "engineering" = "chocolate3",
                     "montage" = "#00499f",  #A2596B
                     "staalconstructies" = "gray15")
    
    earth_palette <- c("#5D6D7E", "#AF601A", "#154360", "#0E6251", "#7D6608", "#512E5F")
    
    ggplot(cost_proportions_dataset, aes(x = "", y = Percentage, fill = Column, label = paste0(round(Percentage), "%"))) +
      geom_bar(stat = "identity") +
      theme_classic() +
      # scale_fill_manual(values = component_colors, labels = c("Columns & floors", "Facades & walls", 'Transport', "Engineering","Assembly","steel construction")) +
      scale_fill_manual(values = earth_palette, 
                        labels = c("Engineering ", "Facades & walls", 'Columns & floors', "Assembly","Steel beams & columns","Transport"),
                        name = "PRC Components") +
      geom_text(size = 6, position = position_stack(vjust = 0.5), colour="white") +
      labs(color = "Component") +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12)) +
      labs(
        title = "Cost Proportion of the PRC Components", # Set the plot title
        fill = "PRC Components" # Set the legend title
      )
    
  })
  
  output$cost_formulas <- renderPlot({
    library(tibble)
    
    formulas <- list(
      `Columns and floors` = list(`Giet_beton` = 0.4, `Energie` = 0.05, `Reinforcing steel nets` = 0.25, `Looncomponent` = 0.3),
      `Facades and walls` = list(`Giet_beton` = 0.4, `Energie` = 0.05, `Reinforcing steel bars` = 0.25, `Looncomponent` = 0.3),
      `Transport` = list(`Looncomponent` = 0.35, `Materiaalcomponent` = 0.25, `Diesel` = 0.4),
      `Engineering` = list(`Looncomponent` = 0.9, `Materiaalcomponent` = 0.1),
      `Assembly` = list(`Looncomponent` = 0.85, `Diesel` = 0.1, `Materiaalcomponent` = 0.05),
      `Construction steel` = list(`Energie` = 0.05, `Construction steel` = 0.7, `Looncomponent` = 0.25)
    )
    
    # Get the unique components
    components <- sort(unique(unlist(lapply(formulas, names))))
    
    # Create an empty dataframe
    cost_formula_dataset <- tibble(Category = names(formulas))
    
    # Fill in the values for each component
    for (component in components) {
      cost_formula_dataset[[component]] <- sapply(cost_formula_dataset$Category, function(category) {
        value <- formulas[[category]][[component]]
        if (is.null(value)) {
          0
        } else {
          value
        }
      })
    }
    
    cost_formula_dataset[, 2:9] <- cost_formula_dataset[, 2:9] * 100
    
    cost_formula_dataset <- cost_formula_dataset %>%
      pivot_longer(cols = -Category, names_to = "Component", values_to = "Value")
    
    component_colors <- c("Diesel" = "chocolate3", #"Bouwkosten totaal" = "darkslateblue",
                          "Energie" = "darkolivegreen",
                          "Giet_beton" = "gray15",
                          "Looncomponent" = "#00499f",
                          "Materiaalcomponent" = "burlywood4",
                          "Reinforcing steel" = "grey50",
                          "Reinforcing steel bars" = "purple",
                          "Construction steel nets" ="#0099B4")
    
    earth_palette2 <- c("#A67C52", "#1A5276", "#B5838D", "#9A8C75", "tomato4", "#352F20", "#E6B0AA", "#0099B4")
    
    
    # Update the code for the plot
    ggplot(cost_formula_dataset, aes(x = Category, y = Value, fill = Component, label = paste0(round(Value), "%"))) +
      geom_bar(stat = "identity") +
      theme_classic() +
      scale_fill_manual(values = earth_palette2, 
                        labels = c("Construction steel", 'Fuel', "Energy","Concrete", 'Salary','Equipment', 'Reinforcing steel bars', 'Reinforcing steel nets'),
                        name = "Subelements") + 
      geom_text(data = subset(cost_formula_dataset, Value != 0), size = 6, position = position_stack(vjust = 0.5), colour = "white") +
      theme(axis.title = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line.x = element_blank(),
            legend.text = element_text(size = 12),  # Increase the font size for legend labels
            legend.title = element_text(size = 14), 
            axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, 
                                       lineheight = 0.8, 
                                       face = "plain", 
                                       size = 13)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      labs(
        title = "Cost Proportion of the Subelements within PRC Components", # Set the plot title
        fill = "Subelements" # Set the legend title
      ) +
      coord_flip()
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)