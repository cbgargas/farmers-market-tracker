# Load required libraries
library(shiny)
library(DT)
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)

# Connect to SQLite database
con <- dbConnect(RSQLite::SQLite(), "bakery.db")

# Create sales table if it doesn't exist
dbExecute(con, "
CREATE TABLE IF NOT EXISTS sales (
  id INTEGER PRIMARY KEY,
  week TEXT,
  item TEXT,
  type TEXT,
  qty_projected INTEGER,
  qty_actual INTEGER,
  qty_difference INTEGER,
  qty_sold INTEGER,
  qty_remaining INTEGER,
  price REAL,
  material_cost REAL,
  rev_projected REAL,
  rev_actual REAL,
  rev_difference REAL,
  notes TEXT
)
")

# Create fixed costs table if it doesn't exist
dbExecute(con, "
CREATE TABLE IF NOT EXISTS fixed_costs (
  id INTEGER PRIMARY KEY,
  week TEXT,
  rent REAL,
  market_fee REAL,
  labor REAL
)
")

# Helper to recalculate derived columns
updateSalesData <- function(df) {
	df <- df %>%
		mutate(
			qty_difference = qty_actual - qty_projected,
			qty_remaining = qty_actual - qty_sold,
			rev_projected = qty_projected * price,
			rev_actual = qty_actual * price,
			rev_difference = rev_actual - rev_projected
		)
	for (i in 1:nrow(df)) {
		dbExecute(con, "UPDATE sales SET 
      qty_difference = ?, 
      qty_remaining = ?,
      rev_projected = ?, 
      rev_actual = ?, 
      rev_difference = ? 
      WHERE id = ?",
							params = list(
								df$qty_difference[i],
								df$qty_remaining[i],
								df$rev_projected[i],
								df$rev_actual[i],
								df$rev_difference[i],
								df$id[i]
							)
		)
	}
	df
}

# UI
ui <- fluidPage(
	titlePanel("Bakery Sales Tracker"),
	tabsetPanel(
		tabPanel("Quick Entry",
						 sidebarLayout(
						 	sidebarPanel(
						 		textInput("week", "Week", value = "YYYY-MM-DD"),
						 		textInput("item", "Item Name", value = ""),
						 		selectInput("type", "Type", choices = c("Sourdough Loaf", "Sourdough Bagel", "Challah", "Focaccia", "Popover")),
						 		numericInput("qty_projected", "Quantity - Projected", 0),
						 		numericInput("qty_actual", "Quantity - Actual", 0),
						 		numericInput("qty_sold", "Quantity - Sold", 0),
						 		numericInput("price", "Price", 0, step = 0.01),
						 		numericInput("material_cost", "Material Cost", 0, step = 0.01),
						 		textInput("notes", "Notes", ""),
						 		actionButton("submit_entry", "Submit Entry")
						 	),
						 	mainPanel(
						 		DTOutput("recent_entries")
						 	)
						 )
		),
		tabPanel("Data Entry",
						 sidebarLayout(
						 	sidebarPanel(
						 		selectInput("filter_week", "Filter by Week:", choices = NULL, selected = NULL),
						 		selectInput("filter_type", "Filter by Type:", choices = NULL, selected = NULL),
						 		actionButton("add_row", "Add New Row"),
						 		downloadButton("download_sales", "Download CSV")
						 	),
						 	mainPanel(
						 		DTOutput("sales_table")
						 	)
						 )
		),
		tabPanel("Visualizations",
						 sidebarLayout(
						 	sidebarPanel(
						 		selectInput("viz_metric", "Metric:", choices = c("rev_actual", "rev_projected", "rev_difference")),
						 		selectInput("viz_week", "Filter Week:", choices = NULL, selected = NULL),
						 		selectInput("viz_type", "Filter Type:", choices = NULL, selected = NULL)
						 	),
						 	mainPanel(
						 		plotOutput("sales_plot")
						 	)
						 )
		),
		tabPanel("Fixed Costs",
						 sidebarLayout(
						 	sidebarPanel(
						 		dateInput("cost_week", "Week", value = Sys.Date(), format = "yyyy-mm-dd"),
						 		numericInput("rent", "Rent", 0, step = 0.01),
						 		numericInput("labor", "Labor", 0, step = 0.01),
						 		actionButton("submit_cost_entry", "Submit Fixed Cost Entry"),
						 		actionButton("add_cost_row", "Add Cost Entry")
						 	),
						 	mainPanel(
						 		DTOutput("fixed_costs_table")
						 	)
						 )
		)
	),
	tags$script(HTML("
  $(document).on('click', '.delete_btn', function() {
    var id = $(this).data('id');
    console.log('Clicked delete with id:', id); // debug
    if (confirm('Are you sure you want to delete this sales entry?')) {
      Shiny.setInputValue('delete_sale_id', id, {priority: 'event'});
    }
  });
  $(document).on('click', '.delete_cost_btn', function() {
    var id = $(this).data('id');
    if (confirm('Are you sure you want to delete this fixed cost entry?')) {
      Shiny.setInputValue('delete_cost_id', id, {priority: 'event'});
    }
  });
"))
	
	
	
	
)

# Server
server <- function(input, output, session) {
	
	loadSalesData <- reactiveVal({
		df <- dbReadTable(con, "sales")
		df[is.na(df)] <- 0
		df <- updateSalesData(df)
		df
	})
	
	loadFixedCosts <- reactiveVal({
		df <- dbReadTable(con, "fixed_costs")
		df[is.na(df)] <- 0
		df
	})
	
	observe({
		df <- loadSalesData()
		updateSelectInput(session, "filter_week", choices = c("All", unique(df$week)), selected = "All")
		updateSelectInput(session, "filter_type", choices = c("All", unique(df$type)), selected = "All")
		updateSelectInput(session, "viz_week", choices = c("All", unique(df$week)), selected = "All")
		updateSelectInput(session, "viz_type", choices = c("All", unique(df$type)), selected = "All")
	})
	
	filteredData <- reactive({
		df <- loadSalesData()
		if (input$filter_week != "All") {
			df <- df[df$week == input$filter_week, ]
		}
		if (input$filter_type != "All") {
			df <- df[df$type == input$filter_type, ]
		}
		df
	})
	
	output$sales_table <- renderDT({
		df <- filteredData()
		df$Delete <- sprintf('<button class="btn btn-danger btn-sm delete_btn" data-id="%s">Delete</button>', df$id)
		
		# Render the table
		dt <- datatable(df, escape = FALSE, editable = TRUE, rownames = FALSE,
										options = list(scrollX = TRUE, scrollY = "400px", paging = FALSE))
		
		# Send the custom message to bind delete buttons
		session$sendCustomMessage(type = "bindDeleteSales", message = list())
		
		return(dt)
	})
	
	observeEvent(input$sales_table_cell_edit, {
		info <- input$sales_table_cell_edit
		df <- loadSalesData()
		row_id <- df$id[info$row]
		col_name <- names(df)[info$col + 1]
		new_value <- info$value
		query <- sprintf("UPDATE sales SET %s = ? WHERE id = ?", col_name)
		dbExecute(con, query, params = list(new_value, row_id))
		df <- dbReadTable(con, "sales")
		df <- updateSalesData(df)
		loadSalesData(df)
	})
	
	observeEvent(input$add_row, {
		dbExecute(con, "INSERT INTO sales (
                week, item, type, qty_projected, qty_actual, qty_difference,
                qty_sold, qty_remaining, price, material_cost, rev_projected, rev_actual, rev_difference, notes
               ) VALUES ('', '', '', 0, 0, 0, 0, 0, 0.0, 0.0, 0.0, 0.0, 0.0, '')")
		df <- dbReadTable(con, "sales")
		df <- updateSalesData(df)
		loadSalesData(df)
	})
	
	observeEvent(input$submit_entry, {
		dbExecute(con, "INSERT INTO sales (
                week, item, type, qty_projected, qty_actual, qty_difference,
                qty_sold, qty_remaining, price, material_cost, rev_projected, rev_actual, rev_difference, notes
               ) VALUES (?, ?, ?, ?, ?, 0, ?, 0, ?, ?, 0, 0, 0, ?)",
							params = list(
								input$week,
								input$item,
								input$type,
								input$qty_projected,
								input$qty_actual,
								input$qty_sold,
								input$price,
								input$material_cost,
								input$notes
							))
		df <- dbReadTable(con, "sales")
		df <- updateSalesData(df)
		loadSalesData(df)
	})
	
	observeEvent(input$submit_cost_entry, {
		df <- dbReadTable(con, "sales")
		week_df <- df[df$week == as.character(input$cost_week), ]
		total_rev <- sum(week_df$rev_actual, na.rm = TRUE)
		calc_market_fee <- total_rev * 0.0005
		
		dbExecute(con, "INSERT INTO fixed_costs (week, rent, market_fee, labor) VALUES (?, ?, ?, ?)",
							params = list(
								input$cost_week,
								input$rent,
								calc_market_fee,
								input$labor
							))
		loadFixedCosts(dbReadTable(con, "fixed_costs"))
	})
	
	output$recent_entries <- renderDT({
		datatable(loadSalesData(), options = list(pageLength = 5, scrollX = TRUE, scrollY = "400px"))
	})
	
	output$download_sales <- downloadHandler(
		filename = function() {
			paste0("sales_data_", Sys.Date(), ".csv")
		},
		content = function(file) {
			write.csv(loadSalesData(), file, row.names = FALSE)
		}
	)
	
	output$sales_plot <- renderPlot({
		df <- loadSalesData()
		if (input$viz_week != "All") {
			df <- df[df$week == input$viz_week, ]
		}
		if (input$viz_type != "All") {
			df <- df[df$type == input$viz_type, ]
		}
		if (nrow(df) == 0) return(NULL)
		ggplot(df, aes(x = week, y = .data[[input$viz_metric]], fill = type)) +
			geom_bar(stat = "identity", position = "dodge") +
			labs(title = paste("Weekly", input$viz_metric), x = "Week", y = input$viz_metric) +
			theme_minimal()
	})
	
	output$fixed_costs_table <- renderDT({
		df <- loadFixedCosts()
		df$Delete <- sprintf('<button class="btn btn-danger btn-sm delete_cost_btn" data-id="%s">Delete</button>', df$id)
		
		# Render the table
		dt <- datatable(df, escape = FALSE, editable = TRUE, rownames = FALSE,
										options = list(scrollX = TRUE, scrollY = "400px", paging = FALSE))
		
		# Send the custom message to bind delete buttons
		session$sendCustomMessage(type = "bindDeleteCosts", message = list())
		
		return(dt)
	})
	
	observeEvent(input$delete_cost_id, {
		dbExecute(con, "DELETE FROM fixed_costs WHERE id = ?", params = list(input$delete_cost_id))
		loadFixedCosts(dbReadTable(con, "fixed_costs"))
	})
	
	observeEvent(input$fixed_costs_table_cell_edit, {
		info <- input$fixed_costs_table_cell_edit
		df <- loadFixedCosts()
		row_id <- df$id[info$row]
		col_name <- names(df)[info$col + 1]
		new_value <- info$value
		query <- sprintf("UPDATE fixed_costs SET %s = ? WHERE id = ?", col_name)
		dbExecute(con, query, params = list(new_value, row_id))
		loadFixedCosts(dbReadTable(con, "fixed_costs"))
	})
	
	observeEvent(input$add_cost_row, {
		df <- loadSalesData()
		total_revenue <- sum(df$rev_actual, na.rm = TRUE)
		market_fee <- total_revenue * 0.0005
		dbExecute(con, "INSERT INTO fixed_costs (week, rent, market_fee, labor) VALUES ('', 0.0, ?, 0.0)", params = list(market_fee))
		loadFixedCosts(dbReadTable(con, "fixed_costs"))
	})
	
	observeEvent(input$delete_sale_id, {
		print(paste("Deleting sale with id:", input$delete_sale_id))
		dbExecute(con, "DELETE FROM sales WHERE id = ?", params = list(input$delete_sale_id))
		df <- dbReadTable(con, "sales")
		df <- updateSalesData(df)
		loadSalesData(df)
	})
	
	observe({
		session$sendCustomMessage(type = "bindDeleteSales", message = list())
		session$sendCustomMessage(type = "bindDeleteCosts", message = list())
	})

	
}


shinyApp(ui = ui, server = server)