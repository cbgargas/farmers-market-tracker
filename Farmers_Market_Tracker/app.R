# Load required libraries
library(shiny)
library(DT)
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(viridis)
library(bslib)

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
  rent INTEGER,
  market_fee INTEGER,
  labor INTEGER
)
")

# Helper to recalculate derived columns
updateSalesData <- function(df) {
	df <- df %>%
		mutate(
			qty_difference = qty_actual - qty_projected,
			qty_remaining = qty_actual - qty_sold,
			rev_projected = qty_actual * price,
			rev_actual = qty_sold * price,
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
	theme = bs_theme(version = 5, 
									 bootswatch = "darkly", 
									 # bg = '#F6EEE9',
									 # fg = '#000',
									 # primary = '#AFABD5',
									 # secondary = '#2e2245',
									 # success = '#38471F',
									 # info = '#0b403e',
									 # warning = '#d9a89d',
									 # danger = '#964a31',
									 # base_font = 'Sans Serif',
									 # code_font ='Monospace'
									 ),
	titlePanel("Bakery Sales Tracker"),
	tabsetPanel(
		tabPanel("Quick Entry",
						 sidebarLayout(
						 	sidebarPanel(
						 		dateInput("week", "Week", value = Sys.Date(), format = "yyyy-mm-dd"),
						 		selectizeInput("item", "Item Name", choices = NULL, selected = NULL, options = list(create = TRUE)),
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
						 		downloadButton("download_sales", "Download Sales CSV"),  # Already there
						 		downloadButton("download_fixed_costs", "Download Fixed Costs CSV"),
						 		downloadButton("download_combined_report", "Download Weekly Summary"),
						 		tags$h4("Export Reports"),
						 		downloadButton("download_sales", "Sales Data"),
						 		downloadButton("download_fixed_costs", "Fixed Costs"),
						 		downloadButton("download_combined_report", "Summary Report"),
						 		dateRangeInput("report_range", "Select Date Range:",
						 									 start = Sys.Date() - 30, end = Sys.Date(),
						 									 format = "yyyy-mm-dd"),
						 		downloadButton("download_pdf_report", "Download PDF Report")
						 		
						 		
						 	),
						 	mainPanel(
						 		DTOutput("sales_table")
						 	)
						 )
		),
		tabPanel("Weekly Summary",
						 sidebarLayout(
						 	sidebarPanel(
						 		dateRangeInput("summary_range", "Date Range:",
						 									 start = Sys.Date() - 30, end = Sys.Date(),
						 									 format = "yyyy-mm-dd")
						 	),
						 	mainPanel(
						 		DTOutput("weekly_summary_table")
						 	)
						 )
		),
		tabPanel("Profit & Loss",
						 sidebarLayout(
						 	sidebarPanel(
						 		dateRangeInput("pl_range", "Select Date Range:",
						 									 start = Sys.Date() - 30, end = Sys.Date(),
						 									 format = "yyyy-mm-dd"
						 		)
						 	),
						 	mainPanel(
						 		DTOutput("pl_table"),
						 		plotOutput("pl_plot")
						 	)
						 )
		),
		tabPanel("Visualizations",
						 sidebarLayout(
						 	sidebarPanel(
						 		selectInput("viz_metric", "Metric:", choices = c("rev_actual", "rev_projected", "rev_difference")),
						 		selectInput("viz_week", "Filter Week:", choices = NULL, selected = NULL),
						 		selectInput("viz_type", "Filter Type:", choices = NULL, selected = NULL),
						 		checkboxInput("facet_by_item", "Facet by Item", value = FALSE)
						 	),
						 	mainPanel(
						 		plotOutput("sales_plot")
						 	)
						 )
		),
		tabPanel("Fixed Costs",
						 sidebarLayout(
						 	sidebarPanel(
						 		dateInput("week", "Week", value = Sys.Date(), format = "yyyy-mm-dd"),
						 		numericInput("rent", "Rent", 0),
						 		numericInput("labor", "Labor", 0),
						 		# actionButton("submit_cost_entry", "Submit Fixed Cost Entry"),
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
		item_names <- unique(dbGetQuery(con, "SELECT DISTINCT item FROM sales")$item)
		updateSelectizeInput(session, "item", choices = item_names, server = TRUE)
		
	})
	
	observe({
		item_names <- unique(dbGetQuery(con, "SELECT DISTINCT item FROM sales")$item)
		updateSelectizeInput(session, "item", choices = item_names, server = TRUE)
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
								as.character(input$week), # SQLite doesn't like dates, so convert to character
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
	
	output$weekly_summary_table <- renderDT({
		sales <- loadSalesData()
		costs <- loadFixedCosts()
		
		filtered_sales <- sales %>%
			filter(as.Date(week) >= input$summary_range[1],
						 as.Date(week) <= input$summary_range[2]) %>%
			mutate(total_material_cost = material_cost)
		
		filtered_costs <- costs %>%
			filter(as.Date(week) >= input$summary_range[1],
						 as.Date(week) <= input$summary_range[2])
		
		summary <- filtered_sales %>%
			group_by(week) %>%
			summarise(
				total_rev_actual = sum(rev_actual, na.rm = TRUE),
				total_rev_projected = sum(rev_projected, na.rm = TRUE),
				total_rev_difference = sum(rev_difference, na.rm = TRUE),
				total_qty_sold = sum(qty_sold, na.rm = TRUE),
				total_material_cost = sum(total_material_cost, na.rm = TRUE),
				.groups = 'drop'
			) %>%
			left_join(filtered_costs %>%
									group_by(week) %>%
									summarise(
										total_rent = sum(rent, na.rm = TRUE),
										total_labor = sum(labor, na.rm = TRUE),
										total_market_fee = sum(market_fee, na.rm = TRUE),
										.groups = 'drop'),
								by = "week") %>%
			mutate(
				total_cost = total_rent + total_labor + total_market_fee + total_material_cost,
				net_profit = total_rev_actual - total_cost
			)
		
		datatable(summary, options = list(scrollX = TRUE, pageLength = 10))
	})
	
	output$pl_table <- renderDT({
		sales <- loadSalesData()
		costs <- loadFixedCosts()
		
		filtered_sales <- sales %>%
			filter(as.Date(week) >= input$pl_range[1],
						 as.Date(week) <= input$pl_range[2]) %>%
			mutate(material_total = material_cost)
		
		filtered_costs <- costs %>%
			filter(as.Date(week) >= input$pl_range[1],
						 as.Date(week) <= input$pl_range[2])
		
		pl_summary <- filtered_sales %>%
			group_by(week) %>%
			summarise(
				revenue = sum(rev_actual, na.rm = TRUE),
				material_cost = sum(material_total, na.rm = TRUE),
				.groups = 'drop'
			) %>%
			left_join(
				filtered_costs %>%
					group_by(week) %>%
					summarise(
						rent = sum(rent, na.rm = TRUE),
						labor = sum(labor, na.rm = TRUE),
						market_fee = sum(market_fee, na.rm = TRUE),
						.groups = 'drop'
					),
				by = "week"
			) %>%
			mutate(
				fixed_cost = rent + labor + market_fee,
				total_cost = fixed_cost + material_cost,
				net_profit = revenue - total_cost
			)
		
		datatable(pl_summary, options = list(scrollX = TRUE, pageLength = 10))
	})
	
	output$pl_plot <- renderPlot({
		sales <- loadSalesData()
		costs <- loadFixedCosts()
		
		filtered_sales <- sales %>%
			filter(as.Date(week) >= input$pl_range[1],
						 as.Date(week) <= input$pl_range[2]) %>%
			group_by(week) %>%
			summarise(revenue = sum(rev_actual, na.rm = TRUE),
								material_cost = sum(material_cost, na.rm = TRUE),
								.groups = 'drop')
		
		filtered_costs <- costs %>%
			filter(as.Date(week) >= input$pl_range[1],
						 as.Date(week) <= input$pl_range[2]) %>%
			group_by(week) %>%
			summarise(rent = sum(rent, na.rm = TRUE),
								labor = sum(labor, na.rm = TRUE),
								market_fee = sum(market_fee, na.rm = TRUE),
								.groups = 'drop')
		
		plot_data <- left_join(filtered_sales, filtered_costs, by = "week") %>%
			mutate(
				fixed_cost = rent + labor + market_fee,
				total_cost = fixed_cost + material_cost,
				net_profit = revenue - total_cost
			)
		
		ggplot(plot_data, aes(x = as.Date(week), y = net_profit)) +
			geom_col(fill = ifelse(plot_data$net_profit >= 0, "#4CAF50", "#F44336")) +
			labs(title = "Weekly Profit/Loss",
					 x = "Week", y = "Net Profit") +
			theme_minimal()
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
		
		# Set desired order of types
		type_levels <- c("Sourdough Loaf", "Sourdough Bagel", "Challah", "Focaccia", "Popover")
		
		df <- df %>%
			mutate(
				type = factor(type, levels = type_levels),
				item = factor(item),
				facet_label = interaction(type, item, sep = " - "),
				facet_label = factor(facet_label, levels = unique(facet_label[order(type, item)]))  # order facets by type first
			)
		
		p <- df %>% 
			ggplot(aes(x = week, y = .data[[input$viz_metric]], fill = type)) +
			geom_bar(stat = "identity", position = "dodge") +
			labs(title = paste("Weekly", input$viz_metric), x = "Week", y = input$viz_metric) +
			scale_fill_viridis(discrete = TRUE) +
			geom_hline(yintercept=0, color = "grey", size=2) +
			theme_minimal()
		
		if (input$facet_by_item) {
			p <- p + facet_wrap(~ facet_label, scales = "free_y")
		}
		
		p
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
		week_selected <- as.character(input$week ) # get the input from the UI, convert to character to satisft SQLite
		df <- loadSalesData()
		
		# Filter by selected week
		week_df <- df[df$week == week_selected, ]
		week_revenue <- sum(week_df$rev_actual, na.rm = TRUE)
		
		market_fee <- week_revenue * 0.05
		
		dbExecute(con, "INSERT INTO fixed_costs (week, rent, market_fee, labor) VALUES (?, ?, ?, ?)",
							params = list(
								week_selected,
								input$rent,
								market_fee,
								input$labor
							))
		
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

	# Download Sales Data
	output$download_sales <- downloadHandler(
		filename = function() {
			paste0("sales_data_", Sys.Date(), ".csv")
		},
		content = function(file) {
			write.csv(loadSalesData(), file, row.names = FALSE)
		}
	)
	
	# Download Fixed Costs Data
	output$download_fixed_costs <- downloadHandler(
		filename = function() {
			paste0("fixed_costs_", Sys.Date(), ".csv")
		},
		content = function(file) {
			write.csv(loadFixedCosts(), file, row.names = FALSE)
		}
	)
	
	# Download Combined Weekly Summary Report
	output$download_combined_report <- downloadHandler(
		filename = function() {
			paste0("weekly_summary_", Sys.Date(), ".csv")
		},
		content = function(file) {
			sales <- loadSalesData()
			costs <- loadFixedCosts()
			
			summary <- sales %>%
				group_by(week) %>%
				summarise(
					total_rev_actual = sum(rev_actual, na.rm = TRUE),
					total_rev_projected = sum(rev_projected, na.rm = TRUE),
					total_rev_difference = sum(rev_difference, na.rm = TRUE),
					total_qty_sold = sum(qty_sold, na.rm = TRUE),
					.groups = 'drop'
				) %>%
				left_join(costs %>%
										group_by(week) %>%
										summarise(
											total_rent = sum(rent, na.rm = TRUE),
											total_labor = sum(labor, na.rm = TRUE),
											total_market_fee = sum(market_fee, na.rm = TRUE),
											.groups = 'drop'),
									by = "week") %>%
				mutate(
					total_cost = total_rent + total_labor + total_market_fee,
					net_profit = total_rev_actual - total_cost
				)
			
			write.csv(summary, file, row.names = FALSE)
		}
	)
	
	output$download_pdf_report <- downloadHandler(
		filename = function() {
			paste0("Bakery_Report_", Sys.Date(), ".pdf")
		},
		content = function(file) {
			sales <- loadSalesData()
			costs <- loadFixedCosts()
			
			# Filter by selected date range
			filtered_sales <- sales %>%
				filter(as.Date(week) >= input$report_range[1],
							 as.Date(week) <= input$report_range[2])
			
			filtered_costs <- costs %>%
				filter(as.Date(week) >= input$report_range[1],
							 as.Date(week) <= input$report_range[2])
			
			# Temporary file
			tempReport <- file.path(tempdir(), "report_template.Rmd")
			file.copy("report_template.Rmd", tempReport, overwrite = TRUE)
			
			# Render RMarkdown to PDF
			rmarkdown::render(tempReport, output_file = file,
												params = list(
													sales_data = filtered_sales,
													fixed_costs = filtered_costs,
													start_date = input$report_range[1],
													end_date = input$report_range[2]
												),
												envir = new.env(parent = globalenv())
			)
		}
	)
	
}


shinyApp(ui = ui, server = server)