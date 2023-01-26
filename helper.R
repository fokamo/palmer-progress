# helper, non-reactive stuff

# load all libraries ----

PACKAGES <- c("shiny", "shinydashboard", "shinyjs", "shinyWidgets", 
              "shinymanager", "RPostgres", "rjson", "lares", "DT", "DBI", 
              "dplyr", "reshape2", "ggpubr", "ggplot2", "plotly", "scrypt")

sapply(PACKAGES, require, character.only = TRUE)

# constants ----

# contrasting colors which tolerate black text on top
COLORBLIND_PALETTE <- c("#77AADD", "#EE8866", "#EEDD88", "#FFAABB", "#99DDFF",
                        "#44BB99", "#BBCC33", "#AAAA00", "#AAAAAA", "#444444")
                        
# report URLS and dates to display
URLS <- data.frame(urls = 
                     rep(paste0("https://dl.dropboxusercontent.com/s/",
                                "el6kpsqrpyuv4j0/hs_rats_n1912_02022021_",
                                "genotyping_summary.html?dl=0"), 3), 
                   date = rep("8/17/21", 3))

# hardcoded list of cells to asterisk
ASTERISK_CELLS <- data.frame(
  project_name = c("r01_su_guo_larvae", "r01_su_guo_breeders",
                   "p50_shelly_flagel_2014", "p50_hao_chen_2014", 
                   "pcal_brian_trainor", "p50_david_dietz_2020",
                   "p50_jerry_richards_2014"),
  column_names = I(list(list("genotyped", "pheno_geno"), list("genotyped"), 
                        list("phenotyped", "pheno_geno", 
                             "dna_extracted", "sequenced"), 
                        list("dna_extracted", "sequenced"),
                        list("genotyped"), list("dead_exclude"),
                        list("target_n", "at_phenotyping_center",
                             "phenotyped", "dna_extracted", "sequenced",
                             "genotyped", "pheno_geno")))
)

# hardcoded links to extra reports
REPORT_LOOKUP <- data.frame(
  project_name = c("p50_hao_chen_2014", "p50_shelly_flagel_2014",
                   "p50_jerry_richards_2014", "p50_paul_meyer_2014"),
  report_names = I(list(list("elevated_plus_maze", "nicsa", "open_field",
                             "social_interaction", "novel_object"),
                        list("crf_MI", "ccc", "pavca_MI", "novelty_seeking"),
                        list("locomotor_testing", "delay_discounting", 
                             "light_reinforcement", "social_reinforcement", 
                             "reaction_time"),
                        list("crf_NY", "pavca_NY", "ccp")))
)
                                 
# location of .database-access file when this app is live or local
SERVER_CREDS <- "/srv/shiny-server/palmer-progress/.database-access.json"
LOCAL_CREDS <- paste0(getwd(), "/.database-access.json")

# schemas to exclude from the project catalog
NON_PROJECT_SCHEMAS <- c("information_schema", "orchid", "public",
                         "pg_catalog", "sample_tracking")

# establish database link ----

# reads json file (hidden in the server) for PostgreSQL credentials
ama_creds <- fromJSON(file = SERVER_CREDS) 

# connect to database
get_con <- function() {
  # uses the list generated from JSON file to enter in credentials
  dbConnect(RPostgres::Postgres(), dbname = ama_creds$database,
                 host = ama_creds$host , port = 5432, user = ama_creds$user, 
                 password = ama_creds$password) 
}
con <- get_con()

# name the application being connected to database
dbSendQuery(con, "set application_name = 'Palmer Shiny App'")

# load tables ----

# credentials to log in to the app, not database
credentials <- dbGetQuery(con, "SELECT * FROM sample_tracking.credentials")

# progress tables
individual_progress <- 
  dbGetQuery(con, "SELECT * FROM sample_tracking.progress_checklist")
project_progress_active <- 
  dbGetQuery(con, "SELECT * FROM sample_tracking.db_progress")
project_progress_complete <-
  dbGetQuery(con, "SELECT * FROM sample_tracking.db_complete")
# project metadata
project_meta <- 
  dbGetQuery(con, "SELECT * FROM sample_tracking.project_metadata")
sample_meta <-
  dbGetQuery(con, "SELECT * FROM sample_tracking.sample_metadata")

# prepare meta-info for later use ----

catalog <- dbGetQuery(con, "SELECT * FROM pg_catalog.pg_tables")
# remove the ones that aren't for a specific project
catalog <- catalog[!(catalog$schemaname %in% NON_PROJECT_SCHEMAS),]
# group tables by their schema
tables_by_schema <- aggregate(tablename ~ schemaname, catalog, unique)

# all the different project names in the data
projects <- sort(unique(c(catalog$schemaname, individual_progress$project_name, 
                          project_progress_complete$project_name,
                          project_meta$project_name)),decreasing = TRUE)

# turn sample meta table into one that matches project names to organism/strain
sample_meta <- aggregate(cbind(organism,strain) ~ project_name, 
                         sample_meta, unique)
sample_meta$animal_type <- paste(sample_meta$organism, "-", sample_meta$strain)

# helper functions for using meta-info ----

# helper function to check if a certain table exists for a certain project
table_exists <- function(project, table) {
  # look up the schema for this project and check through its table catalog
  project %in% tables_by_schema$schemaname &&
    table %in% tables_by_schema[tables_by_schema$schemaname == project,2][[1]]
}

# helper function to check if a data table exists before actually loading it
safely_load_df <- function(project, table, msg) {
  # display an understandable message if the data table is not found
  validate(need(table_exists(project, table), 
                paste0(msg, " (.", table, " table missing)")))
  # grab table if it exists
  dbGetQuery(con, paste("select * from ", project, ".", table))
}

# table visualizations of said meta-info ----

# make a table of which projects are in which tables

# table to store information about whether other tables have each project
projects_in_tables <- data.frame(project_name = projects)

# manually fill in information about the projects
projects_in_tables$progress_checklist <- 
  sapply(projects_in_tables$project_name,
         function(x) x %in% individual_progress$project_name)
projects_in_tables$db_progress <- 
  sapply(projects_in_tables$project_name, 
         function(x) x %in% project_progress_active$project_name)
projects_in_tables$db_complete <- sapply(
  projects_in_tables$project_name, 
  function(x) x %in% project_progress_complete$project_name)
projects_in_tables$progress_metadata <- 
  sapply(projects_in_tables$project_name, 
         function(x) x %in% project_meta$project_name)
  
# strings for Javascript interpretation
projects_in_tables[] <- lapply(projects_in_tables, as.character)

# make a table of which schemas have which tables
tables_in_schemas <- function(show_empty) {
  # determine tables which have more than one occurrence in project schemas
  common_tables <- table(catalog$tablename)
  common_tables <- common_tables[common_tables > 1]
  
  # determine whether each project has the common tables or not
  has_table <- data.frame(project_name = projects)
  # for each common table name, add a column to global has_table data frame
  sapply(names(common_tables), function(table) has_table[[table]] <<- 
           # for each project_name (row), check if table_name is in its schema
           sapply(has_table$project_name,
                  function(project) {
                    ifelse(table_exists(project, table), 
                           # query database to check if tables empty if asked
                           ifelse(show_empty && 
                                    nrow(dbGetQuery(con, 
                                                    paste0("select * from ", 
                                                           project, ".", table))
                                    ) == 0, 
                                  "EMPTY", "TRUE"), "FALSE")
                  })
  )
  
  has_table
}

# functions for processing complicated enough to factor out ----

# remove values of 0, add asterisks if hardcoded
process_progress_df <- function(data) {
  data[] <- lapply(data, as.character)
  
  # don't display 0s
  data <- mutate_all(data, function(x) ifelse(x == "0" | is.na(x), "", x))
  
  apply(ASTERISK_CELLS, 1, function(x) {
    # only try to update a row if it exists
    if(x[1] %in% data$project_name) {
      # for every column which needs an asterisk, add it
      sapply(unlist(x[2]), function(col) {
        data[data$project_name == x[1],][[col]] <<- 
          paste0(data[data$project_name == x[1],][[col]], "*")
      })
    }
  })
  
  data
}

progress_bars <- function(data) {
  # create all possible project-stage combinations for a lookup data frame
  proj_stage <- expand.grid(data$project_name, colnames(data)[2:11])
  colnames(proj_stage) <- c('Project', 'Stage')
  
  # look up the counts, using as.numeric to prevent integer64 problems
  proj_stage$Count <- apply(proj_stage, 1, function(x) as.numeric(
    data[data$project_name == x[1],][[x[2]]]))
  
  # make sure the bars will a) have values and b) be ordered
  proj_stage <- proj_stage[!is.na(proj_stage$Count) & proj_stage$Count != 0,]
  stages <- colnames(data[,-c(1)])
  stages <- c(stages[stages != 'dead_exclude'], 'dead_exclude')
  proj_stage$Stage <- factor(proj_stage$Stage, levels = stages)
  
  # length of bars in plot indicates number of rats, color/position is stage
  ggplot(proj_stage, aes(x = Count, y = Stage, fill = Stage)) + 
    geom_col() + facet_wrap(~Project, scales = 'free_x') + theme_bw() +
    # color bars for visual contrast, then reverse legend to match y order
    scale_fill_manual(values = COLORBLIND_PALETTE, 
                      guide = guide_legend(reverse = TRUE)) +
    # remove y axis labels since they're the same as the color labels
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
          axis.title.y = element_blank(), 
          # bigger text & gaps between facets
          text = element_text(size = 15), panel.spacing = unit(15, "pt")) + 
    labs(title = 'Specimens progressed through stages, by project')
}

# build a ul of all report URLs associated with this project
get_report_URL <- function(project) {
  # start building report URL
  link_start <- paste0(
    "https://palmerlab.s3.sdsc.edu/", 
    ifelse(startsWith(project, "u01"), "u01", "p50"), 
    "_reports/")
  # check hardcoded dictionary for anything extra to put in
  extra_reports <- unlist(REPORT_LOOKUP[
    REPORT_LOOKUP$project_name == project,]$report_names)

  # initialize dataframe with just extra reports
  reports <- if(length(extra_reports) > 0) {
    data.frame(text = extra_reports, 
               link = paste0(link_start, extra_reports, ".html"))
  } else { data.frame() }
  # add in eponymous report
  reports <- rbind(reports, 
                   data.frame(text = "Eponymous report",
                              link = paste0(link_start, project, ".html")))
  
  # build final ul
  tags$ul(apply(reports, 1, 
                function(x) tags$li(tags$a(href = x[2], x[1], 
                                           target = "_blank"))))
}

# functions for parts of DT datatable display ----

# produce the common list of options with parameters for minor variations
table_options <- function(paging, filename)
  list(paging = paging, scrollX = T, ordering = T, searching = T, 
       dom = 'Brtip',
       buttons = list(
         list(extend = 'csv', filename = filename,
              title = paste(filename, "from Palmer Database")),
         list(extend = 'excel', filename = filename, 
              title = paste(filename, "from Palmer Database"))
       ))

# produce the long string description for the project progress tables
project_caption <- function(qualifier)
  paste(
    "Total counts of animals in each stage without factoring in their", 
    "current location/stage. Ex. An animal that leaves the phenotyping",
    "center for DNA extraction after being phenotyped should be counted in",
    "the pool of animals that have been at the phenotyping center and",
    "phenotyped. ONLY", qualifier, "projects included."
  )

# color table cells by whether they are true or false, ignoring first column
color_tf <- function(data)
  formatStyle(data, -1, backgroundColor =
                # green for true, red for false, dark green for empty
                JS(paste("value === 'TRUE' ? '", COLORBLIND_PALETTE[7], 
                         "' : (value === 'FALSE' ? '", COLORBLIND_PALETTE[4], 
                         "' : '", COLORBLIND_PALETTE[3], "')")))

# bold table cells by whether they end with an asterisk, ignoring first column
bold_asterisk <- function(data)
  formatStyle(data, -1, fontWeight = 
                JS("value.at(-1) === '*' ? 'bold' : 'normal'"))