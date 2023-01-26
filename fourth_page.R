# fourth page items

# dynamically create a list of genotyping reports based of the urls df
output$genotype_reports <- renderUI({
  # non-admin users are blocked from viewing this section
  if (res_auth$user != "PalmerAdmin") {
    fluidRow(
      align = "center", 
      tags$h3("Sorry! You don't have the permissions required to view this.")
    )
  } else {
    # build up a list with each link and stuff them into a panel for display
    mainPanel(tags$ul(
      apply(URLS, 1, function(x) tags$li(tags$a(href = x[1], x[2])))
    ))
  }
})