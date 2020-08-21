fluidPage(
  useShinyjs(),
  includeScript(file.path('www', 'enterKey.js')),
  ##for testing#### uncomment if needed
  # textOutput('transitionTester'),
  # textOutput('startTester'),
  uiOutput('mainUI', align = 'center')
  
)
