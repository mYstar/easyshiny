app <- ShinyDriver$new("../", seed = 42)
app$snapshotInit("bareboneApp")

app$snapshot()
app$setInputs(sidebarCollapsed = TRUE)
app$snapshot()
app$setInputs(sidebarCollapsed = FALSE)
app$snapshot()
