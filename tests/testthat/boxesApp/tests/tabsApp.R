app <- ShinyDriver$new("../", seed = 42)
app$snapshotInit("tabsApp")

app$setInputs(sidebar = "Text")
app$snapshot()
app$setInputs(sidebar = "Plot")
app$snapshot()
app$setInputs(title = "Test")
app$snapshot()
