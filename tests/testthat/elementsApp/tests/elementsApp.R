app <- ShinyDriver$new("../", seed = 42)
app$snapshotInit("elementsApp")

app$setInputs(sidebar = "Output")
app$setInputs(title = "Plot Title")
app$snapshot()
