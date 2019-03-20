app <- ShinyDriver$new("../", seed = 42)
app$snapshotInit("boxesApp")

app$setInputs(sidebar = "Output")
app$snapshot()
app$setInputs(title = "Test")
app$snapshot()
