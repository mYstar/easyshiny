app <- ShinyDriver$new("../", seed = 42)
app$snapshotInit("combinedApp")

app$setInputs(sidebar = "tab-1")
app$snapshot()
app$setInputs(sidebar = "tab-2")
app$snapshot()
app$setInputs(sidebar = "tab-3")
app$snapshot()
