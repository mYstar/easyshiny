app <- ShinyDriver$new("../", seed = 42)
app$snapshotInit("zoomIn")

app$setInputs(sidebar = "Output")
app$setInputs(modal_output_4 = TRUE)
app$snapshot()
