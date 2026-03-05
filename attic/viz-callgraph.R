library(biocGraph)
library(CodeDepends)
tf_obj <- objects("package:tidyfun")[str_detect(
  objects("package:tidyfun"),
  "tf"
)]

g <- makeCallGraph(tf_obj)
g <- layoutGraph(g, layoutType = "circo")
tooltips <- nodes(g)
names(tooltips) <- nodes(g)


# files to save
fhtml = "callgraph.html"
fpng = "callgraph.png"
## Open plot device
width = height = 1444
png(fpng, width = width, height = height)
par(mai = rep(0, 4))
## Layout and render
lg = agopen(g, name = "callgraph")
plot(lg)

## Write an HTML file with the image map
con = file(fhtml, open = "wt")
writeLines("<html><head><title>Click Me</title></head><body>\n", con)
imageMap(
  lg,
  con,
  fpng,
  tags = list(TITLE = tooltips),
  width = width,
  height = height
)
writeLines("</body></html>", con)
close(con)
dev.off()

browseURL(fhtml)
