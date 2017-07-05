iris

tabplot::tableplot(iris, select_string = c("Species", colnames(iris)[1:4]),
                   numPals = "PRGn",
                   pals = list("Set1", "Set5", "Set6",  "HCL1", "HCL3"),
                   fontsize = 18)

