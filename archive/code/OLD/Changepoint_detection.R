library(cpm)

detectChangePoint(data_out_list[[5]]$buffer, cpmType = "Student")

processStream(data_out_list[[5]]$buffer, cpmType = "Student")


