#
# Load the data used globally in the different experiments for DWR
#
LOADERS_PATH <- "munge/loaders/"
DATASET <- "ESWA"

print(paste("Loading Dataset... - ",DATASET,"-"))

if (DATASET == "ESWA") {
  source(file = paste(LOADERS_PATH,"ESWA_munge.R",sep = ""))
}

print(paste("Loaded Dataset - ",DATASET,"-"))