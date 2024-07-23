### Process output of bcftools stats -s- 
### Generate list of data frames representing the stats section
### Works with bcftools stats v1.20
### Author: Edoardo Giacopuzzi

library(stringr)
library(dplyr)
library(tidyr)
library(data.table)

BCFTOOLS_TAGS <- c("AF",
          "DP",
          "HWE",
          "ID",
          "IDD",
          "PSC",
          "PSI",
          "QUAL",
          'SN',
          "ST",
          "SiS",
          "TSTV",
          "VAF")

checkNumbers <- function(values) {
  if (length(grep("^-?[0-9.]+$", values, perl=T)) == length(values)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

readVCFstat <- function(stat_file, sep="\t", tags=BCFTOOLS_TAGS, fileID=FALSE) {
  message("Works with bcftools stats (1.20)")
  df <- fread(file=params$vcf_stats, sep="\t", header = F, fill=T)
  res <- list()
  for (t in tags) {
    message("Processing ", t)
    header = df[grepl(paste0("# ",t,"\t"), df$V1), V1]
    header = unlist(strsplit(header, "\t"))
    header = gsub("# ", "", header)
    header = gsub("\\[[0-9]+\\]", "", header)
    header = gsub(" ", "_", header)
    header = gsub("-", "_", header)
    header = gsub("[\\(\\)]","_", header)
    res[[t]] <- df[grepl(paste0("^",t,"\t"), df$V1)] 
    res[[t]] <- as.data.table(res[[t]] %>% separate(V1, into = header, sep = "\t"))
    res[[t]] <- as.data.table(res[[t]] %>% mutate_if(checkNumbers,as.numeric))
    if (fileID != FALSE) {
      if (!is.null(res[[t]])) {res[[t]]$dataset <- fileID}
    }
  }

  return(res) 
}

