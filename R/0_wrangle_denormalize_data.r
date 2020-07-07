### Nicholas Spyrison for VAST Challenge 2020 MC1
### July 2020

##### PREAMBLE =====
library(tictoc); library(beepr); library(tidyverse); library(lubridate)

subset_nms <- c("Template", paste0("Suspect", 1:5))

.template_filepath <- "./Submissions/MC1/data/CGCS-Template.csv"
.suspect_filepath_vect <- paste0("./Submissions/MC1/data/Q1-Graph", 1:5, ".csv")


### LOOKUP TABLES =====
eType_tbl <- 
  data.frame("eType" = 0:6,
             "eName" = c("phone",
                         "email",
                         "sell",
                         "purchase",
                         "co-authorship",
                         "demographic, financial",
                         "travel"),
             "Weight_unit" = c("count",
                               "count",
                               "value",
                               "value",
                               "authorship fraction",
                               "value",
                               "duration [days]")
  )

.demographic_filepath <- "./Submissions/MC1/data/DemographicCategories.csv"
demographic_tbl <- read.csv2(.demographic_filepath, 
                             sep = (","),
                             header = T, 
                             check.names = T, 
                             stringsAsFactors = F,
                             na.strings = ""
)
.demographic_tbl_Target <- select(demographic_tbl, 
                                  Target = NodeID, 
                                  TargetDemographicCategory = Category)
.demographic_tbl_Source <- select(demographic_tbl, 
                                  Source = NodeID, 
                                  SourceDemographicCategory = Category)

.nodeType_graphdata_filepath   <- "./Submissions/MC1/data/CGCS-GraphData-NodeTypes.csv"
.nodeType_template_filepath    <- "./Submissions/MC1/data/CGCS-Template-NodeTypes.csv"
.nodeType_description_filepath <- "./Submissions/MC1/data/NodeTypeDescriptions.csv"
.nodeType_graphdata <- read.csv2(.nodeType_graphdata_filepath, 
                                 sep = (","),
                                 header = T, 
                                 check.names = T, 
                                 stringsAsFactors = F,
                                 na.strings = ""
)
.nodeType_template <- read.csv2(.nodeType_template_filepath, 
                                sep = (","),
                                header = T, 
                                check.names = T, 
                                stringsAsFactors = F,
                                na.strings = ""
)
.nodeType_union <- dplyr::union(.nodeType_graphdata, .nodeType_template)
.nodeType_description <- read.csv2(.nodeType_description_filepath, 
                                   sep = (","),
                                   header = T, 
                                   check.names = T, 
                                   stringsAsFactors = F,
                                   na.strings = ""
)
nodeType_tbl <- left_join(.nodeType_union, .nodeType_description, by = "NodeType")
unique(nodeType_tbl$NodeID)
.nodeType_tbl_Source <- select(nodeType_tbl,
                               Source = NodeID,
                               SourceNodeType = NodeType,
                               SourceDescription = Description,
                               SourceNodeTypeUsedIn = Used.in
)
.nodeType_tbl_Target <- select(nodeType_tbl,
                               Target = NodeID,
                               TargetNodeType = NodeType,
                               TargetDescription = Description,
                               TargetNodeTypeUsedIn = Used.in
)

save(file = "./Data/denormalizationLookupTables.rds",
     eType_tbl,
     demographic_tbl,
     .demographic_tbl_Target,
     .demographic_tbl_Source,
     nodeType_tbl,
     .nodeType_tbl_Source,
     .nodeType_tbl_Target)
cat("NS: saved ./Data/denormalizationLookupTables.rds \n")
## Read the lookup tables into the global envirnment:
#### eType_tbl,
#### demographic_tbl, .demographic_tbl_Target, .demographic_tbl_Source,
#### nodeType_tbl, .nodeType_tbl_Source, .nodeType_tbl_Target
# load(file = "./Data/denormalizationLookupTables.rds")


### FORMATING FUNCTIONS ======
ns_format_df <- function(dat){
  dat$Weight <- as.numeric(dat$Weight) ## Was string of a numeric.
  dat$Datetime = lubridate::as_datetime(dat$Time) + lubridate::years(55)
  dat <- left_join(dat, eType_tbl, by = "eType")
  dat <- left_join(dat, .demographic_tbl_Source, by = "Source")
  dat <- left_join(dat, .demographic_tbl_Target, by = "Target")
  dat <- left_join(dat, .nodeType_tbl_Source, by = "Source")
  dat <- left_join(dat, .nodeType_tbl_Target, by = "Target")
  dat[dat == -99] <- NA
  
  ## reordered, esp for: 'to/Source' and 'from/Target' first.
  dat <- select(dat, 
                Source, 
                Target,
                DataSource,
                Datetime,
                ## POSIXt ("Unix timestamps"), but shifted 55 years forward from 1970 to 2025 indexing
                SecondsAfterStart = Time, ## Relative to 2025 Jan 1.
                eType, 
                eName,
                Weight,
                Weight_unit,
                SourceNodeType,
                SourceDescription,
                SourceNodeTypeUsedIn,
                TargetDemographicCategory,
                SourceLocation,
                SourceLatitude,
                SourceLongitude,
                TargetNodeType,
                TargetDescription,
                TargetNodeTypeUsedIn,
                TargetDemographicCategory,
                TargetLocation,
                TargetLatitude,
                TargetLongitude)
  
  ## return tibble (1 row is 1 edges of the network)
  as_tibble(dat)
}



### LOAD AND DENORMALIZE TEMPLATE AND SUSPECTS =====
dat_templateSuspect <- read.csv2(.template_filepath, 
                                 sep = (","),
                                 header = T, 
                                 check.names = T, 
                                 stringsAsFactors = F,
                                 na.strings = "",
)
dat_templateSuspect$DataSource <- "Template"

for (i in 1:length(.suspect_filepath_vect)){
  .dat <- read.csv2(.suspect_filepath_vect[i], 
                    sep = (","),
                    header = T, 
                    check.names = T, 
                    stringsAsFactors = F,
                    na.strings = "",
  )
  .dat$DataSource <- paste0("Suspect", i)
  dat_templateSuspect <- rbind(dat_templateSuspect, .dat)
}

dat_templateSuspect <- ns_format_df(dat_templateSuspect)
table(dat_templateSuspect$DataSource)

dat_templateSuspect
skimr::skim(dat_templateSuspect)

df_templateSuspect <- as.data.frame(dat_templateSuspect)
save(df_templateSuspect, file = "./Data/df_templateSuspect.rds")
cat("NS: saved ./Data/df_templateSuspect.rds \n")
# ## Reads file, creating the data frame object "df_templateSuspect" 
# #### Denormalized, but unaggregated data frame of the network data for VAST challenge 2020 MC1 
# load(file = "./Data/df_templateSuspect.rds") 

