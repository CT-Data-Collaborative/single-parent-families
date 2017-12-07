library(dplyr)
library(datapkg)
library(tidyr)

##################################################################
#
# Processing Script for Single Parent Families
# Created by Jenna Daly
# On 09/11/2017
#
##################################################################

#Run getData script to derive ACS totals
source('./scripts/getData.R')

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))

#Merge in FIPS with ACS data
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

sp_families <- merge(get_data, fips, by = "FIPS", all.y=T)

#Convert from long to wide to calculate rates
sp_families_value <- sp_families %>% select(-MOE)
sp_families_value <- spread(sp_families_value, Group, Value)

sp_families_moe <- sp_families %>% select(-Value)
sp_families_moe <- spread(sp_families_moe, Group, MOE)

#Append either Value or MOE to columns so we can merge columns
names(sp_families_value)[4:9] <- gsub("$", " Value", names(sp_families_value)[4:9])
names(sp_families_moe)[4:9] <- gsub("$", " MOE", names(sp_families_moe)[4:9])

sp_families_wide <- merge(sp_families_value, sp_families_moe, by = c("FIPS", "Year", "Town"))

#############################################################################################
# Helper function for MOE
calcMOE <- function(x, y, moex, moey) {
  moex2 <- moex^2
  moey2 <- moey^2
  d <- x/y
  d2 <- d^2
  
  radicand <- ifelse(
    moex2 < (d2 * moey2),
    moex2 + (d2 * moey2),
    moex2 - (d2 * moey2)
  )
  
  return(sqrt(radicand)/y)
}

# calculate group rates with total denominators,
# keep MOES, calculating appropriately
sp_families_wide_calc <- sp_families_wide %>%
  mutate(`% Families, Single-parent` = ((`Total Single-Parent Families Value` / `Total Families Value`)*100),
         `MOE of % Families, Single-parent` = (calcMOE(`Total Single-Parent Families Value`, 
                                                       `Total Families Value`, 
                                                       `Total Single-Parent Families MOE`, 
                                                       `Total Families MOE`)),
         `% Families, Single-parent, without Children of Their Own` = ((`Total Single-Parent Families without Children of Their Own Value` / `Total Families Value`)*100),  
         `MOE of % Families, Single-parent, without Children of Their Own` = (calcMOE(`Total Single-Parent Families without Children of Their Own Value`, 
                                                                                      `Total Families Value`, 
                                                                                      `Total Single-Parent Families without Children of Their Own MOE`, 
                                                                                      `Total Families MOE`)),
         `% Families, Single-parent, with Children of Their Own` = ((`Total Single-Parent Families with Children of Their Own Value` / `Total Families Value`)*100),
         `MOE of % Families, Single-parent, with Children of Their Own` = (calcMOE(`Total Single-Parent Families with Children of Their Own Value`, 
                                                                                   `Total Families Value`, 
                                                                                   `Total Single-Parent Families with Children of Their Own MOE`, 
                                                                                   `Total Families MOE`)))
options(scipen=999)

sp_families_long <- gather(sp_families_wide_calc, Group, Value, 4:21, factor_key = FALSE)

sp_families_long$`Measure Type` <- "Number"
sp_families_long$`Measure Type`[which(grepl("%", sp_families_long$Group))] <- "Percent"

sp_families_long$Variable <- "Single-Parent Families"
sp_families_long$Variable[which(grepl("MOE", sp_families_long$Group))] <- "Margins of Error"

sp_families_long$`Family Type` <- "All"
sp_families_long$`Family Type`[which(grepl("Single", sp_families_long$Group))] <- "Single-Parent"

sp_families_long$`Child Relation` <- "All"
sp_families_long$`Child Relation`[which(grepl("without", sp_families_long$Group))] <- "No Own Children"
sp_families_long$`Child Relation`[which(grepl("with ", sp_families_long$Group))] <- "Own Children"

sp_families_long$Group <- NULL

sp_families_long$Value <- round(sp_families_long$Value, 2)

sp_families_long <- sp_families_long %>% 
  select(Town, FIPS, Year, `Family Type`, `Child Relation`, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, `Measure Type`, Variable, `Family Type`, `Child Relation`)

# Write to File
write.table(
  sp_families_long,
  file.path(getwd(), "data", "single_parent_families_2016.csv"),
  sep = ",",
  row.names = F
)

