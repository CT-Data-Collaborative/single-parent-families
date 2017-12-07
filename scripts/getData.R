library(acs)
source('./scripts/acsHelpers.R')

# ACS B23008
# Get geography object for CT and subcounty divisions
acsdata <- getACSData(
  getCTGeos("town"),
  yearList = 2010:2016,
  get_this_one = "B11003"
)

get_data <- data.table()
for (data in acsdata) {
  year <- as.numeric(data@endyear)
  year <- paste(year-4, year, sep="-")
  
  #based on the VD0x <- the x is what you want as the range, signifies which columns you want to sum
  total_families <- acsSum(data, 1, "Total Families") 
  total_families_own_children <- acsSum(data, c(3,10,16), "Total Families with Children of Their Own") 
  total_families_no_own_children <- acsSum(data, c(7,14,20), "Total Families without Children of Their Own") 
  total_single_parent_families <- acsSum(data, 8, "Total Single-Parent Families")
  total_single_parent_own_children <- acsSum(data, c(10,16), "Total Single-Parent Families with Children of Their Own") 
  total_single_parent_no_own_children <- acsSum(data, c(14,20), "Total Single-Parent Families without Children of Their Own") 

  datafips <- data.table(fips = getACSFips(data))
  
  estimates <- data.table(
    FIPS = datafips$fips,
    Year = year,
    estimate(total_families),
    estimate(total_families_own_children),
    estimate(total_families_no_own_children),
    estimate(total_single_parent_families),
    estimate(total_single_parent_own_children),
    estimate(total_single_parent_no_own_children)
  )
  
  names(estimates)[names(estimates) == "HD01_VD01.Estimate; Total:"] <- "Total Families"
  names(estimates)[names(estimates) == "HD01_VD08.Estimate; Other family:"] <- "Total Single-Parent Families"
  
  estimates <- melt(
    estimates,
    id.vars = c("FIPS", "Year"),
    variable.name = "Group",
    variable.factor = F,
    value.name = "Value",
    value.factor = F
  )
  
  moes <- data.table(
    FIPS = datafips$fips,
    Year = year,
    #multiplying by 1.645 assigns confidence level to 90%
    # SE = MOE / 1.645
    standard.error(total_families) * 1.645,
    standard.error(total_families_own_children) * 1.645,
    standard.error(total_families_no_own_children) * 1.645,
    standard.error(total_single_parent_families) * 1.645,
    standard.error(total_single_parent_own_children) * 1.645,
    standard.error(total_single_parent_no_own_children) * 1.645
  )
  
  names(moes)[names(moes) == "HD01_VD01.Estimate; Total:"] <- "Total Families"
  names(moes)[names(moes) == "HD01_VD08.Estimate; Other family:"] <- "Total Single-Parent Families"
  
  
  moes <- melt(
    moes,
    id.vars = c("FIPS", "Year"),
    variable.name = "Group",
    variable.factor = F,
    value.name = "MOE",
    value.factor = F
  )
  
  setkey(estimates, FIPS, Year, Group)
  setkey(moes, FIPS, Year, Group)
  
  get_data <- rbind(get_data, estimates[moes])
}

get_data <- get_data[get_data$FIPS != "0900100000",]

# Write to File
write.table(
  get_data,
  file.path(getwd(), "raw", "group_totals.csv"),
  sep = ",",
  row.names = F
)