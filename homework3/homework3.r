# Install packages once in the R Console if needed:
# install.packages(c("ggplot2","dplyr","tidyr","readr","sf"))

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)

out <- "homework3_output"
dir.create(out,showWarnings=FALSE)
options(timeout=300)

vehicles <- ggplot2::mpg
gems <- ggplot2::diamonds


# Problem 1

# Part A

p_engine <- ggplot(vehicles,aes(x=displ,y=hwy)) +
  geom_point(aes(color=drv),alpha=0.6,na.rm=TRUE) +
  geom_smooth(method="lm",se=FALSE,color="blue",na.rm=TRUE) +
  facet_wrap(~drv,nrow=1) +
  coord_cartesian(xlim=c(1.5,5)) +
  labs(x="Engine displacement (litres)",
       y="Highway fuel economy (mpg)",color="drv") +
  theme_bw()

print(p_engine)
ggsave(file.path(out,"p_engine.png"),p_engine,width=10,height=4)

# Faceting puts each drive type in its own panel.
# geom_smooth fits one line in each panel, so there are three lines.
# The color mapping only applies to the points.
# Use the full dataset for the fitted lines, as the instructions require.
# coord_cartesian shows the reference's x range without excluding observations.
# The reference appears to fit only the displayed range, so the lines differ.


# Part B

p_drive_count <- ggplot(vehicles,aes(x=drv,fill=factor(cyl))) +
  geom_bar() +
  labs(x="drv",y="count",fill="factor(cyl)") +
  theme_bw()

p_drive_prop <- ggplot(vehicles,aes(x=drv,fill=factor(cyl))) +
  geom_bar(position="fill") +
  labs(x="drv",y="Proportion within drive type",fill="factor(cyl)") +
  theme_bw()

print(p_drive_count)
print(p_drive_prop)
print(count(vehicles,drv))

ggsave(file.path(out,"p_drive_count.png"),p_drive_count,width=6,height=4)
ggsave(file.path(out,"p_drive_prop.png"),p_drive_prop,width=6,height=4)

# position="fill" divides the counts by the total for each drive type.
# Each bar adds to 1 even when the drive types have different sample sizes.


# Part C

# The assignment does not give a cutoff for the smaller table.
# I use diamonds under 3 carats for both histograms.
gems_small <- gems |>
  filter(!is.na(carat),carat<3)

print(tibble(retained=nrow(gems_small),excluded=nrow(gems)-nrow(gems_small)))

p_fine <- ggplot(gems_small,aes(x=carat)) +
  geom_histogram(binwidth=0.01,boundary=0,closed="right") +
  coord_cartesian(xlim=c(0,3)) +
  labs(title="Diamond carats: bin width 0.01",x="Carat",y="Count") +
  theme_bw()

p_coarse <- ggplot(gems_small,aes(x=carat)) +
  geom_histogram(binwidth=0.5,boundary=0,closed="right") +
  coord_cartesian(xlim=c(0,3)) +
  labs(title="Diamond carats: bin width 0.5",x="Carat",y="Count") +
  theme_bw()

print(p_fine)
print(p_coarse)

print(gems |>
  filter(carat %in% c(0.99,1.00)) |>
  count(carat))

ggsave(file.path(out,"p_fine.png"),p_fine,width=7,height=4)
ggsave(file.path(out,"p_coarse.png"),p_coarse,width=7,height=4)

# Narrow bins show spikes at common sizes, such as 1.00 carat.
# They also show the difference between 0.99 and 1.00 carat.
# The wider bins combine these sizes and hide the sharp changes.


# Problem 2

# Part A

# Official sources:
# https://data.cityofchicago.org/d/x8fc-8rcq
# https://data.cityofchicago.org/d/igwz-8jzy
# Terms: https://www.chicago.gov/city/en/narr/foia/data_disclaimer.html
# Both datasets say SEE_TERMS_OF_USE; public access is not a separate license.
# These describe published locations and boundaries, not guaranteed live service.

library_url <- paste0(
  "https://data.cityofchicago.org/resource/x8fc-8rcq.csv?",
  "$select=:id%20as%20site_id,branch_%20as%20facility_name,",
  "location.latitude%20as%20lat,location.longitude%20as%20lon,address",
  "&$order=:id&$limit=50000"
)
area_url <- paste0("https://data.cityofchicago.org/resource/igwz-8jzy.geojson?",
                   "$order=area_numbe&$limit=50000")

library_file <- file.path(out,"libraries_raw.csv")
boundary_file <- file.path(out,"community_areas_raw.geojson")

# Keep the downloaded raw files unchanged when the script is run again.
if(!file.exists(library_file)){
  download.file(library_url,library_file,mode="wb")
}
if(!file.exists(boundary_file)){
  download.file(area_url,boundary_file,mode="wb")
}

data_sources <- tibble(
  dataset=c("Library locations","Community areas"),
  publisher=c("Chicago Public Library","City of Chicago"),
  source_url=c("https://data.cityofchicago.org/d/x8fc-8rcq",
               "https://data.cityofchicago.org/d/igwz-8jzy"),
  download_url=c(library_url,area_url),
  downloaded_on=as.character(as.Date(c(file.info(library_file)$mtime,
                                     file.info(boundary_file)$mtime))),
  terms="https://www.chicago.gov/city/en/narr/foia/data_disclaimer.html",
  license="SEE_TERMS_OF_USE"
)
print(data_sources)
write_csv(data_sources,file.path(out,"data_sources.csv"))
date_used <- paste(data_sources$downloaded_on,collapse=" / ")

# Save the metadata too, including the publisher's update information.
for(id in c("x8fc-8rcq","igwz-8jzy")){
  metadata_file <- file.path(out,paste0(id,"_metadata.json"))
  if(!file.exists(metadata_file)){
    download.file(paste0("https://data.cityofchicago.org/api/views/",id,".json"),
                  metadata_file,mode="wb")
  }
}

libraries_raw <- read_csv(library_file,show_col_types=FALSE)
districts_raw <- st_read(boundary_file,quiet=TRUE)

# Compare with API totals so a download cannot silently stop at its row limit.
library_total <- read_csv(paste0("https://data.cityofchicago.org/resource/",
                                 "x8fc-8rcq.csv?$select=count(*)"),
                          show_col_types=FALSE)
area_total <- read_csv(paste0("https://data.cityofchicago.org/resource/",
                              "igwz-8jzy.csv?$select=count(*)"),
                       show_col_types=FALSE)
stopifnot(nrow(libraries_raw)==library_total[[1]][1],
          nrow(districts_raw)==area_total[[1]][1])
# If cached data and live totals differ, inspect the change before downloading again.

libraries <- libraries_raw |>
  mutate(
    site_id=as.character(site_id),
    facility_name=trimws(facility_name),
    reason=case_when(
      is.na(facility_name) | facility_name=="" ~ "missing name",
      !is.finite(lon) | !is.finite(lat) ~ "missing coordinates",
      lon < -180 | lon > 180 | lat < -90 | lat > 90 ~ "invalid coordinates",
      TRUE ~ "kept"
    )
  )

cat("Before cleaning:",nrow(libraries),"\n")
print(count(libraries,reason))
cat("Repeated coordinates kept:",sum(duplicated(libraries[,c("lon","lat")])),"\n")
cat("Repeated names:",sum(duplicated(libraries$facility_name)),"\n")
print(libraries |> filter(reason!="kept"))
write_csv(libraries,file.path(out,"cleaning.csv"))

sites_m <- libraries |>
  filter(reason=="kept") |>
  select(site_id,facility_name,lon,lat) |>
  st_as_sf(coords=c("lon","lat"),crs=4326) |>
  st_transform(26916)

cat("After cleaning:",nrow(sites_m),"\n")
stopifnot(nrow(sites_m)>=20,!anyNA(sites_m$site_id),
          all(sites_m$site_id!=""),!anyDuplicated(sites_m$site_id))

# EPSG:26916 is NAD83 / UTM zone 16N, with distances in meters.
# Shared coordinates are retained because they can represent different facilities.
st_write(sites_m,file.path(out,"sites_m.gpkg"),delete_dsn=TRUE,quiet=TRUE)


# Part B

zone500 <- st_union(st_buffer(sites_m,500))
zone1000 <- st_union(st_buffer(sites_m,1000))

buffer_summary <- data.frame(
  radius_m=c(500,1000),
  union_area_km2=c(as.numeric(st_area(zone500)),
                   as.numeric(st_area(zone1000)))/1000000
)
print(buffer_summary)
stopifnot(buffer_summary$union_area_km2[2]>=buffer_summary$union_area_km2[1])
write_csv(buffer_summary,file.path(out,"buffer_summary.csv"))
st_write(st_sf(radius_m=500,geometry=zone500),
         file.path(out,"buffer_500.gpkg"),delete_dsn=TRUE,quiet=TRUE)
st_write(st_sf(radius_m=1000,geometry=zone1000),
         file.path(out,"buffer_1000.gpkg"),delete_dsn=TRUE,quiet=TRUE)

# st_union counts overlapping buffer areas only once.
# The buffers are not clipped, so area outside Chicago is included.
# These areas measure straight-line proximity, not walking routes or access.
# In this download, the union areas are about 64.31 and 250.11 square kilometers.


# Part C

p_sites <- ggplot() +
  geom_sf(data=sites_m,aes(color="Libraries")) +
  labs(title="Chicago public libraries",color="Sites",
       caption=paste("Chicago Public Library, downloaded",date_used)) +
  theme_bw()

p_buffers <- ggplot() +
  geom_sf(data=zone500,aes(fill="500 m area"),alpha=0.4) +
  geom_sf(data=sites_m,aes(color="Libraries"),size=0.8) +
  labs(title="Libraries and combined 500 m buffers",fill="Buffer",color="Sites",
       caption=paste("Chicago Public Library, downloaded",date_used)) +
  theme_bw()

print(p_sites)
print(p_buffers)
ggsave(file.path(out,"p_sites.png"),p_sites,width=7,height=7)
ggsave(file.path(out,"p_buffers.png"),p_buffers,width=7,height=7)


# Problem 3

# Part A

districts <- districts_raw
names(districts) <- tolower(names(districts))
cat("Raw areas:",nrow(districts),"\n")
cat("Invalid areas:",sum(!st_is_valid(districts)),"\n")
cat("Empty areas:",sum(st_is_empty(districts)),"\n")
stopifnot(all(c("area_numbe","community") %in% names(districts)))
districts <- districts |>
  mutate(district_id=as.character(area_numbe),
         district_name=as.character(community))

print(sum(is.na(districts$district_id) | districts$district_id==""))
print(sum(duplicated(districts$district_id)))

districts <- districts |>
  st_make_valid() |>
  filter(!st_is_empty(geometry)) |>
  select(district_id,district_name) |>
  st_transform(26916)

stopifnot(nrow(districts)>=5,!anyNA(districts$district_id),
          !anyDuplicated(districts$district_id))
districts$area_km2 <- as.numeric(st_area(districts))/1000000
stopifnot(all(districts$area_km2>0))

st_write(districts,file.path(out,"districts_m.gpkg"),delete_dsn=TRUE,quiet=TRUE)

p_area_sites <- ggplot() +
  geom_sf(data=districts,fill="white") +
  geom_sf(data=sites_m,aes(color="Libraries"),size=0.7) +
  labs(title="Libraries and Chicago community areas",color="Sites",
       caption=paste("City of Chicago datasets, accessed",date_used)) +
  theme_bw()
print(p_area_sites)
ggsave(file.path(out,"areas_and_sites.png"),p_area_sites)

# The sources may have different update dates.
# The library sites and community areas use the same projected CRS.
# Saved metadata reports library updates in September 2026 and boundaries in April 2025.

# Part B

matches <- st_intersects(sites_m,districts)
site_match_audit <- data.frame(
  site_id=sites_m$site_id,
  n_area_matches=lengths(matches),
  district_id=sapply(matches,function(x){
    if(length(x)==1) districts$district_id[x] else NA_character_
  })
)
write_csv(site_match_audit,file.path(out,"site_match_audit.csv"))

print(table(factor(site_match_audit$n_area_matches,
                   levels=0:max(2,max(site_match_audit$n_area_matches)))))
print(site_match_audit |> filter(n_area_matches!=1))

assigned <- site_match_audit |>
  filter(n_area_matches==1) |>
  count(district_id,name="n_sites")

district_counts <- districts |>
  st_drop_geometry() |>
  left_join(assigned,by="district_id") |>
  mutate(n_sites=replace_na(n_sites,0L),
         sites_per_km2=n_sites/area_km2) |>
  select(district_id,district_name,n_sites,area_km2,sites_per_km2)

stopifnot(sum(district_counts$n_sites)==sum(site_match_audit$n_area_matches==1))
print(as_tibble(district_counts),n=Inf)
write_csv(district_counts,file.path(out,"district_counts.csv"))

# Areas with no libraries still have a count of zero.
# Only libraries matching exactly one area are counted.

# Part C

map_areas <- districts |>
  left_join(district_counts |>
              select(district_id,n_sites,sites_per_km2),by="district_id")

box <- st_bbox(map_areas)
unassigned <- sum(site_match_audit$n_area_matches!=1)
caption <- paste("City of Chicago datasets, accessed",date_used,
                 "| Not assigned:",unassigned)

p_count <- ggplot(map_areas) +
  geom_sf(aes(fill=n_sites)) +
  scale_fill_gradient(low="white",high="blue",na.value="grey70",
                      name="Library count") +
  coord_sf(xlim=box[c("xmin","xmax")],ylim=box[c("ymin","ymax")]) +
  labs(title="Libraries per community area",caption=caption) +
  theme_bw()

p_density <- ggplot(map_areas) +
  geom_sf(aes(fill=sites_per_km2)) +
  scale_fill_gradient(low="white",high="darkgreen",na.value="grey70",
                      name="Libraries / km2") +
  coord_sf(xlim=box[c("xmin","xmax")],ylim=box[c("ymin","ymax")]) +
  labs(title="Libraries per square kilometer",caption=caption) +
  theme_bw()

print(p_count)
print(p_density)
ggsave(file.path(out,"p_count.png"),p_count)
ggsave(file.path(out,"p_density.png"),p_density)

cat("Most libraries:\n")
print(district_counts |> arrange(desc(n_sites)) |> head(5))
cat("Highest density:\n")
print(district_counts |> arrange(desc(sites_per_km2)) |> head(5))

# Counts show the total libraries, while density accounts for area size.
# Smaller areas can have higher densities even if they have fewer libraries.
# In this download, five areas tie for the highest count at 3 libraries.
# Douglas has the highest density at about 0.468 libraries per square kilometer,
# even though it has only 2 libraries.
# White areas have zero libraries; missing values would be gray.

cat("Homework 3 finished. Files saved in:",normalizePath(out),"\n")
