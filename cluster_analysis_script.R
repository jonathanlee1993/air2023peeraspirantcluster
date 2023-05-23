# 1: DATA AND LIBRARIES ----

# * Load libraries ----
library(tidyverse)
library(ggplot2)
library(ggbiplot)
library(recipes)
library(factoextra)

# * Custom functions ----

# * Set seed ----
set.seed(100)

# * Load RDS files ----
# ** dclust_vars_tbl ----
dclust_vars_tbl <- read_rds(
  "F:/Work Stuff/air2023peeraspirantcluster/dclust_vars_tbl.rds"
) %>% 
  as_tibble()

# 2: PCA of the variables used ----

# * Create variable table ----
vars_tbl <- tibble(
  vars_chr    = dclust_vars_tbl %>% 
    select(-c("UnitID", "Institution.Name", "Sector")) %>% 
    colnames(),
  varnums_chr = str_glue("var_{seq_along(vars_chr)}")
)

# * Create a temp table renaming fields to var_{} format for graphing ----
dclust_vars_pca_tbl <- dclust_vars_tbl %>% 
  rename_with(
    function(x) vars_tbl %>% filter(vars_chr == x) %>% pull(varnums_chr),
    -c("UnitID", "Institution.Name", "Sector")
  )

# * Run PCA on the variables ----
dclust_vars_pca <- dclust_vars_pca_tbl %>% 
  select(-c("UnitID", "Institution.Name", "Sector")) %>%
  prcomp(center = T, scale. = T)

# * Create a biplot on the PCA results ----
dclust_vars_pca_biplot <- dclust_vars_pca %>% 
  ggbiplot::ggbiplot(varname.size = 5, var.scale = 1)

# 3: K-MEANS CLUSTER ANALYSIS - RUN AND GET STATISTICS ----

# * Create recipe to apply to data (turn all data into Z-scores) ----
set.seed(100)

dclust_vars_prep_tbl <- recipe(
  UnitID ~ ., 
  data = dclust_vars_tbl %>% 
    select(-c("Institution.Name", "Sector"))
) %>% 
  step_center(all_numeric()) %>% 
  step_scale(all_numeric()) %>% 
  prep() %>% 
  bake(dclust_vars_tbl)

# * Run k-means cluster analysis with k = n clusters ----
clustnum <- 10
kclust_obj <- dclust_vars_prep_tbl %>% 
  select(-UnitID) %>% 
  dist() %>% 
  kmeans(centers = clustnum, nstart = 50)

# * Get R-squared value ----
kclust_r2 <- kclust_obj$betweenss / kclust_obj$totss

# * Get Pseudo F Statistic value ----
kclust_pseudo_f <- (kclust_obj$betweenss / (clustnum-1)) / (kclust_obj$tot.withinss / (dim(dclust_vars_prep_tbl)[1]-clustnum))

# * Get eigenvalues ----
kclust_eigen_tbl <- dclust_vars_prep_tbl %>% 
  select(-UnitID) %>% 
  prcomp(center = T, scale. = T) %>% 
  factoextra::get_eigenvalue()

# 4: K-MEANS CLUSTER ANALYSIS RESULTS ----

# * Get k-means graphing data and join to institution data----
kclust_pts <- factoextra::fviz_cluster(
  kclust_obj, 
  data         = dclust_vars_tbl %>% select(-c("UnitID", "Institution.Name", "Sector")), 
  geom         = c("euclid")
  ) %>%
  .$data %>% 
  as_tibble() %>% 
  select(-name) %>% 
  bind_cols(
    dclust_vars_tbl %>% 
      select(UnitID, Institution.Name)
  )

# * Graph k-means results ----
factoextra::fviz_cluster(
  kclust_obj, 
  data         = dclust_vars_tbl %>% select(-c("UnitID", "Institution.Name", "Sector")), 
  geom         = c("point"),
  ellipse.type = 'none', 
  shape = 21, 
  show.clust.cent = FALSE,
  xlab = 'PC1', 
  ylab = 'PC2') +
  #scale_shape_manual(values = rep(21,10)) +
  ggtitle('Plot 1: Scatter Plot of Institution Clusters', subtitle = 'Standardized Variables') +
  tidyquant::theme_tq() +
  #tidyquant::scale_color_tq() +
  ggplot2::labs(
    caption = str_glue(
      'UWF is found within Cluster {kclust_pts %>% filter(UnitID == 138354) %>% pull(cluster)} 
      and is highlighted as a black dot'
    )
  ) +
  theme(plot.caption = element_text(size = 10,
                                    hjust = 0.5)) +
  geom_point(
    aes(
      x = kclust_pts %>% filter(UnitID == 138354) %>% pull(x),
      y = kclust_pts %>% filter(UnitID == 138354) %>% pull(y)
    ),
    size = 3,
  )

# * Filter peer institution list and check for current peers ----
kclust_pts %>% 
  filter(cluster == kclust_pts %>% filter(UnitID == 138354) %>% pull(cluster)) %>% 
  filter(
    UnitID %in% c(106458, 201441, 433660, 159647, 171571, 149231, 228529, 178420, 199148, 136172, 157951, 237011)
  ) %>% 
  select(UnitID, Institution.Name, cluster)

kclust_pts %>% 
  filter(
    UnitID %in% c(106458, 201441, 433660, 159647, 171571, 149231, 228529, 178420, 199148, 136172, 157951, 237011)
  ) %>% 
  select(UnitID, Institution.Name, cluster)

# * Filter aspirant institution list and check for current aspirants ----
# * (You need to check the visualization and then manually input the cluster(s) of interest)
kclust_pts %>% 
  filter(cluster %in% c(5, 7)) %>% 
  filter(
    UnitID %in% c(197869,150136,170082,145813,232423,203517,204024,164076,199218,216764)
  ) %>% 
  select(UnitID, Institution.Name, cluster)

kclust_pts %>% 
  filter(
    UnitID %in% c(197869,150136,170082,145813,232423,203517,204024,164076,199218,216764)
  ) %>% 
  select(UnitID, Institution.Name, cluster)

# 5: K-MEANS CLUSTER ANALYSIS DATA EXPORT FOR TABLEAU ----

# * Add Type column ----
kclust_pts <- kclust_pts %>% 
  mutate(Type = "K-means")

# * Add Year column ----
year_chr <- "2023"

kclust_pts <- kclust_pts %>% 
  mutate(
    Year    = str_glue("{year_chr}") %>% as.character,
    cluster = as.character(cluster),
    UnitID  = as.character(UnitID)
  )

# * Join to (data, will do this as the need arises w/n the Tableau report) ----
#

# * Add the original 2022 run's data to the file ----
kclust_orig_tbl <- read_csv(
  "Z:/Research On/Peer Institution Cluster Analysis/2-16-22 Run/Cluster Coordinate Files/peer and aspirant.csv"
) %>% 
  as_tibble() %>% 
  select(x, y, coord, cluster, UnitID, Institution_Name, Type) %>% 
  mutate(Year = "2022") %>% 
  filter(Type == "Peer") %>% 
  mutate(
    cluster = as.character(cluster),
    UnitID  = as.character(UnitID)
  ) %>% 
  dplyr::rename(Institution.Name = Institution_Name)

# * Add prior years data to the file ----
# Empty for now but will be updated in the future

# * Combine tables ----
kclust_pts <- kclust_pts %>% 
  bind_rows(kclust_orig_tbl)

# * Export table as csv ----
kclust_pts %>% 
  write.table(
    "Z:/Research On/Peer Institution Cluster Analysis/2023 Updated Analysis Prep/Output/per and aspirant output data.csv",
    row.names = F,
    sep       = ","
  )