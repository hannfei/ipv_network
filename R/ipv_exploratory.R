###############################################################################

# Exploratory analysis

################################################################################

# Run estimations --------------------------------------------------------------

source("R/ipv_networks.R") 

# Strength of GN01 across networks----------------------------------------------

# FF

ipv_ff_GN01 <- centrality_auto(ipv_ff_ggm, 
                               weighted = TRUE, 
                               signed = TRUE)

ipv_ff_GN01 <- ipv_ff_GN01$node.centrality

ipv_ff_GN01 <- ipv_ff_GN01["GN01", "Strength"]


# FM

ipv_fm_GN01 <- centrality_auto(ipv_fm_ggm, 
                               weighted = TRUE, 
                               signed = TRUE)

ipv_fm_GN01 <- ipv_fm_GN01$node.centrality

ipv_fm_GN01 <- ipv_fm_GN01["GN01", "Strength"]


# MF

ipv_mf_GN01 <- centrality_auto(ipv_mf_ggm, 
                               weighted = TRUE, 
                               signed = TRUE)

ipv_mf_GN01 <- ipv_mf_GN01$node.centrality

ipv_mf_GN01 <- ipv_mf_GN01["GN01", "Strength"]

# MM

ipv_mm_GN01 <- centrality_auto(ipv_mm_ggm, 
                               weighted = TRUE, 
                               signed = TRUE)

ipv_mm_GN01 <- ipv_mm_GN01$node.centrality

ipv_mm_GN01 <- ipv_mm_GN01["GN01", "Strength"]

# Store in dataframe

ipv_GN01 <- data_frame(ipv_ff_GN01, 
                       ipv_fm_GN01,
                       ipv_mf_GN01,
                       ipv_mm_GN01)

rownames(ipv_GN01) <- c("Strength")

colnames(ipv_GN01) <- c("FF", "FM", "MF", "MM")

write.csv(ipv_GN01, "output/ipv_GN01.csv")

# Extract Edge Weights --------------------------------------------------------------

# Create list of data frames

ipv_ggm <- list(ipv_ff_ggm, 
                ipv_fm_ggm, 
                ipv_mf_ggm,
                ipv_mm_ggm)

names(ipv_ggm) <- c("FF", "FM", "MF", "MM")

# Function 

edge_weights <- function(ipv_ggm, row, col) {
  
  data <- data.frame()
  
  for (i in seq_along(ipv_ggm)) {
    
    ggm <- ipv_ggm[[i]]
    
    if (row %in% rownames(ggm) && col %in% colnames(ggm)) 
       {
        edge <- ggm[row, col]  
      } else  
        {
        edge <- "-"   
      }
    
    rowname <- names(ipv_ggm)[i]
    
    colname <- paste(row, col, sep = ", ")
    
    edges <- data.frame(edges = edge)
    
    rownames(edges) <- rowname
    
    colnames(edges) <- colname
    
    data <- rbind(data, edges)
    
  }
    
  return(data)
  
}

# Output -----------------------------------------------------------------------


ipv_GN04_PT02 <- edge_weights(ipv_ggm, "GN04", "PT02")
ipv_GN03_GN04 <- edge_weights(ipv_ggm, "GN03", "GN04")
ipv_AE02_MP02 <- edge_weights(ipv_ggm, "AE02", "MP02")
ipv_PT09_AE03 <- edge_weights(ipv_ggm, "PT09", "AE03")
ipv_AE02_VT08 <- edge_weights(ipv_ggm, "AE02", "VT08")
ipv_GN03_VB01 <- edge_weights(ipv_ggm, "GN03", "VB01")
ipv_AE01_PT11 <- edge_weights(ipv_ggm, "AE01", "PT11")
ipv_AE02_VT02 <- edge_weights(ipv_ggm, "AE02", "VT02")
ipv_CT03_AE01 <- edge_weights(ipv_ggm, "CT03", "AE01")
ipv_AE01_AE03 <- edge_weights(ipv_ggm, "AE01", "AE03")
ipv_GN03_VT07 <- edge_weights(ipv_ggm, "GN03", "VT07")
ipv_AE07_VT10 <- edge_weights(ipv_ggm, "AE07", "VT10")
ipv_AE03_PT09 <- edge_weights(ipv_ggm, "AE03", "PT09")


ipv_edges <- data.frame(ipv_GN04_PT02,
                       ipv_GN03_GN04,
                       ipv_AE02_MP02,
                       ipv_PT09_AE03,
                       ipv_AE02_VT08,
                       ipv_GN03_VB01,
                       ipv_AE01_PT11,
                       ipv_AE02_VT02,
                       ipv_CT03_AE01,
                       ipv_AE01_AE03,
                       ipv_GN03_VT07,
                       ipv_AE07_VT10,
                       ipv_AE03_PT09)


write.csv(ipv_edges, "output/ipv_edges.csv")


                              