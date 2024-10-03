################################################################################

# Creating dataframes for the results

################################################################################

# H1.1 Global Strength

p_values_h1.1 <- data.frame(nct_global_mf_fm_p,
                           nct_global_mf_ff_p,
                           nct_global_mf_mm_p)

# H1.2 Global Density

p_values_h1.2 <- data.frame(nct_density_mf_fm_p,
                            nct_density_mf_ff_p,
                            nct_density_mf_mm_p)

# H2 Local Strength Physical Abuse

p_values_h2 <- data.frame(nct_physab_mf_fm_p,
                          nct_physab_mf_ff_p,
                          nct_physab_mf_mm_p)

# H4 Local Strength of Victim Blaming

p_values_h4 <- data.frame(nct_vb_fm_mf_p,
                          nct_vb_mm_mf_p,
                          nct_vb_fm_ff_p,
                          nct_vb_mm_ff_p)

# H5.1 Local Strength Physical Strength/Superiority (male vs. female perp)

p_values_h5.1 <- data.frame(nct_physstr_h5.1_mf_ff_p,
                            nct_physstr_h5.1_mf_fm_p,
                            nct_physstr_h5.1_mm_ff_p,
                            nct_physstr_h5.1_mm_fm_p)

# H5.2 Local Strength Physical Strength/Superiority (MF vs. all)

p_values_h5.2 <- data.frame(nct_physstr_h5.2_mf_fm_p,
                            nct_physstr_h5.2_mf_ff_p,
                            nct_physstr_h5.2_mf_mm_p)

# Saving dataframes ------------------------------------------------------------

write.csv(p_values_h1.1, "output/p_values/p_values_h1.1.csv")
write.csv(p_values_h1.2, "output/p_values/p_values_h1.2.csv")
write.csv(p_values_h2, "output/p_values/p_values_h2.csv")
write.csv(p_values_h4, "output/p_values/p_values_h4.csv")
write.csv(p_values_h5.1, "output/p_values/p_values_h5.1.csv")
write.csv(p_values_h5.2, "output/p_values/p_values_h5.2.csv")