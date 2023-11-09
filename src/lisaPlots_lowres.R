## -----------------------------------------------------------------------------
## Evidence classification plots ## --------------------------------------------
## -----------------------------------------------------------------------------

source("src/ms.R")

## Wrangle ## ------------------------------------------------------------------

ec_df <- summsa2all %>% 
  mutate(LISA_mu = case_when(
    LISA_mu == "HH" ~ "HC",
    LISA_mu == "LL" ~ "LC"
  ),
         ec = ifelse(mu_EP > 0.8, "H", 
                       ifelse(mu_EP < 0.2, "L", NA)),
         out = factor(ifelse(is.na(LISA_mu) & !is.na(ec), 
                             ec, as.character(LISA_mu)),
                      levels = c("HC", "H", "L", "LC")),
         out = fct_explicit_na(out, na_level = "N"),
         model = getRFFullNames(model),
         ra = factor(ifelse(ra_sa2_3c == "Outer regional to very remote", 
                            "Outer regional\nto very remote", 
                            as.character(ra_sa2_3c)),
                     levels = c("Major Cities",
                                "Inner Regional",
                                "Outer regional\nto very remote"))) %>% 
  relocate(out)

# IRSD

# weighted
ec1 <- ec_df %>% 
  mutate(ww =  2221*8 * N_persons/sum(N_persons)) %>% 
  group_by(model, irsd_5c) %>% 
  summarise(HC = sum(ww*(out == "HC"), na.rm= T),
            H = sum(ww*(out == "H"), na.rm= T),
            N = sum(ww*(out == "N"), na.rm= T),
            L = sum(ww*(out == "L"), na.rm= T),
            LC = sum(ww*(out == "LC"), na.rm= T),
            .groups = "drop") %>% 
  pivot_longer(-c(model, irsd_5c)) %>% 
  mutate(out = factor(name, levels = c("HC", "H", "N", "L", "LC")))

# non-weighted
ec2 <- ec_df %>% 
  mutate(ww =  2221*8 * N_persons/sum(N_persons)) %>% 
  group_by(model, irsd_5c) %>% 
  summarise(HC = sum((out == "HC"), na.rm= T),
            H = sum((out == "H"), na.rm= T),
            N = sum((out == "N"), na.rm= T),
            L = sum((out == "L"), na.rm= T),
            LC = sum((out == "LC"), na.rm= T),
            .groups = "drop") %>% 
  pivot_longer(-c(model, irsd_5c)) %>% 
  mutate(out = factor(name, levels = c("HC", "H", "N", "L", "LC")),
         uw_value = value) %>% 
  dplyr::select(uw_value)

# join
ec_irsd <- cbind(ec1, ec2); rm(ec1,ec2)

# remoteness

# weighted
ec1 <- ec_df %>% 
  mutate(ww =  2221*8 * N_persons/sum(N_persons)) %>% 
  group_by(model, ra) %>% 
  summarise(HC = sum(ww*(out == "HC"), na.rm= T),
            H = sum(ww*(out == "H"), na.rm= T),
            N = sum(ww*(out == "N"), na.rm= T),
            L = sum(ww*(out == "L"), na.rm= T),
            LC = sum(ww*(out == "LC"), na.rm= T),
            .groups = "drop") %>% 
  pivot_longer(-c(model, ra)) %>% 
  mutate(out = factor(name, levels = c("HC", "H", "N", "L", "LC")))

# non-weighted
ec2 <- ec_df %>% 
  mutate(ww =  2221*8 * N_persons/sum(N_persons)) %>% 
  group_by(model, ra) %>% 
  summarise(HC = sum((out == "HC"), na.rm= T),
            H = sum((out == "H"), na.rm= T),
            N = sum((out == "N"), na.rm= T),
            L = sum((out == "L"), na.rm= T),
            LC = sum((out == "LC"), na.rm= T),
            .groups = "drop") %>% 
  pivot_longer(-c(model, ra)) %>% 
  mutate(out = factor(name, levels = c("HC", "H", "N", "L", "LC")),
         uw_value = value) %>% 
  dplyr::select(uw_value)

# join
ec_ra <- cbind(ec1, ec2); rm(ec1,ec2)

## FACTOR: EC - Population weighted ## -----------------------------------------

# irsd - nofill
ec_irsd %>% 
  ggplot()+theme_bw()+
  geom_col(aes(x = value, y = irsd_5c, fill = out), position = position_dodge2())+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("red", "coral", "grey", "skyblue", "royalblue"),
                    breaks = c("HC", "H", "N", "L", "LC"))+
  scale_x_continuous(breaks=c(0,150, 300))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_irsd_barchart_pw_cec.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

# irsd - stack
ec_irsd %>% 
  ggplot()+theme_bw()+
  geom_bar(aes(x = value, y = irsd_5c, fill = out), position = "stack", stat = "identity")+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("red", "coral", "grey", "skyblue", "royalblue"),
                    #labels = expression(H[cluster], H[single], N, L[single], L[cluster]),
                    breaks = c("HC", "H", "N", "L", "LC"))+
  scale_x_continuous(breaks=c(0,200, 400))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_irsd_barstack_pw_cec.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

# irsd - fill
ec_irsd %>% 
  ggplot()+theme_bw()+
  geom_col(aes(x = value, y = irsd_5c, fill = out), position = "fill")+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("red", "coral", "grey", "skyblue", "royalblue"),
                    breaks = c("HC", "H", "N", "L", "LC"))+
  scale_x_continuous(breaks=c(0,150, 300))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_irsd_barfill_pw_cec.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

# ra - nofill
ec_ra %>% 
  ggplot()+theme_bw()+
  geom_col(aes(x = value, y = ra, fill = out), position = position_dodge2())+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("red", "coral", "grey", "skyblue", "royalblue"),
                    breaks = c("HC", "H", "N", "L", "LC"))+
  scale_x_continuous(breaks=c(0,150, 300))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_ra_barchart_pw_cec.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

# ra - stack
ec_ra %>% 
  ggplot()+theme_bw()+
  geom_bar(aes(x = value, y = ra, fill = out), position = "stack", stat = "identity")+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("red", "coral", "grey", "skyblue", "royalblue"),
                    #labels = expression(H[cluster], H[single], N, L[single], L[cluster]),
                    breaks = c("HC", "H", "N", "L", "LC"))+
  scale_x_continuous(breaks=c(0,750,1500))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_ra_barstack_pw_cec.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)
jsave(filename = "fig8.jpeg", 
      base_folder = paste0(base_folder, "/paper"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)
jsave(filename = "fig8_500dpi.tiff", 
      base_folder = paste0(base_folder, "/paper"),
      square = F, ratio = 9:6,
      square_size = 1200,
      scale = 1.67,
      dpi = 500)
jsave(filename = "fig8_300dpi.tiff", 
      base_folder = paste0(base_folder, "/paper"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

# ra - fill
ec_ra %>% 
  ggplot()+theme_bw()+
  geom_col(aes(x = value, y = ra, fill = out), position = "fill")+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("red", "coral", "grey", "skyblue", "royalblue"),
                    breaks = c("HC", "H", "N", "L", "LC"))+
  scale_x_continuous(breaks=c(0,150, 300))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_ra_barfill_pw_cec.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

## FACTOR: Factor - Population weighted ## -------------------------------------

# irsd - nofill
ec_irsd %>% 
  ggplot()+theme_bw()+
  geom_col(aes(x = value, fill = irsd_5c, y = out), position = position_dodge2())+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  addIRSDColor()+
  scale_x_continuous(breaks=c(0,150, 300))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_irsd_barchart_pw_cf.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

# irsd - fill
ec_irsd %>% 
  ggplot()+theme_bw()+
  geom_col(aes(x = value, fill = irsd_5c, y = out), position = "fill")+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  addIRSDColor()+
  scale_x_continuous(breaks=c(0,150, 300))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_irsd_barfill_pw_cf.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

# ra - nofill
ec_ra %>% 
  ggplot()+theme_bw()+
  geom_col(aes(x = value, fill = ra, y = out), position = position_dodge2())+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(breaks = c("Major Cities", "Inner Regional",
                               "Outer regional\nto very remote"),
                    values = c('#fef0d9','#fc8d59','#b30000'))+
  scale_x_continuous(breaks=c(0,150, 300))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_ra_barchart_pw_cf.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

# ra - fill
ec_ra %>% 
  ggplot()+theme_bw()+
  geom_col(aes(x = value, fill = ra, y = out), position = "fill")+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(breaks = c("Major Cities", "Inner Regional",
                               "Outer regional\nto very remote"),
                    values = c('#fef0d9','#fc8d59','#b30000'))+
  scale_x_continuous(breaks=c(0,150, 300))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_ra_barfill_pw_cf.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)


## END SCRIPT #### -------------------------------------------------------------

## START DEPREC #### -----------------------------------------------------------

## FACTOR: EC ## ---------------------------------------------------------------

# irsd - nofill
ec_irsd %>% 
  ggplot()+theme_bw()+
  geom_col(aes(x = uw_value, y = irsd_5c, fill = out), position = position_dodge2())+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("red", "coral", "skyblue", "royalblue"),
                    breaks = c("HC", "H", "L", "LC"))+
  scale_x_continuous(breaks=c(0,150, 300))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_irsd_barchart_npw_cec.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec/npw"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

# irsd - fill
ec_irsd %>% 
  ggplot()+theme_bw()+
  geom_col(aes(x = uw_value, y = irsd_5c, fill = out), position = "fill")+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("red", "coral", "skyblue", "royalblue"),
                    breaks = c("HC", "H", "L", "LC"))+
  scale_x_continuous(breaks=c(0,150, 300))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_irsd_barfill_npw_cec.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec/npw"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

# ra - nofill
ec_ra %>% 
  ggplot()+theme_bw()+
  geom_col(aes(x = uw_value, y = ra, fill = out), position = position_dodge2())+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("red", "coral", "skyblue", "royalblue"),
                    breaks = c("HC", "H", "L", "LC"))+
  scale_x_continuous(breaks=c(0,150, 300))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_ra_barchart_npw_cec.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec/npw"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

# ra - fill
ec_ra %>% 
  ggplot()+theme_bw()+
  geom_col(aes(x = uw_value, y = ra, fill = out), position = "fill")+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("red", "coral", "skyblue", "royalblue"),
                    breaks = c("HC", "H", "L", "LC"))+
  scale_x_continuous(breaks=c(0,150, 300))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_ra_barfill_npw_cec.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec/npw"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

## FACTOR: Factor ## -----------------------------------------------------------

# irsd - nofill
ec_irsd %>% 
  ggplot()+theme_bw()+
  geom_col(aes(x = uw_value, fill = irsd_5c, y = out), position = position_dodge2())+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  addIRSDColor()+
  scale_x_continuous(breaks=c(0,150, 300))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_irsd_barchart_npw_cf.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec/npw"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

# irsd - fill
ec_irsd %>% 
  ggplot()+theme_bw()+
  geom_col(aes(x = uw_value, fill = irsd_5c, y = out), position = "fill")+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  addIRSDColor()+
  scale_x_continuous(breaks=c(0,150, 300))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_irsd_barfill_npw_cf.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec/npw"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

# ra - nofill
ec_ra %>% 
  ggplot()+theme_bw()+
  geom_col(aes(x = uw_value, fill = ra, y = out), position = position_dodge2())+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(breaks = c("Major Cities", "Inner Regional",
                               "Outer regional\nto very remote"),
                    values = c('#fef0d9','#fc8d59','#b30000'))+
  scale_x_continuous(breaks=c(0,150, 300))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_ra_barchart_npw_cf.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec/npw"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

# ra - fill
ec_ra %>% 
  ggplot()+theme_bw()+
  geom_col(aes(x = uw_value, fill = ra, y = out), position = "fill")+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(breaks = c("Major Cities", "Inner Regional",
                               "Outer regional\nto very remote"),
                    values = c('#fef0d9','#fc8d59','#b30000'))+
  scale_x_continuous(breaks=c(0,150, 300))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_ra_barfill_npw_cf.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec/npw"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

## DEPREC ## -------------------------------------------------------------------

## Unweighted ## ---------------------------------------------------------------

# color: factor
ec_df %>% 
  mutate(model = getRFFullNames(model),
         ra_sa2 = fct_relevel(ra_sa2, "Major Cities")) %>% 
  filter(!is.na(out)) %>% 
  ggplot()+theme_bw()+
  geom_bar(aes(y = out, fill = ra_sa2), position = position_dodge2())+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  addRemotenessColor()+
  scale_x_continuous(breaks=c(0,250,500))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_ra_barchart_npw_cf.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec/npw"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

# color: EC
ec_df %>% 
  filter(!is.na(out)) %>% 
  ggplot()+theme_bw()+
  geom_bar(aes(y = ra, fill = out), position = position_dodge2())+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("red", "coral", "skyblue", "royalblue"),
                    breaks = c("HC", "H", "L", "LC")) +
  scale_x_continuous(breaks=c(0,150,300))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_ra_barchart_npw_cec.png.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec/npw"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

# filled
ec_df %>% 
  filter(!is.na(out)) %>% 
  ggplot()+theme_bw()+
  geom_bar(aes(y = ra, fill = out), position = "fill")+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("red", "coral", "skyblue", "royalblue"),
                    breaks = c("HC", "H", "L", "LC"))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_ra_barfill_npw_cec.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec/npw"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

## Barchart - RA - Population weighted ## --------------------------------------

ec_ra %>% 
  ggplot()+theme_bw()+
  geom_col(aes(x = value, y = ra, fill = out), position = position_dodge2())+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("red", "coral", "skyblue", "royalblue"),
                    breaks = c("HC", "H", "L", "LC"))+
  scale_x_continuous(breaks=c(0,250,500))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_ra_barchart_pw_cec.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

## Barchart - SES ## -----------------------------------------------------------

# Socioeconomic status
ec_df %>% 
  filter(!is.na(out)) %>% 
  ggplot()+theme_bw()+
  geom_bar(aes(y = out, fill = irsd_5c), position = position_dodge2())+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  addIRSDColor() +
  scale_x_continuous(breaks=c(0,150,300))+
  theme(text = element_text(size = 8))


# save object
jsave(filename = paste0("ec_irsd_barchart_npw_cf.png.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec/npw"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

# Socioeconomic on yaxis instead
ec_df %>% 
  filter(!is.na(out)) %>% 
  ggplot()+theme_bw()+
  geom_bar(aes(y = irsd_5c, fill = out), position = position_dodge2())+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("red", "coral", "skyblue", "royalblue"),
                    breaks = c("HC", "H", "L", "LC")) +
  scale_x_continuous(breaks=c(0,150,300))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_irsd_barchart_npw_cec.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec/npw"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

# Socioeconomic - fill
ec_df %>% 
  filter(!is.na(out)) %>% 
  ggplot()+theme_bw()+
  geom_bar(aes(y = irsd_5c, fill = out), position = "fill")+
  facet_wrap(.~model)+
  labs(fill = "",
       x = "",
       y = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("red", "coral", "skyblue", "royalblue"),
                    breaks = c("HC", "H", "L", "LC"))+
  theme(text = element_text(size = 8))

# save object
jsave(filename = paste0("ec_irsd_barfill_npw_cec.png"), 
      base_folder = paste0(base_folder, "/figures_lowres/ec/npw"),
      square = F, ratio = 9:6,
      square_size = 1200,
      dpi = 300)

## END DEPREC #### -------------------------------------------------------------