check <- read_csv("./data/reservoir_data.csv")
range(check$datetime, na.rm = TRUE)
unique(check$variable)
turb <- check %>%
  filter(variable == "Turbidity_FNU_mean")

ggplot(data = turb, aes(x = datetime, y = observation))+
  geom_line()+
  facet_wrap(facets = vars(site_id), scales = "free_x")+
  theme_bw()

fdom <- check %>%
  filter(variable == "fDOM_QSU_mean")

ggplot(data = fdom, aes(x = datetime, y = observation))+
  geom_line()+
  facet_wrap(facets = vars(site_id), scales = "free_x")+
  theme_bw()

url <- "https://amnh1.osn.mghpcc.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"
targets <- read_csv(url, show_col_types = FALSE)
unique(targets$variable)

focal <- targets %>%
  filter(variable %in% c("DOC_mgL_sample", "fDOM_QSU_mean" )) %>%
  pivot_wider(names_from = "variable", values_from = "observation") %>%
  filter(!is.na(DOC_mgL_sample) & site_id == "fcre" & !is.na(fDOM_QSU_mean))

ggplot(data = focal, aes(x = fDOM_QSU_mean, y = DOC_mgL_sample))+
  geom_point()+
  theme_bw()

fcr_fdom <- targets %>%
  filter(variable %in% c("fDOM_QSU_mean" ) & site_id == "fcre") %>%
  mutate(year = year(datetime),
         month = month(datetime)) %>%
  filter(year == 2021 & month %in% c(1,2))

# increasing could be Jul 20 - Aug 20, 2019
# decreasing could be Feb 01 - Mar 01, 2021
# variable could be Mar 15 - Apr 15, 2022

ggplot(data = fcr_fdom, aes(x = datetime, y = observation))+
  geom_point()+
  theme_bw()

mod <- lm(DOC_mgL_sample ~ fDOM_QSU_mean, data = focal)
summary(mod)
mean(focal$fDOM_QSU_mean, na.rm = TRUE)

bvr_doc <- targets %>%
  filter(variable %in% c("DOC_mgL_sample")) %>%
  pivot_wider(names_from = "variable", values_from = "observation") %>%
  filter(site_id == "bvre" & !is.na(DOC_mgL_sample) & depth_m <=3) %>%
  select(-depth_m) %>%
  group_by(datetime) %>%
  summarize(DOC = mean(DOC_mgL_sample, na.rm = TRUE))

bvr_fdom <- targets %>%
  filter(variable %in% c("fDOM_QSU_mean" )) %>%
  pivot_wider(names_from = "variable", values_from = "observation") %>%
  filter(site_id == "bvre" & !is.na(fDOM_QSU_mean)) %>%
  select(datetime, fDOM_QSU_mean)

bvr <- full_join(bvr_doc, bvr_fdom, by = "datetime") %>%
  filter(!is.na(DOC) & !is.na(fDOM_QSU_mean))

ggplot(data = bvr, aes(x = fDOM_QSU_mean, y = DOC))+
  geom_point()+
  theme_bw()

mod <- lm(DOC ~ fDOM_QSU_mean, data = bvr)
summary(mod)
mean(focal$fDOM_QSU_mean, na.rm = TRUE)
