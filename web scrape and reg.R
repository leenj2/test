nyc_trees <- 
  fromJSON("https://data.cityofnewyork.us/resource/uvpi-gqnh.json") %>% 
  as_tibble()

nyc_trees %>%
  select(longitude, latitude, stump_diam, spc_common, spc_latin, tree_id) %>%
  mutate_at(vars(longitude:stump_diam), as.numeric) %>%
  ggplot(aes(x = longitude, y = latitude, size = stump_diam))+
  geom_point(alpha = 0.5)+
  scale_size_continuous(name = "Stump Diameter")+
  labs(
    x = "Longitude", y = "Latitude",
    title = "Sample of New York City trees",
    caption = "Source: NYC Open Data"
  )

endpoint <- "series/observations"

params = list(
  api_key ="08a312174d4768790f6e845eeb1f627c",
  file_type="json",
  series_id="GNPCA"
)

fred <- 
  httr::GET(
    url = "https://api.stlouisfed.org/",
    path = paste0("fred/", endpoint),
    query = params
  )

fred %>%
  httr::content("text") %>%
  jsonlite::fromJSON() %>%
  listviewer::jsonedit(mode = "view")

fred <- 
  fred %>%
  httr::content("text") %>%
  jsonlite::fromJSON() %>%
  purrr::pluck("observations") %>%
  as_tibble()

fred <-
  fred %>%
  mutate_at(vars(realtime_start:date), ymd) %>%
  mutate(value = as.numeric(value))

fred %>%
  ggplot(aes(date, value)) + 
  geom_line() +
  scale_y_continuous(labels = scales::comma) + 
  labs(
    x = "Date", y = "2012 USD (Billions)",
    title="US Real Gross National Product", caption = "Source: FRED"
  )




my_api_key <- Sys.getenv("FRED_API_KEY")

fredr_set_key(my_api_key)

alsofred <-
  fredr(
  series_id = "GNPCA",
  observation_start = as.Date("1929-01-01")
)

alsofred %>%
  ggplot(aes(date, value))+
  geom_line()+
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Date", y = "2012 USD (Billions)",
    title = "Real Gross National Product", caption = "Source: FRED"
  )
  
###rugby rankings

start_date <- ymd("2004-01-01")
end_date <- floor_date(today(), unit="years")
dates <- seq(start_date, end_date, by="years")
dates <- floor_date(dates, "week", week_start = getOption("lubridate.week.start", 1))

rugby_scrape <- 
  function(x) {
    endpoint <- paste0("https://cmsapi.pulselive.com/rugby/rankings/mru?date=", x, "&client=pulse")
    rugby <- fromJSON(endpoint)
    rankings <- bind_cols(
      rugby$entries$team,
      rugby$entries %>% select(matches:previousPos)
    ) %>%
      clean_names() %>%
      mutate(date = x) %>%
      select(-c(id, alt_id, annotations)) %>%
      select(date, pos, pts, everything()) %>%
      as_tibble()
    Sys.sleep(3)
    return(rankings)
  }

rankings_history <- 
  lapply(dates, rugby_scrape) %>%
  bind_rows()

rankings_history

teams <- c("NZL", "RSA", "ENG", "JPN")
teams_cols <- c("NZL"="black", "RSA"="#4DAF4A", "ENG"="#377EB8", "JPN" = "red")

rankings_history %>%
  ggplot(aes(x=date, y=pts, group=abbreviation)) +
  geom_line(col = "grey")+
  geom_line(
    data = rankings_history %>% filter(abbreviation %in% teams),
    aes(col=fct_reorder2(abbreviation, date, pts)),
    lwd = 1
  ) +
  scale_color_manual(values = teams_cols) +
  labs(
    x = "Date", y = "Points",
    title = "International rugby rankings", caption = "Source: World Rugby"
  ) +
  theme(legend.title = element_blank())

##Regression

pacman::p_load(tidyverse, broom, hrbrthemes, plm, estimatr, sandwich, lmtest, AER, lfe, huxtable, margins)

olsl <- lm(mass ~ height, data = starwars)
summary(olsl)
summary(olsl)$coefficients
broom::tidy(olsl, conf.int = T)
glance(olsl)

starwars %>%
  ggplot(aes(x = height, y = mass))+
  geom_point(alpha = 0.5)+
  geom_point(
    data = starwars %>% filter(mass==max(mass, na.rm = T)),
    col = "red"
  ) +
  geom_text(
    aes(label=name),
    data = starwars %>% filter(mass==max(mass, na.rm = T)), 
    col = "red", vjust = 0, nudge_y = 25
  ) +
  labs(
    title = "Spot the outlier...",
    caption = "Aside: Always plot your data!"
  )

starwars2 <- 
  starwars %>%
  filter(!(grepl("Jabba", name)))

ols2 <- lm(mass ~ height, data = starwars2)
ols2a <- lm(mass ~ height, data = starwars %>% filter(!(grepl("Jabba", name))))
olsl_robust <- lm_robust(mass ~ height, data = starwars)
tidy(olsl_robust, conf.int = T)

olsl_robust_clustered <- lm_robust(mass ~ height, data = starwars, clusters = homeworld)
tidy(olsl_robust_clustered, conf.int = T)

> NeweyWest(olsl)

olsl %>%
  lmtest::coeftest(vcov = NeweyWest) %>%
  tidy() %>%
  mutate(
    conf.low = estimate - qt(0.975, df=olsl$df.residual)*std.error,
    conf.high = estimate - qt(0.975, df=olsl$df.residual)*std.error
  )
  
  humans <- 
    starwars %>%
    filter(species == "Human") %>%
    mutate(gender_factored = as.factor(gender)) %>%
    select(contains("gender"), everything())
  
olsl_dv <- lm(mass ~ height + gender_factored, data = humans)
summary(olsl_dv)

olsl_ie <- lm(mass ~ height*gender, data = humans)
summary(olsl_ie)
library(lfe)
ols_fe <- felm(mass ~ height | species, data = humans)
coefs_fe <- tidy(ols_fe, conf.int = T)

ols_hdfe <- 
  felm(
    mass ~ height |
      species + homeworld |
      0 |
      homeworld,
    data = starwars)

coefs_hdfe <- tidy(ols_hdfe, conf.int = T)

bind_rows(
  coefs_fe %>% mutate(reg = "Model 4 (FE and no clustering)"),
  coefs_hdfe %>% mutate(reg = "Model 5 (HDFE and clustering)")
) %>%
  ggplot(aes(x = reg, y =estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  labs(Title = "Marginal effect of height on mass") +
  geom_hline(yintercept = 0, col = "orange") +
  ylim(-0.5, NA) +
  labs(
    title = "'Effect' of height on mass",
    caption = "Data: Characters from the Star Wars universe"
  ) +
  theme(axis.title.x = element_blank())
 

data("CigarettesSW", package = "AER")

cigs <- 
  CigarettesSW %>%
  mutate(
    rprice = price/cpi,
    rincome = income/population/cpi,
    rtax = tax/cpi,
    tdiff = (taxs - tax)/cpi
  ) %>%
  as_tibble()

cigs95 <- cigs %>% filter(year==1995)
cigs95

iv_reg <- 
  ivreg(
    log(packs) ~log(price) + log(rincome) |
      log(rincome) + tdiff + rtax,
    data = cigs95
  )

summary(iv_reg, diagnostics = TRUE)

iv_reg_robust <- 
  iv_robust(
    log(packs) ~ log(price) + log(rincome) |
      log(rincome) + tdiff + rtax, 
    data = cigs95
  )

summary(iv_reg_robust, diagnostics = TRUE)

iv_felm <- 
  felm(
    log(packs) ~ log(rincome) |
      0 |
      (log(rprice) ~ tdiff + rtax),
    data = cigs95
  )

summary(iv_felm)

iv_felm_all <- 
  felm(
    log(packs) ~ log(rincome) |
      year + state |
      (log(rprice) ~ tdiff + rtax),
    data = cigs
  )

summary(iv_felm_all)


library(margins)

margins(olsl_ie)
olsl_ie %>% margins() %>% summary()

olsl_ie %>%
  margins(
    variables = "height",
    at = list(gender = c("male", "female"))
  )

cplot(olsl_ie, x = "gender", dx = "height", what = "effect")

par(mfrow = c(1, 2))
cplot(olsl_ie, x ="gender", what = "prediction")
cplot(olsl_ie, x ="height", what = "prediction")
par(mfrow=c(1, 1)) ## Reset plot defaults


library(rstanarm)
bayes_reg <- 
  stan_glm(
    mass ~ gender*height,
    data = humans,
    family = gaussian(), prior = cauchy(), prior_intercept = cauchy()
  )
summary(bayes_reg)        
tidy(bayes_reg)
        
humans %>%
  ggplot(aes(x = mass, y = height, col = gender ))+
  geom_point(alpha = 0.7)+
  geom_smooth(method = "lm", se = F)+
  scale_color_brewer(palette = "Set1")
        
huxreg(olsl_dv, olsl_ie, ols_hdfe)
        
        
        
        
        

  