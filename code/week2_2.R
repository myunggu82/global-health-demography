data(package = "wpp2024")

data(mx1dt)
head(mx5dt)
data(popAge1dt)
head(popAge1dt)

table(popAge1dt$year)

kor_mx <- mx1dt %>%
  filter(name=="Republic of Korea", year==2018) %>%
  select(name, year, age, mxB)
uga_mx <- mx1dt %>%
  filter(name=="Uganda", year==2018) %>%
  select(name, year, age, mxB)

kor_pop <- popAge1dt%>%
  filter(name=="Republic of Korea", year==2018) %>%
  select(name, year, age, pop)

uga_pop <- popAge1dt%>%
  filter(name=="Uganda", year==2018) %>%
  select(name, year, age, pop)

kor_mx
uga_mx 
kor_pop
uga_pop



kor <- kor_pop %>%
  select(age, pop) %>%
  left_join(
    kor_mx %>% select(age, mxB),
    by = "age"
  )

kor <- kor %>%
  mutate(
    deaths = pop * mxB
  )

kor_cdr <- sum(kor$deaths) /
  sum(kor$pop) * 1000

kor_cdr


uga <- uga_pop %>%
  select(age, pop) %>%
  left_join(
    uga_mx %>% select(age, mxB),
    by = "age"
  )

uga <- uga %>%
  mutate(
    deaths = pop * mxB
  )

uga_cdr <- sum(uga$deaths) /
  sum(uga$pop) * 1000

uga_cdr



std_pop <- kor_pop %>%
  select(age, pop_kor = pop) %>%
  left_join(
    uga_pop %>%
      select(age, pop_uga = pop),
    by = "age"
  ) %>%
  mutate(
    std_pop = (pop_kor + pop_uga)/2
  ) %>%
  select(age, std_pop)

std_pop

kor_std <- std_pop %>%
  left_join(
    kor_mx %>%
      select(age, mxB),
    by = "age"
  ) %>%
  mutate(
    expected_deaths = std_pop * mxB
  )

kor_asmr <- sum(kor_std$expected_deaths) /
  sum(kor_std$std_pop) * 1000

kor_asmr

uga_std <- std_pop %>%
  left_join(
    uga_mx %>%
      select(age, mxB),
    by = "age"
  ) %>%
  mutate(
    expected_deaths = std_pop * mxB
  )

uga_asmr <- sum(uga_std$expected_deaths) /
  sum(uga_std$std_pop) * 1000

uga_asmr


tibble(
  Country = c("Korea", "Uganda"),
  CDR = c(kor_cdr, uga_cdr),
  ASMR = c(kor_asmr, uga_asmr)
)
