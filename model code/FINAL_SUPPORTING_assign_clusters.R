# This script assigns individuals to specific clusters / sub-groups, which are modelled as closed groups within which transmission occurs

set.seed(20)

# shelters (actual shelter numbers and sizes)
# DC_Shelters.csv
# LA_Shelters.csv
# SF_Shelters.csv
# Seattle_Shelters.csv

hl <- scan("DC_Shelters.csv")
h_cl <- rep(seq_along(hl), hl) # hotel cluster ID's

# overall encampment population size
# DC = 608L
# LA = 
# SF = 
# Seattle = 
rs <- 608L

# night shelters (assumed to be zero for this project)
number_in_ns <- round(0L, 0L)
nss <- scan("https://raw.githubusercontent.com/maxeyre/Homeless-COVID-19/master/model/SUPPORTING_nightshelter_sizes_london.csv")
ns_sample <- sample(nss, floor(number_in_ns / mean(nss) * 1.5), replace = T)
ns_sample <- ns_sample[cumsum(ns_sample) < number_in_ns]
ns_sample <- c(ns_sample, number_in_ns - sum(ns_sample))

# encampments
street_sleeping_pop <- rs - number_in_ns
ss_mx_group <- 100L
ss <- scan("DC_Encampments.csv")
ss_cl <- rep(seq_along(ss), ss)
ss_cl <- ss_cl + max(h_cl) # street-sleeping cluster ID's

# total population
cl <- c(h_cl, ss_cl) # cluster id
type <- c(rep(1L, sum(hl)), rep(2L, number_in_ns), rep(3L, street_sleeping_pop)) # 1 = hostel, 2 = night shelters, 3 = street-sleeping
n <- length(cl) # total population size

# Hotels (people from encampments)
hotels <- scan("DC_Hotels.csv")
hotel_sample <- sample(hotels, floor(rs / mean(hotels) * 1.5), replace = T)
hotel_sample <- hotel_sample[cumsum(hotel_sample) < rs]
hotel_sample <- c(hotel_sample, rs - sum(hotel_sample))
hotel_cl <- rep(seq_along(hotel_sample), hotel_sample)
hotel_cl <- hotel_cl + max(cl)
hotel_cl <- c(rep(0L, length(h_cl)), hotel_cl)

# -----------------
# create RData file
# =================

save(list = c('cl', 'type', 'n', 'hotel_cl'), file = 'TESTcluster_ids.RData')
