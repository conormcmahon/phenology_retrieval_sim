
library(tidyverse)

set.seed(1)

# Add some Gaussian noise to each term
noise_sd <- 0.05
noise_mean <- 0

num_spectral_examples <- 100

test_data <- crossing(red = (1:num_spectral_examples)/num_spectral_examples/2+0.1,
                      NIR = (1:num_spectral_examples)/num_spectral_examples/2+0.1)
test_data <- test_data %>% 
  mutate(red_noise = rnorm(num_spectral_examples^2, mean=0, sd=noise_sd),
         NIR_noise = rnorm(num_spectral_examples^2, mean=0, sd=noise_sd)) %>%
  mutate(red_final = red_noise+red,
         NIR_final = NIR+NIR_noise) %>%
  mutate(NDVI_noiseless = (NIR-red)/(NIR+red),
         NDVI_observed = (NIR_final-red_final)/(NIR_final+red_final))

# Test fit between expected and noise-injected values
summary(lm(data=test_data, NDVI_noiseless ~ NDVI_observed))
# Visualize results
ggplot(test_data) + 
  geom_density_2d_filled(aes(x=NDVI_noiseless, NDVI_observed)) + 
  scale_x_continuous(limits=c(-1, 1)) + scale_y_continuous(limits=c(-1, 1))
ggplot(test_data) + 
  geom_histogram(aes(x = NDVI_noiseless - NDVI_observed))
ggqqplot(test_data$NDVI_noiseless - test_data$NDVI_observed)



# In the second example, add some Gaussian noise which is NOT centered on zero but is instead always negative
# This is an example for the case where both reflectances decrease 

# Add some Gaussian noise to each term
noise_sd <- 0.05
noise_mean <- -.03

num_spectral_examples <- 100

test_data <- crossing(red = (1:num_spectral_examples)/num_spectral_examples/2+0.1,
                      NIR = (1:num_spectral_examples)/num_spectral_examples/2+0.1)
test_data <- test_data %>% 
  mutate(red_noise = rnorm(num_spectral_examples^2, mean=noise_mean, sd=noise_sd),
         NIR_noise = rnorm(num_spectral_examples^2, mean=noise_mean, sd=noise_sd)) %>%
  mutate(red_final = red_noise+red,
         NIR_final = NIR+NIR_noise) %>%
  mutate(NDVI_noiseless = (NIR-red)/(NIR+red),
         NDVI_observed = (NIR_final-red_final)/(NIR_final+red_final))

# Test fit between expected and noise-injected values
summary(lm(data=test_data, NDVI_noiseless ~ NDVI_observed))
# Visualize results
ggplot(test_data) + 
  geom_density_2d_filled(aes(x=NDVI_noiseless, NDVI_observed)) + 
  scale_x_continuous(limits=c(-1, 1)) + scale_y_continuous(limits=c(-1, 1))
ggplot(test_data) + 
  geom_histogram(aes(x = NDVI_noiseless - NDVI_observed))
ggqqplot(test_data$NDVI_noiseless - test_data$NDVI_observed)




# In the third example, add some Gaussian noise which is NOT centered on zero for either band and is DIFFERENT by band
# This is an example for the case where one reflectance decreases and the other increases

# Add some Gaussian noise to each term
noise_sd <- 0.05
noise_mean <- -.03

num_spectral_examples <- 100

test_data <- crossing(red = (1:num_spectral_examples)/num_spectral_examples/2+0.1,
                      NIR = (1:num_spectral_examples)/num_spectral_examples/2+0.1)
test_data <- test_data %>% 
  mutate(red_noise = rnorm(num_spectral_examples^2, mean=noise_mean, sd=noise_sd),
         NIR_noise = rnorm(num_spectral_examples^2, mean=-noise_mean, sd=noise_sd)) %>%
  mutate(red_final = red_noise+red,
         NIR_final = NIR+NIR_noise) %>%
  mutate(NDVI_noiseless = (NIR-red)/(NIR+red),
         NDVI_observed = (NIR_final-red_final)/(NIR_final+red_final))

# Test fit between expected and noise-injected values
summary(lm(data=test_data, NDVI_noiseless ~ NDVI_observed))
# Visualize results
ggplot(test_data) + 
  geom_density_2d_filled(aes(x=NDVI_noiseless, NDVI_observed)) + 
  scale_x_continuous(limits=c(-1, 1)) + scale_y_continuous(limits=c(-1, 1))
ggplot(test_data) + 
  geom_histogram(aes(x = NDVI_noiseless - NDVI_observed))
ggqqplot(test_data$NDVI_noiseless - test_data$NDVI_observed)

