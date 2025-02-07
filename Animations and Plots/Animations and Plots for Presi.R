# Install necessary packages
install.packages("ggplot2")
install.packages("gganimate")
install.packages("gifski")

# Load libraries
library(ggplot2)
library(gganimate)

# Create base data for the sine wave
x_values <- seq(0, 4 * pi, length.out = 500)  # High resolution for smooth wave

# Create a data frame for animation
single_wave <- expand.grid(
  x = x_values,             # x-axis values
  time = seq(1, 100, by = 1)  # Time frames
)

# Compute y-values with a phase shift based on time
single_wave$y <- sin(single_wave$x + single_wave$time * 0.1)

# Ensure the structure is correct
str(single_wave)

# Create the plot
p_smooth <- ggplot(single_wave, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Single Sine Wave Over Time",
    subtitle = "Time: {frame_time}",
    x = "Time",
    y = "Amplitude"
  ) +
  ylim(-2, 2) + 
  theme_minimal() +
  transition_time(time) +
  ease_aes("linear")  # Smooth transitions

# Save as a smooth GIF
animate(p_smooth, renderer = gifski_renderer("smooth_sine_wave.gif"), width = 600, height = 400, fps = 20)


# Create base data for two waves
x_values <- seq(0, 4 * pi, length.out = 500)  # High resolution for smooth waves

# Create a data frame for animation
wave_sum <- expand.grid(
  x = x_values,             # x-axis values
  time = seq(1, 200, by = 1)  # Time frames
)

# Compute y-values for two sine waves and their sum
wave_sum$y1 <- sin(wave_sum$x)                     # Wave 1
wave_sum$y2 <- sin(wave_sum$x + wave_sum$time * 0.1)  # Wave 2 with phase shift
wave_sum$y_sum <- wave_sum$y1 + wave_sum$y2        # Summation of waves

# Create the plot for wave summation
p_wave_sum <- ggplot(wave_sum, aes(x = x)) +
  geom_line(aes(y = y1), color = "blue", size = 0.8, alpha = 0.6, linetype = "dashed") +  # Wave 1
  geom_line(aes(y = y2), color = "red", size = 0.8, alpha = 0.6, linetype = "dashed") +   # Wave 2
  geom_line(aes(y = y_sum), color = "purple", size = 1.2) +                               # Summed Wave
  labs(
    title = "Wave Summation Over Time",
    subtitle = "Blue + Red = Purple (Sum)",
    x = "Time",
    y = "Amplitude"
  ) +
  ylim(-2, 2) +  # Consistent y-axis range
  theme_minimal() +
  transition_time(time) +
  ease_aes("linear")  # Smooth transitions

# Save as a smooth GIF
animate(p_wave_sum, renderer = gifski_renderer("wave_summation.gif"), width = 600, height = 400, fps = 15, duration = 20)

#step 3, asynchronous waves



# Step 3: Asynchronous Waves Canceling Each Other Out
# Load necessary libraries
library(ggplot2)
library(gganimate)

# Create base data for two high-frequency waves
x_values <- seq(0, 4 * pi, length.out = 500)  # High resolution for smooth waves

# Create a data frame for animation
modulation <- expand.grid(
  x = x_values,             # x-axis values
  time = seq(1, 200, by = 1)  # Time frames
)

# Define the two high-frequency waves with slightly different frequencies
modulation$high_freq1 <- sin(modulation$x)  # High-frequency wave 1
modulation$high_freq2 <- sin(modulation$x + modulation$time * 0.05)  # Phase-shifted high-frequency wave 2

# Calculate the envelope modulation
modulation$modulated_wave <- modulation$high_freq1 + modulation$high_freq2  # Combine high-frequency waves
modulation$envelope <- abs(sin(modulation$time * 0.05))  # Envelope from difference frequency

# Create the plot
p_modulation <- ggplot(modulation, aes(x = x)) +
  geom_line(aes(y = high_freq1), color = "blue", size = 0.6, alpha = 0.6, linetype = "dashed") +  # High-frequency wave 1
  geom_line(aes(y = high_freq2), color = "red", size = 0.6, alpha = 0.6, linetype = "dashed") +   # High-frequency wave 2
  geom_line(aes(y = modulated_wave), color = "purple", size = 1) +  # Combined modulated wave
  geom_line(aes(y = envelope), color = "green", size = 1, linetype = "dotted") +  # Positive envelope
  geom_line(aes(y = -envelope), color = "green", size = 1, linetype = "dotted") + # Negative envelope
  labs(
    title = "Envelope Modulation Visualization",
    subtitle = "High-Frequency Waves and Their Low-Frequency Envelope",
    x = "Time",
    y = "Amplitude"
  ) +
  ylim(-2, 2) +  # Consistent y-axis range
  theme_minimal() +
  transition_time(time) +
  ease_aes("linear")  # Smooth transitions

# Save the animation
animate(
  p_modulation,
  renderer = gifski_renderer("envelope_modulation.gif"),
  width = 600,
  height = 400,
  fps = 10
)



# Create base data for two waves with slightly different frequencies
x_values <- seq(0, 4 * pi, length.out = 500)  # Sufficient resolution
modulation <- expand.grid(
  x = x_values,
  time = seq(1, 300, by = 1)  # Animation frames
)

# Define two close-frequency waves
modulation$wave1 <- sin(5 * modulation$x)  # Base wave
modulation$wave2 <- sin(5 * modulation$x + modulation$time * 0.05)  # Slightly shifted wave

# Combine waves and calculate envelope
modulation$combined_wave <- modulation$wave1 + modulation$wave2
modulation$envelope <- abs(sin(modulation$time * 0.05))  # Envelope from frequency difference

# Plot with shaded envelope
p_floating <- ggplot(modulation, aes(x = x)) +
  # Original waves (faded for context)
  geom_line(aes(y = wave1), color = "blue", size = 0.4, alpha = 0.4) +
  geom_line(aes(y = wave2), color = "red", size = 0.4, alpha = 0.4) +
  # Combined wave
  geom_line(aes(y = combined_wave), color = "purple", size = 1) +
  # Shaded envelope area
  geom_ribbon(aes(ymin = -envelope, ymax = envelope), fill = "green", alpha = 0.2) +
  # Envelope lines
  geom_line(aes(y = envelope), color = "green", linetype = "dotted", size = 0.8) +
  geom_line(aes(y = -envelope), color = "green", linetype = "dotted", size = 0.8) +
  labs(
    title = "Floating (Beat Frequency) Visualization",
    subtitle = "Interference of Two Close-Frequency Waves",
    x = "Time",
    y = "Amplitude"
  ) +
  ylim(-2, 2) +
  theme_minimal() +
  transition_time(time) +
  ease_aes("linear")

# Save the animation
animate(
  p_floating,
  renderer = gifski_renderer("floating_visualization.gif"),
  width = 600,
  height = 400,
  fps = 20
)



# Create base data for two waves with slightly different frequencies
x_values <- seq(0, 8 * pi, length.out = 500)  # Extend range for better visualization
modulation <- expand.grid(
  x = x_values,
  time = seq(1, 100, by = 1)  # Animation frames
)

# Define two close-frequency waves
modulation$wave1 <- sin(5 * modulation$x)  # Base wave
modulation$wave2 <- sin(5 * modulation$x + modulation$time * 0.05)  # Slightly shifted wave

# Combined wave with floating effect
modulation$combined_wave <- modulation$wave1 + modulation$wave2

# Envelope as low-frequency oscillation
modulation$envelope <- abs(sin(modulation$time * 0.05))  # Envelope from frequency difference

# Plot with combined wave and envelope
p_floating <- ggplot(modulation, aes(x = x)) +
  # Combined wave showing floating effect
  geom_line(aes(y = combined_wave), color = "purple", size = 1) +
  # Add envelope lines to show peaks and troughs
  geom_line(aes(y = envelope), color = "green", linetype = "dotted", size = 1.2) +
  geom_line(aes(y = -envelope), color = "green", linetype = "dotted", size = 1.2) +
  labs(
    title = "Floating (Beat Frequency) with Envelope Visualization",
    subtitle = "Amplitude Modulation Due to Interference of Two Close-Frequency Waves",
    x = "Time",
    y = "Amplitude"
  ) +
  ylim(-3, 3) +  # Adjust y-axis for better visualization
  theme_minimal() +
  transition_time(time) +  # Animate over time
  ease_aes("linear")  # Smooth animation

# Save the animation
animate(
  p_floating,
  renderer = gifski_renderer("floating_with_envelope.gif"),
  width = 600,
  height = 400,
  fps = 20
)

# Load necessary libraries
library(ggplot2)
library(gganimate)

# Define the x-axis (time) range
x_values <- seq(0, 4 * pi, length.out = 500)

# Create data frame for the message, carrier, and modulated signals
modulation <- data.frame(
  x = x_values,
  message = sin(0.5 * x_values),  # Low-frequency message signal
  carrier = sin(5 * x_values),   # High-frequency carrier signal
  modulated = (1 + sin(0.5 * x_values)) * sin(5 * x_values)  # AM wave
)

# Plot animation
p_modulation <- ggplot(modulation, aes(x = x)) +
  # Step 1: Message signal
  geom_line(aes(y = message), color = "blue", size = 1.2) +
  annotate("text", x = 5, y = 1, label = "Message Signal", color = "blue", hjust = 0) +
  
  # Step 2: Carrier signal
  geom_line(aes(y = carrier), color = "red", size = 0.8, alpha = 0.5) +
  annotate("text", x = 5, y = -1.5, label = "Carrier Signal", color = "red", hjust = 0) +
  
  # Step 3: AM wave
  geom_line(aes(y = modulated), color = "purple", size = 1) +
  annotate("text", x = 2, y = 2, label = "AM Wave", color = "purple", hjust = 0) +
  
  # Envelope lines
  geom_line(aes(y = 1 + message), color = "green", linetype = "dotted", size = 1) +
  geom_line(aes(y = -1 - message), color = "green", linetype = "dotted", size = 1) +
  annotate("text", x = 3, y = 1.8, label = "Envelope", color = "green", hjust = 0) +
  
  labs(
    title = "Amplitude Modulation (AM) and Envelope",
    x = "Time",
    y = "Amplitude"
  ) +
  theme_minimal() +
  transition_reveal(x)

# Save animation as a GIF
animate(
  p_modulation,
  renderer = gifski_renderer("amplitude_modulation.gif"),
  width = 1600,
  height = 400,
  fps = 15
)

# Load necessary library
library(ggplot2)

# Define spatial range and electrode parameters
x <- seq(-10, 10, length.out = 500)  # Spatial axis
E1_strength <- 1.5  # Increased amplitude for Electrode 1
E2_strength <- 0.6  # Amplitude for Electrode 2

# Define the fields using inverse square-like attenuation
E1 <- E1_strength / (1 + (x - 3)^2)  # Field from Electrode 1 (shifted to x = 3)
E2 <- E2_strength / (1 + (x + 3)^2)  # Field from Electrode 2 (shifted to x = -3)

# Calculate the envelope modulation
envelope <- pmin(E1, E2)

# Create a data frame for visualization
data <- data.frame(
  x = x,
  E1 = E1,
  E2 = E2,
  Envelope = envelope
)

# Plot the fields and envelope modulation
ggplot(data) +
  geom_line(aes(x = x, y = E1), color = "blue", linetype = "dashed", size = 1.2, 
            alpha = 0.8, label = "Field from Electrode 1") +
  geom_line(aes(x = x, y = E2), color = "orange", linetype = "dashed", size = 1.2, 
            alpha = 0.8, label = "Field from Electrode 2") +
  geom_line(aes(x = x, y = Envelope), color = "green", size = 1.5, 
            label = "Envelope Modulation") +
  labs(
    title = "Visualization of Temporal Interference with Increased Electrode 1 Strength",
    x = "Position (arbitrary units)",
    y = "Field Strength"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top") +
  scale_y_continuous(expand = c(0, 0)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8)

# Load necessary library
library(ggplot2)

# Define spatial range and electrode parameters
x <- seq(-10, 10, length.out = 500)  # Spatial axis
E1_strength <- 1.5  # Original amplitude for Electrode 1
E2_strength <- 0.5  # Amplitude for Electrode 2

# Define the fields using inverse square-like attenuation
E1 <- E1_strength / (1 + (x - 3)^2)  # Field from Electrode 1 (shifted to x = 3)
E2 <- E2_strength / (1 + (x + 3)^2)  # Field from Electrode 2 (shifted to x = -3)

# Calculate the envelope modulation
envelope <- pmin(E1, E2)

# Find the position where the envelope is maximum
max_envelope_index <- which.max(envelope)
max_envelope_x <- x[max_envelope_index]
max_envelope_y <- envelope[max_envelope_index]

# Create a data frame for visualization
data <- data.frame(
  x = x,
  E1 = E1,
  E2 = E2,
  Envelope = envelope
)

# Plot the fields and envelope modulation with a vertical line
# Load necessary library
library(ggplot2)

# Define spatial range and electrode parameters
x <- seq(-9, 9, length.out = 500)  # Spatial axis
E1_strength <- 1.5  # Original amplitude for Electrode 1
E2_strength <- 0.5  # Amplitude for Electrode 2

# Define the fields using inverse square-like attenuation
E1 <- E1_strength / (1 + (x - 3)^2)  # Field from Electrode 1 (shifted to x = 3)
E2 <- E2_strength / (1 + (x + 3)^2)  # Field from Electrode 2 (shifted to x = -3)

# Calculate the envelope modulation
envelope <- pmin(E1, E2)

# Find the position where the envelope is maximum
max_envelope_index <- which.max(envelope)
max_envelope_x <- x[max_envelope_index]
max_envelope_y <- envelope[max_envelope_index]

# Create a data frame for visualization
data <- data.frame(
  x = x,
  E1 = E1,
  E2 = E2,
  Envelope = envelope
)

# Plot the fields and envelope modulation with a vertical line
ggplot(data) +
  geom_line(aes(x = x, y = E1, color = "Electrode pair 1"), size = 1, linetype = "dashed") +
  geom_line(aes(x = x, y = E2, color = "Electrode pair 2"), size = 1, linetype = "dashed") +
  geom_line(aes(x = x, y = Envelope, color = "Envelope Modulation"), size = 1) +
  geom_vline(xintercept = max_envelope_x, linetype = "dotted", color = "red", size = 1.0) +
  geom_vline(xintercept = 0, linetype = "solid", color = "grey", size = 0.9) +
  annotate(
    "rect",
    xmin = 0, xmax = max_envelope_x,    # x-range of the rectangle
    ymin = 0, ymax = 1.5, # Use the actual maximum value of the Envelope
    fill = "red", alpha = 0.2           # Light red color with transparency
  ) +
  labs(
    title = "TI Fields for 1:3 current ratio",
    x = "Position (arbitrary units of distance)",
    y = "Field Strength"
  ) +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(-9, 9, 3)) +
  scale_y_continuous(breaks = seq(0, 2, 0.5), name = "Field Strength") +
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  scale_color_manual(values = c("Electrode pair 1" = "blue", "Electrode pair 2" = "orange", "Envelope Modulation" = "green"),
                     name = "") +
  theme(legend.position = "top") 


library(ggplot2)

# Parameters for the sine waves
freq1 <- 20   # Frequency of wave 1 in Hz
freq2 <- 21   # Frequency of wave 2 in Hz
sampling_rate <- 1000  # Adjusted sampling rate for lower frequencies
duration <- 1  # Duration to show two full envelope oscillations
amplitude <- 1  # Amplitude of the waves

# Time sequence
time <- seq(0, duration, by = 1 / sampling_rate)

# Generate the waves
wave1 <- amplitude * sin(2 * pi * freq1 * time)
wave2 <- amplitude * sin(2 * pi * freq2 * time)
combined_wave <- wave1 + wave2

# Calculate the envelope using the absolute value of the combined wave


# Data frame for plotting
plot_data <- data.frame(
  time = time,
  Wave1 = wave1,
  Wave2 = wave2,
  Combined = combined_wave
)

# Create the plot
ggplot(plot_data, aes(x = time)) +
  geom_line(aes(y = Wave1, color = "Wave 1 (20 Hz)"), size = 0.7, alpha = 0.5) +
  geom_line(aes(y = Wave2, color = "Wave 2 (21 Hz)"), size = 0.7, alpha = 0.5) +
  geom_line(aes(y = Combined, color = "Combined Wave"), size = 1) +
  scale_color_manual(values = c(
    "Wave 1 (20 Hz)" = "blue",
    "Wave 2 (21 Hz)" = "orange",
    "Combined Wave" = "purple"
  )) +
  labs(
    title = "Wave Interference and Envelope Formation",
    subtitle = "20 Hz and 21 Hz Waves",
    x = "Time (s)",
    y = "Amplitude",
    color = "Legend"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")







  library(ggplot2)
  library(gganimate)
  
  # Parameters
  freq1 <- 2000  # Frequency of wave 1 in Hz
  freq2 <- 2005  # Frequency of wave 2 in Hz
  sampling_rate <- 100000  # Sampling rate for smooth waves
  duration <- 0.01  # Duration in seconds
  amplitude <- 1  # Amplitude of both waves
  
  # Time sequence
  time <- seq(0, duration, by = 1/sampling_rate)
  
  # Generate the waves
  wave1 <- amplitude * sin(2 * pi * freq1 * time)
  wave2 <- amplitude * sin(2 * pi * freq2 * time)
  combined_wave <- wave1 + wave2
  envelope <- 2 * amplitude * abs(sin(pi * (freq2 - freq1) * time))
  
  # Create a data frame
  data <- data.frame(
    time = rep(time, 4),
    value = c(wave1, wave2, combined_wave, envelope),
    type = rep(c("Wave 1 (2 kHz)", "Wave 2 (2.005 kHz)", "Combined Wave", "Envelope"), each = length(time))
  )
  
  # Plot
  p <- ggplot(data, aes(x = time, y = value, color = type, group = type)) +
    geom_line(size = 1) +
    scale_color_manual(values = c("blue", "orange", "purple", "green")) +
    labs(
      title = "Temporal Interference: Combining High-Frequency Waves",
      subtitle = "Wave 1 (2 kHz) + Wave 2 (2.005 kHz) = Combined Wave with Envelope (5 Hz)",
      x = "Time (s)",
      y = "Amplitude",
      color = "Wave Type"
    ) +
    theme_minimal(base_size = 14)
  
  # Animate
  anim <- p +
    transition_reveal(time) +
    enter_fade() +
    exit_fade()
  
  # Save as GIF
  animate(anim, width = 800, height = 400, fps = 60, duration = 10, renderer = gifski_renderer("temporal_interference.gif"))
  
  

  
  
  
  library(ggplot2)
  library(gganimate)
  
  # Parameters
  freq1 <- 2000  # Frequency of wave 1 in Hz
  freq2 <- 2005  # Frequency of wave 2 in Hz
  sampling_rate <- 100000  # Sampling rate for smooth waves
  duration <- 0.1  # Duration to cover half a wave of the envelope (0.1 seconds)
  amplitude <- 1  # Amplitude of both waves
  
  # Time sequence
  time <- seq(0, duration, by = 1/sampling_rate)
  
  # Generate the waves
  wave1 <- amplitude * sin(2 * pi * freq1 * time)
  wave2 <- amplitude * sin(2 * pi * freq2 * time)
  combined_wave <- wave1 + wave2
  envelope <- 2 * amplitude * abs(sin(pi * (freq2 - freq1) * time))
  
  # Create a data frame
  data <- data.frame(
    time = rep(time, 4),
    value = c(wave1, wave2, combined_wave, envelope),
    type = rep(c("Wave 1 (2 kHz)", "Wave 2 (2.005 kHz)", "Combined Wave", "Envelope"), each = length(time))
  )
  
  # Plot
  p <- ggplot(data, aes(x = time, y = value, color = type, group = type)) +
    geom_line(size = 1) +
    scale_color_manual(values = c("blue", "orange", "purple", "green")) +
    labs(
      title = "Temporal Interference: Combining High-Frequency Waves",
      subtitle = "Wave 1 (2 kHz) + Wave 2 (2.005 kHz) = Combined Wave with Envelope (5 Hz)",
      x = "Time (s)",
      y = "Amplitude",
      color = "Wave Type"
    ) +
    theme_minimal(base_size = 14)
  
  
  
  library(ggplot2)
  library(gganimate)
  
  # Parameters for simplified visualization
  freq1 <- 10  # Frequency of wave 1 in Hz
  freq2 <- 11  # Frequency of wave 2 in Hz
  sampling_rate <- 1000  # Sampling rate for smooth waves
  duration <- 2  # Show 2 seconds to include multiple full waves
  amplitude <- 1  # Amplitude of the waves
  
  # Time sequence
  time <- seq(0, duration, by = 1/sampling_rate)
  
  # Generate the waves
  wave1 <- amplitude * sin(2 * pi * freq1 * time)
  wave2 <- amplitude * sin(2 * pi * freq2 * time)
  combined_wave <- wave1 + wave2
  envelope <- 2 * amplitude * abs(sin(pi * (freq2 - freq1) * time))
  
  # Create a data frame
  data <- data.frame(
    time = rep(time, 4),
    value = c(wave1, wave2, combined_wave, envelope),
    type = rep(c("Wave 1 (10 Hz)", "Wave 2 (11 Hz)", "Combined Wave", "Envelope (1 Hz)"), each = length(time))
  )
  
  # Plot
  p <- ggplot(data, aes(x = time, y = value, color = type, group = type)) +
    geom_line(size = 1) +
    scale_color_manual(values = c("Wave 1 (10 Hz)" = "blue", 
                                  "Wave 2 (11 Hz)" = "orange", 
                                  "Combined Wave" = "purple", 
                                  "Envelope (1 Hz)" = "green")) +
    labs(
      title = "Visualization of Overlapping Waves and Envelope Modulation",
      subtitle = "Wave 1 (10 Hz) + Wave 2 (11 Hz) = Combined Wave with 1 Hz Envelope",
      x = "Time (s)",
      y = "Amplitude",
      color = "Wave Type"
    ) +
    theme_minimal(base_size = 14)
  
  # Animate
  anim <- p +
    transition_reveal(time) +
    enter_fade() +
    exit_fade()
  
  # Save as GIF
  animate(anim, width = 800, height = 400, fps = 60, duration = 10, renderer = gifski_renderer("simplified_modulation.gif"))
  
  
  
  
  
  
  
  
  library(ggplot2)
  library(gganimate)
  
  library(ggplot2)
  library(gganimate)
  
  # Parameters for visualization
  freq1 <- 5  # Frequency of wave 1 in Hz
  freq2 <- 6  # Frequency of wave 2 in Hz
  sampling_rate <- 1000  # Sampling rate for smooth waves
  duration <- 2  # Duration in seconds
  amplitude <- 1  # Amplitude of the waves
  
  # Time sequence
  time <- seq(0, duration, by = 1 / sampling_rate)
  
  # Generate the waves
  wave1 <- amplitude * sin(2 * pi * freq1 * time)
  wave2 <- amplitude * sin(2 * pi * freq2 * time)
  combined_wave <- wave1 + wave2
  
  # Smooth envelope calculation: absolute max amplitude over a short time window
  envelope <- amplitude * abs(sin(pi * (freq2 - freq1) * time))
  
  # Create a data frame for the waves
  data <- data.frame(
    time = rep(time, 3),
    value = c(wave1, wave2, combined_wave),
    type = rep(c("Wave 1 (5 Hz)", "Wave 2 (6 Hz)", "Combined Wave"), each = length(time))
  )
  
  # Data frame for the envelope line
  envelope_data <- data.frame(
    time = time,
    value = envelope,
    type = "Envelope Line"
  )
  
  # Combine the datasets
  plot_data <- rbind(data, envelope_data)
  
  # Plot
  p <- ggplot(plot_data, aes(x = time, y = value, color = type, group = type)) +
    geom_line(size = 1) +
    scale_color_manual(values = c(
      "Wave 1 (5 Hz)" = "blue",
      "Wave 2 (6 Hz)" = "orange",
      "Combined Wave" = "purple",
      "Envelope Line" = "green"
    )) +
    labs(
      title = "Overlapping Waves and Envelope Visualization",
      subtitle = "Wave 1 (5 Hz) + Wave 2 (6 Hz) = Combined Wave with Peaks Highlighted",
      x = "Time (s)",
      y = "Amplitude",
      color = "Wave Type"
    ) +
    theme_minimal(base_size = 14) +
    geom_line(data = envelope_data, aes(x = time, y = value), color = "green", size = 1, linetype = "dashed") +
    geom_line(data = envelope_data, aes(x = time, y = -value), color = "green", size = 1, linetype = "dashed")
  
  # Animate
  anim <- p +
    transition_reveal(time) +
    enter_fade() +
    exit_fade()
  
  # Save as GIF
  animate(anim, width = 800, height = 400, fps = 60, duration = 10, renderer = gifski_renderer("combined_wave_with_envelope_line.gif"))
  

# Step 4: Temporal Interference and Envelope Modulation
# Parameters for Temporal Interference
high_freq1 <- 30  # High frequency wave 1 (30 Hz)
high_freq2 <- 33  # High frequency wave 2 (33 Hz)
low_freq_diff <- high_freq2 - high_freq1  # Envelope frequency (3 Hz)

# Generate Temporal Interference Data
ti_wave <- data.frame(
  x = seq(0, 1, length.out = 500),  # Shorter time scale to show high frequency oscillations
  frame = rep(1:100, each = 5)
)
ti_wave$y1 <- sin(2 * pi * high_freq1 * ti_wave$x)  # First high frequency wave
ti_wave$y2 <- sin(2 * pi * high_freq2 * ti_wave$x)  # Second high frequency wave
ti_wave$y_sum <- ti_wave$y1 + ti_wave$y2           # Combined wave (envelope modulation)

# Create Temporal Interference Animation
p_ti <- ggplot(ti_wave, aes(x = x)) +
  geom_line(aes(y = y1), color = "blue", size = 0.8, alpha = 0.6, linetype = "dashed") +
  geom_line(aes(y = y2), color = "red", size = 0.8, alpha = 0.6, linetype = "dashed") +
  geom_line(aes(y = y_sum), color = "purple", size = 1.2) +
  labs(
    title = "Temporal Interference: Envelope Modulation",
    subtitle = "High-Frequency Waves Combine to Form a Low-Frequency Envelope",
    x = "Time (s)",
    y = "Amplitude"
  ) +
  theme_minimal() +
  transition_states(frame, transition_length = 1, state_length = 2)

# Save Temporal Interference as GIF
animate(p_ti, renderer = gifski_renderer("temporal_interference.gif"), width = 600, height = 400, fps = 20)
