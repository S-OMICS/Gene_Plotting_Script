# Load libraries
library(ggplot2)
library(dplyr)

# Set the file path to the gene_info data file
gene_info_file_path <- "/home/shubham/Downloads/hiriing_task2/Homo_sapiens.gene_info.gz"

# Read gene_info data file
data <- read.table(gene_info_file_path, header = TRUE, sep = "\t", quote = "", comment.char = "", stringsAsFactors = FALSE)

# Filter rows with ambiguous chromosome values (containing "|") and "MT" or "Un"
data_filtered <- data %>%
  filter(!grepl("\\|", chromosome) & chromosome %in% c(1:22, "X", "Y", "MT", "Un"))

# Count number of genes per chromosome
genes_per_chromosome <- data_filtered %>%
  group_by(chromosome) %>%
  summarize(Num_Genes = n())

# Reorder 
genes_per_chromosome$chromosome <- factor(genes_per_chromosome$chromosome, levels = c(1:22, "X", "Y", "MT", "Un"))

# Create the plot using ggplot2
my_plot <- ggplot(genes_per_chromosome, aes(x = chromosome, y = Num_Genes)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of genes in each Chromosome",
       x = "Chromosomes",
       y = "Gene count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line())

# Save the plot as a PDF file
ggsave("genes_per_chromosome_plot.pdf", my_plot, width = 8, height = 6, units = "in")

