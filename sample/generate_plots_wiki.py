import os
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Load the CSV file
df = pd.read_csv('species_pageviews_analysis_2025_01.csv')

# Identify date columns
date_columns = [col for col in df.columns if col not in ['Scientific Name', 'Language']]

# Reshape the data
df_long = pd.melt(df, id_vars=['Scientific Name', 'Language'], value_vars=date_columns, var_name='Date', value_name='Pageviews')

# Convert Date to datetime
df_long['Date'] = pd.to_datetime(df_long['Date'], format='%Y%m%d', errors='coerce')

# Ensure 'Pageviews' is numeric, replacing NaNs with 0
df_long['Pageviews'] = pd.to_numeric(df_long['Pageviews'], errors='coerce').fillna(0)

# Create directory for plots
output_dir = 'species_plots'
os.makedirs(output_dir, exist_ok=True)

# Define a high-contrast color palette
palette = sns.color_palette("tab20")  # Try "tab10", "Dark2", or "colorblind" for alternatives

# Loop through each species and create a plot
for species in df_long['Scientific Name'].unique():
    species_data = df_long[df_long['Scientific Name'] == species]

    plt.figure(figsize=(10, 6))
    sns.lineplot(data=species_data, x='Date', y='Pageviews', hue='Language', ci=None, palette=palette, linewidth=2.5, marker='o')

    plt.title(f'Pageview Trends for {species}', fontsize=14, fontweight='bold')
    plt.xlabel('Date', fontsize=12)
    plt.ylabel('Pageviews', fontsize=12)
    plt.xticks(rotation=45)

    # Move legend outside the plot
    plt.legend(loc='upper left', bbox_to_anchor=(1, 1), title='Language')

    # Save the plot
    plot_filename = os.path.join(output_dir, f"{species.replace(' ', '_')}.png")
    plt.tight_layout()
    plt.savefig(plot_filename, bbox_inches='tight')
    plt.clf()

print(f"Plots saved to the '{output_dir}' directory.")