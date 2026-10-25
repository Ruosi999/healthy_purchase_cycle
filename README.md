# Healthy Purchase Cycle

Research repository for studying household healthy consumption dynamics.

## Project Structure

- `data_cleaning/`: R scripts for data preparation, merging, and UPC classification.
- `analysis/`: Python scripts and notebooks for HMM estimation, event studies, and descriptive statistics.
- `results/`: Approved research tables and figures.
- `slides/`: Research presentations.
- `docs/`: Data dictionaries, methodology, and reproducibility documentation.
- `config/`: Configuration templates.

## Data Availability

Restricted research data are not stored in this repository.
Raw, intermediate, and processed datasets must remain in authorized local storage.

## Coding Conventions

- Preserve `upc` and `upc_ver_uc` as character/string identifiers.
- Use clear English comments and documentation.
- Document important cleaning decisions, joins, and model assumptions.
- Keep all machine-specific paths outside version control.
