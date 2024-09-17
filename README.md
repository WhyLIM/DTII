# DTII: Drug-Target-Indication Interaction Query Package

**DTII** is an R package designed to query drug-target-indication interactions using the Open Targets Platform GraphQL API. The package provides functions to search for interactions between drugs, targets, and diseases and retrieve detailed information about known drugs, targets, and associated diseases.

## Features

- **`search()`**: Perform a keyword search for drug, target or indication.
- **`get_interactions()`**: Retrieve detailed interaction data for drugs, targets, or diseases based on unique identifiers (ChEMBL ID, Ensembl ID, or EFO ID).

## Installation

You can install the package directly from GitHub:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install DTII package from GitHub
devtools::install_github("WhyLIM/DTII")
```

## Usage

Here are some examples of how to use the functions provided by the **DTII** package.

### 1. Searching for Drug-Target-Indication Interactions

You can use the `search()` function to perform a keyword-based search on the Open Targets Platform.

```r
library(DTII)

# Search for interactions using a keyword
result <- search("lung cancer")

# View the results
print(result)
```

### 2. Querying Interactions by Drug, Target, or Disease ID

The `get_interactions()` function allows you to retrieve detailed interaction data by providing a specific drug (ChEMBL ID), target (Ensembl ID), or disease (EFO ID).

```r
# Query interactions for a drug using ChEMBL ID
drug_interactions <- get_interactions("CHEMBL25", "drug")

# Query interactions for a target using Ensembl ID
target_interactions <- get_interactions("ENSG00000157764", "target")

# Query interactions for a disease using EFO ID
disease_interactions <- get_interactions("EFO_0000311", "disease")

# View the results
print(drug_interactions)
```

### 3. Handling API Limitations

The Open Targets API has a maximum limit of 10,000 results per query. The `search()` and `get_interactions()` functions handle this by returning the first 10,000 results if the query exceeds this limit.

## Dependencies

The **DTII** package relies on the following R packages:

- `httr`: For making HTTP requests.
- `jsonlite`: For parsing JSON responses.

These packages will be automatically installed when you install **DTII**.

## License

This package is licensed under the MIT License. See the `LICENSE` file for more details.

## Contributing

If you encounter any issues or have suggestions for improvements, feel free to submit an issue.
