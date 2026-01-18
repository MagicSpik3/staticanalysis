# staticanalysis: Legacy R Modernization Toolkit

**Author:** Mark London  
**License:** MIT  

## Overview
`staticanalysis` is a dual-purpose toolkit designed to bridge the gap between legacy R code
(often embedded in Excel or monolithic scripts) and modern, software engineering practices. 

It focuses on **Safety**, **Transparency**, and **Diplomacy**.

## Architecture
The package operates on two parallel tracks:

### Track A: The Repo Auditor (Verification)
* **Goal:** Ensure refactored code matches legacy behavior.
* **Mechanism:** AST-based parsing (not regex) to "fingerprint" functions and detect drift.
* **Status:** Planned (Migration of legacy prototype).

### Track B: The Excel Compiler (Execution)
* **Goal:** Safely execute business logic defined in non-developer formats (Excel/CSV).
* **Mechanism:** 1.  **Compiler:** Reads text rules $\rightarrow$ Validates Schema $\rightarrow$ Generates a "Recipe" (State Machine).
    2.  **Runtime:** Executes the Recipe in a sandboxed environment.
    3.  **Visualizer:** Maps the Recipe to a flow diagram for stakeholders.
* **Status:** Active Prototype (`compile_rules`, `run_recipe`).

## Usage

### Safe Execution of Accountant Rules
```r
library(staticanalysis)

# 1. Compile text rules into a safe Recipe object
# (Catches typos and forbidden variables before running)
recipe <- compile_rules("config/accountant_rules.csv")

# 2. Visualize logic for sign-off
print(visualize_recipe(recipe))

# 3. Execute safely
results <- run_recipe(recipe)
