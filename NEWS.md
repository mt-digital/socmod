# socmod 0.2.6 (2025-09-28)

- Added OpinionAgent and opinion dynamics with social influence
- Revised observation dispatch to take any user-defined observer function.
- Added `well_mixed_selection` partner selection for empty social networks.
- Organized R/ code directory and source files.

# socmod 0.2.5 (2025-09-27)

- Added observation dispatch to `run_trial()` and `run_trials()`.  
- Default remains `"behavior"`, ensuring backward compatibility.  

# socmod 0.2.4 (2025-09-24)

- If only `agents` are provided to `make_abm` an empty graph is created by
  default.
- If both `agents` and `graph` are provided to `make_abm`, the provided graph is
  used. Previously `make_abm` would create a fully connected network.

# socmod 0.2.3 (2025-09-03)

- Changed LearningStrategy and related functions to be ModelDynamics and
  removed the `frequency_bias_learning_strategy` and similar objects (see
  `R/model-dynamics.R`)

# socmod 0.2.2 (2025-05-14)

- `summarise_*` now work even if there are vectors/lists in parameters by
  removing them via helper `.clean_summary_params`

# socmod 0.2.1 (2025-05-13)

- Refined analysis helpers with updated, passing tests
- Updated README.Rmd with tools/make-figures-readme.R to simplify home/index 

# socmod 0.2.0 (2025-05-07)

- First full vignette-based pipeline working
- Added `initialize_agents()`
- Quarto-compatible documentation with reproducible examples

# socmod 0.1.0 (2025-04-15)

- Initial package release with working architecture for agent-based modeling
- Includes Agent, AgentBasedModel, Trial, and ModelParameters classes
- Implements success-biased, frequency-biased, and contagion learning strategies
- Core simulation functions: run_trial(), run_trials(), and stop conditions
- Basic plotting and outcome summarization: plot_prevalence(), summarise_by_metadata()
- Internal testing framework and documentation scaffolding

