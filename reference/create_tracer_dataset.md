# Create Tracer Dataset (The Micro-Verse)

Reads the full raw data, filters it down to a specific set of IDs, and
saves mini-versions to a temporary directory.

## Usage

``` r
create_tracer_dataset(target_ids, paths_list, output_dir = "test_data/TRACER")
```

## Arguments

- target_ids:

  Vector. The IDs to "hand carry" (e.g., c("HAS18107...",
  "HAS18108...")).

- paths_list:

  List. Your current data_paths list.

- output_dir:

  String. Where to save the micro-files.
