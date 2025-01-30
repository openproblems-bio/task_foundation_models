#!/bin/bash

# get the root of the directory
REPO_ROOT=$(git rev-parse --show-toplevel)

# ensure that the command below is run from the root of the repository
cd "$REPO_ROOT"

set -e

OUT_DIR=resources_test/task_foundation_models

TASKS=(
  "task_label_projection"
  "task_batch_integration"
)

if [ -d "$OUT_DIR" ]; then
  echo "Removing existing directory '$OUT_DIR'"
  rm -rf "$OUT_DIR"
fi

mkdir -p "$OUT_DIR"

for TASK in "${TASKS[@]}"; do
  BASE_DIR="s3://openproblems-data/resources/$TASK/results"

  # find subdir in bucket with latest date which has a 'task_info.yaml' file
  DATE=$(aws s3 ls "$BASE_DIR/" --recursive --no-sign-request | awk '{print $4}' | grep 'task_info.yaml' | sort -r | head -n 1 | sed 's#.*/run_\(.*\)/[^/]*$#\1#')

  INPUT_DIR="$BASE_DIR/run_$DATE"
  TASK_STRIP_PREFIX=$(echo $TASK | sed 's/task_//')
  RESULTS_DIR="$OUT_DIR/$TASK_STRIP_PREFIX/results"
  PROCESSED_DIR="$OUT_DIR/$TASK_STRIP_PREFIX/processed"

  echo "Syncing '$INPUT_DIR' to '$RESULTS_DIR'"
  aws s3 sync "$INPUT_DIR" "$RESULTS_DIR" --delete --no-sign-request

  echo "Processing '$RESULTS_DIR' to '$PROCESSED_DIR'"
  nextflow run openproblems-bio/openproblems \
    -r build/main \
    -main-script target/nextflow/reporting/process_task_results/main.nf \
    -profile docker \
    --input_scores "$RESULTS_DIR/score_uns.yaml" \
    --input_dataset_info "$RESULTS_DIR/dataset_uns.yaml" \
    --input_method_configs "$RESULTS_DIR/method_configs.yaml" \
    --input_metric_configs "$RESULTS_DIR/metric_configs.yaml" \
    --input_execution "$RESULTS_DIR/trace.txt" \
    --input_task_info "$RESULTS_DIR/task_info.yaml" \
    --output_state "state.yaml" \
    --publish_dir "$PROCESSED_DIR"
done

# only run this if you have access to the openproblems-data bucket
aws s3 sync --profile op \
  "resources_test/task_foundation_models" \
  s3://openproblems-data/resources_test/task_foundation_models \
  --delete --dryrun
