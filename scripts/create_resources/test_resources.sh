#!/bin/bash

# get the root of the directory
REPO_ROOT=$(git rev-parse --show-toplevel)

# ensure that the command below is run from the root of the repository
cd "$REPO_ROOT"

set -e

OUT_DIR=resources_test/task_foundation_models/results

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
  OUTPUT_DIR="$OUT_DIR/$TASK_STRIP_PREFIX"

  echo "Syncing '$INPUT_DIR' to '$OUTPUT_DIR'"
  aws s3 sync "$INPUT_DIR" "$OUTPUT_DIR" --delete --no-sign-request
done

# only run this if you have access to the openproblems-data bucket
aws s3 sync --profile op \
  "resources_test/task_foundation_models" \
  s3://openproblems-data/resources_test/task_foundation_models \
  --delete --dryrun
