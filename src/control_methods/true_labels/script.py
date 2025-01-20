import anndata as ad

## VIASH START
# Note: this section is auto-generated by viash at runtime. To edit it, make changes
# in config.vsh.yaml and then run `viash config inject config.vsh.yaml`.
par = {
  'input_train': 'resources_test/task_template/cxg_mouse_pancreas_atlas/train.h5ad',
  'input_test': 'resources_test/task_template/cxg_mouse_pancreas_atlas/test.h5ad',
  'input_solution': 'resources_test/task_template/cxg_mouse_pancreas_atlas/solution.h5ad',
  'output': 'output.h5ad'
}
meta = {
  'name': 'true_labels'
}
## VIASH END

print('Reading input files', flush=True)
input_train = ad.read_h5ad(par['input_train'])
input_test = ad.read_h5ad(par['input_test'])
input_solution = ad.read_h5ad(par['input_solution'])

print('Preprocess data', flush=True)
# ... preprocessing ...

print('Train model', flush=True)
# ... train model ...

print('Generate predictions', flush=True)
# ... generate predictions ...
obs_label_pred = input_solution.obs["label"]

print("Write output AnnData to file", flush=True)
output = ad.AnnData(
  uns={
    'dataset_id': input_train.uns['dataset_id'],
    'normalization_id': input_train.uns['normalization_id'],
    'method_id': meta['name']
  },
  obs={
    'label_pred': obs_label_pred
  }
)
output.obs_names = input_test.obs_names

output.write_h5ad(par['output'], compression='gzip')
