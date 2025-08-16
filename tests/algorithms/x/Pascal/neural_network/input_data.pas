{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
type DataSet = record
  images: array of IntArray;
  labels: array of IntArray;
  num_examples: integer;
  index_in_epoch: integer;
  epochs_completed: integer;
end;
type Datasets = record
  train: DataSet;
  validation: DataSet;
  test_ds: DataSet;
end;
type BatchResult = record
  dataset: DataSet;
  images: array of IntArray;
  labels: array of IntArray;
end;
var _nowSeed: int64 = 0;
var _nowSeeded: boolean = false;
procedure init_now();
var s: string; v: int64;
begin
  s := GetEnvironmentVariable('MOCHI_NOW_SEED');
  if s <> '' then begin
    Val(s, v);
    _nowSeed := v;
    _nowSeeded := true;
  end;
end;
function _now(): integer;
begin
  if _nowSeeded then begin
    _nowSeed := (_nowSeed * 1664525 + 1013904223) mod 2147483647;
    _now := _nowSeed;
  end else begin
    _now := Integer(GetTickCount64()*1000);
  end;
end;
function _bench_now(): int64;
begin
  _bench_now := GetTickCount64()*1000;
end;
function _mem(): int64;
var h: TFPCHeapStatus;
begin
  h := GetFPCHeapStatus;
  _mem := h.CurrHeapUsed;
end;
procedure panic(msg: string);
begin
  writeln(msg);
  halt(1);
end;
procedure error(msg: string);
begin
  panic(msg);
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
function list_int_to_str(xs: array of integer): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  batch_size: integer;
  test_labels_raw: IntArray;
  num_classes: integer;
  labels: IntArrayArray;
  images: IntArrayArray;
  validation_size: integer;
  ds: DataSet;
  train_labels_raw: IntArray;
  train_images: IntArrayArray;
  test_images: IntArrayArray;
function makeBatchResult(dataset_var: DataSet; images: IntArrayArray; labels: IntArrayArray): BatchResult; forward;
function makeDatasets(train: DataSet; validation: DataSet; test_ds: DataSet): Datasets; forward;
function makeDataSet(images: IntArrayArray; labels: IntArrayArray; num_examples: integer; index_in_epoch: integer; epochs_completed: integer): DataSet; forward;
function dense_to_one_hot(labels: IntArray; num_classes: integer): IntArrayArray; forward;
function new_dataset(images: IntArrayArray; labels: IntArrayArray): DataSet; forward;
function next_batch(ds: DataSet; batch_size: integer): BatchResult; forward;
function read_data_sets(train_images: IntArrayArray; train_labels_raw: IntArray; test_images: IntArrayArray; test_labels_raw: IntArray; validation_size: integer; num_classes: integer): Datasets; forward;
procedure main(); forward;
function makeBatchResult(dataset_var: DataSet; images: IntArrayArray; labels: IntArrayArray): BatchResult;
begin
  Result.dataset := dataset_var;
  Result.images := images;
  Result.labels := labels;
end;
function makeDatasets(train: DataSet; validation: DataSet; test_ds: DataSet): Datasets;
begin
  Result.train := train;
  Result.validation := validation;
  Result.test_ds := test_ds;
end;
function makeDataSet(images: IntArrayArray; labels: IntArrayArray; num_examples: integer; index_in_epoch: integer; epochs_completed: integer): DataSet;
begin
  Result.images := images;
  Result.labels := labels;
  Result.num_examples := num_examples;
  Result.index_in_epoch := index_in_epoch;
  Result.epochs_completed := epochs_completed;
end;
function dense_to_one_hot(labels: IntArray; num_classes: integer): IntArrayArray;
var
  dense_to_one_hot_result_: array of IntArray;
  dense_to_one_hot_i: integer;
  dense_to_one_hot_row: array of integer;
  dense_to_one_hot_j: integer;
begin
  dense_to_one_hot_result_ := [];
  dense_to_one_hot_i := 0;
  while dense_to_one_hot_i < Length(labels) do begin
  dense_to_one_hot_row := [];
  dense_to_one_hot_j := 0;
  while dense_to_one_hot_j < num_classes do begin
  if list_int_to_str(dense_to_one_hot_j) = list_int_to_str(labels[dense_to_one_hot_i]) then begin
  dense_to_one_hot_row := concat(dense_to_one_hot_row, IntArray([1]));
end else begin
  dense_to_one_hot_row := concat(dense_to_one_hot_row, IntArray([0]));
end;
  dense_to_one_hot_j := dense_to_one_hot_j + 1;
end;
  dense_to_one_hot_result_ := concat(dense_to_one_hot_result_, [dense_to_one_hot_row]);
  dense_to_one_hot_i := dense_to_one_hot_i + 1;
end;
  exit(dense_to_one_hot_result_);
end;
function new_dataset(images: IntArrayArray; labels: IntArrayArray): DataSet;
begin
  exit(makeDataSet(images, labels, Length(images), 0, 0));
end;
function next_batch(ds: DataSet; batch_size: integer): BatchResult;
var
  next_batch_start: integer;
  next_batch_rest: integer;
  next_batch_images_rest: array of integer;
  next_batch_labels_rest: array of integer;
  next_batch_new_index: integer;
  next_batch_images_new: array of integer;
  next_batch_labels_new: array of integer;
  next_batch_batch_images: array of integer;
  next_batch_batch_labels: array of integer;
  next_batch_new_ds: DataSet;
  next_batch_end_: integer;
begin
  next_batch_start := ds.index_in_epoch;
  if (next_batch_start + batch_size) > ds.num_examples then begin
  next_batch_rest := ds.num_examples - next_batch_start;
  next_batch_images_rest := copy(ds.images, next_batch_start, (ds.num_examples - (next_batch_start)));
  next_batch_labels_rest := copy(ds.labels, next_batch_start, (ds.num_examples - (next_batch_start)));
  next_batch_new_index := batch_size - next_batch_rest;
  next_batch_images_new := copy(ds.images, 0, (next_batch_new_index - (0)));
  next_batch_labels_new := copy(ds.labels, 0, (next_batch_new_index - (0)));
  next_batch_batch_images := concat(next_batch_images_rest, next_batch_images_new);
  next_batch_batch_labels := concat(next_batch_labels_rest, next_batch_labels_new);
  next_batch_new_ds := makeDataSet(ds.images, ds.labels, ds.num_examples, next_batch_new_index, ds.epochs_completed + 1);
  exit(makeBatchResult(next_batch_new_ds, next_batch_batch_images, next_batch_batch_labels));
end else begin
  next_batch_end_ := next_batch_start + batch_size;
  next_batch_batch_images := copy(ds.images, next_batch_start, (next_batch_end_ - (next_batch_start)));
  next_batch_batch_labels := copy(ds.labels, next_batch_start, (next_batch_end_ - (next_batch_start)));
  next_batch_new_ds := makeDataSet(ds.images, ds.labels, ds.num_examples, next_batch_end_, ds.epochs_completed);
  exit(makeBatchResult(next_batch_new_ds, next_batch_batch_images, next_batch_batch_labels));
end;
end;
function read_data_sets(train_images: IntArrayArray; train_labels_raw: IntArray; test_images: IntArrayArray; test_labels_raw: IntArray; validation_size: integer; num_classes: integer): Datasets;
var
  read_data_sets_train_labels: IntArrayArray;
  read_data_sets_test_labels: IntArrayArray;
  read_data_sets_validation_images: array of integer;
  read_data_sets_validation_labels: array of integer;
  read_data_sets_train_images_rest: array of integer;
  read_data_sets_train_labels_rest: array of integer;
  read_data_sets_train: DataSet;
  read_data_sets_validation: DataSet;
  read_data_sets_testset: DataSet;
begin
  read_data_sets_train_labels := dense_to_one_hot(train_labels_raw, num_classes);
  read_data_sets_test_labels := dense_to_one_hot(test_labels_raw, num_classes);
  read_data_sets_validation_images := copy(train_images, 0, (validation_size - (0)));
  read_data_sets_validation_labels := copy(read_data_sets_train_labels, 0, (validation_size - (0)));
  read_data_sets_train_images_rest := copy(train_images, validation_size, (Length(train_images) - (validation_size)));
  read_data_sets_train_labels_rest := copy(read_data_sets_train_labels, validation_size, (Length(read_data_sets_train_labels) - (validation_size)));
  read_data_sets_train := new_dataset(read_data_sets_train_images_rest, read_data_sets_train_labels_rest);
  read_data_sets_validation := new_dataset(read_data_sets_validation_images, read_data_sets_validation_labels);
  read_data_sets_testset := new_dataset(test_images, read_data_sets_test_labels);
  exit(makeDatasets(read_data_sets_train, read_data_sets_validation, read_data_sets_testset));
end;
procedure main();
var
  main_train_images: array of array of integer;
  main_train_labels_raw: array of integer;
  main_test_images: array of array of integer;
  main_test_labels_raw: array of integer;
  main_data: Datasets;
  main_ds: DataSet;
  main_res: BatchResult;
begin
  main_train_images := [[0, 1], [1, 2], [2, 3], [3, 4], [4, 5]];
  main_train_labels_raw := [0, 1, 2, 3, 4];
  main_test_images := [[5, 6], [6, 7]];
  main_test_labels_raw := [5, 6];
  main_data := read_data_sets(main_train_images, main_train_labels_raw, main_test_images, main_test_labels_raw, 2, 10);
  main_ds := main_data.train;
  main_res := next_batch(main_ds, 2);
  main_ds := main_res.dataset;
  writeln(list_list_int_to_str(main_res.images));
  writeln(list_list_int_to_str(main_res.labels));
  main_res := next_batch(main_ds, 2);
  main_ds := main_res.dataset;
  writeln(list_list_int_to_str(main_res.images));
  writeln(list_list_int_to_str(main_res.labels));
  main_res := next_batch(main_ds, 2);
  main_ds := main_res.dataset;
  writeln(list_list_int_to_str(main_res.images));
  writeln(list_list_int_to_str(main_res.labels));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
