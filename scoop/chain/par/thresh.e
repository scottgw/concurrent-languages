-- thresh: histogram thresholding
--
-- input:
--   matrix: the integer matrix to be thresholded
--   nrows, ncols: the number of rows and cols
--   percent: the percentage of cells to retain
--
-- output:
--   mask: a boolean matrix filled with true for cells kept

class THRESH
inherit ARGUMENTS
create make_empty
feature
  make_empty do end

  create_array(): separate ARRAY[INTEGER]
  do
    create {separate ARRAY[INTEGER]} Result.make_empty
  end

  put(array: separate ARRAY[INTEGER]; value, index: INTEGER)
  do
    array.force(value, index)
  end

  item(array: separate ARRAY[INTEGER]; index: INTEGER): INTEGER
  do
    Result := array.item(index)
  end

  thresh(nrows, ncols: INTEGER; matrix: ARRAY[separate ARRAY[INTEGER]];
      percent: INTEGER; mask: ARRAY[separate ARRAY[INTEGER]])
  local
    nmax: INTEGER
    histogram: ARRAY[INTEGER]
    count: INTEGER
    prefixsum, threshold: INTEGER
    i: INTEGER
  do
    nmax := reduce2d(nrows, ncols, matrix);

    create histogram.make_filled(0, 0, nmax + 1)

    across 0 |..| (nmax + 1) as ic loop
      histogram.put(reduce2d_with_filter(nrows, ncols, matrix, ic.item),
          ic.item)
    end

    count := (nrows * ncols * percent) // 100

    prefixsum := 0
    threshold := nmax

    from i := nmax until not(i >= 0 and prefixsum <= count) loop
      prefixsum := prefixsum + histogram[i];
      threshold := i;
      i := i - 1
    end

    -- parallel for on matrix
    parfor(nrows, ncols, matrix, mask, threshold)

  end

  reduce2d(nrows, ncols: INTEGER; matrix: ARRAY[separate ARRAY[INTEGER]])
      : INTEGER
  do
    Result := reduce2d_impl(nrows, ncols, matrix, 0,
      {THRESH_REDUCE2D_OPERATOR}.max, {THRESH_REDUCE2D_OPERATOR}.max)
  end

  reduce2d_with_filter(nrows, ncols: INTEGER;
      matrix: ARRAY[separate ARRAY[INTEGER]]; value: INTEGER): INTEGER
  do
    Result := reduce2d_impl(nrows, ncols, matrix, value,
      {THRESH_REDUCE2D_OPERATOR}.sum, {THRESH_REDUCE2D_OPERATOR}.filter)
  end

  reduce2d_impl(nrows, ncols: INTEGER;
      matrix: ARRAY[separate ARRAY[INTEGER]];
      value: INTEGER;
      op_aggregator: INTEGER;
      op: INTEGER): INTEGER
  local
    worker: separate THRESH_REDUCE2D_WORKER
    workers: LINKED_LIST [separate THRESH_REDUCE2D_WORKER]
  do
    create workers.make
    create reduce2d_aggregator.make(nrows, op_aggregator)
    across 1 |..| nrows as ic loop
      create worker.make_with_filter(nrows, ncols, matrix.item(ic.item), 
        reduce2d_aggregator, op, value)
      workers.extend(worker)
    end
    -- parallel for on rows
    workers.do_all(agent launch_reduce2d_worker)
    Result := reduce2d_result(reduce2d_aggregator)
  end

  reduce2d_result(aggregator: separate THRESH_REDUCE2D_AGGREGATOR): INTEGER
  require
    aggregator.is_all_done
  do
    Result := aggregator.get_result
  end

  -- parallel for on matrix
  parfor(nrows, ncols: INTEGER;
    matrix, mask: ARRAY[separate ARRAY[INTEGER]];
    threshold: INTEGER)
  local
    worker: separate THRESH_PARFOR_WORKER
    workers: LINKED_LIST[separate THRESH_PARFOR_WORKER]
  do
    create workers.make
    create parfor_aggregator.make(nrows)
    across 1 |..| nrows as ic loop
      mask.force(create_array(), ic.item)
      create worker.make(nrows, ncols, matrix.item(ic.item),
        mask.item(ic.item), threshold, parfor_aggregator)
      workers.extend(worker)
    end
    -- parallel for on rows
    workers.do_all(agent launch_parfor_worker)
    parfor_result(parfor_aggregator)
  end

  parfor_result(aggregator: separate THRESH_PARFOR_AGGREGATOR)
  require
    aggregator.is_all_done
  do
  end

feature {NONE}
  reduce2d_aggregator: separate THRESH_REDUCE2D_AGGREGATOR
  parfor_aggregator: separate THRESH_PARFOR_AGGREGATOR

  launch_reduce2d_worker(worker: separate THRESH_REDUCE2D_WORKER)
  do
    worker.live
  end

  launch_parfor_worker(worker: separate THRESH_PARFOR_WORKER)
  do
    worker.live
  end
end -- class THRESH

