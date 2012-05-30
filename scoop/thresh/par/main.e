-- thresh: histogram thresholding
--
-- input:
--   matrix: the integer matrix to be thresholded
--   nrows, ncols: the number of rows and cols
--   percent: the percentage of cells to retain
--
-- output:
--   mask: a boolean matrix filled with true for cells kept

class MAIN
inherit ARGUMENTS
create make
feature
  make
  local
    nrows, ncols, percent: INTEGER
    matrix, mask: ARRAY[separate ARRAY[INTEGER]]
    in: PLAIN_TEXT_FILE
    file_name: STRING
  do
    file_name := separate_character_option_value('i')
    create in.make_open_read(separate_character_option_value('i'))

    in.read_integer
    nrows := in.last_integer

    in.read_integer
    ncols := in.last_integer

    create matrix.make_empty
    read_matrix(nrows, ncols, matrix, in)

    in.read_integer
    percent := in.last_integer

    create mask.make_empty
    thresh(nrows, ncols, matrix, percent, mask)

    --across 1 |..| nrows as ic loop
      --across 1 |..| ncols as jc loop
        --print(item(mask.item(ic.item), jc.item).out + " ")
      --end
      --print("%N")
    --end
  end

  read_matrix(nrows, ncols: INTEGER; matrix: ARRAY[separate ARRAY[INTEGER]];
      in: PLAIN_TEXT_FILE)
  do
    across 1 |..| nrows as ic loop
      matrix.force(create_array(), ic.item)
      across 1 |..| ncols as jc loop
        in.read_integer
        put(matrix.item(ic.item), in.last_integer, jc.item)
      end
    end
  end

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
      {REDUCE2D_OPERATOR}.max, {REDUCE2D_OPERATOR}.max)
  end

  reduce2d_with_filter(nrows, ncols: INTEGER;
      matrix: ARRAY[separate ARRAY[INTEGER]]; value: INTEGER): INTEGER
  do
    Result := reduce2d_impl(nrows, ncols, matrix, value,
      {REDUCE2D_OPERATOR}.sum, {REDUCE2D_OPERATOR}.filter)
  end

  reduce2d_impl(nrows, ncols: INTEGER;
      matrix: ARRAY[separate ARRAY[INTEGER]];
      value: INTEGER;
      op_aggregator: INTEGER;
      op: INTEGER): INTEGER
  local
    worker: separate REDUCE2D_WORKER
    workers: LINKED_LIST [separate REDUCE2D_WORKER]
    reader: separate REDUCE2D_READER
  do
    create workers.make
    create reader.make
    create reduce2d_aggregator.make(nrows, op_aggregator)
    across 1 |..| nrows as ic loop
      create worker.make_with_filter(nrows, ncols, matrix.item(ic.item), 
        reduce2d_aggregator, op, value)
      workers.extend(worker)
    end
    -- parallel for on rows
    workers.do_all(agent launch_reduce2d_worker)
    Result := reduce2d_result(reader)
  end

  reduce2d_result(reader: separate REDUCE2D_READER): INTEGER
  do
    Result := reader.get_result(reduce2d_aggregator)
  end

  -- parallel for on matrix
  parfor(nrows, ncols: INTEGER;
    matrix, mask: ARRAY[separate ARRAY[INTEGER]];
    threshold: INTEGER)
  local
    worker: separate PARFOR_WORKER
    workers: LINKED_LIST[separate PARFOR_WORKER]
    reader: separate PARFOR_READER
  do
    create workers.make
    create reader.make
    create parfor_aggregator.make(nrows)
    across 1 |..| nrows as ic loop
      mask.force(create_array(), ic.item)
      create worker.make(nrows, ncols, matrix.item(ic.item),
        mask.item(ic.item), threshold, parfor_aggregator)
      workers.extend(worker)
    end
    -- parallel for on rows
    workers.do_all(agent launch_parfor_worker)
    parfor_result(reader)
  end

  parfor_result(reader: separate PARFOR_READER)
  do
    reader.get_result(parfor_aggregator)
  end

feature {NONE}
  reduce2d_aggregator: separate REDUCE2D_AGGREGATOR
  parfor_aggregator: separate PARFOR_AGGREGATOR

  launch_reduce2d_worker(worker: separate REDUCE2D_WORKER)
  do
    worker.live
  end

  launch_parfor_worker(worker: separate PARFOR_WORKER)
  do
    worker.live
  end
end -- class MAIN 

