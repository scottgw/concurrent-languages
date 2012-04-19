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
    i, j: INTEGER
    in: PLAIN_TEXT_FILE
    file_name: STRING
  do
    print("HERE%N")
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

    create mask.make_empty -- TODO initialize mask
    thresh(nrows, ncols, matrix, percent, mask)

    from i := 1 until i > nrows loop
      from j := 1 until j > ncols loop
        --print(mask.item(i, j).out + " ")
        j := j + 1
      end
      --print("%N")
      i := i + 1
    end
  end

  read_matrix(nrows, ncols: INTEGER; matrix: ARRAY[separate ARRAY[INTEGER]];
      in: PLAIN_TEXT_FILE)
  local
    i, j: INTEGER
  do
    from i := 1 until i > nrows loop
      matrix.force(create_array(), i)
      from j := 1 until j > ncols loop
        in.read_integer
        put(matrix.item(i), in.last_integer, j)
        --print(item(matrix.item(i), j).out + " ");
        j := j + 1
      end
      --print("%N")
      i := i + 1
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
    i, j: INTEGER
    nmax: INTEGER
    histogram: ARRAY[INTEGER]
    count: REAL_64
    prefixsum, threshold: INTEGER
    index: INTEGER
  do
    -- across matrix as m loop
      -- nmax := nmax.max(m.item)
    -- end
    nmax := reduce2d(nrows, ncols, matrix);
    print("--> nmax: " + nmax.out + "%N")

    --create histogram.make(0, nmax + 1)

    --from i:= 1 until i > nrows loop
      --from j := 1 until j > ncols loop
        --index := item(matrix.item(i), j)
        --histogram.put(histogram.item(index) + 1, index)
        --j := j + 1
      --end
      --i := i + 1
    --end
--
    --count := (nrows * ncols * percent) / 100
--
    --prefixsum := 0
    --threshold := nmax
--
    --from i := nmax until not(i >= 0 and prefixsum <= count) loop
      --prefixsum := prefixsum + histogram[i];
      --threshold := i;
      --i := i - 1
    --end
--
    --from i := 1 until i > nrows loop
      --from j := 1 until j > ncols loop
        --if item(matrix.item(i), j) >= threshold then
          --put(mask.item(i), 1, j)
        --end
        --j := j + 1
      --end
      --i := i + 1
    --end

  end

  reduce2d(nrows, ncols: INTEGER; matrix: ARRAY[separate ARRAY[INTEGER]])
      : INTEGER
  local
    i: INTEGER
    worker: separate REDUCE2D_WORKER
    workers: LINKED_LIST [separate REDUCE2D_WORKER]
    reader: separate REDUCE2D_READER
  do
    print("HERE%N")
    create workers.make
    create reader.make
    create aggregator.make(nrows)
    from i := 1 until i > nrows loop
      create worker.make(nrows, ncols, matrix.item(i), aggregator)
      workers.extend(worker)
      i := i + 1
    end
    workers.do_all(agent launch_reduce2d_worker)
    Result := reduce2d_result(reader)
  end

  reduce2d_result(reader: separate REDUCE2D_READER): INTEGER
  do
    Result := reader.get_result(aggregator)
  end

feature {NONE}
  aggregator: separate REDUCE2D_AGGREGATOR

  launch_reduce2d_worker(worker: separate REDUCE2D_WORKER)
  do
    print("+")
    worker.live
    print("-")
  end

end -- class MAIN 
