-- randmat: random number generation
--
-- input:
--   nrows, ncols: the number of rows and cols
--   s: the seed
--
-- output:
--   matrix: a nrows by ncols integer matrix

class MAIN
inherit ARGUMENTS
create make

feature
  make
  local
    nrows, ncols, s: INTEGER
    matrix: ARRAY[separate ARRAY[INTEGER]]
  do
    create in.make_open_read(separate_character_option_value('i'))

    nrows := read_integer
    ncols := read_integer
    s := read_integer

    --print("nrows: " + nrows.out + ", ncols: " + ncols.out + ", s: " +
        --s.out + "%N")

    create matrix.make_empty

    randmat(nrows, ncols, s, matrix)

    --across 1 |..| nrows as ic loop
      --print("line: " + ic.item.out + ": ")
      --across 1 |..| ncols as jc loop
        --print(item(matrix.item(ic.item), jc.item).out + " ")
      --end
      --print("%N")
    --end
  end

  read_integer(): INTEGER
  do
    in.read_integer
    Result := in.last_integer
  end

  randmat(nrows, ncols, s: INTEGER; matrix: ARRAY[separate ARRAY[INTEGER]])
  do
    -- parallel for on matrix
    parfor(nrows, ncols, s, matrix)
    --print("randmat%N")
  end

  -- parallel for on matrix
  parfor(nrows, ncols, seed: INTEGER;
    matrix: ARRAY[separate ARRAY[INTEGER]])
  local
    worker: separate RANDMAT_PARFOR_WORKER
    workers: LINKED_LIST[separate RANDMAT_PARFOR_WORKER]
  do
    create workers.make
    create parfor_aggregator.make(nrows)
    across 1 |..| nrows as ic loop
      matrix.force(create_array(), ic.item)
      create worker.make(nrows, ncols, seed, matrix.item(ic.item),
          parfor_aggregator)
      workers.extend(worker)
    end
    -- parallel for on rows
    workers.do_all(agent launch_parfor_worker)
    parfor_result(parfor_aggregator)
    --print("parfor%N")
  end

  parfor_result(aggregator: separate RANDMAT_PARFOR_AGGREGATOR)
  require
    aggregator.is_all_done
  do
    --print("parfor_result%N")
  end

  launch_parfor_worker(worker: separate RANDMAT_PARFOR_WORKER)
  do
    --print("<")
    worker.live
    --print(">")
  end

  create_array(): separate ARRAY[INTEGER]
  do
    create {separate ARRAY[INTEGER]} Result.make_empty
  end

  item(array: separate ARRAY[INTEGER]; index: INTEGER): INTEGER
  do
    Result := array.item(index)
  end

feature {NONE}
  parfor_aggregator: separate RANDMAT_PARFOR_AGGREGATOR
  in: PLAIN_TEXT_FILE

end -- class RANDMAT

