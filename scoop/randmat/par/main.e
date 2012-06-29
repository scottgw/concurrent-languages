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
  EXCEPTIONS
create make

feature
  make
  local
    nrows, ncols, s: INTEGER
    matrix: ARRAY[separate ARRAY[INTEGER]]
    is_bench: BOOLEAN
    arg: STRING_8
  do
    create in.make_open_read(separate_character_option_value('i'))
    arg := separate_character_option_value('e')
    is_bench := False
    if arg /= Void then
      is_bench := arg.is_equal("is_bench")
    end

    nrows := read_integer
    ncols := read_integer
    s := read_integer

    --print("nrows: " + nrows.out + ", ncols: " + ncols.out + ", s: " +
        --s.out + "%N")

    create matrix.make_empty
    nrows_ := nrows
    ncols_ := ncols
    matrix_ := matrix
    is_bench_ := is_bench
    randmat(nrows, ncols, s, matrix)

    if not is_bench then
      across 1 |..| nrows as ic loop
        across 1 |..| ncols as jc loop
          print(item(matrix.item(ic.item), jc.item).out + " ")
        end
        print("%N")
      end
    end
    --print("main dead%N%N")
    die(0)
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
    create parfor_aggregator.make(nrows, Current)
    across 1 |..| nrows as ic loop
      matrix.force(create_array(), ic.item)
      create worker.make(nrows, ncols, seed + ic.item, matrix.item(ic.item),
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
  nrows_, ncols_: INTEGER
  matrix_: ARRAY[separate ARRAY[INTEGER]]
  is_bench_: BOOLEAN

end

