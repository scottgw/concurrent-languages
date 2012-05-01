-- winnow: weighted point selection
--
-- input:
--   matrix: an integer matrix, whose values are used as masses
--   mask: a boolean matrix showing which points are eligible for
--     consideration
--   nrows, ncols: the number of rows and cols
--   nelts: the number of points to select
--
-- output:
--   points: a vector of (x, y) points

class MAIN
inherit ARGUMENTS
create make
feature
  make
  local
    nrows, ncols, nelts: INTEGER
    matrix, mask: ARRAY[separate ARRAY[INTEGER]]
    i, j: INTEGER
    file_name: STRING
    points: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
  do
    file_name := separate_character_option_value('i')
    !!in.make_open_read(separate_character_option_value('i'))

    nrows := read_integer
    ncols := read_integer
    create matrix.make_empty
    read_matrix(nrows, ncols, matrix)
    create mask.make_empty
    read_matrix(nrows, ncols, mask)
    nelts := read_integer

    points := winnow(nrows, ncols, matrix, mask, nelts)

    print(nelts.out + "%N");
    across 1 |..| nelts as ic loop
      print(points.item(ic.item).integer_32_item(2).out + " " +
          points.item(ic.item).integer_32_item(3).out + "%N");
    end
    print("%N");
  end

  read_integer(): INTEGER
  do
    in.read_integer
    Result := in.last_integer
  end

  read_matrix(nrows, ncols: INTEGER; matrix: ARRAY[separate ARRAY[INTEGER]])
  do
    across 1 |..| nrows as ic loop
      matrix.force(create_array(), ic.item)
      across 1 |..| ncols as jc loop
        put(matrix.item(ic.item), read_integer, jc.item)
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

  winnow(nrows, ncols: INTEGER;
    matrix, mask: ARRAY[separate ARRAY[INTEGER]];
    nelts: INTEGER) : ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
  local
    points, values: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
    sorter: QUICK_SORTER[TUPLE[INTEGER, INTEGER, INTEGER]]
    comparator: TUPLE_COMPARATOR
    n, chunk, index: INTEGER
  do
    values := parfor(nrows, ncols, matrix, mask);

    create comparator
    create sorter.make(comparator)
    sorter.sort(values)

    n := values.count
    chunk := n // nelts

    create points.make(1, nelts);
    across 1 |..| nelts as ic loop
      index := (ic.item - 1) * chunk + 1
      points[ic.item] := values[index]
    end

    Result := points
  end

  parfor(nrows, ncols: INTEGER;
      matrix, mask: ARRAY[separate ARRAY[INTEGER]])
      : ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
  local
    worker: separate PARFOR_WORKER
    workers: LINKED_LIST[separate PARFOR_WORKER]
    reader: separate PARFOR_READER
  do
    create workers.make
    create reader.make
    create parfor_aggregator.make(nrows)
    across 1 |..| nrows as ic loop
      create worker.make(nrows, ncols, ic.item, matrix.item(ic.item),
        mask.item(ic.item), parfor_aggregator)
      workers.extend(worker)
    end
    workers.do_all(agent launch_parfor_worker)
    Result := parfor_result(reader)
  end

  launch_parfor_worker(worker: separate PARFOR_WORKER)
  do
    worker.live
  end

  parfor_result(reader: separate PARFOR_READER)
      : ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
  do
    Result := reader.get_result(parfor_aggregator)
  end

feature {NONE}
  in: PLAIN_TEXT_FILE
  parfor_aggregator: separate PARFOR_AGGREGATOR

end -- class MAIN 
