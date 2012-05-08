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

class WINNOW
inherit ARGUMENTS
create make
feature
  make
  local
    nrows, ncols, nelts: INTEGER
    matrix, mask: ARRAY[separate ARRAY[INTEGER]]
    points: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
  do
    create in.make_open_read(separate_character_option_value('i'))

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
    points: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
    values: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
    sorter: TUPLE_SORTER
    n, chunk, index: INTEGER
  do
    -- parallel for on matrix
    values := parfor(nrows, ncols, matrix, mask);

    create sorter.make()
    values := sorter.sort(values)

    n := values.count
    chunk := n // nelts

    create points.make_filled([0, 0, 0], 1, nelts);
    -- this should also be a parallel for
    across 1 |..| nelts as ic loop
      index := (ic.item - 1) * chunk + 1
      points[ic.item] := values[index]
    end

    Result := points
  end

  -- parallel for on matrix
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
    -- parallel for on rows
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
    Result := to_local_values(reader.get_result(parfor_aggregator))
  end

  to_local_values(values: separate ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]])
      : ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
  local
    local_values: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
  do
    create local_values.make_empty
    across 1 |..| values.count as ic loop
      local_values.force([
        values.item(ic.item).integer_32_item(1),
        values.item(ic.item).integer_32_item(2),
        values.item(ic.item).integer_32_item(3)], ic.item)
    end
    Result := local_values
  end

feature {NONE}
  in: PLAIN_TEXT_FILE
  parfor_aggregator: separate PARFOR_AGGREGATOR

end -- class WINNOW
