-- outer: outer product
--
-- input:
--   vector: a vector of (x, y) points
--   nelts: the number of points
--
-- output:
--   matrix: a real matrix, whose values are filled with inter-point
--     distances
--   vector: a real vector, whose values are filled with origin-to-point
--     distances

class MAIN
inherit ARGUMENTS
create make
feature
  make
  local
    nelts: INTEGER
    points: ARRAY[TUPLE[INTEGER, INTEGER]]
    matrix: ARRAY[separate ARRAY[DOUBLE]]
    vector: ARRAY[DOUBLE]
    file_name: STRING
  do
    file_name := separate_character_option_value('i')
    create in.make_open_read(separate_character_option_value('i'))

    nelts := read_integer
    points := read_vector_of_points(nelts)
    create matrix.make_empty
    across 1 |..| nelts as ic loop
      matrix.force(create_array(), ic.item)
    end
    create vector.make_filled(0.0, 1, nelts)

    outer(nelts, points, matrix, vector)

    print(nelts.out + " " + nelts.out + "%N");
    across 1 |..| nelts as ic loop
      across 1 |..| nelts as jc loop
        print(item(matrix.item(ic.item), jc.item).out + " ");
      end
      print("%N");
    end
    print("%N");

    print(nelts.out + "%N");
    across 1 |..| nelts as ic loop
      print(vector[ic.item].out + " ");
    end
    print("%N");
  end

  create_array(): separate ARRAY[DOUBLE]
  do
    create {separate ARRAY[DOUBLE]} Result.make_empty
  end

  read_integer(): INTEGER
  do
    in.read_integer
    Result := in.last_integer
  end

  read_vector_of_points(nelts: INTEGER): ARRAY[TUPLE[INTEGER, INTEGER]]
  local
    vector: ARRAY[TUPLE[INTEGER, INTEGER]]
  do
    create vector.make_filled([0, 0], 1, nelts)
    across 1 |..| nelts as ic loop
      vector.put([read_integer, read_integer], ic.item)
    end
    Result := vector
  end

  outer(nelts: INTEGER; points: ARRAY[TUPLE[INTEGER, INTEGER]];
      matrix: ARRAY[separate ARRAY[DOUBLE]]; vector: ARRAY[DOUBLE])
  do
    -- parallel for on nelts
    parfor(nelts, points, matrix, vector);

    across 1 |..| nelts as ic loop
      vector.put(distance([0, 0], points.item(ic.item)), ic.item)
    end
  end

  -- parallel for on nelts
  parfor(nelts: INTEGER; points: ARRAY[TUPLE[INTEGER, INTEGER]];
      matrix: ARRAY[separate ARRAY[DOUBLE]]; vector: ARRAY[DOUBLE])
  local
    worker: separate PARFOR_WORKER
    workers: LINKED_LIST[separate PARFOR_WORKER]
    reader: separate PARFOR_READER
  do
    create workers.make
    create reader.make
    create parfor_aggregator.make(nelts)
    across 1 |..| nelts as ic loop
      create worker.make(nelts, ic.item, matrix.item(ic.item),
        to_separate(nelts, points), parfor_aggregator)
      workers.extend(worker)
    end
    -- parallel for on rows
    workers.do_all(agent launch_parfor_worker)
    parfor_result(reader)
  end

  to_separate(nelts: INTEGER; points: ARRAY[TUPLE[INTEGER, INTEGER]])
    : separate ARRAY[TUPLE[INTEGER, INTEGER]]
  local
    res: separate ARRAY[TUPLE[INTEGER, INTEGER]]
  do
    create res.make_empty
    across 1 |..| nelts as ic loop
      res.force(points.item(ic.item), ic.item);
    end
    Result := res
  end

  launch_parfor_worker(worker: separate PARFOR_WORKER)
  do
    worker.live
  end

  parfor_result(reader: separate PARFOR_READER)
  do
    reader.get_result(parfor_aggregator)
  end

  sqr(a: DOUBLE): DOUBLE
  do
    Result := a * a
  end

  distance(a, b: TUPLE[INTEGER, INTEGER]): DOUBLE
  do
    Result := {DOUBLE_MATH}.sqrt(
      sqr(a.integer_32_item(1) - b.integer_32_item(1)) +
      sqr(a.integer_32_item(2) - b.integer_32_item(2)));
  end

  item(array: separate ARRAY[DOUBLE]; index: INTEGER): DOUBLE
  do
    Result := array.item(index)
  end

feature {NONE}
  in: PLAIN_TEXT_FILE
  parfor_aggregator: PARFOR_AGGREGATOR

end -- class MAIN 

