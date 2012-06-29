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

class OUTER
inherit ARGUMENTS
create make_empty
feature
  make_empty do end

  create_array(): separate ARRAY[DOUBLE]
  do
    create {separate ARRAY[DOUBLE]} Result.make_empty
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
    worker: separate OUTER_PARFOR_WORKER
    workers: LINKED_LIST[separate OUTER_PARFOR_WORKER]
    reader: separate OUTER_PARFOR_READER
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
    create res.make_filled([0, 0], 1, nelts)
    across 1 |..| nelts as ic loop
      put(res, points.item(ic.item), ic.item)
    end
    Result := res
  end

  put(vector: separate ARRAY[TUPLE[INTEGER, INTEGER]];
    value: TUPLE[INTEGER, INTEGER]; index: INTEGER)
  do
    vector.put(value, index)
  end

  launch_parfor_worker(worker: separate OUTER_PARFOR_WORKER)
  do
    worker.live
  end

  parfor_result(reader: separate OUTER_PARFOR_READER)
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
  parfor_aggregator: OUTER_PARFOR_AGGREGATOR

end -- class OUTER

