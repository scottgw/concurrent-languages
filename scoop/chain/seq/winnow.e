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
create make_empty
feature
  make_empty do end

  winnow(nrows, ncols: INTEGER; matrix, mask: ARRAY2[INTEGER];
    nelts: INTEGER) : ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
  local
    points, values: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
    count: INTEGER
    sorter: QUICK_SORTER[TUPLE[INTEGER, INTEGER, INTEGER]]
    comparator: TUPLE_COMPARATOR
    n, chunk, index: INTEGER
  do
    count := 1
    create values.make_empty
    across 1 |..| nrows as ic loop
      across 1 |..| ncols as jc loop
        if (mask.item(ic.item, jc.item) = 1) then
          values.force([matrix.item(ic.item, jc.item), ic.item, jc.item],
              count)
          count := count + 1
        end
      end
    end

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

end
