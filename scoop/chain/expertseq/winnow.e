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
feature
  winnow(nrows, ncols: INTEGER; matrix, mask: ARRAY2[INTEGER];
        nelts: INTEGER) : ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
    local
      points, values: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
      count: INTEGER
      sorter: QUICK_SORTER[TUPLE[INTEGER, INTEGER, INTEGER]]
      comparator: TUPLE_COMPARATOR
      n, chunk, index: INTEGER
      i, j: INTEGER
    do
      count := 1
      create values.make_empty

      from i := 1
      until i > nrows
      loop
        from j := 1
        until j > ncols
        loop
          if mask [i, j] = 1 then
            values.force([matrix [i, j], i, j], count)
            count := count + 1
          end
          j := j + 1
        end
        i := i + 1
      end

      create comparator
      create sorter.make(comparator)
      sorter.sort(values)

      n := values.count
      chunk := n // nelts

      create points.make(1, nelts);
      from i := 1
      until i > nelts
      loop
        index := (i - 1) * chunk + 1
        points[i] := values[index]
      
        i := i + 1
      end

      Result := points
    end

end
