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
    matrix, mask: ARRAY2[INTEGER]
    i, j: INTEGER
    file_name: STRING
    points: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
  do
    file_name := separate_character_option_value('i')
    !!in.make_open_read(separate_character_option_value('i'))

    is_bench := index_of_word_option ("bench") > 0

    nrows := read_integer
    ncols := read_integer
    matrix := read_matrix(nrows, ncols)
    mask := read_mask(nrows, ncols)
    nelts := read_integer

    points := winnow(nrows, ncols, matrix, mask, nelts)

    if not is_bench then
      print(nelts.out + "%N");
      across 1 |..| nelts as ic loop
        print(points.item(ic.item).integer_32_item(2).out + " " +
            points.item(ic.item).integer_32_item(3).out + "%N");
      end
      print("%N");
    end
  end

  is_bench: BOOLEAN

  read_integer(): INTEGER
  do
    in.read_integer
    Result := in.last_integer
  end

  read_matrix(nrows, ncols: INTEGER): ARRAY2[INTEGER]
  local
    i, j, v: INTEGER
    matrix: ARRAY2[INTEGER]
  do
    create matrix.make(nrows, ncols)
    across 1 |..| nrows as ic loop
      across 1 |..| ncols as jc loop
        if is_bench then
          v := 0
        else
          v := read_integer
        end 
         
        matrix.put(v, ic.item, jc.item)
      end
    end
    Result := matrix
  end

  read_mask(nrows, ncols: INTEGER): ARRAY2[INTEGER]
  local
    i, j, v: INTEGER
    matrix: ARRAY2[INTEGER]
  do
    create matrix.make(nrows, ncols)
    across 1 |..| nrows as ic loop
      across 1 |..| ncols as jc loop
        if is_bench then
          if ((ic.item * jc.item) \\ (ncols + 1)) = 1 then
            v := 1
          else
            v := 0
          end
        else
          v := read_integer
        end
          
        matrix.put(v, ic.item, jc.item)
      end
    end
    Result := matrix
  end

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

feature {NONE}
  in: PLAIN_TEXT_FILE

end -- class MAIN 
