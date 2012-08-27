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

inherit
  ARGUMENTS

create
  make

feature
  make
    local
      nrows, ncols, nelts: INTEGER
      matrix, mask: ARRAY2[INTEGER]
      k: INTEGER
      file_name: STRING
      points: ARRAY[TUPLE[value: INTEGER; i,j: INTEGER]]
    do
      file_name := separate_character_option_value('i')
      create in.make_open_read(file_name)

      nrows := read_integer
      ncols := read_integer
      matrix := read_matrix(nrows, ncols)
      mask := read_matrix(nrows, ncols)
      nelts := read_integer

      points := winnow(nrows, ncols, matrix, mask, nelts)

      print(nelts.out + "%N");

      from k := 1
      until k > nelts
      loop
        print(points [k].i.out + " " + points [k].j.out + "%N");
        k := k + 1
      end
      print("%N");
    end

  read_integer(): INTEGER
    do
      in.read_integer
      Result := in.last_integer
    end

  read_matrix(nrows, ncols: INTEGER): ARRAY2[INTEGER]
    local
      i, j: INTEGER
    do
      create Result.make(nrows, ncols)
      from i := 1
      until i > nrows
      loop
        from j := 1
        until j > ncols
        loop
          Result [i, j] := read_integer
          j := j + 1
        end
        i := i + 1
      end
    end

  winnow(nrows, ncols: INTEGER; matrix, mask: ARRAY2[INTEGER];
         nelts: INTEGER) : ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
    local
      values: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
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
          if (mask [i, j] = 1) then
            values.force ([matrix [i,j], i, j], count)
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
      
      create Result.make (1, nelts);
      from i := 1
      until i > nelts
      loop
        index := (i - 1) * chunk + 1
        Result [i] := values[index]
        i := i + 1
      end
    end

feature {NONE}
  in: PLAIN_TEXT_FILE

end -- class MAIN 
