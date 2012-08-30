-- product: matrix-vector product
--
-- input:
--   matrix: a real matrix
--   vector: a real vector
--   nelts: the number of elements
--
-- output:
--   res: a real vector, whose values are the result of the product

class MAIN
  
inherit
  ARGUMENTS
  
create
  make

feature
  make
    local
      nelts: INTEGER
      matrix: ARRAY2[DOUBLE]
      vector: ARRAY[DOUBLE]
      res: ARRAY[DOUBLE]
      i, j: INTEGER
    do
      create in.make_open_read(separate_character_option_value('i'))

      is_bench := index_of_word_option ("is_bench") > 0
      
      in.read_integer
      nelts := in.last_integer

      create matrix.make_filled(0.0, nelts, nelts)
      create vector.make_filled(0.0, 1, nelts)

      if not is_bench then

        from i := 1
        until i > nelts
        loop
          from j := 1
          until j > nelts
          loop
            matrix [i, j] := read_double
            j := j + 1
          end
          i := i + 1
        end

        from i := 1
        until i > nelts
        loop
          vector [i] := read_double
          i := i + 1
        end
      end
  
      res := product(nelts, matrix, vector)

      if not is_bench then
        print(nelts.out + "%N");
        from i := 1
        until i > nelts
        loop
          print(res [i].out + " ");
          i := i + 1
        end
        print("%N");
      end
    end

  read_double: DOUBLE
    do
      in.read_double
      Result := in.last_double
    end

  product(nelts: INTEGER; matrix: ARRAY2[DOUBLE]; vector: ARRAY[DOUBLE])
    : ARRAY[DOUBLE]
    local
      sum: DOUBLE
      i, j: INTEGER
    do
      create Result.make_filled(0.0, 1, nelts)
      from i := 1
      until i > nelts
      loop
        sum := 0
        from j := 1
        until j > nelts
        loop
          sum := sum + matrix [i,j] * vector [j]
          j := j + 1
        end
        Result [i] := sum
        i := i + 1
      end
    end

feature {NONE}
  in: PLAIN_TEXT_FILE
  is_bench: BOOLEAN
  
end -- class MAIN 

