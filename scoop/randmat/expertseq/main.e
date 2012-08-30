-- randmat: random number generation
--
-- input:
--   nrows, ncols: the number of rows and cols
--   s: the seed
--
-- output:
--   matrix: a nrows by ncols integer matrix

class MAIN

inherit
  ARGUMENTS

create make

feature
  make
    local
      nrows, ncols, s: INTEGER
      matrix: ARRAY2[INTEGER]
      is_bench: BOOLEAN
      arg: STRING_8
      i, j: INTEGER
    do
      create input.make_open_read(separate_character_option_value('i'))
      
      arg := separate_character_option_value('e')
      is_bench := False
      if arg /= Void then
        is_bench := arg.is_equal("is_bench")
      end

      nrows := read_integer
      ncols := read_integer
      s := read_integer

      create matrix.make(nrows, ncols)
      randmat(nrows, ncols, s.to_natural_32 , matrix)

      if not is_bench then
        from i := 1
        until i > nrows
        loop
          from j := 1
          until j > ncols
          loop
            print (matrix [i,j].out + " ")
            j := j + 1
          end
          print ("%N")
          i := i + 1
        end
      end
    end

  input: PLAIN_TEXT_FILE

  
  read_integer: INTEGER
    do
      input.read_integer
      Result := input.last_integer
    end

  randmat(nrows, ncols:INTEGER;  s: NATURAL; matrix: ARRAY2[INTEGER])
    local
      seed, lcg_a, lcg_c, rand_max: NATURAL
      i, j: INTEGER
    do
      lcg_a := 1664525
      lcg_c := 1013904223
      rand_max := 100

      from i := 1
      until i > nrows
      loop
        seed := s + i.to_natural_32 - 1
        from j := 1
        until j > ncols
        loop
          seed := lcg_a * seed + lcg_c
          matrix [i, j] := (seed \\ rand_max).to_integer_32
          j := j + 1
        end
        i := i + 1
      end
    end
end
