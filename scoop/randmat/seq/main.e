-- randmat: random number generation
--
-- input:
--   nrows, ncols: the number of rows and cols
--   s: the seed
--
-- output:
--   matrix: a nrows by ncols integer matrix

class MAIN
inherit ARGUMENTS
create make

feature
  make
  local
    nrows, ncols, s: INTEGER
    matrix: ARRAY2[INTEGER]
    is_bench: BOOLEAN
    arg: STRING_8
  do
    create in.make_open_read(separate_character_option_value('i'))

    is_bench := index_of_word_option ("is_bench") > 0

    nrows := read_integer
    ncols := read_integer
    s := read_integer

    create matrix.make(nrows, ncols)
    randmat(nrows, ncols, s, matrix)

    if not is_bench then
      across 1 |..| nrows as ic loop
        across 1 |..| ncols as jc loop
          print(matrix.item(ic.item, jc.item).out + " ")
        end
        print("%N")
      end
    end
  end

  read_integer(): INTEGER
  do
    in.read_integer
    Result := in.last_integer
  end

  randmat(nrows, ncols, s: INTEGER; matrix: ARRAY2[INTEGER])
  local
    seed, lcg_a, lcg_c, rand_max, int_max: INTEGER
  do
    lcg_a := 1664525
    lcg_c := 1013904223
    rand_max := 100
    int_max := 2147483647
    across 1 |..| nrows as ic loop
      seed := s + ic.item
      across 1 |..| ncols as jc loop
        seed := (lcg_a * seed + lcg_c) \\ int_max
        matrix.put((((seed \\ rand_max) + rand_max) \\ rand_max),
            ic.item, jc.item)
      end
    end
  end

feature {NONE}
  in: PLAIN_TEXT_FILE

end
