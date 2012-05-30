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
    i, j: INTEGER
  do
    create in.make_open_read(separate_character_option_value('i'))

    nrows := read_integer
    ncols := read_integer
    s := read_integer

    --print("nrows: " + nrows.out + ", ncols: " + ncols.out + ", s: " +
        --s.out + "%N")

    create matrix.make(nrows, ncols)

    randmat(nrows, ncols, s, matrix)

    --from
      --i := 1
    --until
      --i > nrows
    --loop
      --from
        --j := 1
      --until
        --j > ncols
      --loop
        --print(matrix.item(i, j).out + " ")
        --j := j + 1
      --end
      --print("%N")
      --i := i + 1
    --end
  end

  read_integer(): INTEGER
  do
    in.read_integer
    Result := in.last_integer
  end

  randmat(nrows, ncols, s: INTEGER; matrix: ARRAY2[INTEGER])
  local
    i, j: INTEGER
    rand: RANDOM
  do
    create rand.set_seed(s)
    from
      i := 1
    until
      i > nrows
    loop
      from
        j := 1
      until
        j > ncols
      loop
        rand.forth
        matrix.put(rand.item, i, j)
        j := j + 1
      end
      i := i + 1
    end
  end

feature {NONE}
  in: PLAIN_TEXT_FILE

end -- class MAIN 
