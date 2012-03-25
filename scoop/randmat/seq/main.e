-- randmat: random number generation
--
-- input:
--   nrows, ncols: the number of rows and cols
--   s: the seed
--
-- output:
--   matrix: a nrows by ncols integer matrix

class MAIN create
  make

feature

  make
    -- Print a simple message.
  local
    nrows, ncols, s: INTEGER
    matrix: ARRAY2[INTEGER]
    i, j: INTEGER
  do
    io.read_integer
    nrows := io.last_integer

    io.read_integer
    ncols := io.last_integer

    io.read_integer
    s := io.last_integer

    create matrix.make(nrows, ncols)

    randmat(nrows, ncols, s, matrix)

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
        print(matrix.item(i, j).out + " ")
        j := j + 1
      end
      print("%N")
      i := i + 1
    end
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

end -- class MAIN 
