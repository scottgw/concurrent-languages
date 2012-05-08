-- randmat: random number generation
--
-- input:
--   nrows, ncols: the number of rows and cols
--   s: the seed
--
-- output:
--   matrix: a nrows by ncols integer matrix

class RANDMAT create
  make

feature

  make
    -- Print a simple message.
  local
    nrows, ncols, s: INTEGER
    matrix: separate ARRAY2[INTEGER]
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

  randmat(nrows, ncols, s: INTEGER; matrix: separate ARRAY2[INTEGER])
  local
    i, j: INTEGER
    worker: ARRAY[separate WORKER]
  do
    create worker.make_empty
    -- parallel for on rows
    from
      i := 1
    until
      i > nrows
    loop
      worker.force(create {WORKER}.make_with_matrix(matrix, ncols, s, i), i)
      worker.item(i).fill
      i := i + 1
    end
  end

end -- class RANDMAT 

