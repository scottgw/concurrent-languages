-- thresh: histogram thresholding
--
-- input:
--   matrix: the integer matrix to be thresholded
--   nrows, ncols: the number of rows and cols
--   percent: the percentage of cells to retain
--
-- output:
--   mask: a boolean matrix filled with true for cells kept

class MAIN
inherit ARGUMENTS
create make
feature
  make
  local
    nrows, ncols, percent: INTEGER
    matrix, mask: ARRAY2[INTEGER]
    i, j: INTEGER
    in: PLAIN_TEXT_FILE
    file_name: STRING
  do
    file_name := separate_character_option_value('i')
    !!in.make_open_read(separate_character_option_value('i'))

    in.read_integer
    nrows := in.last_integer

    in.read_integer
    ncols := in.last_integer

    create matrix.make(nrows, ncols)
    from i := 1 until i > nrows loop
      from j := 1 until j > ncols loop
        in.read_integer
        matrix.put(in.last_integer, i, j)
        j := j + 1
      end
      i := i + 1
    end

    in.read_integer
    percent := in.last_integer

    create mask.make(nrows, ncols)
    thresh(nrows, ncols, matrix, percent, mask)

    from i := 1 until i > nrows loop
      from j := 1 until j > ncols loop
        print(mask.item(i, j).out + " ")
        j := j + 1
      end
      print("%N")
      i := i + 1
    end
  end

  thresh(nrows, ncols: INTEGER; matrix: ARRAY2[INTEGER]; percent: INTEGER;
    mask: ARRAY2[INTEGER])
  local
    i, j: INTEGER
    nmax: INTEGER
    histogram: ARRAY[INTEGER]
    count: REAL_64
    prefixsum, threshold: INTEGER
  do
    across matrix as m loop
      nmax := nmax.max(m.item)
    end

    create histogram.make(0, nmax + 1)

    across matrix as m loop
      histogram.put(histogram.item(m.item) + 1, m.item)
    end

    count := (nrows * ncols * percent) / 100

    prefixsum := 0
    threshold := nmax

    from i := nmax until not(i >= 0 and prefixsum <= count) loop
      prefixsum := prefixsum + histogram[i];
      threshold := i;
      i := i - 1
    end

    from i := 1 until i > nrows loop
      from j := 1 until j > ncols loop
        if matrix.item(i, j) >= threshold then
          mask.put(1, i, j)
        end
        j := j + 1
      end
      i := i + 1
    end
  end

end -- class MAIN 