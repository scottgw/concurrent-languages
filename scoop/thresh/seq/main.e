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

    across 1 |..| nrows as ic loop
      across 1 |..| ncols as jc loop
        print(mask.item(ic.item, jc.item).out + " ")
      end
      print("%N")
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

    across 1 |..| nrows as ic loop
      across 1 |..| ncols as jc loop
        if matrix.item(ic.item, jc.item) >= threshold then
          mask.put(1, ic.item, jc.item)
        end
      end
    end
  end

end -- class MAIN 
