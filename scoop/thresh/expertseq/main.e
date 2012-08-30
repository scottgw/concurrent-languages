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
    v: INTEGER
    in: PLAIN_TEXT_FILE
    file_name: STRING
  do
    file_name := separate_character_option_value('i')
    is_bench := index_of_word_option ("is_bench") > 0

    create in.make_open_read(separate_character_option_value('i'))

    in.read_integer
    nrows := in.last_integer

    in.read_integer
    ncols := in.last_integer

    create matrix.make(nrows, ncols)
    from i := 1 until i > nrows loop
      from j := 1 until j > ncols loop
        if is_bench then
          v := (i * j) \\ 100
        else
          in.read_integer
          v := in.last_integer            
        end
        matrix.put(v, i, j)
        j := j + 1
      end
      i := i + 1
    end

    in.read_integer
    percent := in.last_integer

    create mask.make(nrows, ncols)
    thresh(nrows, ncols, matrix, percent, mask)

    if not is_bench then
      print (nrows.out + " " + ncols.out + "%N")

      from i := 1
      until i > nrows
      loop
        from j := 1
        until j > ncols
        loop
          print (mask [i, j].out + " ")
          j := j + 1
        end
        print ("%N")
        i := i + 1
      end
    end
  end

  is_bench: BOOLEAN

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

    from i := 1
    until i > nrows
    loop
      from j := 1
      until j > ncols
      loop
        nmax := nmax.max(matrix [i, j])
        j := j + 1
      end
      i := i + 1
    end

  
    create histogram.make(0, nmax + 1)


    from i := 1
    until i > nrows
    loop
      from j := 1
      until j > ncols
      loop
        histogram [matrix[i,j]] := histogram [matrix [i,j]] + 1
        j := j + 1
      end
      i := i + 1
    end

    count := (nrows * ncols * percent) / 100

    prefixsum := 0
    threshold := nmax

    from i := nmax
    until not (i >= 0 and prefixsum <= count)
    loop
      prefixsum := prefixsum + histogram[i];
      threshold := i;
      i := i - 1
    end


    from i := 1
    until i > nrows
    loop
      from j := 1
      until j > ncols
      loop
        if matrix [i, j] >= threshold then
          mask [i, j] := 1
        end

        j := j + 1
      end
      i := i + 1
    end
  end

end -- class MAIN 
