-- thresh: histogram thresholding
--
-- input:
--   matrix: the integer matrix to be thresholded
--   nrows, ncols: the number of rows and cols
--   percent: the percentage of cells to retain
--
-- output:
--   mask: a boolean matrix filled with true for cells kept

class THRESH
create make_empty
feature
  make_empty do end

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

end
