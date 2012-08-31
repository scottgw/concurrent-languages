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

feature
  thresh(nrows, ncols: INTEGER; matrix: ARRAY2[INTEGER]; percent: INTEGER;
         mask: ARRAY2[INTEGER])
    local
      i, j: INTEGER
      v: INTEGER
      nmax: INTEGER
      histogram: ARRAY[INTEGER]
      count: REAL_64
      prefixsum, threshold: INTEGER
    do
      create histogram.make(0, 100)

      nmax := -1
      from i := 1
      until i > ncols
      loop
        from j := 1
        until j > ncols
        loop
          v := matrix [i, j]
        
          nmax := nmax.max (v)
          histogram [v] := histogram [v] + 1
        
          j := j + 1
        end
        i := i + 1
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
          if matrix [i, j] >= threshold then
            mask [i, j] := 1
          end
          j := j + 1
        end
        i := i + 1
      end
    end
end
