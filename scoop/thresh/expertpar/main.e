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
      mask: ARRAY2[INTEGER]
      in: PLAIN_TEXT_FILE
      file_name: STRING
      i, j: INTEGER
    do
      file_name := separate_character_option_value ('i')
      create in.make_open_read (file_name)

      in.read_integer
      nrows := in.last_integer

      in.read_integer
      ncols := in.last_integer

      create matrix.make_filled (0, nrows, ncols)
      create histogram.make_filled(0, 0, 100)
      create accum.make_filled (0, 1, 1)

      read_matrix (nrows, ncols, matrix, in)

      in.read_integer
      percent := in.last_integer

      mask := thresh (nrows, ncols, percent)

      from i := 1
      until i > nrows
      loop
        from j := 1
        until j > ncols
        loop
          print (mask[i,j].out + " ")
          j := j + 1
        end
        print ("%N")
        i := i + 1
      end
    end

  thresh(nrows, ncols: INTEGER;
         percent: INTEGER;):  ARRAY2 [INTEGER]
    local
      threshold: INTEGER
    do
      reduce2d (nrows, ncols)

      threshold := calculate_threshold (nrows, ncols, percent, accum, histogram)

      -- parallel for on matrix
      Result := parfor(nrows, ncols, threshold)
    end

  reduce2d (nrows, ncols: INTEGER)
    local
      worker: separate REDUCE2D_WORKER
      workers: LINKED_LIST [separate REDUCE2D_WORKER]
      start, height, i: INTEGER
    do
      create workers.make

      from
        start := 0
        i := 0
      until i >= num_workers
      loop
        height := (nrows - start) // (num_workers - i)

        if height > 0 then
          create worker.make_with_filter
                     (start + 1
                      , start + height
                      , ncols
                      , matrix
                      , accum
                      , histogram)

          workers.extend(worker)
        end

        start := start + height
        i := i + 1
      end
      -- parallel for on rows
      workers_reduce_live (workers)

      -- join workers
      workers_reduce_join (workers)
    end

  workers_reduce_live (workers: LINKED_LIST [separate REDUCE2D_WORKER])
    do
      from workers.start
      until workers.after
      loop
        live_reduce (workers.item)
        workers.forth
      end
    end

  workers_reduce_join (workers: LINKED_LIST [separate REDUCE2D_WORKER])
    do
      from workers.start
      until workers.after
      loop
        join_reduce (workers.item)
        workers.forth
      end
    end

  live_reduce (worker: separate REDUCE2D_WORKER)
    do
      worker.live
    end

  workers_parfor_live (workers: LINKED_LIST [separate PARFOR_WORKER])
    do
      from workers.start
      until workers.after
      loop
        live_parfor (workers.item)
        workers.forth
      end
    end

  live_parfor (worker: separate PARFOR_WORKER)
    do
      worker.live
    end

  calculate_threshold (nrows, ncols, percent: INTEGER;
                       a_accum: separate ARRAY [INTEGER];
                       a_histogram: separate ARRAY [INTEGER]): INTEGER
    require
      a_accum.generator /= Void and a_histogram.generator /= Void
    local
      count: INTEGER
      nmax: INTEGER
      threshold: INTEGER
      prefixsum: INTEGER
      i: INTEGER
      h: INTEGER
    do
      nmax := a_accum.item (1)
      count := (nrows * ncols * percent) // 100

      prefixsum := 0
      threshold := nmax

      from i := nmax until not(i >= 0 and prefixsum <= count) loop
      	h := a_histogram.item (i)
        prefixsum := prefixsum + h
        threshold := i;
        i := i - 1
      end

      Result := threshold
    end

  -- parallel for on matrix
  parfor(nrows, ncols: INTEGER;
         threshold: INTEGER): ARRAY2 [INTEGER]
    local
      worker: separate PARFOR_WORKER
      workers: LINKED_LIST[separate PARFOR_WORKER]
      i, start, height: INTEGER
    do
      create workers.make
      create shared.make_filled (0, nrows, ncols)

      from
        start := 0
        i := 0
      until i >= num_workers
      loop
        height := (nrows - start) // (num_workers - i)

        if height > 0 then
          create worker.make
                     (start + 1
                     , start + height
                     , ncols
                     , matrix
                     , shared
                     , threshold)

          workers.extend(worker)
        end


        start := start + height
        i := i + 1
      end

      -- parallel for on rows
      workers_parfor_live (workers)

      -- join workers
      workers.do_all(agent join_parfor)

      Result := fetch_matrix (nrows, ncols, shared)
    end

  fetch_matrix (nrows, ncols: INTEGER;
                a_array: separate ARRAY2[INTEGER]): ARRAY2 [INTEGER]
    local
      i, j: INTEGER
    do
      create Result.make_filled (0, nrows, ncols)

      from i := 1
      until i > nrows
      loop
        from j := 1
        until j > ncols
        loop
          Result [i, j] := a_array.item (i, j)
          j := j + 1
        end
        i := i + 1
      end
    end

  read_matrix(nrows, ncols: INTEGER;
              a_matrix: separate ARRAY2[INTEGER];
              in: PLAIN_TEXT_FILE)
    require
      a_matrix.generator /= Void
    local
      i, j: INTEGER
    do
      from i := 1
      until i > nrows
      loop
        from j := 1
        until j > ncols
        loop
          in.read_integer
          a_matrix.put (in.last_integer, i, j)
          a_matrix.item (i,j).generator.do_nothing
          j := j + 1
        end
        i := i + 1
      end
    end


feature {NONE}
  matrix: separate ARRAY2[INTEGER]
  shared: separate ARRAY2[INTEGER]
  accum: separate ARRAY [INTEGER]
  histogram: separate ARRAY[INTEGER]

  num_workers: INTEGER = 32

  join_reduce (s: separate REDUCE2D_WORKER)
    require
      s.generator = s.generator
    do
    end

  join_parfor (s: separate PARFOR_WORKER)
    require
      s.generator = s.generator
    do
    end
  end -- class MAIN
