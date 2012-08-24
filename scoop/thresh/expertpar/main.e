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
      file_name := separate_character_option_value('i')
      create in.make_open_read(file_name)
      
      in.read_integer
      nrows := in.last_integer

      in.read_integer
      ncols := in.last_integer

      create matrix.make_filled (0, nrows, ncols)
      read_matrix(nrows, ncols, matrix, in)
      print ("main read_matrix%N")
      
      in.read_integer
      percent := in.last_integer

      mask := thresh(nrows, ncols, percent)
      
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

  read_matrix(nrows, ncols: INTEGER; a_matrix: separate ARRAY2[INTEGER];
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

  thresh(nrows, ncols: INTEGER;
         percent: INTEGER;):  ARRAY2 [INTEGER]
    local
      threshold: INTEGER
    do
      print ("thresh%N")
      create histogram.make_filled(0, 0, 100)
            
      reduce2d (nrows, ncols)

      threshold := calculate_threshold (nrows, ncols, percent, accum, histogram)
      
      -- parallel for on matrix
      Result := parfor(nrows, ncols, threshold)
    end

  calculate_threshold (nrows, ncols, percent: INTEGER;
                       a_accum: separate CELL [INTEGER];
                       a_histogram: separate ARRAY [INTEGER]): INTEGER
    local
      count: INTEGER
      nmax: INTEGER
      threshold: INTEGER
      prefixsum: INTEGER
      i: INTEGER
    do
      nmax := a_accum.item
      count := (nrows * ncols * percent) // 100
      
      prefixsum := 0
      threshold := nmax
      
      from i := nmax until not(i >= 0 and prefixsum <= count) loop
        prefixsum := prefixsum + a_histogram[i];
        threshold := i;
        i := i - 1
      end

      Result := threshold
    end

  num_workers: INTEGER = 32
  
  reduce2d (nrows, ncols: INTEGER)
    local
      worker: separate REDUCE2D_WORKER
      workers: LINKED_LIST [separate REDUCE2D_WORKER]
      start, height, i: INTEGER
    do
      print ("reduce2d%N")
      create workers.make
      create accum.put (0)
      
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
      print ("reduce2d do_all live%N")
      workers.do_all(agent {REDUCE2D_WORKER}.live) -- launch_reduce2d_worker)
      workers.do_all(agent join_reduce)
    end

  reduce2d_result(a_accum: separate CELL[INTEGER]): INTEGER
    do
      Result := a_accum.item
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

        create worker.make
                   (start + 1
                   , start + height
                   , ncols
                   , matrix
                   , shared
                   , threshold)
    
        workers.extend(worker)        
        
        start := start + height
        i := i + 1
      end
 
      -- parallel for on rows
      workers.do_all(agent {PARFOR_WORKER}.live)
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

feature {NONE}
  matrix: separate ARRAY2[INTEGER]
  shared: separate ARRAY2[INTEGER]
  accum: separate CELL [INTEGER]
  histogram: separate ARRAY[INTEGER]
  
  join_reduce (s: separate REDUCE2D_WORKER)
    require
      s.generator /= Void
    do
    end

  join_parfor (s: separate PARFOR_WORKER)
    require
      s.generator /= Void
    do
    end
  end -- class MAIN 
