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
      matrix: separate ARRAY2[INTEGER]
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

      create matrix.make (nrows, ncols)
      read_matrix(nrows, ncols, matrix, in)

      in.read_integer
      percent := in.last_integer

      mask := thresh(nrows, ncols, matrix, percent)

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

  read_matrix(nrows, ncols: INTEGER; matrix: separate ARRAY2[INTEGER];
              in: PLAIN_TEXT_FILE)
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
          matrix [i,j] := in.last_integer
          j := j + 1
        end
        i := i + 1
      end
    end

  create_array(): separate ARRAY[INTEGER]
    do
      create {separate ARRAY[INTEGER]} Result.make_empty
    end

      put(array: separate ARRAY[INTEGER]; value, index: INTEGER)
    do
      array.force(value, index)
    end

  item(array: separate ARRAY[INTEGER]; index: INTEGER): INTEGER
    do
      Result := array.item(index)
    end

  thresh(nrows, ncols: INTEGER; matrix: separate ARRAY2[INTEGER];
         percent: INTEGER;):  ARRAY2 [INTEGER]
    local
      nmax: INTEGER
      histogram: ARRAY[INTEGER]
      count: INTEGER
      prefixsum, threshold: INTEGER
      i: INTEGER
    do
      nmax := reduce2d (nrows, ncols, matrix);

      create histogram.make_filled(0, 0, nmax + 1)

      from i := 0
      until i > nmax + 1
      loop
        histogram [i] := reduce2d_with_filter(nrows, ncols, matrix, i)
        i := i + 1
      end

      count := (nrows * ncols * percent) // 100
      
      prefixsum := 0
      threshold := nmax
      
      from i := nmax until not(i >= 0 and prefixsum <= count) loop
        prefixsum := prefixsum + histogram[i];
        threshold := i;
        i := i - 1
      end
      
      -- parallel for on matrix
      Result := parfor(nrows, ncols, matrix, threshold)
    end

  reduce2d(nrows, ncols: INTEGER; matrix: separate ARRAY2[INTEGER]):
      INTEGER
    do
      Result :=
        reduce2d_impl(nrows
                     , ncols
                     , matrix
                     , 0
                     , {REDUCE2D_OPERATOR}.max)
    end

  reduce2d_with_filter(nrows, ncols: INTEGER;
                       matrix: separate ARRAY2[INTEGER];
                       value: INTEGER): INTEGER
    do
      Result :=
        reduce2d_impl(nrows
                     , ncols
                     , matrix
                     , value
                     , {REDUCE2D_OPERATOR}.sum)
    end

  num_workers: INTEGER = 32
  
  reduce2d_impl(nrows, ncols: INTEGER;
                matrix: separate ARRAY2[INTEGER];
                value: INTEGER;
                op: INTEGER): INTEGER
    local
      worker: separate REDUCE2D_WORKER
      workers: LINKED_LIST [separate REDUCE2D_WORKER]
      start, height, i: INTEGER
      accum: separate CELL [INTEGER]
    do
      create workers.make
      create accum.put (0)
--      create reduce2d_aggregator.make(nrows, op_aggregator)
      
      from
        start := 0
        i := 0
      until i >= num_workers
      loop
        height := (nrows - start) // (num_workers - i)

        create worker.make_with_filter
                  (start + 1
                   , start + height
                   , ncols
                   , matrix
                   , accum
                   , op
                   , value)
    
        workers.extend(worker)        
          
        start := start + height
        i := i + 1
      end
 
      -- parallel for on rows
      workers.do_all(agent launch_reduce2d_worker)
      workers.do_all(agent join_reduce)
      Result := reduce2d_result(accum)
    end

  reduce2d_result(accum: separate CELL[INTEGER]): INTEGER
    do
      Result := accum.item
    end
  
  -- parallel for on matrix
  parfor(nrows, ncols: INTEGER;
         matrix: separate ARRAY2[INTEGER];
         threshold: INTEGER): ARRAY2 [INTEGER]
    local
      worker: separate PARFOR_WORKER
      workers: LINKED_LIST[separate PARFOR_WORKER]
      shared: separate ARRAY2[INTEGER]
      reader: separate PARFOR_READER
      i, start, height: INTEGER
    do
      create workers.make
      create reader.make
      create shared.make (nrows, ncols)
      
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
      workers.do_all(agent launch_parfor_worker)
      workers.do_all(agent join_parfor)
      
      Result := fetch_matrix (nrows, ncols, shared)
    end

  fetch_matrix (nrows, ncols: INTEGER;
                a_array: separate ARRAY2[INTEGER]): ARRAY2 [INTEGER]
    local
      i, j: INTEGER
    do
      create Result.make (nrows, ncols)

      from i := 1
      until i > nrows
      loop
        from j := 1
        until j > ncols
        loop
          Result [i, j] := a_array [i, j]
          j := j + 1
        end
        i := i + 1
      end
    end

  parfor_result(reader: separate PARFOR_READER)
    do
      reader.get_result(parfor_aggregator)
    end

feature {NONE}
  reduce2d_aggregator: separate REDUCE2D_AGGREGATOR
  parfor_aggregator: separate PARFOR_AGGREGATOR


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
  
  launch_reduce2d_worker(worker: separate REDUCE2D_WORKER)
    do
      worker.live
    end

  launch_parfor_worker(worker: separate PARFOR_WORKER)
    do
      worker.live
    end
end -- class MAIN 
