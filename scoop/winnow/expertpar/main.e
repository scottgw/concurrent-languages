-- winnow: weighted point selection
--
-- input:
--   matrix: an integer matrix, whose values are used as masses
--   mask: a boolean matrix showing which points are eligible for
--     consideration
--   nrows, ncols: the number of rows and cols
--   nelts: the number of points to select
--
-- output:
--   points: a vector of (x, y) points

class MAIN
inherit ARGUMENTS
create make
feature
  make
    local
      nrows, ncols, nelts: INTEGER
      points: ARRAY[TUPLE[value, i, j: INTEGER]]
      k: INTEGER
    do
      create in.make_open_read(separate_character_option_value('i'))

      is_bench := index_of_word_option ("bench") > 0
      
      nrows := read_integer
      ncols := read_integer
      
      create v_vector.make_empty
      create x_vector.make_empty
      create y_vector.make_empty
      
      create matrix.make (nrows, ncols)
      read_matrix(nrows, ncols, matrix)
      
      create mask.make (nrows, ncols)
      read_mask(nrows, ncols, mask)
      
      nelts := read_integer
      
      points := winnow(nrows, ncols, nelts)

      if not is_bench then
        print(nelts.out + "%N")
        from k := 1
        until k > nelts
        loop
          print(points [k].i.out + " " + points [k].j.out + "%N")
          k := k + 1
        end
        print("%N")
      end
    end
  
  read_integer: INTEGER
    do
      in.read_integer
      Result := in.last_integer
    end

  read_matrix(nrows, ncols: INTEGER; a_matrix: separate ARRAY2[INTEGER])
    local
      i, j: INTEGER
      v: INTEGER
    do
      from i := 1
      until i > nrows
      loop
        from j := 1
        until j > ncols
        loop
          if is_bench then
            v := 0
          else
            v := read_integer
          end
          a_matrix [i, j] := v
          j := j + 1
        end
        i := i + 1
      end
    end

  read_mask(nrows, ncols: INTEGER; a_matrix: separate ARRAY2[INTEGER])
    local
      i, j: INTEGER
      v: INTEGER
    do
      from i := 1
      until i > nrows
      loop
        from j := 1
        until j > ncols
        loop
          if is_bench then
            if ((i * j) \\ (ncols + 1)) = 1 then
              v := 1
            else
              v := 0
            end
          else
            v := read_integer
          end
          
          a_matrix [i, j] := v
          j := j + 1
        end
        i := i + 1
      end
    end
  
  winnow(nrows, ncols: INTEGER; nelts: INTEGER):
      ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
    local
      points: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
      values: ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
      sorter: TUPLE_SORTER
      i: INTEGER
      n, chunk, index: INTEGER
    do
      -- parallel for on matrix
      values := parfor (nelts, nrows, ncols);
      
      create sorter.make()
      values := sorter.sort(values)
      
      n := values.count
      chunk := n // nelts
      
      create points.make_filled([0, 0, 0], 1, nelts);
      -- this should also be a parallel for
      from i := 1
      until i > nelts
      loop
        index := (i - 1) * chunk + 1
        points [i] := values [index]
        i := i + 1
      end
      
      Result := points
    end
  
  num_workers: INTEGER = 32
  
  parfor (nelts, nrows, ncols: INTEGER):
      ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
    local
      worker: separate PARFOR_WORKER
      workers: LINKED_LIST [separate PARFOR_WORKER]
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
          create worker.make
                     (start + 1
                      , start + height
                      , ncols
                      , nelts
                      , matrix
                      , mask
                      , v_vector
                      , x_vector
                      , y_vector)

          workers.extend(worker)
        end

        start := start + height
        i := i + 1
      end
      -- parallel for on rows
      workers_live (workers)

      -- join workers
      workers_join (workers)

      Result := to_local_values (v_vector, x_vector, y_vector)
    end

feature {NONE}
  workers_live (workers: LINKED_LIST [separate PARFOR_WORKER])
    do
      from workers.start
      until workers.after
      loop
        worker_live (workers.item)
        workers.forth
      end
    end

  workers_join (workers: LINKED_LIST [separate PARFOR_WORKER])
    do
      from workers.start
      until workers.after
      loop
        worker_join (workers.item)
        workers.forth
      end
    end
  
  worker_live (worker: separate PARFOR_WORKER)
    do
      worker.live
    end

  worker_join(worker: separate PARFOR_WORKER)
    require
      worker.generator /= Void
    do
    end

  
  launch_parfor_worker(worker: separate PARFOR_WORKER)
    do
      worker.live
    end

  to_local_values(vs, xs, ys: separate ARRAY[INTEGER]):
      ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
    require
      xs.generator /= Void
    local
      i: INTEGER
      n: INTEGER
      v,x,y: INTEGER
    do
      n := vs.count
      create Result.make (1, n)
      
      from i := 1
      until i > n
      loop
        v := vs [i]
        x := xs [i]
        y := ys [i]
        
        Result [i] := [v, x, y]
        i := i + 1
      end
    end
  
feature {NONE}
  in: PLAIN_TEXT_FILE
  is_bench: BOOLEAN
  
  matrix, mask: separate ARRAY2[INTEGER]
  v_vector, x_vector, y_vector: separate ARRAY [INTEGER]
  
end -- class MAIN 
