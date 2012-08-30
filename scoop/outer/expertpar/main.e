-- outer: outer product
--
-- input:
--   vector: a vector of (x, y) points
--   nelts: the number of points
--
-- output:
--   matrix: a real matrix, whose values are filled with inter-point
--     distances
--   vector: a real vector, whose values are filled with origin-to-point
--     distances

class MAIN
inherit ARGUMENTS
create make
feature
  make
    local
      nelts: INTEGER
      local_matrix: ARRAY2[DOUBLE]
      local_vector: ARRAY [DOUBLE]
      file_name: STRING
      i, j: INTEGER
    do
      file_name := separate_character_option_value('i')
      create in.make_open_read(separate_character_option_value('i'))

      is_bench := index_of_word_option ("is_bench") > 0
      
      nelts := read_integer

      
      create x_points.make (1, nelts)
      create y_points.make (1, nelts)
      read_vector_of_points(nelts, x_points, y_points)
  
      create result_vector.make_filled(0.0, 1, nelts)
      create result_matrix.make (nelts, nelts)

      local_matrix := outer (nelts)

      if not is_bench then
        print(nelts.out + " " + nelts.out + "%N")
        from i := 1
        until i > nelts
        loop
          from j := 1
          until j > nelts
          loop
            print (local_matrix [i, j].out + " ")
            j := j + 1
          end
          print ("%N")
          i := i + 1
        end
        print("%N")

        local_vector := get_vector (nelts, result_vector)
        print(nelts.out + "%N")
        from i := 1
        until i> nelts
        loop
          print(local_vector[i].out + " ")
          i := i + 1
        end
        print("%N")
      end
    end


  get_vector (nelts: INTEGER; a_vector: separate ARRAY [DOUBLE]):
      ARRAY [DOUBLE]
    local
      i: INTEGER
      d: DOUBLE
    do
      create Result.make (1, nelts)
      from i := 1
      until i > nelts
      loop
        d := a_vector.item (i)
        Result [i] := d
        i := i + 1
      end
    end
  
  read_integer: INTEGER
    do
      in.read_integer
      Result := in.last_integer
    end

  read_vector_of_points(nelts: INTEGER;
                        x_vector, y_vector: separate ARRAY[INTEGER])
    local
      i: INTEGER
      x, y: INTEGER
    do      
      from i := 1
      until i > nelts 
      loop
        if is_bench then
          x := 0
          y := 0
        else
          x := read_integer
          y := read_integer
        end
        
        x_vector [i] := x
        y_vector [i] := y
        i := i + 1
      end
    end

  num_workers: INTEGER = 1
  
  -- parallel for on nelts
  outer (nelts: INTEGER): ARRAY2[REAL_64]
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
        height := (nelts - start) // (num_workers - i)

        if height > 0 then
          create worker.make
                     (start + 1
                      , start + height
                      , nelts
                      , x_points
                      , y_points
                      , result_vector
                      , result_matrix)

          workers.extend(worker)
        end

        start := start + height
        i := i + 1
      end
      -- parallel for on rows
      workers_live (workers)

      -- join workers
      workers_join (workers)

      Result := to_local (nelts, result_matrix)
    end

  to_local(nelts: INTEGER; a_matrix: separate ARRAY2[REAL_64]): ARRAY2[REAL_64]
    require
      a_matrix.generator /= Void
    local
      i, j: INTEGER
      d: DOUBLE
    do
      create Result.make (nelts, nelts)
      from i := 1
      until i > nelts
      loop
        from j := 1
        until j > nelts
        loop
          d := a_matrix.item (i, j)
          Result [i, j] := d
          j := j + 1
        end
        i := i + 1
      end
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

  sqr(a: DOUBLE): DOUBLE
    do
      Result := a * a
    end

  distance(a, b: TUPLE[INTEGER, INTEGER]): DOUBLE
    do
      Result := {DOUBLE_MATH}.sqrt(
      sqr(a.integer_32_item(1) - b.integer_32_item(1)) +
        sqr(a.integer_32_item(2) - b.integer_32_item(2)));
    end

  item(array: separate ARRAY[DOUBLE]; index: INTEGER): DOUBLE
    do
      Result := array.item(index)
    end

feature {NONE}
  in: PLAIN_TEXT_FILE
  is_bench: BOOLEAN
  
  x_points: separate ARRAY[INTEGER]
  y_points: separate ARRAY[INTEGER]
  result_vector: separate ARRAY[REAL_64]
  result_matrix: separate ARRAY2[REAL_64]
  
end -- class MAIN 

