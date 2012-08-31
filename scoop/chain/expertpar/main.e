-- randmat: random number generation
--
-- input:
--   nrows, ncols: the number of rows and cols
--   s: the seed
--
-- output:
--   matrix: a nrows by ncols integer matrix

class MAIN
inherit ARGUMENTS
  EXCEPTIONS
create make

feature
  make
    local
      nelts, s, percent, winnow_nelts: INTEGER
      is_bench: BOOLEAN
      i: INTEGER
      vector: ARRAY [DOUBLE]
    do
      create in.make_open_read(separate_character_option_value('i'))
      is_bench := index_of_word_option ("bench") > 0

      nelts := read_integer
      s := read_integer
      percent := read_integer
      winnow_nelts := read_integer

      create result_vector.make (1, winnow_nelts)
      create vs.make (1, 20000)
      create xs.make (1, 20000)
      create ys.make (1, 20000)
      create winnow_xs.make (1, 20000)
      create winnow_ys.make (1, 20000)

      run  (nelts, s, percent, winnow_nelts)

      if not is_bench then
        vector := fetch_vector (nelts, result_vector)
        
        from i := 1
        until i > nelts
        loop
          print (vector [i].out + " ")
          i := i + 1
        end
        print ("%N")
      end
    end

  
  read_integer: INTEGER
    do
      in.read_integer
      Result := in.last_integer
    end

  num_workers: INTEGER = 32
  
  -- parallel for on matrix
  run (nelts, seed, percent, winnow_nelts: INTEGER):
    local
      workers: LINKED_LIST[separate WORKER]
      worker: separate WORKER
      i: INTEGER
      start: INTEGER
      height: INTEGER
    do
      create Result.make

      from
        start := 0
        i := 0
      until i >= num_workers
      loop
        height := (nrows - start) // (num_workers - i)

        if height /= 0 then
          create worker.make (start + 1, start + height, ncols, seed,
                              max, histogram, vs, xs, ys,
                              winnow_xs, winnow_ys,
                              result_vector)
          Result.extend(worker)
        end
          
        start := start + height
        i := i + 1
      end

      -- parallel for on rows
      liva_all (workers)
    end

  live_all (workers: LINKED_LIST [separate WORKER])
    do
      -- Randmat creation
      from workers.start
      until workers.after
      loop
          live_randmat (workers.item)
          workers.forth
      end


      -- Threshold discovery
      from workers.start
      until workers.after
      loop
          live_thresh_reduce (workers.item)
          workers.forth
      end

      process_histogram (max, histogram)

      from workers.start
      until workers.after
      loop
          live_thresh_map (workers.item)
          workers.forth
      end

      -- Winnow point collection and sorting
      from workers.start
      until workers.after
      loop
          live_winnow (workers.item)
          workers.forth
      end

      sort_winnow (import_winnow (vs, xs, ys)) 

      -- Outer processing
      from workers.start
      until workers.after
      loop
          live_outer (workers.item)
          workers.forth
      end

      -- Matrix-vector product
      from workers.start
      until workers.after
      loop
          live_product (workers.item)
          workers.forth
      end
    end

  process_historgram (a_max: separate ARRAY [INTEGER];
                       a_histogram: separate ARRAY [INTEGER])
    require
      a_max.generator /= Void and a_histogram.generator /= Void
    local
      count: INTEGER
      nmax: INTEGER
      prefixsum: INTEGER
      i: INTEGER
      h: INTEGER
    do
      nmax := a_max.item (1)
      count := (nrows * ncols * percent) // 100

      prefixsum := 0
      threshold := nmax

      from i := nmax
      until i < 0 or prefixsum > count
      loop
      	h := a_histogram.item (i)
        prefixsum := prefixsum + h
        threshold := i;
        i := i - 1
      end
    end

  sort_winnow (points_: ARRAY [TUPLE [x,y,z: INTEGER]]
               winnow_xs_, winnow_ys_: separate ARRAY [INTEGER])
    local
      points: ARRAY [TUPLE [v,x,y: INTEGER]]
      trim_points: ARRAY [TUPLE [v,x,y: INTEGER]]

      sorter: TUPLE_SORTER

      i, n, chunk, index: INTEGER
    do
      create sorter.make
      
      points := sorter.sort (points_)

      n     := points.count
      chunk := n // winnow_nelts

      create points.make (1, winnow_nelts)
      from i := 1
      until i > winnow_nelts
      loop
        index := (i - 1) * chunk + 1
        winnow_xs_ [i] := points [index].x
        winnow_ys_ [i] := points [index].y
        i := i + 1
      end
    end

feature -- Living routines
  live_randmat (worker: separate WORKER)
    do
      worker.live_randmat
    end

  live_thresh_reduce (worker: separate WORKER)
    do
      worker.live_thresh_reduce
    end

  live_thresh_map (worker: separate WORKER)
    do
      worker.live_thresh_map
    end

  live_winnow (worker: separate WORKER)
    do
        worker.live_winnow
    end

  live_outer (worker: separate WORKER)
    do
      worker.live_outer
    end

  live_product (worker: separate WORKER)
    do
      worker.live_product
    end

  join (obj: separate WORKER)
    require obj.generator /= Void
    do
    end
  
  fetch_vector (nelts: INTEGER; s_vector: separate ARRAY[DOUBLE]):
      ARRAY [DOUBLE]
    local
      i: INTEGER
    do
      create Result.make (1, nelts)
      from i := 1
      until i > nelts
      loop
        Result [i] := s_vector [i]
        i := i + 1
      end
    end
  

  live_worker(worker: separate WORKER)
    do
      worker.live
    end

feature {NONE}
  in: PLAIN_TEXT_FILE
  vs, xs, ys: separate ARRAY [INTEGER]
  winnow_xs, winnow_ys: separate ARRAY [INTEGER]
  histogram: separate ARRAY [INTEGER]
  max: separate ARRAY [INTEGER]
  result_vector: separate ARRAY [DOUBLE]

end
