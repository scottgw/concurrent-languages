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
      nrows, ncols, s: INTEGER
      is_bench: BOOLEAN
      arg: STRING_8
      i, j: INTEGER
      workers: LINKED_LIST[separate RANDMAT_PARFOR_WORKER]
    do
      create in.make_open_read(separate_character_option_value('i'))
      arg := separate_character_option_value('e')
      is_bench := False
      if arg /= Void then
        is_bench := arg.is_equal("is_bench")
      end

      nrows := read_integer
      ncols := read_integer
      s := read_integer

      create matrix.make (nrows,ncols)
      print ("BOO1")
      workers := randmat(nrows, ncols, s)

      if not is_bench then
        print ("BOO2")
        -- workers.do_all (agent fetch_submatrix (ncols, ?))
        fetch_workers (ncols, workers)
        print ("BOO3")
        join_workers (workers)
        -- workers.do_all (agent join)
        print ("BOO4")        
        from i := 1
        until i > nrows
        loop
          from j := 1
          until j > ncols
          loop
            print (matrix [i,j].out + " ")
            j := j + 1
          end
          
          print ("%N")
          i := i + 1
        end
      end
    end

  matrix: ARRAY2[INTEGER]
  
  read_integer(): INTEGER
    do
      in.read_integer
      Result := in.last_integer
    end

  num_workers: INTEGER = 32
  
  -- parallel for on matrix
  randmat(nrows, ncols, seed: INTEGER):
    LINKED_LIST[separate RANDMAT_PARFOR_WORKER]
    local
      worker: separate RANDMAT_PARFOR_WORKER
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

        create worker.make (start + 1, height, ncols, seed + start + 1)
        Result.extend(worker)

        start := start + height
        i := i + 1
      end

      -- parallel for on rows
      Result.do_all(agent launch_parfor_worker)
    end

  join_workers (workers: LINKED_LIST [separate RANDMAT_PARFOR_WORKER])
    do
      from workers.start
      until workers.after
      loop
        join (workers.item)
        workers.forth
      end
    end
  
  join (obj: separate RANDMAT_PARFOR_WORKER)
    require obj.generator /= Void
    do
    end

  fetch_workers (ncols: INTEGER;
                workers: LINKED_LIST [separate RANDMAT_PARFOR_WORKER])
    do
      from workers.start
      until workers.after
      loop
        fetch_submatrix (ncols, workers.item)
        workers.forth
      end
    end

  
  fetch_submatrix (ncols: INTEGER;
                   worker: separate RANDMAT_PARFOR_WORKER)
    local
      i, j: INTEGER
      iend: INTEGER
    do
      from
        i := worker.start
        iend := i + worker.height
      until i >= iend
      loop
        from j := 1
        until j > ncols
        loop
          matrix [i,j] := worker.get (i, j) -- matrix [i,j]
          j := j + 1
        end
        i := i + 1
      end
    end
  

  launch_parfor_worker(worker: separate RANDMAT_PARFOR_WORKER)
    do
      worker.live
    end

feature {NONE}
  in: PLAIN_TEXT_FILE

end
