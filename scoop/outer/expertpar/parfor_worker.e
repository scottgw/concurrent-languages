class
  PARFOR_WORKER

create
  make

feature
  make (start_, final_, nelts_: INTEGER)
    do
      start := start_
      final := final_
      nelts := nelts_
      -- points := points_
      -- result_vector := result_vector_
      -- result_matrix := result_matrix_
    end

feature
  set_it_up (points_: separate ARRAY[TUPLE[INTEGER, INTEGER]];
             result_vector_: separate ARRAY[DOUBLE];
             result_matrix_: separate SARRAY2[DOUBLE])
    do
      points := points_
      -- print ("setting points: " + start.out + " -> " + (points /= Void).out + "%N")
            
      result_vector := result_vector_
      result_matrix := result_matrix_
    end
  
  live
    do
      print ("prefetch%N")
      get_result(fetch_array (points))
    end


  fetch_array (a_sep_array: separate ARRAY[TUPLE[x,y: INTEGER]]):
      ARRAY [TUPLE[INTEGER, INTEGER]]
    require
      a_sep_array.generator = a_sep_array.generator
    local
      i: INTEGER
      x, y: INTEGER
    do
      print ("in fetch%N")
      create Result.make (1, nelts)

      from i := 1
      until i > nelts
      loop
        -- SCOOP bug: this doesn't work if I don't store these into
        -- local variables explicitly.
        x := a_sep_array.item (i).x -- integer_32_item (1)
        y := a_sep_array.item (i).y;
        -- print (x.out + "," + y.out + "%N") -- .do_nothing
        Result [i] := [x, y]
        i := i + 1
      end
    end

  to_local_row (i: INTEGER): INTEGER
    do
      Result := i - start + 1
    end

  
  get_result(a_points: ARRAY[TUPLE[x,y: INTEGER]])
    local
      nmax: DOUBLE
      d: DOUBLE
      p1, p2: TUPLE [x,y : INTEGER]
      i, j: INTEGER
      matrix: ARRAY2[DOUBLE]
      vector: ARRAY [DOUBLE]
    do
      create matrix.make (to_local_row (final), nelts)
      create vector.make (1, to_local_row (final))

      from i := start
      until i > final
      loop
        nmax := -1
        p1 := a_points [i]
        from j := 1
        until j > nelts
        loop
          if i /= j then
            p2 := a_points [j]
            d := distance (p1, p2)
            matrix [to_local_row (i), j] := d
            nmax := nmax.max (d)
          end
          j := j + 1
        end
        matrix [to_local_row (i), i] := nmax * nelts
        vector [to_local_row (i)] := distance ( [0,0], a_points [i])
        i := i + 1
      end

      set_result_vector (vector, result_vector)
      set_result_matrix (matrix, result_matrix)
    end

  set_result_matrix (mat: ARRAY2[DOUBLE]; smat: separate SARRAY2[DOUBLE])
    require
      smat.generator /= Void
    local
      i, j: INTEGER
    do
      from i := start
      until i > final
      loop
        from j := 1
        until j > nelts
        loop
          smat.put (mat [to_local_row (i), j], i, j)
         
          -- print (mat [to_local_row (i), j].out + " -> " + smat.item (i, j).out + "%N")
          j := j + 1
        end
        i := i + 1
      end

    end

  set_result_vector (vec: ARRAY[DOUBLE]; svec: separate ARRAY[DOUBLE])
    local
      i, j: INTEGER
    do
      from i := start
      until i > final
      loop
        svec [i] := vec [to_local_row (i)]
        svec [i].do_nothing
        i := i + 1
      end

    end

  sqr(a: DOUBLE): DOUBLE
    do
      Result := a * a
    end
  
  distance(a, b: TUPLE[x,y: INTEGER]): DOUBLE
    do
      Result := {DOUBLE_MATH}.sqrt(sqr(a.x - b.x) + sqr(a.y - b.y));
    end

feature {NONE}
  start, final, nelts: INTEGER
  result_vector: separate ARRAY[DOUBLE]
  result_matrix: separate SARRAY2[DOUBLE]
  points: separate ARRAY[TUPLE[INTEGER, INTEGER]]

end
