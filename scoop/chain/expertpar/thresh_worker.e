class PARFOR_WORKER
create make
feature
  make (start_, final_: INTEGER;
        ncols_, nelts_: INTEGER;
        matrix_array_, mask_array_: separate ARRAY2[INTEGER];
        v_vector_, x_vector_, y_vector_: separate ARRAY [INTEGER])
    do
      start := start_
      final := final_
      ncols := ncols_
      nelts := nelts_
      matrix_array := matrix_array_
      mask_array := mask_array_
      v_vector := v_vector_
      x_vector := x_vector_
      y_vector := y_vector_
    end

feature
  live
    do
      get_result(fetch_array (matrix_array),
                 fetch_array (mask_array))
    end


  fetch_array (a_sep_array: separate ARRAY2[INTEGER]): ARRAY2 [INTEGER]
    require
      a_sep_array.generator /= Void
    local
      i, j: INTEGER
      e: INTEGER
    do
      create Result.make_filled (0, final - start + 1, ncols)

      from i := start
      until i > final
      loop
        from j := 1
        until j > ncols
        loop
          e := a_sep_array.item (i, j)
          Result [to_local_row (i), j] := e
          j := j + 1
        end
        i := i + 1
      end
    end
  
  
  to_local_row (i: INTEGER): INTEGER
    do
      Result := i - start + 1
    end
  
  get_result(a_matrix, a_mask: ARRAY2[INTEGER])
    local
      vector: ARRAY [TUPLE[INTEGER, INTEGER, INTEGER]]
      i, j: INTEGER
      count: INTEGER
    do
      create vector.make_empty

      from
        count := 1
        i := start
      until
        i > final
      loop
        from j := 1
        until j > ncols
        loop
          if a_mask [to_local_row (i), j] = 1 then
            vector.force ([a_matrix [to_local_row (i), j], i, j], count)
            count := count + 1
          end
          j := j + 1
        end
        i := i + 1
      end

      put_vector (vector, v_vector, x_vector, y_vector)
    end

  put_vector (a_vector: ARRAY [TUPLE[v,x,y: INTEGER]];
              vs, xs, ys: separate ARRAY [INTEGER])
    local
      i: INTEGER
      n: INTEGER
      t: TUPLE [v,x,y: INTEGER]
    do
      n := xs.count
      from i := 1
      until i > a_vector.count
      loop
        t := a_vector [i]
        vs.force (t.v, n + i)
        xs.force (t.x, n + i)
        ys.force (t.y, n + i)
        i := i + 1
      end
    end
              
  
feature {NONE}
  start, final: INTEGER
  nelts: INTEGER
  ncols: INTEGER
  matrix_array, mask_array: separate ARRAY2[INTEGER]
  v_vector, x_vector, y_vector: separate ARRAY [INTEGER]

end
