-- chain: chain all problems
--
-- input:
--   nelts: the number of elements
--   randmat_seed: random number generator seed
--   thresh_percent: percentage of cells to retain
--   winnow_nelts: the number of poitns to select
--
-- output:
--   res: a real vector, whose values are the result of the final product

class MAIN
inherit ARGUMENTS
create make
feature
  make
  local
    nelts, randmat_seed, thresh_percent, winnow_nelts: INTEGER
    randmat_matrix, thresh_mask: ARRAY2[INTEGER]
    winnow_points: ARRAY[TUPLE[INTEGER, INTEGER]]
    outer_matrix: ARRAY2[DOUBLE]
    outer_vector: ARRAY[DOUBLE]
    product_result: ARRAY[DOUBLE]
    randmat: RANDMAT
    thresh: THRESH
    winnow: WINNOW
    outer: OUTER
    product: PRODUCT
    i: INTEGER
  do
    create in.make_open_read(separate_character_option_value('i'))

    nelts := read_integer
    randmat_seed := read_integer
    thresh_percent := read_integer
    winnow_nelts := read_integer

    create randmat_matrix.make(nelts, nelts)
    create thresh_mask.make(nelts, nelts)
    create outer_matrix.make(winnow_nelts, winnow_nelts)
    create outer_vector.make(1, winnow_nelts)

    create randmat.make_empty
    create thresh.make_empty
    create winnow.make_empty
    create outer.make_empty
    create product.make_empty

    randmat.randmat(nelts, nelts, randmat_seed, randmat_matrix)
    thresh.thresh(nelts, nelts, randmat_matrix, thresh_percent, thresh_mask)
    winnow_points := winnow.winnow(nelts, nelts, randmat_matrix,
        thresh_mask, winnow_nelts)
    outer.outer(winnow_nelts, winnow_points, outer_matrix, outer_vector)
    product_result := product.product(winnow_nelts, outer_matrix,
        outer_vector)


    from i :=  1
    until i > nelts
    loop
      print (product_result [i].out + " ")
      i := i + 1
    end
    print("%N")
  end

  read_integer(): INTEGER
  do
    in.read_integer
    Result := in.last_integer
  end

feature {NONE}
  in: PLAIN_TEXT_FILE

end -- class MAIN

