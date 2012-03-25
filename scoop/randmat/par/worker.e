class
	WORKER

create
	make_with_matrix

feature -- Initialization

	make_with_matrix (a_matrix: separate ARRAY2[INTEGER]; a_ncols: INTEGER;
      seed: INTEGER; a_row: INTEGER)
    local
      nseed: INTEGER
		do
			matrix := a_matrix
      ncols := a_ncols
      row := a_row
      nseed := seed + a_row
			create rand.set_seed(nseed);
		end

feature -- Basic operations

	fill
    local
      j: INTEGER
		do
      print("fill row " + row.out + "%N")
      from
        j := 1
      until
        j > ncols
      loop
        rand.forth
        put(matrix, rand.item, row, j)
        j := j + 1
      end
    end

feature {NONE}

	put (a_matrix: separate ARRAY2[INTEGER]; an_element: INTEGER;
       a_row: INTEGER; col: INTEGER)
		do
			a_matrix.put(an_element, a_row, col)
		end

feature {NONE} -- Implementation

	matrix: separate ARRAY2[INTEGER]
  ncols: INTEGER
  row: INTEGER
	rand: RANDOM

end -- class WORKER
