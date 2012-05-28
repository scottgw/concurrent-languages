class RANDMAT_PARFOR_AGGREGATOR
create make
feature
  make (n_: INTEGER)
  local
  do
    n := n_
    count := 0
  end

feature
  put
  do
    count := count + 1
    --print("put -> missing " + (n - count).out + "%N")
  end

  is_all_done(): BOOLEAN
  do
    -- print("is_all_done -> missing " + (n - count).out)
    Result := count = n
    if Result then
      --print("is_all_done%N")
    end
  end

feature {NONE}
  n, count: INTEGER

end
