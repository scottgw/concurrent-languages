class EX
create make
feature
  make
  do
    get_result() -- Should fail!
  end

feature
  is_all_done: BOOLEAN
  do
    Result := False
    print("inside is_all_done = " + Result.out + "%N") -- False
  end

  get_result
  require
    is_all_done -- False
  do
    print("is_all_done() = " + is_all_done().out + "%N") -- False
  end
end
