class THRESH_REDUCE2D_READER
create make
feature
  make
  do
  end

feature
  get_result(aggregator: separate THRESH_REDUCE2D_AGGREGATOR): INTEGER
  require
    aggregator.is_all_done
  do
    Result := aggregator.get_result()
  end
end
