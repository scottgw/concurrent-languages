class WINNOW_PARFOR_READER
create make
feature
  make
  do
  end

feature
  get_result(aggregator: separate WINNOW_PARFOR_AGGREGATOR)
      : separate ARRAY[TUPLE[INTEGER, INTEGER, INTEGER]]
  require
    aggregator.is_all_done
  do
    Result := aggregator.get_result
  end
end
