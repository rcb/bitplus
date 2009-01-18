empty_test() ->
    Empty = bitplus:empty(),
    #bitplus{data=_E} = Empty,
    64 = bitplus:size_compressed(Empty),
    0 = bitplus:size_decompressed(Empty).
