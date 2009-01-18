empty_test() ->
    Empty = bitplus:empty(),
    #bitplus{data=_E} = Empty,
    64 = bitplus:size_compressed(Empty),
    0 = bitplus:size_decompressed(Empty).

compress_decompress_test() ->
    % Input: 128-bit vector - 1,20*0,3*1,79*0,25*1 
    BinA = <<2#10000000000000000000011100000000000000000000000000000000000000000000000000000000000000000000000000000001111111111111111111111111:128>>,
    BA = bitplus:compress(BinA),
    BinA = bitplus:decompress(BA).

and_test() ->
    % Input: two 128-bit vectors
    BinA = <<16#40000380:31, 16#00000000:31, 16#00000000:31, 16#001FFFFF:31, 16#F:4>>,
    BinB = <<16#7FFFFFFF:31, 16#7FFFFFFF:31, 16#7C0001E0:31, 16#3FE00000:31, 16#3:4>>,
    % Expected output: 128-bit vector
    BinC = <<16#40000380:31, 16#00000000:31, 16#00000000:31, 16#00000000:31, 16#3:4>>,
    
    BA = bitplus:compress(BinA),
    BB = bitplus:compress(BinB),
    BC = bitplus:logical_and(BA, BB),
    BinC = bitplus:decompress(BC).

or_test() ->
    % Input: two 128-bit vectors
    BinA = <<16#40000380:31, 16#00000000:31, 16#00000000:31, 16#001FFFFF:31, 16#F:4>>,
    BinB = <<16#7FFFFFFF:31, 16#7FFFFFFF:31, 16#7C0001E0:31, 16#3FE00000:31, 16#3:4>>,
    % Expected output: 128-bit vector
    BinC = <<16#7FFFFFFF:31, 16#7FFFFFFF:31, 16#7C0001E0:31, 16#3FFFFFFF:31, 16#F:4>>,
    
    BA = bitplus:compress(BinA),
    BB = bitplus:compress(BinB),
    BC = bitplus:logical_or(BA, BB),
    BinC = bitplus:decompress(BC).

get_test() ->
    Bin = <<2#10000000000000000000011100000000000000000000000000000000000000000000000000000000000000000000000000000001111111111111111111111111:128>>,
    B = bitplus:compress(Bin),
    1 = bitplus:get(B, 1), % get the 1st bit of bitvector
    0 = bitplus:get(B, 2),
    0 = bitplus:get(B, 75),
    1 = bitplus:get(B, 128),
    ?assertError(function_clause, bitplus:get(B, 1000)). % there's no 1000th bit.

set_test() ->
    Bin = <<2#10000000000000000000011100000000000000000000000000000000000000000000000000000000000000000000000000000001111111111111111111111111:128>>,
    Bin1 = <<2#10000000000000000000011100000000000001000000000000000000000000000000000000000000000000000000000000000001111111111111111111111111:128>>, % Bin1 = changed 38th bit of Bin to 1
    B = bitplus:compress(Bin),
    B1 = bitplus:set(B, 38, 1), % set 38th bit of B to 1
    Bin1 = bitplus:decompress(B1),
    B = bitplus:set(B1, 38, 0). % set it back

append_test() ->
    Bin = <<2#10000000000000000000011100000000000000000000000000000000000000000000000000000000000000000000000000000001111111111111111111111111:128>>,
    Bin1 = <<2#100000000000000000000111000000000000000000000000000000000000000000000000000000000000000000000000000000011111111111111111111111110:129>>,
    Bin2 = <<2#1000000000000000000001110000000000000000000000000000000000000000000000000000000000000000000000000000000111111111111111111111111101:130>>,
    B = bitplus:compress(Bin),
    B1 = bitplus:append(B, 0),
    Bin1 = bitplus:decompress(B1),
    B2 = bitplus:append(B1, 1),
    Bin2 = bitplus:decompress(B2).

append_to_empty_test() ->
    Empty = bitplus:empty(),
    B1 = bitplus:append(Empty, 0),
    1 = bitplus:size_decompressed(B1),
    <<2#0:1>> = bitplus:decompress(B1),
    <<2#01:2>> = bitplus:decompress(bitplus:append(B1, 1)).

large_vector_test() ->
    B = random_bitmap(5000),
    5000 = bitplus:size_decompressed(B),
    B = bitplus:logical_and(B, B),
    B = bitplus:logical_or(B, B).

random_bitmap(N) ->
    random_bitmap(N, bitplus:empty()).
random_bitmap(0, B) -> B;
random_bitmap(N, B) ->
    B1 = bitplus:append(B, round(random:uniform())),
    random_bitmap(N-1, B1).
