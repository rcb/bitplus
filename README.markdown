bitplus
=======

bitplus is an Erlang library which offers an efficient bitvector/bitmap implementation. Bitmaps are stored internally in compressed forms and all operations on the bitmaps themselves including set(), get(), append(), logical operations (AND, OR, NOT) are performed directly on the compressed versions without the need for decompression.

### WAH compression ###

Compression is performed using the Word-Aligned Hybrid compression method. It's similar to run-length encoding except that it honors word boundaries. For more on WAH compression, read [An Efficient Compression Scheme for Bitmap Indices (2004)](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.59.1746).

FIXME: 
    * cannot assume 32-bit words (this depends on the machine).
