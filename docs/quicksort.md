Quicksort/Mergesort comparison
==============================

Hits per level per element:

| Sorting hits      | Frequency | Hits per op | Total hits per level | Levels | Total hits | Grand Total |
|-------------------|-----------|-------------|----------------------|--------|------------|-------------|
| Mergesort Compare | 1         | 2           | 2                    |        |            |             |
| Mergesort Copy    | 1         | 2           | 2                    | lg n   | 4 lg n     | 5.76 ln n   |
| Quicksort Compare | 1         | 1*          | 1                    |        |            |             |
| Quicksort Swap    | 1/6       | 4           | 2/3                  | 2 ln n | 10/3 ln n  | 4.33 ln n   |          

Note that this is for classic (single-pivot) Quicksort and mergesort with the
"no-copy" optimization, whereby we flip the order of the arrays at each level.

* This assumes that the pivot is in a register (does not have to access the array).